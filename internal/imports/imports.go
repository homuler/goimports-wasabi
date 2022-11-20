// Copyright 2013 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:generate go run mkstdlib.go

// Package imports implements a Go pretty-printer (like package "go/format")
// that also adds or removes import statements as necessary.
package imports

import (
	"bytes"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"strings"
)

// Options is golang.org/x/tools/imports.Options with extra internal-only options.
type Options struct {
	Env *ProcessEnv // The environment to use. Note: this contains the cached module and filesystem state.

	// LocalPrefix is a comma-separated string of import path prefixes, which, if
	// set, instructs Process to sort the import paths with the given prefixes
	// into another group after 3rd-party packages.
	LocalPrefix string

	Fragment       bool // Accept fragment of a source file (no package statement)
	AllErrors      bool // Report all errors (not just the first 10 on different lines)
	ReconstructAST bool // Reconstruct AST after rewriting the source code (for testing purposes)

	Comments  bool // Print comments (true if nil *Options provided)
	TabIndent bool // Use tabs for indent (true if nil *Options provided)
	TabWidth  int  // Tab width (8 if nil *Options provided)

	FormatOnly bool // Disable the insertion and deletion of imports
}

// Process implements golang.org/x/tools/imports.Process with explicit context in opt.Env.
func Process(filename string, src []byte, opt *Options) (formatted []byte, err error) {
	fileSet := token.NewFileSet()
	file, adjust, psrc, err := parse(fileSet, filename, src, opt)
	if err != nil {
		return nil, err
	}

	sf := newSourceFile(psrc, fileSet, file, opt)
	if !opt.FormatOnly {
		if err := fixImports(sf, filename); err != nil {
			return nil, err
		}
	}
	return formatFile(sf, src, adjust)
}

// FixImports returns a list of fixes to the imports that, when applied,
// will leave the imports in the same state as Process. src and opt must
// be specified.
//
// Note that filename's directory influences which imports can be chosen,
// so it is important that filename be accurate.
func FixImports(filename string, src []byte, opt *Options) (fixes []*ImportFix, err error) {
	fileSet := token.NewFileSet()
	file, _, _, err := parse(fileSet, filename, src, opt)
	if err != nil {
		return nil, err
	}

	return getFixes(fileSet, file, filename, opt.Env)
}

// ApplyFixes applies all of the fixes to the file and formats it. extraMode
// is added in when parsing the file. src and opts must be specified, but no
// env is needed.
func ApplyFixes(fixes []*ImportFix, filename string, src []byte, opt *Options, extraMode parser.Mode) (formatted []byte, err error) {
	// Don't use parse() -- we don't care about fragments or statement lists
	// here, and we need to work with unparseable files.
	fileSet := token.NewFileSet()
	parserMode := parser.Mode(0)
	if opt.Comments {
		parserMode |= parser.ParseComments
	}
	if opt.AllErrors {
		parserMode |= parser.AllErrors
	}
	parserMode |= extraMode

	file, err := parser.ParseFile(fileSet, filename, src, parserMode)
	if file == nil {
		return nil, err
	}

	sf := newSourceFile(src, fileSet, file, opt)
	// Apply the fixes to the file.
	apply(sf, fixes)

	return formatFile(sf, src, nil)
}

// formatFile formats the file syntax tree.
// It may mutate the token.FileSet.
//
// If an adjust function is provided, it is called after formatting
// with the original source (formatFile's src parameter) and the
// formatted file, and returns the postpocessed result.
func formatFile(sf *SourceFile, orig []byte, adjust func(orig []byte, src []byte) []byte) ([]byte, error) {
	if err := sf.squashImportDecls(); err != nil {
		return nil, err
	}

	src := sf.src
	if adjust != nil {
		src = adjust(orig, sf.src)
	}

	out, err := format.Source(src)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// parse parses src, which was read from filename,
// as a Go source file or statement list.
func parse(fset *token.FileSet, filename string, src []byte, opt *Options) (*ast.File, func(orig, src []byte) []byte, []byte, error) {
	parserMode := parser.Mode(0)
	if opt.Comments {
		parserMode |= parser.ParseComments
	}
	if opt.AllErrors {
		parserMode |= parser.AllErrors
	}

	// Try as whole source file.
	file, err := parser.ParseFile(fset, filename, src, parserMode)
	if err == nil {
		return file, nil, src, nil
	}
	// If the error is that the source file didn't begin with a
	// package line and we accept fragmented input, fall through to
	// try as a source fragment.  Stop and return on any other error.
	if !opt.Fragment || !strings.Contains(err.Error(), "expected 'package'") {
		return nil, nil, nil, err
	}

	// If this is a declaration list, make it a source file
	// by inserting a package clause.
	// Insert using a ;, not a newline, so that parse errors are on
	// the correct line.
	const prefix = "package main;"
	psrc := append([]byte(prefix), src...)
	file, err = parser.ParseFile(fset, filename, psrc, parserMode)
	if err == nil {
		// Gofmt will turn the ; into a \n.
		// Do that ourselves now and update the file contents,
		// so that positions and line numbers are correct going forward.
		psrc[len(prefix)-1] = '\n'
		fset.File(file.Package).SetLinesForContent(psrc)

		// If a main function exists, we will assume this is a main
		// package and leave the file.
		if containsMainFunc(file) {
			return file, nil, psrc, nil
		}

		adjust := func(orig, src []byte) []byte {
			// Remove the package clause.
			src = src[len(prefix):]
			return matchSpace(orig, src)
		}
		return file, adjust, psrc, nil
	}
	// If the error is that the source file didn't begin with a
	// declaration, fall through to try as a statement list.
	// Stop and return on any other error.
	if !strings.Contains(err.Error(), "expected declaration") {
		return nil, nil, nil, err
	}

	// If this is a statement list, make it a source file
	// by inserting a package clause and turning the list
	// into a function body.  This handles expressions too.
	// Insert using a ;, not a newline, so that the line numbers
	// in fsrc match the ones in src.
	fsrc := append(append([]byte("package p; func _() {"), src...), '}')
	file, err = parser.ParseFile(fset, filename, fsrc, parserMode)
	if err == nil {
		adjust := func(orig, src []byte) []byte {
			// Remove the wrapping.
			// Gofmt has turned the ; into a \n\n.
			src = src[len("package p\n\nfunc _() {"):]
			src = src[:len(src)-len("}\n")]
			// Gofmt has also indented the function body one level.
			// Remove that indent.
			src = bytes.Replace(src, []byte("\n\t"), []byte("\n"), -1)
			return matchSpace(orig, src)
		}
		return file, adjust, fsrc, nil
	}

	// Failed, and out of options.
	return nil, nil, nil, err
}

// containsMainFunc checks if a file contains a function declaration with the
// function signature 'func main()'
func containsMainFunc(file *ast.File) bool {
	for _, decl := range file.Decls {
		if f, ok := decl.(*ast.FuncDecl); ok {
			if f.Name.Name != "main" {
				continue
			}

			if len(f.Type.Params.List) != 0 {
				continue
			}

			if f.Type.Results != nil && len(f.Type.Results.List) != 0 {
				continue
			}

			return true
		}
	}

	return false
}

func cutSpace(b []byte) (before, middle, after []byte) {
	i := 0
	for i < len(b) && (b[i] == ' ' || b[i] == '\t' || b[i] == '\n') {
		i++
	}
	j := len(b)
	for j > 0 && (b[j-1] == ' ' || b[j-1] == '\t' || b[j-1] == '\n') {
		j--
	}
	if i <= j {
		return b[:i], b[i:j], b[j:]
	}
	return nil, nil, b[j:]
}

// matchSpace reformats src to use the same space context as orig.
//  1. If orig begins with blank lines, matchSpace inserts them at the beginning of src.
//  2. matchSpace copies the indentation of the first non-blank line in orig
//     to every non-blank line in src.
//  3. matchSpace copies the trailing space from orig and uses it in place
//     of src's trailing space.
func matchSpace(orig []byte, src []byte) []byte {
	before, _, after := cutSpace(orig)
	i := bytes.LastIndex(before, []byte{'\n'})
	before, indent := before[:i+1], before[i+1:]

	_, src, _ = cutSpace(src)

	var b bytes.Buffer
	b.Write(before)
	for len(src) > 0 {
		line := src
		if i := bytes.IndexByte(line, '\n'); i >= 0 {
			line, src = line[:i+1], line[i+1:]
		} else {
			src = nil
		}
		if len(line) > 0 && line[0] != '\n' { // not blank
			b.Write(indent)
		}
		b.Write(line)
	}
	b.Write(after)
	return b.Bytes()
}
