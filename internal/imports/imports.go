// Copyright 2013 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:generate go run mkstdlib.go

// Package imports implements a Go pretty-printer (like package "go/format")
// that also adds or removes import statements as necessary.
package imports

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/printer"
	"go/token"
	"sort"
	"strings"
)

// Options is golang.org/x/tools/imports.Options with extra internal-only options.
type Options struct {
	Env *ProcessEnv // The environment to use. Note: this contains the cached module and filesystem state.

	// LocalPrefix is a comma-separated string of import path prefixes, which, if
	// set, instructs Process to sort the import paths with the given prefixes
	// into another group after 3rd-party packages.
	LocalPrefix string

	Fragment  bool // Accept fragment of a source file (no package statement)
	AllErrors bool // Report all errors (not just the first 10 on different lines)

	Comments  bool // Print comments (true if nil *Options provided)
	TabIndent bool // Use tabs for indent (true if nil *Options provided)
	TabWidth  int  // Tab width (8 if nil *Options provided)

	FormatOnly bool // Disable the insertion and deletion of imports
}

// Process implements golang.org/x/tools/imports.Process with explicit context in opt.Env.
func Process(filename string, src []byte, opt *Options) (formatted []byte, err error) {
	fileSet := token.NewFileSet()
	file, adjust, err := parse(fileSet, filename, src, opt)
	if err != nil {
		return nil, err
	}

	if !opt.FormatOnly {
		if err := fixImports(fileSet, file, filename, opt.Env); err != nil {
			return nil, err
		}
	}
	return formatFile(fileSet, file, src, adjust, opt)
}

// formatFile formats the file syntax tree.
// It may mutate the token.FileSet.
//
// If an adjust function is provided, it is called after formatting
// with the original source (formatFile's src parameter) and the
// formatted file, and returns the postpocessed result.
func formatFile(fset *token.FileSet, file *ast.File, src []byte, adjust func(orig []byte, src []byte) []byte, opt *Options) ([]byte, error) {
	sf := newSourceFile(src, fset, file, opt)
	if err := sf.squashImportDecls(); err != nil {
		return nil, err
	}

	printerMode := printer.UseSpaces
	if opt.TabIndent {
		printerMode |= printer.TabIndent
	}
	printConfig := &printer.Config{Mode: printerMode, Tabwidth: opt.TabWidth}

	var buf bytes.Buffer
	if err := printConfig.Fprint(&buf, sf.fileSet, sf.astFile); err != nil {
		return nil, err
	}
	out := buf.Bytes()
	if adjust != nil {
		out = adjust(src, out)
	}

	out, err := format.Source(out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// parse parses src, which was read from filename,
// as a Go source file or statement list.
func parse(fset *token.FileSet, filename string, src []byte, opt *Options) (*ast.File, func(orig, src []byte) []byte, error) {
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
		return file, nil, nil
	}
	// If the error is that the source file didn't begin with a
	// package line and we accept fragmented input, fall through to
	// try as a source fragment.  Stop and return on any other error.
	if !opt.Fragment || !strings.Contains(err.Error(), "expected 'package'") {
		return nil, nil, err
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
			return file, nil, nil
		}

		adjust := func(orig, src []byte) []byte {
			// Remove the package clause.
			src = src[len(prefix):]
			return matchSpace(orig, src)
		}
		return file, adjust, nil
	}
	// If the error is that the source file didn't begin with a
	// declaration, fall through to try as a statement list.
	// Stop and return on any other error.
	if !strings.Contains(err.Error(), "expected declaration") {
		return nil, nil, err
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
		return file, adjust, nil
	}

	// Failed, and out of options.
	return nil, nil, err
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

type sourceFile struct {
	src         []byte
	fileSet     *token.FileSet
	tokenFile   *token.File
	astFile     *ast.File
	options     *Options
	importDecls []*importDecl
}

func newSourceFile(src []byte, fileSet *token.FileSet, astFile *ast.File, options *Options) *sourceFile {
	tokenFile := fileSet.File(astFile.Pos())
	sf := sourceFile{src: src, fileSet: fileSet, tokenFile: tokenFile, astFile: astFile, options: options}
	idr := newImportDeclReader(src, tokenFile, astFile, options.LocalPrefix)

	for {
		decl, ok := idr.readNext()
		if !ok {
			break
		}
		sf.importDecls = append(sf.importDecls, decl)
	}
	return &sf
}

// sync synchronize astFile and tokenFile with importDecls.
// This will change astFile and tokenFile.
func (sf *sourceFile) sync() error {
	tokenFile := sf.tokenFile
	packageEnd := sf.astFile.Name.End()
	startPos := tokenFile.Pos(tokenFile.Size())
	endPos := token.NoPos

	for _, decl := range sf.importDecls {
		if decl.pos < startPos {
			startPos = decl.pos
		}
		if decl.end > endPos {
			endPos = decl.end
		}
	}

	// fixImports may insert import declarations at pos 1 and can cause a compile error.
	// If the character at packageEnd + 1 is not a newline nor a semicolon, it can fail to output a correct code.
	// TODO: fix fixImports
	if startPos < packageEnd {
		startPos = packageEnd + 1
	}
	if endPos < packageEnd {
		endPos = packageEnd + 1
	}

	start := tokenFile.Offset(startPos)
	end := tokenFile.Offset(endPos)

	sw := newSourceWriter(sf.tokenFile)
	sw.writeByte(sf.src[:start]...)

	for _, decl := range sf.importDecls {
		sw.writeImportDecl(decl)
		sw.writeNewline()
	}
	// delete the last newline
	if len(sf.importDecls) > 0 {
		sw.delete()
	}

	if sw.pos > endPos+1 {
		// If the total length of import declarations get larger than the original, reconstruct AST to correct other tokens' positions.
		sw.writeByte(sf.src[end:]...)
		sf.src = sw.output
		sf.fileSet = token.NewFileSet()
		file, err := parseFile(sf.fileSet, sf.tokenFile.Name(), sf.src, sf.options)

		if err != nil {
			return fmt.Errorf("failed to reconstruct AST: %w", err)
		}
		sf.astFile = file
		sf.tokenFile.SetLinesForContent(sf.src)
		return nil
	}

	for sw.pos < endPos {
		// TODO: is it correct?
		sw.writeNewline()
	}
	sw.writeByte(sf.src[end:]...)
	sf.src = sw.output
	sf.tokenFile.SetLinesForContent(sf.src)

	// Remove merged declarations and comments from AST.
	newDecls := make([]ast.Decl, 0, len(sf.astFile.Decls))
	mergedComments := make([]*ast.CommentGroup, 0)

	for _, decl := range sf.importDecls {
		if len(decl.doc) > 0 {
			decl.node.Doc = nil // reset Doc to sort comments
			for _, doc := range decl.doc {
				if decl.node.Doc == nil {
					decl.node.Doc = doc
					continue
				}
				decl.node.Doc.List = append(decl.node.Doc.List, doc.List...)
				mergedComments = append(mergedComments, doc)
			}
			decl.doc = []*ast.CommentGroup{decl.node.Doc}
		}

		decl.node.Specs = nil // reset Specs to sort specs
		for _, spec := range decl.specs {
			if len(spec.doc) > 0 {
				spec.node.Doc = nil // reset Doc to sort comments
				for _, doc := range spec.doc {
					if spec.node.Doc == nil {
						spec.node.Doc = doc
						continue
					}
					spec.node.Doc.List = append(spec.node.Doc.List, doc.List...)
					mergedComments = append(mergedComments, doc)
				}
				spec.doc = []*ast.CommentGroup{spec.node.Doc}
			}

			if len(spec.comment) > 0 {
				spec.node.Comment = nil // reset Comment to sort comments
				for _, doc := range spec.comment {
					if spec.node.Comment == nil {
						spec.node.Comment = doc
						continue
					}
					spec.node.Comment.List = append(spec.node.Comment.List, doc.List...)
					mergedComments = append(mergedComments, doc)
				}
				spec.comment = []*ast.CommentGroup{spec.node.Comment}
			}
			decl.node.Specs = append(decl.node.Specs, spec.node)
		}
		newDecls = append(newDecls, decl.node)
	}

	for _, d := range sf.astFile.Decls {
		if _, ok := astImportDecl(d); ok {
			continue
		}
		newDecls = append(newDecls, d)
	}
	sf.astFile.Decls = newDecls

	sort.Sort(byCommentPos(sf.astFile.Comments))
	if len(mergedComments) > 0 {
		cindex := 0
		for _, c := range mergedComments {
			for sf.astFile.Comments[cindex] != c {
				cindex++
			}
			sf.astFile.Comments = append(sf.astFile.Comments[:cindex], sf.astFile.Comments[cindex+1:]...)
		}
	}

	return nil
}

func (sf *sourceFile) squashImportDecls() error {
	var cdecls []*importDecl
	var decl *importDecl
	for _, d := range sf.importDecls {
		cs, d := d.distillCImports()
		cdecls = append(cdecls, cs...)
		if d != nil {
			if decl == nil {
				decl = d
			} else {
				decl.merge(d)
			}
		}
	}
	decls := make([]*importDecl, 0, len(cdecls)+1)
	decls = append(decls, cdecls...)
	if decl != nil {
		decl.dedupe()
		decls = append(decls, decl)
	}

	sf.importDecls = decls
	return sf.sync()
}

type byCommentPos []*ast.CommentGroup

func (x byCommentPos) Len() int           { return len(x) }
func (x byCommentPos) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }
func (x byCommentPos) Less(i, j int) bool { return x[i].Pos() < x[j].Pos() }
