// Copyright 2013 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:generate go run mkstdlib.go

// Package imports implements a Go pretty-printer (like package "go/format")
// that also adds or removes import statements as necessary.
package imports

import (
	"go/parser"
	"go/token"
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

	FormatOnly      bool        // Disable the insertion and deletion of imports
	ExtraParserMode parser.Mode // Parser mode
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
	return formatFile(sf, adjust)
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
func ApplyFixes(fixes []*ImportFix, filename string, src []byte, opt *Options) (formatted []byte, err error) {
	// Don't use parse() -- we don't care about fragments or statement lists
	// here, and we need to work with unparseable files.
	fileSet := token.NewFileSet()
	file, _, _, err := parse(fileSet, filename, src, opt)
	if file == nil {
		return nil, err
	}

	sf := newSourceFile(src, fileSet, file, opt)
	// Apply the fixes to the file.
	apply(sf, fixes)

	return formatFile(sf, nil)
}

// formatFile formats the file syntax tree.
// It may mutate the token.FileSet.
//
// If an adjust function is provided, it is called after formatting
// with the original source (formatFile's src parameter) and the
// formatted file, and returns the postpocessed result.
func formatFile(sf *SourceFile, adjust func(src []byte) []byte) ([]byte, error) {
	if err := sf.squashImportDecls(); err != nil {
		return nil, err
	}
	return sf.format(adjust)
}
