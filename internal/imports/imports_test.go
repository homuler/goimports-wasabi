// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package imports

import (
	"bytes"
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"testing"

	"github.com/homuler/goimports-wasabi/internal/diff"
	"github.com/homuler/goimports-wasabi/internal/gocommand"
	"github.com/homuler/goimports-wasabi/internal/testenv"
	"github.com/stretchr/testify/assert"
)

func TestMain(m *testing.M) {
	testenv.ExitIfSmallMachine()
	os.Exit(m.Run())
}

type entry struct {
	source, golden string
}

const datadir = "testdata"

var data []entry = []entry{
	{source: "merge_001.input", golden: "merge_001.golden"},
	{source: "merge_002.input", golden: "merge_002.golden"},
	{source: "merge_003.input", golden: "merge_003.golden"},
	{source: "merge_004.input", golden: "merge_004.golden"},
	{source: "merge_005.input", golden: "merge_005.golden"},
	{source: "merge_006.input", golden: "merge_006.golden"},
	{source: "merge_007.input", golden: "merge_007.golden"},
	{source: "merge_008.input", golden: "merge_008.golden"},
	{source: "merge_009.input", golden: "merge_009.golden"},
	{source: "merge_010.input", golden: "merge_010.golden"},
	{source: "merge_011.input", golden: "merge_011.golden"},
	{source: "merge_012.input", golden: "merge_012.golden"},
	{source: "merge_013.input", golden: "merge_013.golden"},
	{source: "merge_014.input", golden: "merge_014.golden"},
	{source: "merge_015.input", golden: "merge_015.golden"},
	{source: "merge_101.input", golden: "merge_101.golden"},
	{source: "cgo_001.input", golden: "cgo_001.golden"},
	{source: "cgo_002.input", golden: "cgo_002.golden"},
	{source: "cgo_003.input", golden: "cgo_003.golden"},
	{source: "cgo_004.input", golden: "cgo_004.golden"},
	{source: "cgo_005.input", golden: "cgo_005.golden"},
	{source: "cgo_006.input", golden: "cgo_006.golden"},
	{source: "cgo_007.input", golden: "cgo_007.golden"},
	{source: "cgo_008.input", golden: "cgo_008.golden"},
	{source: "cgo_009.input", golden: "cgo_009.golden"},
	{source: "format_001.input", golden: "format_001.input"},
	{source: "format_002.input", golden: "format_002.input"},
	{source: "format_003.input", golden: "format_003.input"},
	{source: "format_004.input", golden: "format_004.input"},
	{source: "format_005.input", golden: "format_005.input"},
	{source: "merge_001.golden", golden: "merge_001.golden"},
	{source: "merge_002.golden", golden: "merge_002.golden"},
	{source: "merge_003.golden", golden: "merge_003.golden"},
	{source: "merge_004.golden", golden: "merge_004.golden"},
	{source: "merge_005.golden", golden: "merge_005.golden"},
	{source: "merge_006.golden", golden: "merge_006.golden"},
	{source: "merge_007.golden", golden: "merge_007.golden"},
	{source: "merge_008.golden", golden: "merge_008.golden"},
	{source: "merge_009.golden", golden: "merge_009.golden"},
	{source: "merge_010.golden", golden: "merge_010.golden"},
	{source: "merge_011.golden", golden: "merge_011.golden"},
	{source: "merge_012.golden", golden: "merge_012.golden"},
	{source: "merge_013.golden", golden: "merge_013.golden"},
	{source: "merge_014.golden", golden: "merge_014.golden"},
	{source: "merge_015.golden", golden: "merge_015.golden"},
	// {source: "merge_101.golden", golden: "merge_101.golden"}, fails due to a bug of go fmt. cf. https://github.com/golang/go/issues/24472
	{source: "cgo_001.golden", golden: "cgo_001.golden"},
	{source: "cgo_002.golden", golden: "cgo_002.golden"},
	{source: "cgo_003.golden", golden: "cgo_003.golden"},
	{source: "cgo_004.golden", golden: "cgo_004.golden"},
	{source: "cgo_005.golden", golden: "cgo_005.golden"},
	{source: "cgo_006.golden", golden: "cgo_006.golden"},
	{source: "cgo_007.golden", golden: "cgo_007.golden"},
	{source: "cgo_008.golden", golden: "cgo_008.golden"},
	{source: "cgo_009.golden", golden: "cgo_009.golden"},
}

var options = &Options{
	TabWidth:    8,
	TabIndent:   true,
	Comments:    true,
	Fragment:    true,
	LocalPrefix: "local",
	Env: &ProcessEnv{
		GocmdRunner: &gocommand.Runner{},
	},
}

func TestProcess(t *testing.T) {
	for _, e := range data {
		source := filepath.Join(datadir, e.source)
		golden := filepath.Join(datadir, e.golden)
		t.Run(source, func(t *testing.T) {
			if err := checkDiff(t, source, golden); err != nil {
				t.Error(err)
			}
		})
	}
}

func checkDiff(t *testing.T, source, golden string) error {
	src, err := os.ReadFile(source)
	if err != nil {
		return err
	}

	gld, err := os.ReadFile(golden)
	if err != nil {
		return err
	}

	options := &Options{
		TabWidth:    8,
		TabIndent:   true,
		Comments:    true,
		Fragment:    true,
		LocalPrefix: "local",
		Env: &ProcessEnv{
			GocmdRunner: &gocommand.Runner{},
		},
	}

	result, err := Process(source, src, options)
	if err != nil {
		return err
	}
	if !bytes.Equal(result, gld) {
		return errors.New(string(diff.Diff(source+".result", result, golden, gld)))
	}

	// The output should not change depending on the execution path
	options.ReconstructAST = true
	result, err = Process(source, src, options)
	if err != nil {
		return err
	}
	if !bytes.Equal(result, gld) {
		return errors.New(string(diff.Diff(source+".result", result, golden, gld)))
	}

	return nil
}

type declValue struct {
	specs      []specValue
	header     [][]string
	doc        [][]string
	preLparen  [][]string
	postLparen [][]string
	bottom     [][]string
	postRparen [][]string
	stdLibDoc  [][]string
	foreignDoc [][]string
	localDoc   [][]string
	footer     [][]string
}

type specValue struct {
	name        string
	path        string
	group       SpecGroup
	doc         [][]string
	nameComment [][]string
	pathComment [][]string
	comment     [][]string
	footer      [][]string
}

func runImportDeclTest(t *testing.T, expected declValue, actual *importDecl) {
	t.Run("ImportDecl", func(t *testing.T) {
		t.Run("Specs", func(t *testing.T) {
			if assert.Len(t, actual.specs, len(expected.specs)) {
				for i, spec := range actual.specs {
					runImportSpecTest(t, expected.specs[i], spec)
				}
			}
		})
		t.Run("Header", func(t *testing.T) { assertCommentGroups(t, expected.header, actual.header) })
		t.Run("Doc", func(t *testing.T) { assertCommentGroups(t, expected.doc, actual.doc) })
		t.Run("PreLparen", func(t *testing.T) { assertCommentGroups(t, expected.preLparen, actual.preLparen) })
		t.Run("PostLparen", func(t *testing.T) { assertCommentGroups(t, expected.postLparen, actual.postLparen) })
		t.Run("Bottom", func(t *testing.T) { assertCommentGroups(t, expected.bottom, actual.bottom) })
		t.Run("PostRparen", func(t *testing.T) { assertCommentGroups(t, expected.postRparen, actual.postRparen) })
		t.Run("StdLibDoc", func(t *testing.T) { assertCommentGroups(t, expected.stdLibDoc, actual.stdLibDoc) })
		t.Run("ForeignDoc", func(t *testing.T) { assertCommentGroups(t, expected.foreignDoc, actual.foreignLibDoc) })
		t.Run("LocalDoc", func(t *testing.T) { assertCommentGroups(t, expected.localDoc, actual.localLibDoc) })
		t.Run("Footer", func(t *testing.T) { assertCommentGroups(t, expected.footer, actual.footer) })
	})
}

func runImportSpecTest(t *testing.T, expected specValue, actual *importSpec) {
	t.Run("ImportSpec", func(t *testing.T) {
		t.Run("Name", func(t *testing.T) { assert.Equal(t, expected.name, actual.name()) })
		t.Run("Path", func(t *testing.T) { assert.Equal(t, expected.path, actual.path()) })
		t.Run("SpecGroup", func(t *testing.T) { assert.Equal(t, expected.group, actual.group) })
		t.Run("Doc", func(t *testing.T) { assertCommentGroups(t, expected.doc, actual.doc) })
		t.Run("NameComment", func(t *testing.T) { assertCommentGroups(t, expected.nameComment, actual.nameComment) })
		t.Run("PathComment", func(t *testing.T) { assertCommentGroups(t, expected.pathComment, actual.pathComment) })
		t.Run("Comment", func(t *testing.T) { assertCommentGroups(t, expected.comment, actual.comment) })
		t.Run("Footer", func(t *testing.T) { assertCommentGroups(t, expected.footer, actual.footer) })
	})
}

func assertCommentGroups(t *testing.T, expected [][]string, actual []*ast.CommentGroup) {
	assert.Equal(t, expected, extractComments(actual))
}

func extractComments(cgs []*ast.CommentGroup) [][]string {
	if len(cgs) == 0 {
		return nil
	}

	result := make([][]string, 0, len(cgs))
	for _, cg := range cgs {
		comments := make([]string, 0, len(cg.List))
		for _, c := range cg.List {
			comments = append(comments, c.Text)
		}
		result = append(result, comments)
	}
	return result
}

func TestNewSourceFile(t *testing.T) {
	type testcase struct {
		src string
		f   func(t *testing.T, name string, sf *SourceFile)
	}

	cases := []testcase{
		{
			src: `// parse empty import declarations
package main

// doc comment
import()

import(/*postlparen comment*/)

// header comment

import/*prelparen comment*/(// postlparen comment
	// bottom comment
)// postrparen comment
// footer comment

// unrelated comments`,
			f: func(t *testing.T, name string, sf *SourceFile) {
				decls := sf.importDecls
				assert.Len(t, decls, 3)

				runImportDeclTest(t, declValue{
					specs: []specValue{},
					doc:   [][]string{{"// doc comment"}},
				}, decls[0])

				runImportDeclTest(t, declValue{
					specs:      []specValue{},
					postLparen: [][]string{{"/*postlparen comment*/"}},
				}, decls[1])

				runImportDeclTest(t, declValue{
					header:     [][]string{{"// header comment"}},
					preLparen:  [][]string{{"/*prelparen comment*/"}},
					postLparen: [][]string{{"// postlparen comment"}},
					postRparen: [][]string{{"// postrparen comment"}},
					bottom:     [][]string{{"// bottom comment"}},
					footer:     [][]string{{"// footer comment"}},
				}, decls[2])
			},
		},
		{
			src: `// parse single import declarations
package main

// not a header comment

// doc comment for fmt
import "fmt"// line comment for fmt
/*footer comment for fmt (1)*//*
footer comment for fmt (2)*/

/*doc comment for context (1)*/// doc comment for context (2) 
import/*path comment for context*/"context"

// header comment for errors

/*assumed doc comment
*/import/*
name comment for e*/
e/*path comment for errors*/"errors"/*comment
for errors (1)*//*comment for errors (2)*/// comment for errors (3)
// footer comment for errors

// header comment for foo (1)

// header comment for foo (2)

/*assumed doc comment for foo*/import
/*path comment for foo
*/"github.com/homuler/foo"// line comment for foo

/*assumed doc comment for x*/import/*name comment for x*/_/*path comment for x*/"x"/*line comment for x*/;/*assumed doc comment for y*/import/*name comment for y*/_/*path comment for y*/"y"// line comment for y

import "local/bar"

// unrelated comments`,
			f: func(t *testing.T, name string, sf *SourceFile) {
				decls := sf.importDecls
				assert.Len(t, decls, 7)

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:    "fmt",
							group:   StdLib,
							comment: [][]string{{"// line comment for fmt"}},
						},
					},
					doc:    [][]string{{"// doc comment for fmt"}},
					footer: [][]string{{"/*footer comment for fmt (1)*/", "/*\nfooter comment for fmt (2)*/"}},
				}, decls[0])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:        "context",
							group:       StdLib,
							pathComment: [][]string{{"/*path comment for context*/"}},
						},
					},
					doc: [][]string{{"/*doc comment for context (1)*/", "// doc comment for context (2) "}},
				}, decls[1])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							name:        "e",
							path:        "errors",
							group:       StdLib,
							nameComment: [][]string{{"/*\nname comment for e*/"}},
							pathComment: [][]string{{"/*path comment for errors*/"}},
							comment:     [][]string{{"/*comment\nfor errors (1)*/", "/*comment for errors (2)*/", "// comment for errors (3)"}},
						},
					},
					header: [][]string{{"// header comment for errors"}},
					doc:    [][]string{{"/*assumed doc comment\n*/"}},
					footer: [][]string{{"// footer comment for errors"}},
				}, decls[2])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:        "github.com/homuler/foo",
							group:       ForeignLib,
							pathComment: [][]string{{"/*path comment for foo\n*/"}},
							comment:     [][]string{{"// line comment for foo"}},
						},
					},
					header: [][]string{{"// header comment for foo (1)"}, {"// header comment for foo (2)"}},
					doc:    [][]string{{"/*assumed doc comment for foo*/"}},
				}, decls[3])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							name:        "_",
							path:        "x",
							group:       StdLib,
							nameComment: [][]string{{"/*name comment for x*/"}},
							pathComment: [][]string{{"/*path comment for x*/"}},
							comment:     [][]string{{"/*line comment for x*/"}},
						},
					},
					doc: [][]string{{"/*assumed doc comment for x*/"}},
				}, decls[4])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							name:        "_",
							path:        "y",
							group:       StdLib,
							nameComment: [][]string{{"/*name comment for y*/"}},
							pathComment: [][]string{{"/*path comment for y*/"}},
							comment:     [][]string{{"// line comment for y"}},
						},
					},
					doc: [][]string{{"/*assumed doc comment for y*/"}},
				}, decls[5])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:  "local/bar",
							group: LocalLib,
						},
					},
				}, decls[6])
			},
		},
		{
			src: `// parse a well-formatted import decl
package main

// assumed doc comment (1)
/*assumed doc comment (2)*/import/*prelparen comment*/(// postlparen comment
	// stdlib comment

	// doc comment for fmt
	f/*path comment for fmt*/"fmt"// line comment for fmt
	// footer comment for fmt (1)
	// footer comment for fmt (2)

	// bottom comment (1)

	// bottom comment (2)

/*prerparen comment*/)// postrparen comment
// footer comment

// header comment

// doc comment
import(/*path comment for context*/"context"/*line comment for context*/;/*name comment for e*/e/*path comment for errors*/"errors"/*line comment for errors*/)/*
postrparen comment*/

import
/*prelparen comment*/
(/*postlparen comment
*/
	// foreign comment (1)

	"somehost.com/bar"

	// stdlib comment

	"fmt"
	"context"

	// local comment

	"local/buz"

	// foreign comment (2)

	"github.com/homuler/foo"

	// bottom comment
)
// footer comment

// unrelated comments`,
			f: func(t *testing.T, name string, sf *SourceFile) {
				decls := sf.importDecls
				assert.Len(t, decls, 3)

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							name:        "f",
							path:        "fmt",
							group:       StdLib,
							doc:         [][]string{{"// doc comment for fmt"}},
							pathComment: [][]string{{"/*path comment for fmt*/"}},
							comment:     [][]string{{"// line comment for fmt"}},
							footer:      [][]string{{"// footer comment for fmt (1)", "// footer comment for fmt (2)"}},
						},
					},
					doc:        [][]string{{"// assumed doc comment (1)", "/*assumed doc comment (2)*/"}},
					preLparen:  [][]string{{"/*prelparen comment*/"}},
					postLparen: [][]string{{"// postlparen comment"}},
					bottom:     [][]string{{"// bottom comment (1)"}, {"// bottom comment (2)"}, {"/*prerparen comment*/"}},
					postRparen: [][]string{{"// postrparen comment"}},
					footer:     [][]string{{"// footer comment"}},
					stdLibDoc:  [][]string{{"// stdlib comment"}},
				}, decls[0])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:        "context",
							group:       StdLib,
							pathComment: [][]string{{"/*path comment for context*/"}},
							comment:     [][]string{{"/*line comment for context*/"}},
						},
						{
							name:        "e",
							path:        "errors",
							group:       StdLib,
							nameComment: [][]string{{"/*name comment for e*/"}},
							pathComment: [][]string{{"/*path comment for errors*/"}},
							comment:     [][]string{{"/*line comment for errors*/"}},
						},
					},
					header:     [][]string{{"// header comment"}},
					doc:        [][]string{{"// doc comment"}},
					postRparen: [][]string{{"/*\npostrparen comment*/"}},
				}, decls[1])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:  "context",
							group: StdLib,
						},
						{
							path:  "fmt",
							group: StdLib,
						},
						{
							path:  "github.com/homuler/foo",
							group: ForeignLib,
						},
						{
							path:  "somehost.com/bar",
							group: ForeignLib,
						},
						{
							path:  "local/buz",
							group: LocalLib,
						},
					},
					preLparen:  [][]string{{"/*prelparen comment*/"}},
					postLparen: [][]string{{"/*postlparen comment\n*/"}},
					stdLibDoc:  [][]string{{"// stdlib comment"}},
					foreignDoc: [][]string{{"// foreign comment (1)"}, {"// foreign comment (2)"}},
					localDoc:   [][]string{{"// local comment"}},
					bottom:     [][]string{{"// bottom comment"}},
					footer:     [][]string{{"// footer comment"}},
				}, decls[2])
			},
		},
	}
	fileSet := token.NewFileSet()

	for i, c := range cases {
		src := []byte(c.src)
		name := fmt.Sprintf("case%03v", i+1)
		t.Run(name, func(t *testing.T) {
			file, err := parser.ParseFile(fileSet, name+".go", src, parser.ImportsOnly|parser.ParseComments)
			if err != nil {
				t.Error(fmt.Errorf("Failed to parse %s.go: %w", name, err))
				return
			}
			sf := newSourceFile(src, fileSet, file, options)
			for _, decl := range sf.importDecls {
				decl.sortAll()
			}
			c.f(t, name, sf)
		})
	}
}

func TestSquashImportDecls(t *testing.T) {
	type testcase struct {
		description string
		src         string
		f           func(t *testing.T, sf *SourceFile)
	}

	cases := []testcase{
		{
			description: "Return single import declaration as is",
			src: `package main

// doc comment for fmt
import/*name comment for f*/f/*path comment for fmt*/"fmt"// line comment for fmt
// footer comment for fmt

// unrelated comments`,
			f: func(t *testing.T, sf *SourceFile) {
				decls := sf.importDecls
				assert.Len(t, decls, 1)

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:        "fmt",
							name:        "f",
							group:       StdLib,
							nameComment: [][]string{{"/*name comment for f*/"}},
							pathComment: [][]string{{"/*path comment for fmt*/"}},
							comment:     [][]string{{"// line comment for fmt"}},
						},
					},
					doc:    [][]string{{"// doc comment for fmt"}},
					footer: [][]string{{"// footer comment for fmt"}},
				}, decls[0])
			},
		},
		{
			description: "Merge same single import declarations",
			src: `package main

// doc comment for fmt (1)
import/*name comment for f (1)*/f/*path comment for fmt (1)*/"fmt"// line comment for fmt (1)
// footer comment for fmt (1)

// doc comment for fmt (2)
import/*name comment for f (2)*/f/*path comment for fmt (2)*/"fmt"// line comment for fmt (2)
// footer comment for fmt (2)

// unrelated comments`,
			f: func(t *testing.T, sf *SourceFile) {
				decls := sf.importDecls
				assert.Len(t, decls, 1)

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:        "fmt",
							name:        "f",
							group:       StdLib,
							doc:         [][]string{{"// doc comment for fmt (1)"}, {"// doc comment for fmt (2)"}},
							nameComment: [][]string{{"/*name comment for f (1)*/"}, {"/*name comment for f (2)*/"}},
							pathComment: [][]string{{"/*path comment for fmt (1)*/"}, {"/*path comment for fmt (2)*/"}},
							comment:     [][]string{{"// line comment for fmt (1)"}, {"// line comment for fmt (2)"}},
							footer:      [][]string{{"// footer comment for fmt (1)"}, {"// footer comment for fmt (2)"}},
						},
					},
				}, decls[0])
			},
		},
		{
			description: "Merge single import declarations",
			src: `package main

// doc comment for fmt
import/*name comment for f*/f/*path comment for fmt*/"fmt"// line comment for fmt
// footer comment for fmt

// doc comment for context (1)
import/*name comment for ctx (1)*/ctx/*path comment for context (1)*/"context"// line comment for context (1)
// footer comment for context (1)

// header comment (1)

// doc comment for fmt (1)
import/*path comment for fmt (1)*/"fmt"// line comment for fmt (1)
// footer comment for fmt (1)

// doc comment for context (2)
import/*name comment for ctx (2)*/ctx/*path comment for context (2)*/"context"// line comment for context (2)
// footer comment for context (2)

// header comment (2)

// doc comment for fmt (2)
import/*path comment for fmt (2)*/"fmt"// line comment for fmt (2)
// footer comment for fmt (2)
// footer comment for fmt (3)

// unrelated comments
`,
			f: func(t *testing.T, sf *SourceFile) {
				decls := sf.importDecls
				assert.Len(t, decls, 1)

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:        "context",
							name:        "ctx",
							group:       StdLib,
							doc:         [][]string{{"// doc comment for context (1)"}, {"// doc comment for context (2)"}},
							nameComment: [][]string{{"/*name comment for ctx (1)*/"}, {"/*name comment for ctx (2)*/"}},
							pathComment: [][]string{{"/*path comment for context (1)*/"}, {"/*path comment for context (2)*/"}},
							comment:     [][]string{{"// line comment for context (1)"}, {"// line comment for context (2)"}},
							footer:      [][]string{{"// footer comment for context (1)"}, {"// footer comment for context (2)"}},
						},
						{
							path:        "fmt",
							group:       StdLib,
							doc:         [][]string{{"// doc comment for fmt (1)"}, {"// doc comment for fmt (2)"}},
							pathComment: [][]string{{"/*path comment for fmt (1)*/"}, {"/*path comment for fmt (2)*/"}},
							comment:     [][]string{{"// line comment for fmt (1)"}, {"// line comment for fmt (2)"}},
							footer:      [][]string{{"// footer comment for fmt (1)"}, {"// footer comment for fmt (2)", "// footer comment for fmt (3)"}},
						},
						{
							path:        "fmt",
							name:        "f",
							group:       StdLib,
							doc:         [][]string{{"// doc comment for fmt"}},
							nameComment: [][]string{{"/*name comment for f*/"}},
							pathComment: [][]string{{"/*path comment for fmt*/"}},
							comment:     [][]string{{"// line comment for fmt"}},
							footer:      [][]string{{"// footer comment for fmt"}},
						},
					},
					header: [][]string{{"// header comment (1)"}, {"// header comment (2)"}},
				}, decls[0])
			},
		},
		{
			description: "Merge import declarations into a single import declaration",
			src: `package main

// doc comment for fmt
import/*path comment for fmt (1)*/"fmt"// line comment for fmt (1)
// footer comment for fmt (1)

// header comment

// doc comment
import/*prelparen comment*/(// postlparen comment
	// stdlib comment

	"context"
	/*path comment for fmt (2)*/"fmt"// line comment for fmt (2)
	// footer comment for fmt (2)

	// bottom comment
)// postrparen comment
// footer comment

// unrelated comments`,
			f: func(t *testing.T, sf *SourceFile) {
				decls := sf.importDecls
				assert.Len(t, decls, 1)

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:  "context",
							group: StdLib,
						},
						{
							path:        "fmt",
							group:       StdLib,
							doc:         [][]string{{"// doc comment for fmt"}},
							pathComment: [][]string{{"/*path comment for fmt (1)*/"}, {"/*path comment for fmt (2)*/"}},
							comment:     [][]string{{"// line comment for fmt (1)"}, {"// line comment for fmt (2)"}},
							footer:      [][]string{{"// footer comment for fmt (1)"}, {"// footer comment for fmt (2)"}},
						},
					},
					header:     [][]string{{"// header comment"}},
					doc:        [][]string{{"// doc comment"}},
					preLparen:  [][]string{{"/*prelparen comment*/"}},
					postLparen: [][]string{{"// postlparen comment"}},
					stdLibDoc:  [][]string{{"// stdlib comment"}},
					bottom:     [][]string{{"// bottom comment"}},
					postRparen: [][]string{{"// postrparen comment"}},
					footer:     [][]string{{"// footer comment"}},
				}, decls[0])
			},
		},
		{
			description: "Merge single import declarations into one import declaration",
			src: `package main

// doc comment
import/*prelparen comment*/(// postlparen comment
	// stdlib comment

	"context"
	/*path comment for fmt (2)*/"fmt"// line comment for fmt (2)
	// footer comment for fmt (2)

	// bottom comment
)// postrparen comment
// footer comment

// header comment

// doc comment for fmt
import/*path comment for fmt (1)*/"fmt"// line comment for fmt (1)
// footer comment for fmt (1)


// unrelated comments`,
			f: func(t *testing.T, sf *SourceFile) {
				decls := sf.importDecls
				assert.Len(t, decls, 1)

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:  "context",
							group: StdLib,
						},
						{
							path:        "fmt",
							group:       StdLib,
							doc:         [][]string{{"// doc comment for fmt"}},
							pathComment: [][]string{{"/*path comment for fmt (2)*/"}, {"/*path comment for fmt (1)*/"}},
							comment:     [][]string{{"// line comment for fmt (2)"}, {"// line comment for fmt (1)"}},
							footer:      [][]string{{"// footer comment for fmt (2)"}, {"// footer comment for fmt (1)"}},
						},
					},
					header:     [][]string{{"// header comment"}},
					doc:        [][]string{{"// doc comment"}},
					preLparen:  [][]string{{"/*prelparen comment*/"}},
					postLparen: [][]string{{"// postlparen comment"}},
					stdLibDoc:  [][]string{{"// stdlib comment"}},
					bottom:     [][]string{{"// bottom comment"}},
					postRparen: [][]string{{"// postrparen comment"}},
					footer:     [][]string{{"// footer comment"}},
				}, decls[0])
			},
		},
		{
			description: "Merge import declarations",
			src: `package main

// doc comment (1)
import/*prelparen comment (1)*/(// postlparen comment (1)
	// stdlib comment (1)

	"context"
	/*name comment for f (1)*/f/*path comment for fmt (1)*/"fmt"// line comment for fmt (1)
	// footer comment for fmt (1)

	// local comment

	"local/buz"

	// bottom comment (1)
)// postrparen comment (1)
// footer comment (1)

// header comment

// doc comment (2)
import/*prelparen comment (2)*/(// postlparen comment (2)
	// foreign comment

	"github.com/homuler/foo"

	// stdlib comment (2)

	/*name comment for f (2)*/f/*path comment for fmt (2)*/"fmt"// line comment for fmt (2)
	// footer comment for fmt (2)

	// bottom comment (2)
)// postrparen comment (2)
// footer comment (2)

// unrelated comments`,
			f: func(t *testing.T, sf *SourceFile) {
				decls := sf.importDecls
				assert.Len(t, decls, 1)

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:  "context",
							group: StdLib,
						},
						{
							path:        "fmt",
							name:        "f",
							group:       StdLib,
							nameComment: [][]string{{"/*name comment for f (1)*/"}, {"/*name comment for f (2)*/"}},
							pathComment: [][]string{{"/*path comment for fmt (1)*/"}, {"/*path comment for fmt (2)*/"}},
							comment:     [][]string{{"// line comment for fmt (1)"}, {"// line comment for fmt (2)"}},
							footer:      [][]string{{"// footer comment for fmt (1)"}, {"// footer comment for fmt (2)"}},
						},
						{
							path:  "github.com/homuler/foo",
							group: ForeignLib,
						},
						{
							path:  "local/buz",
							group: LocalLib,
						},
					},
					header:     [][]string{{"// header comment"}},
					doc:        [][]string{{"// doc comment (1)"}, {"// doc comment (2)"}},
					preLparen:  [][]string{{"/*prelparen comment (1)*/"}, {"/*prelparen comment (2)*/"}},
					postLparen: [][]string{{"// postlparen comment (1)"}, {"// postlparen comment (2)"}},
					stdLibDoc:  [][]string{{"// stdlib comment (1)"}, {"// stdlib comment (2)"}},
					foreignDoc: [][]string{{"// foreign comment"}},
					localDoc:   [][]string{{"// local comment"}},
					bottom:     [][]string{{"// bottom comment (1)"}, {"// bottom comment (2)"}},
					postRparen: [][]string{{"// postrparen comment (1)"}, {"// postrparen comment (2)"}},
					footer:     [][]string{{"// footer comment (1)"}, {"// footer comment (2)"}},
				}, decls[0])
			},
		},
		{
			description: "Keep import C declarations",
			src: `package main

/*
#include <stdlib.h>
*/
import/*path comment for C*/"C"// line comment for C
// footer comment for C

// #include <stdio.h>
import/*prelparen comment*/(// postlparen comment
	/*path comment for C*/"C"// line comment for C
	// footer comment for C

	// bottom comment
)// postrparen comment
// footer comment

// doc comment
import (
	// #include <string.h>
	"C"
)

import (
	"fmt"
)

// unrelated comments`,
			f: func(t *testing.T, sf *SourceFile) {
				decls := sf.importDecls
				assert.Len(t, decls, 4)

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:        "C",
							group:       StdLib,
							pathComment: [][]string{{"/*path comment for C*/"}},
							comment:     [][]string{{"// line comment for C"}},
						},
					},
					doc:    [][]string{{"/*\n#include <stdlib.h>\n*/"}},
					footer: [][]string{{"// footer comment for C"}},
				}, decls[0])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:        "C",
							group:       StdLib,
							pathComment: [][]string{{"/*path comment for C*/"}},
							comment:     [][]string{{"// line comment for C"}},
							footer:      [][]string{{"// footer comment for C"}},
						},
					},
					doc:        [][]string{{"// #include <stdio.h>"}},
					preLparen:  [][]string{{"/*prelparen comment*/"}},
					postLparen: [][]string{{"// postlparen comment"}},
					bottom:     [][]string{{"// bottom comment"}},
					postRparen: [][]string{{"// postrparen comment"}},
					footer:     [][]string{{"// footer comment"}},
				}, decls[1])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:  "C",
							group: StdLib,
							doc:   [][]string{{"// #include <string.h>"}},
						},
					},
					doc: [][]string{{"// doc comment"}},
				}, decls[2])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:  "fmt",
							group: StdLib,
						},
					},
				}, decls[3])
			},
		},
		{
			description: "Extract import C declarations",
			src: `package main

// doc comment
import (
	// #include <stdio.h>
	"C"// line comment for C (1)
	/*
#include <stdlib.h>
*/
	"C"// line comment for C (2)
	// footer comment for C
)

// #include <string.h>
import (
	"C"
)

// #include <math.h>
import(/*path comment*/"C"/*line comment*/)

// unrelated comments`,
			f: func(t *testing.T, sf *SourceFile) {
				decls := sf.importDecls
				assert.Len(t, decls, 5)

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:    "C",
							group:   StdLib,
							comment: [][]string{{"// line comment for C (1)"}},
						},
					},
					doc: [][]string{{"// #include <stdio.h>"}},
				}, decls[0])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:    "C",
							group:   StdLib,
							comment: [][]string{{"// line comment for C (2)"}},
						},
					},
					doc:    [][]string{{"/*\n#include <stdlib.h>\n*/"}},
					footer: [][]string{{"// footer comment for C"}},
				}, decls[1])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:  "C",
							group: StdLib,
						},
					},
					doc: [][]string{{"// #include <string.h>"}},
				}, decls[2])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:        "C",
							group:       StdLib,
							pathComment: [][]string{{"/*path comment*/"}},
							comment:     [][]string{{"/*line comment*/"}},
						},
					},
					doc: [][]string{{"// #include <math.h>"}},
				}, decls[3])

				runImportDeclTest(t, declValue{
					specs: []specValue{},
					doc:   [][]string{{"// doc comment"}},
				}, decls[4])
			},
		},
		{
			description: "Extract import C declarations and merge remainings",
			src: `package main

// doc comment
import (
	// #include <stdio.h>
	"C"// line comment for C (1)
	"context"
	/*
#include <stdlib.h>
*/
	"C"// line comment for C (2)
	// footer comment for C
)

// #include <string.h> (this line is not a comment for C)
import (
	"C"
	"fmt"
)

// unrelated comments`,
			f: func(t *testing.T, sf *SourceFile) {
				decls := sf.importDecls
				assert.Len(t, decls, 4)

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:    "C",
							group:   StdLib,
							comment: [][]string{{"// line comment for C (1)"}},
						},
					},
					doc: [][]string{{"// #include <stdio.h>"}},
				}, decls[0])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:    "C",
							group:   StdLib,
							comment: [][]string{{"// line comment for C (2)"}},
						},
					},
					doc:    [][]string{{"/*\n#include <stdlib.h>\n*/"}},
					footer: [][]string{{"// footer comment for C"}},
				}, decls[1])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:  "C",
							group: StdLib,
						},
					},
				}, decls[2])

				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:  "context",
							group: StdLib,
						},
						{
							path:  "fmt",
							group: StdLib,
						},
					},
					doc: [][]string{{"// doc comment"}, {"// #include <string.h> (this line is not a comment for C)"}},
				}, decls[3])
			},
		},
	}

	fileSet := token.NewFileSet()

	for i, c := range cases {
		src := []byte(c.src)
		srcPath := fmt.Sprintf("case%03v.go", i+1)
		t.Run(c.description, func(t *testing.T) {
			file, err := parser.ParseFile(fileSet, srcPath, src, parser.ImportsOnly|parser.ParseComments)
			if err != nil {
				t.Error(fmt.Errorf("Failed to parse %s: %w", srcPath, err))
				return
			}
			sf := newSourceFile(src, fileSet, file, options)
			sf.squashImportDecls()
			c.f(t, sf)
		})
	}
}
