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

	"github.com/homuler/goimports/internal/diff"
	"github.com/homuler/goimports/internal/gocommand"
	"github.com/homuler/goimports/internal/testenv"
	"github.com/stretchr/testify/assert"
)

func TestMain(m *testing.M) {
	testenv.ExitIfSmallMachine()
	os.Exit(m.Run())
}

type entry struct {
	source, golden string
}

const datadir = "testdata/sort"

var data []entry = []entry{
	{source: "case01.input", golden: "case01.golden"},
	{source: "single_spec_decl.input", golden: "single_spec_decl.input"},
	{source: "merge_single_spec.input", golden: "merge_single_spec.golden"},
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
		if err := checkDiff(t, source, golden); err != nil {
			t.Error(err)
		}
	}
}

func checkDiff(t *testing.T, source, golden string) error {
	src, err := os.ReadFile(source)
	if err != nil {
		return err
	}

	result, err := Process(source, src, options)
	if err != nil {
		return err
	}

	gld, err := os.ReadFile(golden)
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

func TestNewSourceFile(t *testing.T) {
	type testcase struct {
		src string
		f   func(t *testing.T, name string, sf *SourceFile)
	}

	cases := []testcase{
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

				assert.Len(t, decls[0].Specs, 1)
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

				assert.Len(t, decls[1].Specs, 1)
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

				assert.Len(t, decls[2].Specs, 1)
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

				assert.Len(t, decls[3].Specs, 1)
				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:        "github.com/homuler/foo",
							group:       Foreign,
							pathComment: [][]string{{"/*path comment for foo\n*/"}},
							comment:     [][]string{{"// line comment for foo"}},
						},
					},
					header: [][]string{{"// header comment for foo (1)"}, {"// header comment for foo (2)"}},
					doc:    [][]string{{"/*assumed doc comment for foo*/"}},
				}, decls[3])

				assert.Len(t, decls[4].Specs, 1)
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

				assert.Len(t, decls[5].Specs, 1)
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

				assert.Len(t, decls[6].Specs, 1)
				runImportDeclTest(t, declValue{
					specs: []specValue{
						{
							path:  "local/bar",
							group: Local,
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
							group: Foreign,
						},
						{
							path:  "somehost.com/bar",
							group: Foreign,
						},
						{
							path:  "local/buz",
							group: Local,
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
			sf := NewSourceFile(src, fileSet, file, options)
			c.f(t, name, sf)
		})
	}
}

func runImportDeclTest(t *testing.T, expected declValue, actual *ImportDecl) {
	t.Run("ImportDecl", func(t *testing.T) {
		t.Run("Specs", func(t *testing.T) {
			assert.Len(t, actual.Specs, len(expected.specs))
			for i, spec := range actual.Specs {
				runImportSpecTest(t, expected.specs[i], spec)
			}
		})
		t.Run("Header", func(t *testing.T) { assertCommentGroups(t, expected.header, actual.Header) })
		t.Run("Doc", func(t *testing.T) { assertCommentGroups(t, expected.doc, actual.Doc) })
		t.Run("PreLparen", func(t *testing.T) { assertCommentGroups(t, expected.preLparen, actual.PreLparen) })
		t.Run("PostLparen", func(t *testing.T) { assertCommentGroups(t, expected.postLparen, actual.PostLparen) })
		t.Run("Bottom", func(t *testing.T) { assertCommentGroups(t, expected.bottom, actual.Bottom) })
		t.Run("PostRparen", func(t *testing.T) { assertCommentGroups(t, expected.postRparen, actual.PostRparen) })
		t.Run("StdLibDoc", func(t *testing.T) { assertCommentGroups(t, expected.stdLibDoc, actual.StdLibDoc) })
		t.Run("ForeignDoc", func(t *testing.T) { assertCommentGroups(t, expected.foreignDoc, actual.ForeignDoc) })
		t.Run("LocalDoc", func(t *testing.T) { assertCommentGroups(t, expected.localDoc, actual.LocalDoc) })
		t.Run("Footer", func(t *testing.T) { assertCommentGroups(t, expected.footer, actual.Footer) })
	})
}

func runImportSpecTest(t *testing.T, expected specValue, actual *ImportSpec) {
	t.Run("ImportSpec", func(t *testing.T) {
		t.Run("Name", func(t *testing.T) { assert.Equal(t, expected.name, actual.name()) })
		t.Run("Path", func(t *testing.T) { assert.Equal(t, expected.path, actual.path()) })
		t.Run("SpecGroup", func(t *testing.T) { assert.Equal(t, expected.group, actual.Group) })
		t.Run("Doc", func(t *testing.T) { assertCommentGroups(t, expected.doc, actual.Doc) })
		t.Run("NameComment", func(t *testing.T) { assertCommentGroups(t, expected.nameComment, actual.NameComment) })
		t.Run("PathComment", func(t *testing.T) { assertCommentGroups(t, expected.pathComment, actual.PathComment) })
		t.Run("Comment", func(t *testing.T) { assertCommentGroups(t, expected.comment, actual.Comment) })
		t.Run("Footer", func(t *testing.T) { assertCommentGroups(t, expected.footer, actual.Footer) })
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
