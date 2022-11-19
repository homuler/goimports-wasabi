package imports

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"sort"
	"strconv"
	"strings"
)

func astImportDecl(d ast.Decl) (*ast.GenDecl, bool) {
	gen, ok := d.(*ast.GenDecl)
	if !ok || gen.Tok != token.IMPORT {
		return nil, false
	}
	return gen, true
}

func pathValue(s *ast.ImportSpec) string {
	p, err := strconv.Unquote(s.Path.Value)
	if err != nil {
		return ""
	}
	return p
}

func parseFile(fset *token.FileSet, filename string, src []byte, opt *Options) (*ast.File, error) {
	parserMode := parser.Mode(0)
	if opt.Comments {
		parserMode |= parser.ParseComments
	}
	if opt.AllErrors {
		parserMode |= parser.AllErrors
	}
	return parser.ParseFile(fset, filename, src, parserMode)
}

type SpecGroup int

const (
	NoGroup SpecGroup = iota
	StdLib
	AppEngine
	ForeignLib
	LocalLib
)

func specGroupFor(s *ast.ImportSpec, localPrefix string) SpecGroup {
	path := pathValue(s)

	if localPrefix != "" {
		for _, p := range strings.Split(localPrefix, ",") {
			if strings.HasPrefix(path, p) || strings.TrimSuffix(p, "/") == path {
				return LocalLib
			}
		}
	}
	firstComponent := strings.Split(path, "/")[0]
	if strings.Contains(firstComponent, ".") {
		return ForeignLib
	}
	if strings.HasPrefix(path, "appengine") {
		return AppEngine
	}
	return StdLib
}

type bySpecGroup []*importSpec

func (x bySpecGroup) Len() int      { return len(x) }
func (x bySpecGroup) Swap(i, j int) { x[i], x[j] = x[j], x[i] }
func (x bySpecGroup) Less(i, j int) bool {
	ipath := x[i].path()
	jpath := x[j].path()

	igroup := x[i].group
	jgroup := x[j].group
	if igroup != jgroup {
		return igroup < jgroup
	}

	if ipath != jpath {
		return ipath < jpath
	}
	iname := x[i].name()
	jname := x[j].name()

	if iname != jname {
		return iname < jname
	}
	return x[i].firstComment() < x[j].firstComment()
}

// importDecl represents *ast.GenDecl (import declaration) with the relavent comments
type importDecl struct {
	node  *ast.GenDecl
	specs []*importSpec

	header        []*ast.CommentGroup
	doc           []*ast.CommentGroup
	preLparen     []*ast.CommentGroup
	postLparen    []*ast.CommentGroup
	bottom        []*ast.CommentGroup
	postRparen    []*ast.CommentGroup
	stdLibDoc     []*ast.CommentGroup
	appEngineDoc  []*ast.CommentGroup
	foreignLibDoc []*ast.CommentGroup
	localLibDoc   []*ast.CommentGroup
	footer        []*ast.CommentGroup

	pos token.Pos
	end token.Pos
}

func newImportDecl(d *ast.GenDecl, localPrefix string) *importDecl {
	return &importDecl{node: d, pos: d.Pos(), end: d.End()}
}

func newSingleImportDecl(spec *importSpec) *importDecl {
	gen := &ast.GenDecl{Tok: token.IMPORT, TokPos: spec.node.Pos(), Specs: []ast.Spec{spec.node}}
	decl := importDecl{node: gen, pos: spec.pos, end: spec.end}

	decl.addDoc(spec.doc...)
	spec.doc = nil
	decl.addFooter(spec.footer...)
	spec.footer = nil
	decl.addSpec(spec)

	return &decl
}

func (decl *importDecl) hasDeclComments() bool {
	return len(decl.preLparen) != 0 ||
		len(decl.postLparen) != 0 ||
		len(decl.bottom) != 0 ||
		len(decl.postRparen) != 0 ||
		len(decl.stdLibDoc) != 0 ||
		len(decl.appEngineDoc) != 0 ||
		len(decl.foreignLibDoc) != 0 ||
		len(decl.localLibDoc) != 0
}

func (decl *importDecl) hasComments() bool {
	return len(decl.doc) != 0 || len(decl.footer) != 0 || decl.hasDeclComments()
}

func (decl *importDecl) isEmpty() bool {
	return len(decl.specs) == 0 && !decl.hasComments()
}

func (decl *importDecl) isCImportDecl() bool {
	if len(decl.specs) != 1 {
		return false
	}
	return decl.specs[0].isCSpec()
}

func (decl *importDecl) distillCImports() ([]*importDecl, *importDecl) {
	if decl.isCImportDecl() {
		return []*importDecl{decl}, nil
	}

	var cdecls []*importDecl
	specs := make([]*importSpec, 0, len(decl.specs))

	for _, spec := range decl.specs {
		if !spec.isCSpec() {
			specs = append(specs, spec)
			continue
		}
		cdecls = append(cdecls, newSingleImportDecl(spec))
	}

	if len(cdecls) == 0 {
		return nil, decl
	}

	// NOTE: len(specs) can be zero
	decl.specs = specs
	return cdecls, decl
}

func (decl *importDecl) addSpec(ss ...*importSpec) {
	decl.specs = append(decl.specs, ss...)
	for _, s := range ss {
		if s.pos < decl.pos {
			decl.pos = s.pos
		}
		if s.end > decl.end {
			decl.end = s.end
		}
	}
}

func (decl *importDecl) addHeader(cg ...*ast.CommentGroup) {
	decl.header = append(decl.header, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < decl.pos {
			decl.pos = p
		}
	}
}

func (decl *importDecl) addDoc(cg ...*ast.CommentGroup) {
	decl.doc = append(decl.doc, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < decl.pos {
			decl.pos = p
		}
	}
}

func (decl *importDecl) addPreLparen(cg ...*ast.CommentGroup) {
	decl.preLparen = append(decl.preLparen, cg...)
}

func (decl *importDecl) addPostLparen(cg ...*ast.CommentGroup) {
	decl.postLparen = append(decl.postLparen, cg...)
}

func (decl *importDecl) addStdLibDoc(cg ...*ast.CommentGroup) {
	decl.stdLibDoc = append(decl.stdLibDoc, cg...)
}

func (decl *importDecl) addAppEngineDoc(cg ...*ast.CommentGroup) {
	decl.appEngineDoc = append(decl.appEngineDoc, cg...)
}

func (decl *importDecl) addForeignLibDoc(cg ...*ast.CommentGroup) {
	decl.foreignLibDoc = append(decl.foreignLibDoc, cg...)
}

func (decl *importDecl) addLocalLibDoc(cg ...*ast.CommentGroup) {
	decl.localLibDoc = append(decl.localLibDoc, cg...)
}

func (decl *importDecl) addBottom(cg ...*ast.CommentGroup) {
	decl.bottom = append(decl.bottom, cg...)
}

func (decl *importDecl) addPostRparen(cg ...*ast.CommentGroup) {
	decl.postRparen = append(decl.postRparen, cg...)
	for _, c := range cg {
		p := c.End()
		if p.IsValid() && p > decl.end {
			decl.end = p
		}
	}
}

func (decl *importDecl) addFooter(cg ...*ast.CommentGroup) {
	decl.footer = append(decl.footer, cg...)
	for _, c := range cg {
		p := c.End()
		if p.IsValid() && p > decl.end {
			decl.end = p
		}
	}
}

func (decl *importDecl) addGroupDoc(c *ast.CommentGroup, g SpecGroup) {
	switch g {
	case StdLib:
		decl.addStdLibDoc(c)
	case AppEngine:
		decl.addAppEngineDoc(c)
	case ForeignLib:
		decl.addForeignLibDoc(c)
	case LocalLib:
		decl.addLocalLibDoc(c)
	}
}

func (decl *importDecl) sortAll() {
	sort.Sort(bySpecGroup(decl.specs))
	for _, spec := range decl.specs {
		spec.sortComments()
	}
	sort.Sort(byCommentPos(decl.header))
	sort.Sort(byCommentPos(decl.doc))
	sort.Sort(byCommentPos(decl.preLparen))
	sort.Sort(byCommentPos(decl.postLparen))
	sort.Sort(byCommentPos(decl.stdLibDoc))
	sort.Sort(byCommentPos(decl.appEngineDoc))
	sort.Sort(byCommentPos(decl.foreignLibDoc))
	sort.Sort(byCommentPos(decl.localLibDoc))
	sort.Sort(byCommentPos(decl.bottom))
	sort.Sort(byCommentPos(decl.postRparen))
	sort.Sort(byCommentPos(decl.footer))
}

// merge merges x and y.
func (x *importDecl) merge(y *importDecl) {
	x.normalize()
	y.normalize()

	x.addSpec(y.specs...)
	x.addHeader(y.header...)
	x.addDoc(y.doc...)
	x.addPreLparen(y.preLparen...)
	x.addPostLparen(y.postLparen...)
	x.addStdLibDoc(y.stdLibDoc...)
	x.addAppEngineDoc(y.appEngineDoc...)
	x.addForeignLibDoc(y.foreignLibDoc...)
	x.addLocalLibDoc(y.localLibDoc...)
	x.addBottom(y.bottom...)
	x.addPostRparen(y.postRparen...)
	x.addFooter(y.footer...)

	if x.pos > y.pos {
		x.pos = y.pos
	}
	if x.end < y.end {
		x.end = y.end
	}
}

func (decl *importDecl) normalize() {
	if decl.node.Lparen.IsValid() {
		return
	}
	// Set temporary positions.
	// These values will be fixed when printing decl.
	decl.node.Lparen = decl.node.TokPos + 6
	decl.node.Rparen = decl.node.End()

	if len(decl.specs) > 0 { // len equals 1
		spec := decl.specs[0]
		spec.addDoc(decl.doc...)
		decl.doc = nil
		spec.addFooter(decl.footer...)
		decl.footer = nil
	}
}

func (decl *importDecl) dedupe() {
	if len(decl.specs) <= 1 {
		return
	}

	i := 0
	for i < len(decl.specs)-1 {
		spec := decl.specs[i]
		next := decl.specs[i+1]
		if !spec.isSame(next) {
			i++
			continue
		}
		spec.merge(next)
		spec.sortComments()
		decl.specs[i+1] = spec
		decl.specs = append(decl.specs[:i], decl.specs[i+1:]...)
	}
}

// importSpec represents *ast.importSpec with the relavent comments
type importSpec struct {
	node        *ast.ImportSpec
	group       SpecGroup
	doc         []*ast.CommentGroup
	nameComment []*ast.CommentGroup
	pathComment []*ast.CommentGroup
	comment     []*ast.CommentGroup
	footer      []*ast.CommentGroup

	pos token.Pos
	end token.Pos
}

func newImportSpec(astSpec ast.Spec, localPrefix string) *importSpec {
	s, ok := astSpec.(*ast.ImportSpec)
	if !ok {
		return nil
	}
	return &importSpec{node: s, group: specGroupFor(s, localPrefix), pos: s.Pos(), end: s.End()}
}

func (spec *importSpec) namePos() token.Pos {
	if spec.node.Name != nil {
		return spec.node.Name.NamePos
	}
	return token.NoPos
}

func (spec *importSpec) name() string {
	if spec.node.Name != nil {
		return spec.node.Name.Name
	}
	return ""
}

func (spec *importSpec) pathPos() token.Pos {
	if spec.node.Path != nil {
		return spec.node.Path.ValuePos
	}
	return token.NoPos
}

func (spec *importSpec) pathEnd() token.Pos {
	if spec.node.Path != nil {
		return spec.node.Path.End()
	}
	return token.NoPos
}

func (spec *importSpec) path() string {
	return pathValue(spec.node)
}

func (spec *importSpec) firstComment() string {
	if len(spec.comment) == 0 {
		return ""
	}
	return spec.comment[0].Text()
}

func (spec *importSpec) isCSpec() bool {
	return spec.node.Path.Value == `"C"`
}

func (spec *importSpec) isSame(another *importSpec) bool {
	return spec.path() == another.path() && spec.name() == another.name()
}

func (spec *importSpec) addDoc(cg ...*ast.CommentGroup) {
	spec.doc = append(spec.doc, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < spec.pos {
			spec.pos = p
		}
	}
}

func (spec *importSpec) addNameComment(cg ...*ast.CommentGroup) {
	spec.nameComment = append(spec.nameComment, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < spec.pos {
			spec.pos = p
		}
	}
}

func (spec *importSpec) addPathComment(cg ...*ast.CommentGroup) {
	spec.pathComment = append(spec.pathComment, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < spec.pos {
			spec.pos = p
		}
	}
}

func (spec *importSpec) addComment(cg ...*ast.CommentGroup) {
	spec.comment = append(spec.comment, cg...)
	for _, c := range cg {
		p := c.End()
		if p.IsValid() && p > spec.end {
			spec.end = p
		}
	}
}

func (spec *importSpec) addFooter(cg ...*ast.CommentGroup) {
	spec.footer = append(spec.footer, cg...)
	for _, c := range cg {
		p := c.End()
		if p.IsValid() && p > spec.end {
			spec.end = p
		}
	}
}

func (spec *importSpec) sortComments() {
	sort.Sort(byCommentPos(spec.doc))
	sort.Sort(byCommentPos(spec.nameComment))
	sort.Sort(byCommentPos(spec.pathComment))
	sort.Sort(byCommentPos(spec.comment))
	sort.Sort(byCommentPos(spec.footer))
}

// merge merges 2 ImportSpecs.
// Make sure x and y have the same names and paths before calling it.
func (x *importSpec) merge(y *importSpec) {
	x.addDoc(y.doc...)
	x.addNameComment(y.nameComment...)
	x.addPathComment(y.pathComment...)
	x.addComment(y.comment...)
	x.addFooter(y.footer...)
}

type importDeclReader struct {
	src         []byte
	tokenFile   *token.File
	comments    []*ast.CommentGroup
	decls       []ast.Decl
	localPrefix string

	commentIndex int
	declIndex    int
	maxPos       token.Pos
}

func newImportDeclReader(src []byte, tokenFile *token.File, astFile *ast.File, localPrefix string) *importDeclReader {
	comments := make([]*ast.CommentGroup, len(astFile.Comments))
	copy(comments, astFile.Comments)

	maxPos := tokenFile.Pos(tokenFile.Size())
	sentinelComment := ast.Comment{Slash: maxPos + 1}
	comments = append(comments, &ast.CommentGroup{List: []*ast.Comment{&sentinelComment}})

	return &importDeclReader{
		src:          src,
		tokenFile:    tokenFile,
		comments:     comments,
		decls:        astFile.Decls,
		localPrefix:  localPrefix,
		commentIndex: 0,
		declIndex:    -1,
		maxPos:       maxPos,
	}
}

func (idr *importDeclReader) readNext() (*importDecl, bool) {
	idr.declIndex++
	if idr.declIndex >= len(idr.decls) {
		return nil, false
	}
	d, ok := astImportDecl(idr.decls[idr.declIndex])
	if !ok {
		return nil, false
	}
	decl := newImportDecl(d, idr.localPrefix)

	hdrs := idr.readHeaderComments(d)
	if idr.declIndex > 0 { // drop the comments before the first import declaration
		decl.addHeader(hdrs...)
	}

	tokenFile := idr.tokenFile
	comments := idr.comments

	for comments[idr.commentIndex].End() <= decl.node.Pos() {
		decl.addDoc(comments[idr.commentIndex])
		idr.commentIndex++
	}

	if decl.node.Lparen.IsValid() {
		for comments[idr.commentIndex].Pos() <= decl.node.Lparen {
			decl.addPreLparen(comments[idr.commentIndex])
			idr.commentIndex++
		}

		lparenLine := idr.line(decl.node.Lparen)
		firstSpecLine := tokenFile.LineCount()
		if len(d.Specs) > 0 {
			firstSpecLine = idr.line(d.Specs[0].Pos())
		}

		c := comments[idr.commentIndex]
		for idr.line(c.Pos()) == lparenLine && idr.line(c.End()) < firstSpecLine {
			decl.addPostLparen(c)
			idr.commentIndex++
			c = comments[idr.commentIndex]
		}
	}

	for j, s := range d.Specs {
		spec := newImportSpec(s, idr.localPrefix)
		if spec == nil {
			panic("Failed to read ImportSpec")
		}

		if decl.node.Lparen.IsValid() {
			sg := spec.group

			for idr.line(comments[idr.commentIndex].End()) < idr.line(spec.node.Pos())-1 {
				decl.addGroupDoc(comments[idr.commentIndex], sg)
				idr.commentIndex++
			}

			for idr.line(comments[idr.commentIndex].End()) < idr.line(spec.node.Pos()) {
				spec.addDoc(comments[idr.commentIndex])
				idr.commentIndex++
			}
		}

		for comments[idr.commentIndex].End() <= spec.namePos() {
			spec.addNameComment(comments[idr.commentIndex])
			idr.commentIndex++
		}

		for comments[idr.commentIndex].End() <= spec.pathPos() {
			spec.addPathComment(comments[idr.commentIndex])
			idr.commentIndex++
		}

		// NOTE: if there're multiple line comments, they should be grouped into the same CommentGroup.
		if idr.line(comments[idr.commentIndex].Pos()) == idr.line(spec.pathPos()) &&
			!idr.containsSemicolon(spec.pathEnd(), comments[idr.commentIndex].Pos()) {
			spec.addComment(comments[idr.commentIndex])
			idr.commentIndex++
		}

		if decl.node.Rparen.IsValid() {
			// NOTE: if decl has its rparen, comment groups should be divided before the rparen.
			nextPos := idr.maxPos
			if j < len(d.Specs)-1 {
				nextPos = d.Specs[j+1].Pos()
			}
			spec.addFooter(idr.readFooterComments(spec.end, decl.node.Rparen, idr.line(nextPos))...)
		}

		decl.addSpec(spec)
	}

	nextPos := idr.maxPos
	if idr.declIndex < len(idr.decls)-1 {
		nextPos = idr.decls[idr.declIndex+1].Pos()
	}

	if decl.node.Rparen.IsValid() {
		for comments[idr.commentIndex].End() <= decl.node.Rparen {
			decl.addBottom(comments[idr.commentIndex])
			idr.commentIndex++
		}

		c := comments[idr.commentIndex]
		for c.End() <= nextPos && idr.line(c.Pos()) == idr.line(decl.node.Rparen) && !idr.containsSemicolon(decl.node.Rparen, c.Pos()) {
			decl.addPostRparen(c)
			idr.commentIndex++
			c = comments[idr.commentIndex]
		}
	}

	decl.addFooter(idr.readFooterComments(decl.end, nextPos, idr.line(nextPos))...)
	return decl, true
}

func (idr *importDeclReader) line(p token.Pos) int {
	if p > idr.maxPos {
		return 1 << 30
	}
	return idr.tokenFile.Line(p)
}

func (idr *importDeclReader) readHeaderComments(d *ast.GenDecl) []*ast.CommentGroup {
	comments := idr.comments
	dpos := d.Pos()

	var cs []*ast.CommentGroup
	c := comments[idr.commentIndex]

	// NOTE: consecutive comments should be already grouped into the same CommentGroup.
	for idr.line(c.End()) < idr.line(dpos)-1 {
		cs = append(cs, c)
		idr.commentIndex++
		c = comments[idr.commentIndex]
	}
	return cs
}

func (idr *importDeclReader) readFooterComments(pos, end token.Pos, nextLine int) []*ast.CommentGroup {
	comments := idr.comments

	var cs []*ast.CommentGroup
	c := comments[idr.commentIndex]

	// NOTE: consecutive comments should be already grouped into the same CommentGroup.
	if c.End() <= end && idr.line(c.Pos()) <= idr.line(pos)+1 && idr.line(c.End()) < nextLine-1 {
		cs = append(cs, c)
		idr.commentIndex++
	}
	return cs
}

func (idr *importDeclReader) containsSemicolon(from token.Pos, to token.Pos) bool {
	if to <= from {
		return false
	}
	tokenFile := idr.tokenFile
	i := tokenFile.Offset(from)
	j := tokenFile.Offset(to)
	return bytes.ContainsRune(idr.src[i:j], ';')
}

type sourceWriter struct {
	tokenFile *token.File
	output    []byte
	pos       token.Pos
}

func newSourceWriter(tokenFile *token.File) *sourceWriter {
	return &sourceWriter{tokenFile: tokenFile, output: make([]byte, 0, 1<<14), pos: tokenFile.Pos(0)}
}

func (sw *sourceWriter) writeImportDecl(decl *importDecl) {
	sw.writeFloatingComments(decl.header)
	sw.writeComments(decl.doc)
	decl.node.TokPos = sw.pos
	sw.writeString(token.IMPORT.String())

	if !decl.node.Lparen.IsValid() {
		sw.writeSpace()
		if len(decl.specs) > 0 {
			sw.writeImportSpec(decl.specs[0])
		}
		sw.writeComments(decl.footer)
		return
	}

	sw.writeInlineComments(decl.preLparen)
	decl.node.Lparen = sw.pos
	sw.writeString(token.LPAREN.String())
	sw.writeInlineComments(decl.postLparen)
	sw.writeNewline()

	sg := NoGroup
	for _, spec := range decl.specs {
		if spec.group != sg {
			sg = spec.group

			switch sg {
			case StdLib:
				if len(decl.postLparen) > 0 {
					sw.writeNewline()
				}
				if len(decl.stdLibDoc) > 0 {
					sw.writeFloatingComments(decl.stdLibDoc)
				}
			case AppEngine:
				sw.writeNewline()
				if len(decl.appEngineDoc) > 0 {
					sw.writeFloatingComments(decl.appEngineDoc)
				}
			case ForeignLib:
				sw.writeNewline()
				if len(decl.foreignLibDoc) > 0 {
					sw.writeFloatingComments(decl.foreignLibDoc)
				}
			case LocalLib:
				sw.writeNewline()
				if len(decl.localLibDoc) > 0 {
					sw.writeFloatingComments(decl.localLibDoc)
				}
			}
		}
		sw.writeImportSpec(spec)
	}

	if len(decl.bottom) > 0 {
		sw.writeNewline()
		sw.writeComments(decl.bottom)
	}

	decl.node.Rparen = sw.pos
	sw.writeString(token.RPAREN.String())
	sw.writeInlineComments(decl.postRparen)
	sw.writeNewline()
	if len(decl.footer) > 0 {
		sw.writeComments(decl.footer)
		sw.writeNewline()
	}
}

func (sw *sourceWriter) writeImportSpec(spec *importSpec) {
	if len(spec.doc) > 0 {
		sw.writeComments(spec.doc)
	}
	if spec.node.Name != nil {
		sw.writeInlineComments(spec.nameComment)
		spec.node.Name.NamePos = sw.pos
		sw.writeString(spec.node.Name.Name)
	}
	if spec.node.Path != nil {
		sw.writeInlineComments(spec.pathComment)
		spec.node.Path.ValuePos = sw.pos
		sw.writeString(spec.node.Path.Value)
	}
	sw.writeInlineComments(spec.comment)
	sw.writeNewline()
	if len(spec.footer) > 0 {
		sw.writeComments(spec.footer)
		sw.writeNewline()
	}
}

func (sw *sourceWriter) writeCommentGroups(docs []*ast.CommentGroup, separator ...byte) {
	for _, doc := range docs {
		if doc.List == nil {
			continue
		}

		for i, c := range doc.List {
			pos := sw.pos
			sw.writeString(c.Text)
			if i < len(doc.List)-1 && sw.tokenFile.Line(c.End()) != sw.tokenFile.Line(doc.List[i+1].Pos()) {
				sw.writeByte('\n')
			}
			c.Slash = pos
		}
		sw.writeByte(separator...)
	}
}

func (sw *sourceWriter) writeFloatingComments(docs []*ast.CommentGroup) {
	sw.writeCommentGroups(docs, '\n', '\n')
}

func (sw *sourceWriter) writeComments(docs []*ast.CommentGroup) {
	sw.writeCommentGroups(docs, '\n')
}

func (sw *sourceWriter) writeInlineComments(docs []*ast.CommentGroup) {
	for _, doc := range docs {
		if doc.List == nil {
			continue
		}
		for i, c := range doc.List {
			c.Slash = sw.pos
			sw.writeString(c.Text)
			if i < len(doc.List)-1 {
				sw.writeByte(' ')
			}
		}
	}
}

func (sw *sourceWriter) writeString(str string) {
	sw.output = append(sw.output, str...)
	sw.pos += token.Pos(len(str))
}

func (sw *sourceWriter) writeCh(ch byte) {
	sw.output = append(sw.output, ch)
	sw.pos++
}

func (sw *sourceWriter) writeNewline() {
	sw.writeCh('\n')
}

func (sw *sourceWriter) writeSpace() {
	sw.writeCh(' ')
}

func (sw *sourceWriter) writeByte(bs ...byte) {
	sw.output = append(sw.output, bs...)
	sw.pos += token.Pos(len(bs))
}

func (sw *sourceWriter) delete() {
	sw.output = sw.output[:len(sw.output)-1]
	sw.pos--
}

type SourceFile struct {
	src         []byte
	fileSet     *token.FileSet
	tokenFile   *token.File
	astFile     *ast.File
	options     *Options
	importDecls []*importDecl
}

func newSourceFile(src []byte, fileSet *token.FileSet, astFile *ast.File, options *Options) *SourceFile {
	tokenFile := fileSet.File(astFile.Pos())
	sf := SourceFile{src: src, fileSet: fileSet, tokenFile: tokenFile, astFile: astFile, options: options}
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
func (sf *SourceFile) sync() error {
	tokenFile := sf.tokenFile
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

	// no import declarations
	if endPos < startPos {
		endPos = startPos
	}

	start := tokenFile.Offset(startPos)
	end := tokenFile.Offset(endPos)

	sw := newSourceWriter(sf.tokenFile)
	sw.writeByte(sf.src[:start]...)

	// TODO: don't write newline if not necessary
	sw.writeNewline()
	for _, decl := range sf.importDecls {
		if !decl.isEmpty() {
			sw.writeImportDecl(decl)
			sw.writeNewline()
		}
	}
	// delete the last newline
	if len(sf.importDecls) > 0 {
		sw.delete()
	}

	if sw.pos > endPos {
		// If the total length of import declarations get larger than the original, reconstruct AST to correct other tokens' positions.
		sw.writeByte(sf.src[end:]...)
		sf.src = sw.output
		sf.fileSet = token.NewFileSet()
		file, err := parseFile(sf.fileSet, sf.tokenFile.Name(), sf.src, sf.options)

		if err != nil {
			fmt.Printf("parse error: %v\n", string(sf.src))
			return fmt.Errorf("failed to reconstruct AST: %w", err)
		}
		sf.astFile = file
		sf.tokenFile.SetLinesForContent(sf.src)
		return nil
	}

	for sw.pos < endPos {
		sw.writeNewline()
	}
	sw.writeByte(sf.src[end:]...)
	sf.src = sw.output
	sf.tokenFile.SetLinesForContent(sf.src)

	// Remove merged declarations and comments from AST.
	newDecls := make([]ast.Decl, 0, len(sf.astFile.Decls))
	mergedComments := make([]*ast.CommentGroup, 0)

	for _, decl := range sf.importDecls {
		if decl.isEmpty() {
			continue
		}

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

func (sf *SourceFile) squashImportDecls() error {
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
		decl.sortAll()
		decl.dedupe()
		decls = append(decls, decl)
	}

	sf.importDecls = decls
	return sf.sync()
}

func (sf *SourceFile) addNamedImport(name string, path string) {
	newImport := &ast.ImportSpec{
		Path: &ast.BasicLit{
			Kind:  token.STRING,
			Value: strconv.Quote(path),
		},
	}
	if name != "" {
		newImport.Name = &ast.Ident{Name: name}
	}
	spec := newImportSpec(newImport, sf.options.LocalPrefix)

	for _, decl := range sf.importDecls {
		if decl.isCImportDecl() {
			continue
		}
		if decl.node.Lparen.IsValid() {
			// Remove parentheses if the decl is empty
			if len(decl.specs) == 0 && !decl.hasDeclComments() {
				decl.node.Lparen = token.NoPos
				decl.node.Rparen = token.NoPos
			}
			spec.pos = decl.end
			spec.end = decl.end
			decl.addSpec(spec)
			return
		}
	}
	if len(sf.importDecls) > 0 {
		lastDecl := sf.importDecls[len(sf.importDecls)-1]
		spec.pos = lastDecl.end
		spec.end = lastDecl.end
		sf.importDecls = append(sf.importDecls, newSingleImportDecl(spec))
		return
	}

	pkg := sf.astFile.Name.End()
	pkgLine := sf.tokenFile.Line(pkg)
	declPos := pkg
	for _, c := range sf.astFile.Comments {
		if sf.tokenFile.Line(c.Pos()) > pkgLine {
			break
		}
		declPos = c.End()
	}
	spec.pos = declPos + 1
	spec.end = declPos
	sf.importDecls = append(sf.importDecls, newSingleImportDecl(spec))
}

func (sf *SourceFile) deleteNamedImport(name string, path string) {
	for _, decl := range sf.importDecls {
		for j := 0; j < len(decl.specs); j++ {
			spec := decl.specs[j]
			if spec.name() == name && spec.path() == path {
				copy(decl.specs[j:], decl.specs[j+1:])
				decl.specs = decl.specs[:len(decl.specs)-1]

				// e.g. import "math" -> import ()
				if len(decl.specs) == 0 && !decl.hasDeclComments() {
					decl.node.Lparen = spec.pos
					decl.node.Rparen = spec.end
				}
				j--
			}
		}
	}
}

type byCommentPos []*ast.CommentGroup

func (x byCommentPos) Len() int           { return len(x) }
func (x byCommentPos) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }
func (x byCommentPos) Less(i, j int) bool { return x[i].Pos() < x[j].Pos() }
