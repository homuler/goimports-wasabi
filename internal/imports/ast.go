package imports

import (
	"bytes"
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
	Foreign
	Local
)

func specGroupFor(s *ast.ImportSpec, localPrefix string) SpecGroup {
	path := pathValue(s)

	if localPrefix != "" {
		for _, p := range strings.Split(localPrefix, ",") {
			if strings.HasPrefix(path, p) || strings.TrimSuffix(p, "/") == path {
				return Local
			}
		}
	}
	firstComponent := strings.Split(path, "/")[0]
	if strings.Contains(firstComponent, ".") {
		return Foreign
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

	header     []*ast.CommentGroup
	doc        []*ast.CommentGroup
	preLparen  []*ast.CommentGroup
	postLparen []*ast.CommentGroup
	bottom     []*ast.CommentGroup
	postRparen []*ast.CommentGroup
	stdLibDoc  []*ast.CommentGroup
	foreignDoc []*ast.CommentGroup
	localDoc   []*ast.CommentGroup
	footer     []*ast.CommentGroup

	pos token.Pos
	end token.Pos
}

func newImportDecl(d *ast.GenDecl, localPrefix string) *importDecl {
	return &importDecl{node: d, pos: d.Pos(), end: d.End()}
}

func newSingleImportDecl(spec *importSpec) *importDecl {
	gen := &ast.GenDecl{Tok: token.IMPORT, TokPos: spec.node.Pos()}
	decl := importDecl{node: gen, pos: spec.pos, end: spec.end}

	decl.addDoc(spec.doc...)
	spec.doc = nil
	decl.addFooter(spec.footer...)
	spec.footer = nil
	decl.addSpec(spec)

	return &decl
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
	sort.Sort(bySpecGroup(decl.specs))
}

func (decl *importDecl) addHeader(cg ...*ast.CommentGroup) {
	decl.header = append(decl.header, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < decl.pos {
			decl.pos = p
		}
	}
	sort.Sort(byCommentPos(decl.header))
}

func (decl *importDecl) addDoc(cg ...*ast.CommentGroup) {
	decl.doc = append(decl.doc, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < decl.pos {
			decl.pos = p
		}
	}
	sort.Sort(byCommentPos(decl.doc))
}

func (decl *importDecl) addPreLparen(cg ...*ast.CommentGroup) {
	decl.preLparen = append(decl.preLparen, cg...)
	sort.Sort(byCommentPos(decl.preLparen))
}

func (decl *importDecl) addPostLparen(cg ...*ast.CommentGroup) {
	decl.postLparen = append(decl.postLparen, cg...)
	sort.Sort(byCommentPos(decl.postLparen))
}

func (decl *importDecl) addStdLibDoc(cg ...*ast.CommentGroup) {
	decl.stdLibDoc = append(decl.stdLibDoc, cg...)
	sort.Sort(byCommentPos(decl.stdLibDoc))
}

func (decl *importDecl) addForeignDoc(cg ...*ast.CommentGroup) {
	decl.foreignDoc = append(decl.foreignDoc, cg...)
	sort.Sort(byCommentPos(decl.foreignDoc))
}

func (decl *importDecl) addLocalDoc(cg ...*ast.CommentGroup) {
	decl.localDoc = append(decl.localDoc, cg...)
	sort.Sort(byCommentPos(decl.localDoc))
}

func (decl *importDecl) addBottom(cg ...*ast.CommentGroup) {
	decl.bottom = append(decl.bottom, cg...)
	sort.Sort(byCommentPos(decl.bottom))
}

func (decl *importDecl) addPostRparen(cg ...*ast.CommentGroup) {
	decl.postRparen = append(decl.postRparen, cg...)
	for _, c := range cg {
		p := c.End()
		if p.IsValid() && p > decl.end {
			decl.end = p
		}
	}
	sort.Sort(byCommentPos(decl.postRparen))
}

func (decl *importDecl) addFooter(cg ...*ast.CommentGroup) {
	decl.footer = append(decl.footer, cg...)
	for _, c := range cg {
		p := c.End()
		if p.IsValid() && p > decl.end {
			decl.end = p
		}
	}
	sort.Sort(byCommentPos(decl.footer))
}

func (decl *importDecl) addGroupDoc(c *ast.CommentGroup, g SpecGroup) {
	switch g {
	case StdLib:
		decl.addStdLibDoc(c)
	case Foreign:
		decl.addForeignDoc(c)
	case Local:
		decl.addLocalDoc(c)
	}
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
	x.addForeignDoc(y.foreignDoc...)
	x.addLocalDoc(y.localDoc...)
	x.addBottom(y.bottom...)
	x.addPostRparen(y.postRparen...)
	x.addFooter(y.footer...)
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
	if len(decl.specs) < 2 {
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
	sort.Sort(byCommentPos(spec.doc))
}

func (spec *importSpec) addNameComment(cg ...*ast.CommentGroup) {
	spec.nameComment = append(spec.nameComment, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < spec.pos {
			spec.pos = p
		}
	}
	sort.Sort(byCommentPos(spec.nameComment))
}

func (spec *importSpec) addPathComment(cg ...*ast.CommentGroup) {
	spec.pathComment = append(spec.pathComment, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < spec.pos {
			spec.pos = p
		}
	}
	sort.Sort(byCommentPos(spec.pathComment))
}

func (spec *importSpec) addComment(cg ...*ast.CommentGroup) {
	spec.comment = append(spec.comment, cg...)
	for _, c := range cg {
		p := c.End()
		if p.IsValid() && p > spec.end {
			spec.end = p
		}
	}
	sort.Sort(byCommentPos(spec.comment))
}

func (spec *importSpec) addFooter(cg ...*ast.CommentGroup) {
	spec.footer = append(spec.footer, cg...)
	for _, c := range cg {
		p := c.End()
		if p.IsValid() && p > spec.end {
			spec.end = p
		}
	}
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
}

func newImportDeclReader(src []byte, tokenFile *token.File, astFile *ast.File, localPrefix string) *importDeclReader {
	comments := make([]*ast.CommentGroup, len(astFile.Comments))
	copy(comments, astFile.Comments)

	sentinelComment := ast.Comment{Slash: tokenFile.Pos(tokenFile.Size())}
	comments = append(comments, &ast.CommentGroup{List: []*ast.Comment{&sentinelComment}})

	return &importDeclReader{
		src:          src,
		tokenFile:    tokenFile,
		comments:     comments,
		decls:        astFile.Decls,
		localPrefix:  localPrefix,
		commentIndex: 0,
		declIndex:    -1,
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

		lparenLine := tokenFile.Line(decl.node.Lparen)
		firstSpecLine := tokenFile.LineCount()
		if len(d.Specs) > 0 {
			firstSpecLine = tokenFile.Line(d.Specs[0].Pos())
		}

		c := comments[idr.commentIndex]
		for tokenFile.Line(c.Pos()) == lparenLine && tokenFile.Line(c.End()) < firstSpecLine {
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

			for tokenFile.Line(comments[idr.commentIndex].End()) < tokenFile.Line(spec.node.Pos())-1 {
				decl.addGroupDoc(comments[idr.commentIndex], sg)
				idr.commentIndex++
			}

			for tokenFile.Line(comments[idr.commentIndex].End()) < tokenFile.Line(spec.node.Pos()) {
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
		if tokenFile.Line(comments[idr.commentIndex].Pos()) == tokenFile.Line(spec.pathPos()) &&
			!idr.containsSemicolon(spec.pathEnd(), comments[idr.commentIndex].Pos()) {
			spec.addComment(comments[idr.commentIndex])
			idr.commentIndex++
		}

		if decl.node.Rparen.IsValid() {
			// NOTE: if decl has its rparen, comment groups should be divided before the rparen.
			nextPos := tokenFile.Pos(tokenFile.Size())
			if j < len(d.Specs)-1 {
				nextPos = d.Specs[j+1].Pos()
			}
			spec.addFooter(idr.readFooterComments(spec.end, decl.node.Rparen, tokenFile.Line(nextPos))...)
		}

		decl.addSpec(spec)
	}

	nextPos := tokenFile.Pos(tokenFile.Size())
	if idr.declIndex < len(idr.decls)-1 {
		nextPos = idr.decls[idr.declIndex+1].Pos()
	}

	if decl.node.Rparen.IsValid() {
		for comments[idr.commentIndex].End() <= decl.node.Rparen {
			decl.addBottom(comments[idr.commentIndex])
			idr.commentIndex++
		}

		c := comments[idr.commentIndex]
		for c.End() <= nextPos && tokenFile.Line(c.Pos()) == tokenFile.Line(decl.node.Rparen) && !idr.containsSemicolon(decl.node.Rparen, c.Pos()) {
			decl.addPostRparen(c)
			idr.commentIndex++
			c = comments[idr.commentIndex]
		}
	}

	decl.addFooter(idr.readFooterComments(decl.end, nextPos, tokenFile.Line(nextPos))...)
	return decl, true
}

func (idr *importDeclReader) readHeaderComments(d *ast.GenDecl) []*ast.CommentGroup {
	tokenFile := idr.tokenFile
	comments := idr.comments
	dpos := d.Pos()

	var cs []*ast.CommentGroup
	c := comments[idr.commentIndex]

	// NOTE: consecutive comments should be already grouped into the same CommentGroup.
	for tokenFile.Line(c.End()) < tokenFile.Line(dpos)-1 {
		cs = append(cs, c)
		idr.commentIndex++
		c = comments[idr.commentIndex]
	}
	return cs
}

func (idr *importDeclReader) readFooterComments(pos, end token.Pos, nextLine int) []*ast.CommentGroup {
	tokenFile := idr.tokenFile
	comments := idr.comments

	var cs []*ast.CommentGroup
	c := comments[idr.commentIndex]

	// NOTE: consecutive comments should be already grouped into the same CommentGroup.
	if c.End() <= end && tokenFile.Line(c.Pos()) <= tokenFile.Line(pos)+1 && tokenFile.Line(c.End()) < nextLine-1 {
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
	if !decl.node.Lparen.IsValid() {
		sw.writeFloatingComments(decl.header)
		sw.writeComments(decl.doc)
		decl.node.TokPos = sw.pos
		sw.writeString(token.IMPORT.String())
		sw.writeSpace()
		sw.writeImportSpec(decl.specs[0])
		sw.writeComments(decl.bottom)
		return
	}

	sw.writeFloatingComments(decl.header)
	sw.writeComments(decl.doc)
	decl.node.TokPos = sw.pos
	sw.writeString(token.IMPORT.String())
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
				if decl.stdLibDoc != nil {
					sw.writeFloatingComments(decl.stdLibDoc)
				}
			case Foreign:
				sw.writeNewline()
				if decl.foreignDoc != nil {
					sw.writeFloatingComments(decl.foreignDoc)
				}
			case Local:
				sw.writeNewline()
				if decl.localDoc != nil {
					sw.writeFloatingComments(decl.localDoc)
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
