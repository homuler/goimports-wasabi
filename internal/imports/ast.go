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

func specGroup(s *ast.ImportSpec, localPrefix string) SpecGroup {
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

type bySpecGroup []*ImportSpec

func (x bySpecGroup) Len() int      { return len(x) }
func (x bySpecGroup) Swap(i, j int) { x[i], x[j] = x[j], x[i] }
func (x bySpecGroup) Less(i, j int) bool {
	ipath := x[i].path()
	jpath := x[j].path()

	igroup := x[i].Group
	jgroup := x[j].Group
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

// ImportDecl represents *ast.GenDecl (import declaration) with the relavent comments
type ImportDecl struct {
	Node  *ast.GenDecl
	Specs []*ImportSpec

	Header     []*ast.CommentGroup
	Doc        []*ast.CommentGroup
	PreLparen  []*ast.CommentGroup
	PostLparen []*ast.CommentGroup
	Bottom     []*ast.CommentGroup
	PostRparen []*ast.CommentGroup
	StdLibDoc  []*ast.CommentGroup
	ForeignDoc []*ast.CommentGroup
	LocalDoc   []*ast.CommentGroup
	Footer     []*ast.CommentGroup

	pos token.Pos
	end token.Pos
}

func newImportDecl(d *ast.GenDecl, localPrefix string) *ImportDecl {
	return &ImportDecl{Node: d, pos: d.Pos(), end: d.End()}
}

func newSingleImportDecl(spec *ImportSpec) *ImportDecl {
	gen := &ast.GenDecl{Tok: token.IMPORT, TokPos: spec.Node.Pos()}
	decl := ImportDecl{Node: gen, pos: spec.pos, end: spec.end}

	decl.Doc = spec.Doc
	spec.Doc = nil
	decl.Footer = spec.Footer
	spec.Footer = nil

	return &decl
}

func (decl *ImportDecl) isCImportDecl() bool {
	if len(decl.Specs) != 1 {
		return false
	}
	return decl.Specs[0].isCSpec()
}

func (decl *ImportDecl) distillCImports() (*ImportDecl, *ImportDecl) {
	if decl.isCImportDecl() {
		return decl, nil
	}

	var cspec *ImportSpec
	specs := make([]*ImportSpec, 0, len(decl.Specs))

	for _, spec := range decl.Specs {
		if !spec.isCSpec() {
			specs = append(specs, spec)
			continue
		}
		if cspec == nil {
			cspec = spec
		} else {
			cspec.merge(spec)
		}
	}

	if cspec == nil {
		return nil, decl
	}
	decl.Specs = specs
	return newSingleImportDecl(cspec), decl
}

func (decl *ImportDecl) addSpec(ss ...*ImportSpec) {
	decl.Specs = append(decl.Specs, ss...)
	for _, s := range ss {
		if s.pos < decl.pos {
			decl.pos = s.pos
		}
		if s.end > decl.end {
			decl.end = s.end
		}
	}
	sort.Sort(bySpecGroup(decl.Specs))
}

func (decl *ImportDecl) addHeader(cg ...*ast.CommentGroup) {
	decl.Header = append(decl.Header, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < decl.pos {
			decl.pos = p
		}
	}
	sort.Sort(byCommentPos(decl.Header))
}

func (decl *ImportDecl) addDoc(cg ...*ast.CommentGroup) {
	decl.Doc = append(decl.Doc, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < decl.pos {
			decl.pos = p
		}
	}
	sort.Sort(byCommentPos(decl.Doc))
}

func (decl *ImportDecl) addPreLparen(cg ...*ast.CommentGroup) {
	decl.PreLparen = append(decl.PreLparen, cg...)
	sort.Sort(byCommentPos(decl.PreLparen))
}

func (decl *ImportDecl) addPostLparen(cg ...*ast.CommentGroup) {
	decl.PostLparen = append(decl.PostLparen, cg...)
	sort.Sort(byCommentPos(decl.PostLparen))
}

func (decl *ImportDecl) addStdLibDoc(cg ...*ast.CommentGroup) {
	decl.StdLibDoc = append(decl.StdLibDoc, cg...)
	sort.Sort(byCommentPos(decl.StdLibDoc))
}

func (decl *ImportDecl) addForeignDoc(cg ...*ast.CommentGroup) {
	decl.ForeignDoc = append(decl.ForeignDoc, cg...)
	sort.Sort(byCommentPos(decl.ForeignDoc))
}

func (decl *ImportDecl) addLocalDoc(cg ...*ast.CommentGroup) {
	decl.LocalDoc = append(decl.LocalDoc, cg...)
	sort.Sort(byCommentPos(decl.LocalDoc))
}

func (decl *ImportDecl) addBottom(cg ...*ast.CommentGroup) {
	decl.Bottom = append(decl.Bottom, cg...)
	sort.Sort(byCommentPos(decl.Bottom))
}

func (decl *ImportDecl) addPostRparen(cg ...*ast.CommentGroup) {
	decl.PostRparen = append(decl.PostRparen, cg...)
	for _, c := range cg {
		p := c.End()
		if p.IsValid() && p > decl.end {
			decl.end = p
		}
	}
	sort.Sort(byCommentPos(decl.PostRparen))
}

func (decl *ImportDecl) addFooter(cg ...*ast.CommentGroup) {
	decl.Footer = append(decl.Footer, cg...)
	for _, c := range cg {
		p := c.End()
		if p.IsValid() && p > decl.end {
			decl.end = p
		}
	}
	sort.Sort(byCommentPos(decl.Footer))
}

func (decl *ImportDecl) addGroupDoc(c *ast.CommentGroup, g SpecGroup) {
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
func (x *ImportDecl) merge(y *ImportDecl) {
	x.normalize()
	y.normalize()

	x.addSpec(y.Specs...)
	x.addHeader(y.Header...)
	x.addDoc(y.Doc...)
	x.addPreLparen(y.PreLparen...)
	x.addPostLparen(y.PostLparen...)
	x.addStdLibDoc(y.StdLibDoc...)
	x.addForeignDoc(y.ForeignDoc...)
	x.addLocalDoc(y.LocalDoc...)
	x.addBottom(y.Bottom...)
	x.addPostRparen(y.PostRparen...)
	x.addFooter(y.Footer...)
}

func (decl *ImportDecl) normalize() {
	if decl.Node.Lparen.IsValid() {
		return
	}
	// Set temporary positions.
	// These values will be fixed when printing decl.
	decl.Node.Lparen = decl.Node.TokPos + 6
	decl.Node.Rparen = decl.Node.End()

	if len(decl.Specs) > 0 { // len equals 1
		spec := decl.Specs[0]
		spec.Doc = append(spec.Doc, decl.Doc...)
		decl.Doc = nil
		spec.Footer = append(spec.Footer, decl.Footer...)
		decl.Footer = nil
	}
}

func (decl *ImportDecl) dedupe() {
	if len(decl.Specs) < 2 {
		return
	}
	i := 0
	for i < len(decl.Specs)-1 {
		spec := decl.Specs[i]
		next := decl.Specs[i+1]
		if spec.path() != next.path() || spec.name() != next.name() {
			i++
			continue
		}
		spec.merge(next)
		decl.Specs = append(decl.Specs[:i], decl.Specs[i+1:]...)
	}
}

// ImportSpec represents *ast.ImportSpec with the relavent comments
type ImportSpec struct {
	Node        *ast.ImportSpec
	Group       SpecGroup
	Doc         []*ast.CommentGroup
	NameComment []*ast.CommentGroup
	PathComment []*ast.CommentGroup
	Comment     []*ast.CommentGroup
	Footer      []*ast.CommentGroup

	pos token.Pos
	end token.Pos
}

func newImportSpec(astSpec ast.Spec, localPrefix string) *ImportSpec {
	s, ok := astSpec.(*ast.ImportSpec)
	if !ok {
		return nil
	}
	return &ImportSpec{Node: s, Group: specGroup(s, localPrefix), pos: s.Pos(), end: s.End()}
}

func (spec *ImportSpec) namePos() token.Pos {
	if spec.Node.Name != nil {
		return spec.Node.Name.NamePos
	}
	return token.NoPos
}

func (spec *ImportSpec) name() string {
	if spec.Node.Name != nil {
		return spec.Node.Name.Name
	}
	return ""
}

func (spec *ImportSpec) pathPos() token.Pos {
	if spec.Node.Path != nil {
		return spec.Node.Path.ValuePos
	}
	return token.NoPos
}

func (spec *ImportSpec) pathEnd() token.Pos {
	if spec.Node.Path != nil {
		return spec.Node.Path.End()
	}
	return token.NoPos
}

func (spec *ImportSpec) path() string {
	return pathValue(spec.Node)
}

func (spec *ImportSpec) firstComment() string {
	if len(spec.Comment) == 0 {
		return ""
	}
	return spec.Comment[0].Text()
}

func (spec *ImportSpec) isCSpec() bool {
	return spec.Node.Path.Value == `"C"`
}

func (spec *ImportSpec) addDoc(cg ...*ast.CommentGroup) {
	spec.Doc = append(spec.Doc, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < spec.pos {
			spec.pos = p
		}
	}
	sort.Sort(byCommentPos(spec.Doc))
}

func (spec *ImportSpec) addNameComment(cg ...*ast.CommentGroup) {
	spec.NameComment = append(spec.NameComment, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < spec.pos {
			spec.pos = p
		}
	}
	sort.Sort(byCommentPos(spec.NameComment))
}

func (spec *ImportSpec) addPathComment(cg ...*ast.CommentGroup) {
	spec.PathComment = append(spec.PathComment, cg...)
	for _, c := range cg {
		p := c.Pos()
		if p.IsValid() && p < spec.pos {
			spec.pos = p
		}
	}
	sort.Sort(byCommentPos(spec.PathComment))
}

func (spec *ImportSpec) addComment(cg ...*ast.CommentGroup) {
	spec.Comment = append(spec.Comment, cg...)
	for _, c := range cg {
		p := c.End()
		if p.IsValid() && p > spec.end {
			spec.end = p
		}
	}
	sort.Sort(byCommentPos(spec.Comment))
}

func (spec *ImportSpec) addFooter(cg ...*ast.CommentGroup) {
	spec.Footer = append(spec.Footer, cg...)
	for _, c := range cg {
		p := c.End()
		if p.IsValid() && p > spec.end {
			spec.end = p
		}
	}
	sort.Sort(byCommentPos(spec.Footer))
}

// merge merges 2 ImportSpecs.
// Make sure x and y have the same names and paths before calling it.
func (x *ImportSpec) merge(y *ImportSpec) {
	x.addDoc(y.Doc...)
	x.addNameComment(y.NameComment...)
	x.addPathComment(y.PathComment...)
	x.addComment(y.Comment...)
	x.addFooter(y.Footer...)
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

func (idr *importDeclReader) readNext() (*ImportDecl, bool) {
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

	for comments[idr.commentIndex].End() <= decl.Node.Pos() {
		decl.addDoc(comments[idr.commentIndex])
		idr.commentIndex++
	}

	if decl.Node.Lparen.IsValid() {
		for comments[idr.commentIndex].Pos() <= decl.Node.Lparen {
			decl.addPreLparen(comments[idr.commentIndex])
			idr.commentIndex++
		}

		lparenLine := tokenFile.Line(decl.Node.Lparen)
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

		if decl.Node.Lparen.IsValid() {
			sg := spec.Group

			for tokenFile.Line(comments[idr.commentIndex].End()) < tokenFile.Line(spec.Node.Pos())-1 {
				decl.addGroupDoc(comments[idr.commentIndex], sg)
				idr.commentIndex++
			}

			for tokenFile.Line(comments[idr.commentIndex].End()) < tokenFile.Line(spec.Node.Pos()) {
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

		if decl.Node.Rparen.IsValid() {
			// NOTE: if decl has its rparen, comment groups should be divided before the rparen.
			nextPos := tokenFile.Pos(tokenFile.Size())
			if j < len(d.Specs)-1 {
				nextPos = d.Specs[j+1].Pos()
			}
			spec.addFooter(idr.readFooterComments(spec.end, decl.Node.Rparen, tokenFile.Line(nextPos))...)
		}

		decl.addSpec(spec)
	}

	nextPos := tokenFile.Pos(tokenFile.Size())
	if idr.declIndex < len(idr.decls)-1 {
		nextPos = idr.decls[idr.declIndex+1].Pos()
	}

	if decl.Node.Rparen.IsValid() {
		for comments[idr.commentIndex].End() <= decl.Node.Rparen {
			decl.addBottom(comments[idr.commentIndex])
			idr.commentIndex++
		}

		c := comments[idr.commentIndex]
		for c.End() <= nextPos && tokenFile.Line(c.Pos()) == tokenFile.Line(decl.Node.Rparen) && !idr.containsSemicolon(decl.Node.Rparen, c.Pos()) {
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
	return &sourceWriter{tokenFile: tokenFile, output: make([]byte, 0, 1<<14), pos: 1}
}

func (sw *sourceWriter) writeImportDecl(decl *ImportDecl) {
	sw.writeFloatingComments(decl.Header)
	sw.writeComments(decl.Doc)
	decl.Node.TokPos = sw.pos
	sw.writeString(token.IMPORT.String())
	sw.writeInlineComments(decl.PreLparen)
	decl.Node.Lparen = sw.pos
	sw.writeString(token.LPAREN.String())
	sw.writeInlineComments(decl.PostLparen)
	sw.writeNewline()

	sg := NoGroup
	for _, spec := range decl.Specs {
		if spec.Group != sg {
			sg = spec.Group

			switch sg {
			case StdLib:
				if decl.StdLibDoc != nil {
					sw.writeFloatingComments(decl.StdLibDoc)
				}
			case Foreign:
				sw.writeNewline()
				if decl.ForeignDoc != nil {
					sw.writeFloatingComments(decl.ForeignDoc)
				}
			case Local:
				sw.writeNewline()
				if decl.LocalDoc != nil {
					sw.writeFloatingComments(decl.LocalDoc)
				}
			}
		}
		sw.writeImportSpec(spec)
	}

	if len(decl.Bottom) > 0 {
		sw.writeNewline()
		sw.writeComments(decl.Bottom)
	}

	decl.Node.Rparen = sw.pos
	sw.writeString(token.RPAREN.String())
	sw.writeInlineComments(decl.PostRparen)
	sw.writeNewline()
}

func (sw *sourceWriter) writeSingleImportDecl(decl *ImportDecl) {
	sw.writeComments(decl.Doc)
	decl.Node.TokPos = sw.pos
	sw.writeString(token.IMPORT.String())
	sw.writeSpace()
	sw.writeImportSpec(decl.Specs[0])
	sw.writeComments(decl.Bottom)
}

func (sw *sourceWriter) writeImportSpec(spec *ImportSpec) {
	if len(spec.Doc) > 0 {
		sw.writeComments(spec.Doc)
	}
	if spec.Node.Name != nil {
		sw.writeInlineComments(spec.NameComment)
		spec.Node.Name.NamePos = sw.pos
		sw.writeString(spec.Node.Name.Name)
	}
	if spec.Node.Path != nil {
		sw.writeInlineComments(spec.PathComment)
		spec.Node.Path.ValuePos = sw.pos
		sw.writeString(spec.Node.Path.Value)
	}
	sw.writeInlineComments(spec.Comment)
	sw.writeNewline()
	if len(spec.Footer) > 0 {
		sw.writeComments(spec.Footer)
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

func (sw *sourceWriter) delete(n int) {
	sw.output = sw.output[:len(sw.output)-n]
	sw.pos--
}
