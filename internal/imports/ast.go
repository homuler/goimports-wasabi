package imports

import (
	"bytes"
	"fmt"
	"go/ast"
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

func needsNewlineAfter(cs []*ast.CommentGroup) bool {
	if len(cs) == 0 {
		return false
	}
	for i := len(cs) - 1; i >= 0; i-- {
		c := cs[i]
		if len(c.List) == 0 {
			continue
		}
		return c.List[len(c.List)-1].Text[1] == '/'
	}
	return false
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
		nextPos := d.Rparen // if len(d.Specs) == 0, then Rparen should be valid
		limitLine := tokenFile.LineCount() + 1
		if len(d.Specs) > 0 {
			nextPos = d.Specs[0].Pos()
			limitLine = idr.line(nextPos)
		}

		c := comments[idr.commentIndex]
		for idr.line(c.Pos()) == lparenLine && c.End() <= nextPos && idr.line(c.End()) < limitLine {
			decl.addPostLparen(c)
			idr.commentIndex++
			c = comments[idr.commentIndex]
		}
	}

	for j, s := range d.Specs {
		spec := newImportSpec(s, idr.localPrefix)
		if spec == nil {
			return nil, false
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

	nextLine := idr.line(nextPos)
	if nextPos == idr.maxPos {
		nextLine += 2 // any greater values will do
	}
	decl.addFooter(idr.readFooterComments(decl.end, nextPos, nextLine)...)
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

// sync synchronize sf.src with importDecls.
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

	if end < len(sf.src) && sf.src[end] == ';' {
		end++
	}
	sw.writeByte(sf.src[end:]...)
	sf.src = sw.output

	if sf.options.ReconstructAST {
		// If the total length of import declarations get larger than the original, reconstruct AST to correct other tokens' positions.
		sf.fileSet = token.NewFileSet()
		file, _, _, err := parse(sf.fileSet, sf.tokenFile.Name(), sf.src, sf.options)

		if err != nil {
			fmt.Printf("parse error: %v\n", string(sf.src))
			return fmt.Errorf("failed to reconstruct AST: %w", err)
		}
		sf.astFile = file
		sf.tokenFile.SetLinesForContent(sf.src)
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

func (sf *SourceFile) addNamedImport(name, path string) {
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
		cpos := c.Pos()
		if cpos < pkg {
			continue
		}
		if sf.tokenFile.Line(cpos) > pkgLine {
			break
		}
		declPos = c.End()
	}
	spec.pos = declPos + 1
	spec.end = declPos
	sf.importDecls = append(sf.importDecls, newSingleImportDecl(spec))
}

func (sf *SourceFile) deleteNamedImport(name, path string) {
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

// setImportName finds the matching import path and change the name.
func (sf *SourceFile) setImportName(name, path string) {
	for _, decl := range sf.importDecls {
		for _, spec := range decl.specs {
			if spec.path() == path {
				spec.node.Name = &ast.Ident{
					Name:    name,
					NamePos: spec.node.Pos(),
				}
			}
		}
	}
}

type byCommentPos []*ast.CommentGroup

func (x byCommentPos) Len() int           { return len(x) }
func (x byCommentPos) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }
func (x byCommentPos) Less(i, j int) bool { return x[i].Pos() < x[j].Pos() }
