package imports

import (
	"go/ast"
	"go/token"
)

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
	if needsNewlineAfter(decl.preLparen...) {
		sw.writeNewline()
	}
	decl.node.Lparen = sw.pos
	sw.writeString(token.LPAREN.String())
	sw.writeInlineComments(decl.postLparen)
	// NOTE: if there's no specs and no other comments between parentheses, we don't need to insert a newline
	if len(decl.specs) > 0 || len(decl.bottom) > 0 || needsNewlineAfter(decl.postLparen...) {
		sw.writeNewline()
	}

	sg := NoGroup
	for _, spec := range decl.specs {
		if spec.group != sg {
			if sg != NoGroup || len(decl.postLparen) > 0 {
				sw.writeNewline()
			}
			sg = spec.group

			switch sg {
			case StdLib:
				if len(decl.stdLibDoc) > 0 {
					sw.writeFloatingComments(decl.stdLibDoc)
				}
			case AppEngine:
				if len(decl.appEngineDoc) > 0 {
					sw.writeFloatingComments(decl.appEngineDoc)
				}
			case ForeignLib:
				if len(decl.foreignLibDoc) > 0 {
					sw.writeFloatingComments(decl.foreignLibDoc)
				}
			case LocalLib:
				if len(decl.localLibDoc) > 0 {
					sw.writeFloatingComments(decl.localLibDoc)
				}
			}
		}
		sw.writeImportSpec(spec)
	}

	if len(decl.bottom) > 0 {
		if len(decl.specs) > 0 {
			sw.writeNewline()
		}
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
	shouldInsertNewline := false

	for _, doc := range docs {
		for i, c := range doc.List {
			if shouldInsertNewline {
				sw.writeNewline()
				shouldInsertNewline = false
			}
			c.Slash = sw.pos
			sw.writeString(c.Text)
			if c.Text[1] == '/' {
				shouldInsertNewline = true
			} else if i < len(doc.List)-1 {
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
