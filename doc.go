// Copyright 2013 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

/*
Command goimports-wasabi updates your Go import lines,
adding missing ones and removing unreferenced ones.

	$ go install github.com/homuler/goimports-wasabi@latest

In addition to fixing imports, goimports-wasabi also formats
your code in the same style as gofmt so it can be used
as a replacement for your editor's gofmt-on-save hook.

For emacs, make sure you have the latest go-mode.el:

	https://github.com/dominikh/go-mode.el

Then in your .emacs file:

	(setq gofmt-command "goimports-wasabi")
	(add-hook 'before-save-hook 'gofmt-before-save)

For vim, set "gofmt_command" to "goimports-wasabi":

	https://golang.org/change/39c724dd7f252
	https://golang.org/wiki/IDEsAndTextEditorPlugins
	etc

For GoSublime, follow the steps described here:

	http://michaelwhatcott.com/gosublime-goimports/

For other editors, you probably know what to do.

To exclude directories in your $GOPATH from being scanned for Go
files, goimports respects a configuration file at
$GOPATH/src/.goimportsignore which may contain blank lines, comment
lines (beginning with '#'), or lines naming a directory relative to
the configuration file to ignore when scanning. No globbing or regex
patterns are allowed. Use the "-v" verbose flag to verify it's
working and see what goimports-wasabi is doing.

File bugs or feature requests at:

	https://github.com/homuler/goimports-wasabi/issues

Happy hacking!
*/
package main // import "github.com/homuler/goimports-wasabi"
