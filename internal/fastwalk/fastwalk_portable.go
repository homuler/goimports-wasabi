// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build appengine || (!linux && !darwin && !freebsd && !openbsd && !netbsd)
// +build appengine !linux,!darwin,!freebsd,!openbsd,!netbsd

package fastwalk

import (
	"io/fs"
	"os"
)

// readDir calls fn for each directory entry in dirName.
// It does not descend into directories or follow symlinks.
// If fn returns a non-nil error, readDir returns with that error
// immediately.
func readDir(dirName string, fn func(dirName, entName string, typ os.FileMode) error) error {
	fis, err := os.ReadDir(dirName)
	if err != nil {
		return err
	}
	infos := make([]fs.FileInfo, 0, len(fis))
	for _, entry := range fis {
		info, err := entry.Info()
		if err != nil {
			return err
		}
		infos = append(infos, info)
	}

	skipFiles := false
	for _, fi := range infos {
		if fi.Mode().IsRegular() && skipFiles {
			continue
		}
		if err := fn(dirName, fi.Name(), fi.Mode()&os.ModeType); err != nil {
			if err == ErrSkipFiles {
				skipFiles = true
				continue
			}
			return err
		}
	}
	return nil
}
