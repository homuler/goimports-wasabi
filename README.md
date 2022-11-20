# goimports-wasabi

[![Run Tests](https://github.com/homuler/goimports-wasabi/actions/workflows/test.yml/badge.svg)](https://github.com/homuler/goimports-wasabi/actions/workflows/test.yml)

This is a fork of [`goimports`](https://pkg.go.dev/golang.org/x/tools/cmd/goimports), inspired by [the `gosimports` project](https://github.com/rinchsan/gosimports).
`goimports-wasabi` also tries to solve [the import grouping/ordering problem](https://github.com/golang/go/issues/20818), but in a different way.

## Overview

`goimports` and `gosimports` have the following issues (as of 2022/11).

- `goimports` doesn't sort import specs deterministically.
- `goimports` doesn't treat comments associated with import specs properly.
- `goimports` doesn't work well for artificial inputs.
- `gosimports` removes comments associated with import specs.

`goimports-wasabi` is designed to work deterministically with any valid input, leaving as many comments as possible in the (supposedly) correct place.

## Installation

```sh
go install github.com/homuler/goimports-wasabi@latest
```

## Examples

### Grouping/Ordering Problem

#### Input

```go
import (
	// doc comment for fmt
	"fmt"

	/*
	 * block comments
	 */

	 // doc comment for foo
	"github.com/homuler/foo"
	"context"
	// footer comment (maybe for context)

	"strings" // line comment for strings

	b "github.com/homuler/bar"

	_ "runtime/pprof" // line comment for pprof
)
```

#### Output

```go
import (
	"context"
	// footer comment (maybe for context)

	// doc comment for fmt
	"fmt"
	_ "runtime/pprof" // line comment for pprof
	"strings"         // line comment for strings

	/*
	 * block comments
	 */

	b "github.com/homuler/bar"
	// doc comment for foo
	"github.com/homuler/foo"
)
```

<details>
<summary>goimports</summary>

```go
import (
	// doc comment for fmt
	"fmt"

	/*
	 * block comments
	 */

	// doc comment for foo
	"context"

	"github.com/homuler/foo"

	// footer comment (maybe for context)

	"strings" // line comment for strings

	b "github.com/homuler/bar"

	_ "runtime/pprof" // line comment for pprof
)
```

</details>

### Merging Problem

#### Input

```go
// doc comment for fmt
import "fmt"

// doc comment for context
import "context"

// doc comment
import (
	// doc comment for errors
	"errors"
)
```

#### Output

`goimports-wasabi`

```go
// doc comment
import (
	// doc comment for context
	"context"
	// doc comment for errors
	"errors"
	// doc comment for fmt
	"fmt"
)
```

<details>
<summary>goimports</summary>

```go
// doc comment for fmt
import (
	"context"
	"errors"
	"fmt"
)

// doc comment for context

// doc comment

// doc comment for errors
```

</details>

### Unusual Input

#### Input

```go
import ("fmt";"fmt";"context";"github.com/homuler/foo")
```

#### Output

`goimports-wasabi -format-only`

```go
import (
	"context"
	"fmt"

	"github.com/homuler/foo"
)
```

<details>
<summary>goimports -format-only</summary>

```go
panic: invalid line number 2 (should be < 2)
```

</details>

## Known Issues

`goimports-wasabi` runs [`format.Source`](https://pkg.go.dev/go/format#Source) internally only once.
That means if [`gofmt` is not idempotent](https://github.com/golang/go/issues/24472), nor is `goimports-wasabi`.

### Input

```input.go
import (
	/* c (1) */"fmt" /* comment
	for fmt */ // comment for fmt
)

import "context"
// footer comment for context

// doc comment for fmt
import /* c (2) */ "fmt"
```

### Output

`cat input.go | goimports-wasabi`

```go
import (
	"context"
	// footer comment for context

	// doc comment for fmt
	/* c (1) */ /* c (2) */
	"fmt" /* comment
	for fmt */ // comment for fmt
)
```

`cat input.go | goimports-wasabi | goimports-wasabi`

```go
import (
	"context"
	// footer comment for context

	// doc comment for fmt
	/* c (1) */ /* c (2) */
	"fmt"       /* comment
	for fmt */ // comment for fmt
)
```

## License

[BSD-3-Clause](https://github.com/homuler/goimports-wasabi/blob/main/LICENSE)

Copyright (c) 2009 The Go Authors. All rights reserved.
