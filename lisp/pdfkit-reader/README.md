# pdfkit-reader

`pdfkit-reader` is a deliberately small PDF reader for macOS Emacs. It
embeds Apple's native `PDFView` in the Emacs window, so drawing and
trackpad scrolling are handled directly by PDFKit.

The package currently provides only reading essentials:

- continuous vertical display;
- smooth native scrolling;
- next/previous and first/last page;
- go to page;
- zoom, fit width, and fit page;
- resize and split-window tracking;
- automatic cleanup when a buffer closes;
- automatic fallback to Emacs DocView if PDFKit cannot open a file.

There are intentionally no tabs, toolbars, annotations, browser features,
or session management.

## Build

The first PDF open builds the native module automatically. To build it
manually:

```sh
cd ~/.emacs.d/lisp/pdfkit-reader
make
```

This requires macOS, Xcode Command Line Tools, and an Emacs installation
whose `emacs-module.h` is available. The Makefile detects the header in
`/Applications/Emacs.app` by default.

## Keys

| Key | Action |
| --- | --- |
| `n`, `p` | Next/previous page |
| `SPC`, `DEL` | Scroll down/up one screen |
| `C-n`, `C-p`, arrows | Scroll down/up |
| `<`, `>` | First/last page |
| `g` | Go to page |
| `+`, `-` | Zoom in/out |
| `w`, `0` | Fit width/page |
| `r` | Reload |
| `q`, `Q` | Quit window/kill buffer |

Use `M-x pdfkit-reader-open-in-doc-view` for the built-in fallback.

## Copyright and license

Copyright (C) 2026 Rui Ying.

The native-view embedding approach is derived in part from
[Appine](https://github.com/chaoswork/appine), Copyright (C) 2026 Chao
Huang. Appine is distributed under GNU GPL version 3 or later.

This package is likewise free software distributed under GNU GPL version
3 or later; the complete license is in [`LICENSE`](LICENSE). Keeping the
attribution and compatible license is intentional: it preserves the
original author's rights while allowing this smaller reader to be
modified and shared.
