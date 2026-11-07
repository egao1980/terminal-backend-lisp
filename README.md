# terminal-backend-lisp

Pure Common Lisp [`terminal-protocol`](https://github.com/egao1980/terminal-protocol) backend. It has no foreign code and covers the VT core subset from the [brief](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/terminal.md). Its contract is the [`terminal-parity`](https://github.com/egao1980/terminal-parity) corpus (libghostty-vt as oracle), plus unit tests for host effects, OSC 52/8, frames and scrollback.

Passes all 98 parity fixtures (fed whole and byte by byte) plus 13 unit tests.

Features: `:scrollback :alternate-screen :osc-52 :format-vt :kitty-graphics`. Kitty graphics covers raw RGB/RGBA pixels sent directly (`t=d`). Compressed (`o=z`), PNG and file payloads get an `EINVAL` reply, so `:kitty-zlib` is not claimed. It is not ghostty: no reflow when columns change, SU/SD and ED 2 never add to scrollback, invalid UTF-8 becomes U+FFFD, and 8-bit C1 bytes are not controls.

## License

MIT
