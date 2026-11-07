# terminal-backend-lisp

Pure Common Lisp [`terminal-protocol`](https://github.com/egao1980/terminal-protocol) backend. It has no foreign code and covers the VT core subset from the [brief](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/terminal.md). Its contract is the [`terminal-parity`](https://github.com/egao1980/terminal-parity) corpus (libghostty-vt as oracle), plus unit tests for host effects, OSC 52/8, frames and scrollback.

Status: in progress.

## License

MIT
