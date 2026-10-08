# AutoCAD LISP Routines

A collection of AutoLISP routines. Each file in `lisp/` defines one command.

| Command  | File              | What it does |
|----------|-------------------|--------------|
| `TLEN`   | `lisp/tlen.lsp`   | Total length of selected lines, arcs, circles, polylines, splines, ellipses |
| `TAREA`  | `lisp/tarea.lsp`  | Total area of selected closed polylines, circles, ellipses, splines, regions, hatches |
| `BCOUNT` | `lisp/bcount.lsp` | Count block references by name (dynamic blocks grouped by effective name); Enter = whole drawing |
| `NUMINC` | `lisp/numinc.lsp` | Click to place incrementing numbers with optional prefix/suffix |

## Loading

- **One-off:** type `APPLOAD`, browse to the `.lsp` file, click *Load*. Or drag the file into the drawing window.
- **Every drawing:** `APPLOAD` → *Startup Suite* → *Contents…* → add the files.
- **Via `acaddoc.lsp`:** add the `lisp/` folder to *Options → Files → Support File Search Path* (and *Trusted Locations*), then put lines like `(load "tlen.lsp")` in an `acaddoc.lsp` on that path.

Requires full AutoCAD (not AutoCAD LT before 2024, which lacks AutoLISP). Routines using `vla-*`/`vlax-*` functions are Windows-only.
