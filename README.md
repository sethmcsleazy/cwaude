# AutoCAD LISP Routines

A collection of AutoLISP routines. Each file in `lisp/` defines one command.

## Sheet / drawing management

| Command | File | What it does |
|---|---|---|
| `SETLAYOUT` | `lisp/setlayout.lsp` | Renames the current layout to the drawing's file name, dropping characters layouts can't use (`< > / \ " : ; ? * \| , = `` ` ``). From the Model tab it renames the only layout. |
| `SETUPALL` | `lisp/setupall.lsp` | Pick a named page setup from this drawing; it's copied into every open drawing and applied to all their layouts (or just this drawing's layouts with the *Current* option). Drawings aren't saved for you. |
| `XREFLOAD` | `lisp/xrefload.lsp` | Reloads every xref in every open drawing. |
| `UNRUS` | `lisp/unrus.lsp` | Thaws every viewport-frozen layer in every viewport on every layout. |
| `EXDWG` | `lisp/exdwg.lsp` | Opens Windows Explorer at the drawing's folder with the file selected. |

## Drafting

| Command | File | What it does |
|---|---|---|
| `DOUBLEOFFSET` | `lisp/doubleoffset.lsp` | Enter a total width; each picked object is offset half that distance to both sides. Width is remembered for the session. |
| `AUTOTRANS` | `lisp/autotrans.lsp` | Pick 2 parallel lines of the first run, then 2 of the second run. Draws angled transition lines from the end of the first run, trims/extends the second run to meet them, and draws a line across the run at each end of the transition. Angle defaults to 30 deg; change it with the **[Angle]** option at the first prompt (remembered, like FILLET radius). |
| `FLOWTOTAL` | `lisp/flowtotal.lsp` | Adds up the first number in each selected TEXT/MTEXT/MLEADER. Shows a running total and keeps prompting so you can add more; already-counted text is ignored. Enter to finish and optionally place the total. |
| `MTFORMAT` | `lisp/mtformat.lsp` | For each selected MTEXT, finds the closed border around it, sets Middle Center justification, centers it, and sets its width so the grips sit on the borders. Border must be on screen. |

## Measuring / counting

| Command | File | What it does |
|---|---|---|
| `TLEN` | `lisp/tlen.lsp` | Total length of selected lines, arcs, circles, polylines, splines, ellipses |
| `TAREA` | `lisp/tarea.lsp` | Total area of selected closed polylines, circles, ellipses, splines, regions, hatches |
| `BCOUNT` | `lisp/bcount.lsp` | Count block references by name (dynamic blocks grouped by effective name); Enter = whole drawing |
| `NUMINC` | `lisp/numinc.lsp` | Click to place incrementing numbers with optional prefix/suffix |

## Loading

- **One-off:** type `APPLOAD`, browse to the `.lsp` file, click *Load*. Or drag the file into the drawing window.
- **Every drawing:** `APPLOAD` → *Startup Suite* → *Contents…* → add the files.
- **Via `acaddoc.lsp`:** add the `lisp/` folder to *Options → Files → Support File Search Path* (and *Trusted Locations*), then put lines like `(load "tlen.lsp")` in an `acaddoc.lsp` on that path.

Requires full AutoCAD (not AutoCAD LT before 2024, which lacks AutoLISP). Routines using `vla-*`/`vlax-*` functions are Windows-only.
