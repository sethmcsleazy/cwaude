;;; TLEN - Total length of selected curves
;;; Sums the length of lines, arcs, circles, polylines, splines and ellipses.

(defun c:TLEN (/ *error* ss i ent total)
  (vl-load-com)
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (prompt "\nSelect objects to measure: ")
  (if (setq ss (ssget '((-4 . "<OR")
                          (0 . "LINE,ARC,CIRCLE,LWPOLYLINE,SPLINE,ELLIPSE")
                          (-4 . "<AND")
                            (0 . "POLYLINE")
                            ;; skip 3D meshes / polyface meshes
                            (-4 . "<NOT") (-4 . "&") (70 . 80) (-4 . "NOT>")
                          (-4 . "AND>")
                        (-4 . "OR>"))))
    (progn
      (setq total 0.0 i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              total (+ total (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent)))
              i (1+ i)))
      (princ (strcat "\n" (itoa (sslength ss)) " object(s), total length = " (rtos total))))
    (princ "\nNothing selected."))
  (princ))

(princ "\nTLEN loaded.")
(princ)
