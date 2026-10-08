;;; TAREA - Total area of selected closed objects
;;; Sums the area of circles, ellipses, splines, regions, hatches and closed polylines.

(defun c:TAREA (/ *error* ss i obj total)
  (vl-load-com)
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (prompt "\nSelect closed objects: ")
  (if (setq ss (ssget '((-4 . "<OR")
                          (0 . "CIRCLE,ELLIPSE,SPLINE,REGION,HATCH")
                          (-4 . "<AND") (0 . "LWPOLYLINE,POLYLINE") (-4 . "&") (70 . 1) (-4 . "AND>")
                        (-4 . "OR>"))))
    (progn
      (setq total 0.0 i 0)
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i))
              total (+ total (vla-get-Area obj))
              i (1+ i)))
      (princ (strcat "\n" (itoa (sslength ss)) " object(s), total area = " (rtos total))))
    (princ "\nNothing selected."))
  (princ))

(princ "\nTAREA loaded.")
(princ)
