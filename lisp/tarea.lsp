;;; TAREA - Total area of selected closed objects
;;; Sums the area of closed polylines, circles, ellipses, splines, regions and
;;; hatches. Open curves and objects without an area (3D polylines, meshes)
;;; are skipped and counted. In Architectural/Engineering units the total is
;;; also shown in square feet.

(defun c:TAREA (/ *error* ss i ent a n skip total)
  (vl-load-com)
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (prompt "\nSelect closed objects: ")
  (if (setq ss (ssget '((-4 . "<OR")
                          (0 . "CIRCLE,ELLIPSE,SPLINE,REGION,HATCH")
                          (-4 . "<AND") (0 . "LWPOLYLINE,POLYLINE") (-4 . "&") (70 . 1)
                            ;; not 3D meshes / polyface meshes
                            (-4 . "<NOT") (-4 . "&") (70 . 80) (-4 . "NOT>")
                          (-4 . "AND>")
                        (-4 . "OR>"))))
    (progn
      (setq total 0.0 i 0 n 0 skip 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i) i (1+ i))
        (if (and (or (wcmatch (cdr (assoc 0 (entget ent))) "REGION,HATCH")
                     (vlax-curve-isClosed ent))
                 (not (vl-catch-all-error-p
                        (setq a (vl-catch-all-apply 'vla-get-Area (list (vlax-ename->vla-object ent)))))))
          (setq total (+ total a) n (1+ n))
          (setq skip (1+ skip))))
      (princ (strcat "\n" (itoa n) " object(s), total area = " (rtos total 2)
                     (if (member (getvar "LUNITS") '(3 4))
                       (strcat " sq in (" (rtos (/ total 144.0) 2) " sq ft)")
                       "")))
      (if (> skip 0)
        (princ (strcat "\n" (itoa skip) " open or unmeasurable object(s) skipped."))))
    (princ "\nNothing selected."))
  (princ))

(princ "\nTAREA loaded.")
(princ)
