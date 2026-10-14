;;; TKOLIB / TKOSTACK - Build a size-switching Crown 3300-DS takeoff block (run once)
;;;
;;; Crown Products Model 3300-DS: 45 deg takeoff with gasket flange and manual
;;; volume damper. Plan view, real size, drawn on the side of a trunk duct.
;;;
;;; TKOLIB   Creates block TKO-3300DS with every size drawn, one size per row,
;;;          60" apart (4" at the origin row).
;;; Then, in the Block Editor (BEDIT TKO-3300DS): add one Visibility parameter
;;;          with a state per size, hiding every other size in each state.
;;; TKOSTACK Moves every size back onto the insertion point. Run it after the
;;;          visibility states are saved, outside the Block Editor.
;;; Then add a Flip parameter (mirror line along the collar centerline, the
;;; block's Y axis) so the 45 deg side can face either way along the trunk.
;;;
;;; Insertion point = on the duct wall, on the collar centerline. The fitting
;;; projects toward +Y; the 45 deg (tapered) side is toward -X, the straight
;;; side toward +X (drawn for airflow in the trunk running toward +X).
;;; "SIZE" labels are on non-plotting layer M-TKO-NPLT; everything else is on
;;; layer 0 and takes the layer the block is inserted on. Units: inches.
;;;
;;; HEIGHT is read as the projection from the duct wall (flange face) to the
;;; end of the 5 3/8" collar - confirm against a real fitting.
;;; The tapered side is a true 45: it rises (opening - dia) and then runs
;;; straight up the collar side to the top of the shoe. Size 6 is the one
;;; exception - its shoe is shallower than the 45 needs, so the 45 stops at
;;; the shoe top and a short ledge closes it to the collar.

;;   (dia  rect-opening-length-along-duct  height)
(setq tkolib:*sizes*
  '(( 4.0  8.0 11.5)
    ( 5.0 10.0 10.5)
    ( 6.0 12.0 10.5)
    ( 7.0 12.0 11.5)
    ( 8.0 12.0 10.5)
    ( 9.0 15.0 12.5)
    (10.0 16.0 12.0)
    (12.0 18.0 12.0)
    (14.0 20.0 12.5)
    (16.0 24.0 13.5)
    (18.0 26.0 14.0)
    (20.0 28.0 16.0)))

(setq tkolib:*collar*  5.375    ; collar length (5 3/8")
      tkolib:*spacing* 60.0)    ; row spacing while the sizes are laid out

(defun tkolib:layer (name color plot)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0) (cons 62 color) '(6 . "Continuous")
                   (cons 290 (if plot 1 0))))))

(defun tkolib:line (x1 y1 x2 y2 lay)
  (entmake (list '(0 . "LINE") (cons 8 lay) (list 10 x1 y1 0.0) (list 11 x2 y2 0.0))))

(defun tkolib:rect (x1 y1 x2 y2 lay)
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay)
                 '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1)
                 (list 10 x1 y1) (list 10 x2 y1) (list 10 x2 y2) (list 10 x1 y2))))

(defun tkolib:text (x y h str lay)
  (entmake (list '(0 . "TEXT") (cons 8 lay) (list 10 x y 0.0) (list 11 x y 0.0)
                 (cons 40 h) (cons 1 str) '(72 . 1) '(73 . 2))))

;; One size with the duct wall at y = dy
(defun tkolib:size (z dy / d a h b rise xr xl cl cr ym)
  (setq d  (nth 0 z) a (nth 1 z) h (nth 2 z)
        b  (- h tkolib:*collar*)              ; shoe body depth
        rise (min (- a d) b)                  ; rise of the 45 deg side
        cr (/ d 2.0)                          ; collar right side (straight side)
        cl (/ d -2.0)                         ; collar left side
        xr cr                                 ; opening right end (flush with collar)
        xl (- cr a)                           ; opening left end (45 deg side)
        ym (+ dy b (/ tkolib:*collar* 2.0)))  ; damper blade position
  ;; shoe: straight side, 45 deg side, then straight up to the collar
  (tkolib:line xr dy cr (+ dy b) "0")
  (tkolib:line xl dy (+ xl rise) (+ dy rise) "0")
  (if (< rise b) (tkolib:line cl (+ dy rise) cl (+ dy b) "0"))
  ;; top of the shoe (a short ledge where the 45 runs out of depth, size 6)
  (tkolib:line (min cl (+ xl rise)) (+ dy b) cr (+ dy b) "0")
  ;; collar
  (tkolib:rect cl (+ dy b) cr (+ dy h) "0")
  ;; manual volume damper: blade across the collar, handle on the straight side
  (tkolib:line cl ym cr ym "0")
  (tkolib:line cr ym (+ cr 2.0) ym "0")
  (tkolib:line (+ cr 2.0) (- ym 0.75) (+ cr 2.0) (+ ym 0.75) "0")
  ;; size label
  (tkolib:text 0.0 (+ dy (/ b 2.0)) 1.5 (strcat "SIZE " (rtos d 2 0)) "M-TKO-NPLT"))

(defun c:TKOLIB (/ i)
  (if (tblsearch "BLOCK" "TKO-3300DS")
    (princ "\nBlock TKO-3300DS already exists - rename or purge it first, then run TKOLIB again.")
    (progn
      (tkolib:layer "M-TKO-NPLT" 9 nil)
      (entmake '((0 . "BLOCK") (2 . "TKO-3300DS") (70 . 0) (10 0.0 0.0 0.0)))
      (setq i 0)
      (foreach z tkolib:*sizes*
        (tkolib:size z (* i tkolib:*spacing*))
        (setq i (1+ i)))
      (entmake '((0 . "ENDBLK")))
      (princ (strcat "\nBlock TKO-3300DS created with " (itoa (length tkolib:*sizes*))
                     " sizes laid out " (rtos tkolib:*spacing* 2 0) "\" apart (4\" at the bottom)."
                     "\nNext: BEDIT TKO-3300DS and add the size visibility states."))))
  (princ))

;; Move every size row back onto the insertion point
(defun c:TKOSTACK (/ blk i cy mn mx n)
  (vl-load-com)
  (setq blk (vl-catch-all-apply 'vla-Item
              (list (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object))) "TKO-3300DS"))
        n 0)
  (if (vl-catch-all-error-p blk)
    (princ "\nNo TKO-3300DS block in this drawing - run TKOLIB first.")
    (progn
      (vlax-for o blk
        (if (and (vlax-method-applicable-p o 'GetBoundingBox)
                 (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list o 'mn 'mx)))))
          (progn
            (setq cy (/ (+ (cadr (vlax-safearray->list mn)) (cadr (vlax-safearray->list mx))) 2.0)
                  ;; a row runs from its duct wall up to at most 16"
                  i  (fix (/ (+ cy 20.0) tkolib:*spacing*)))
            (if (> i 0)
              (progn
                (vla-Move o (vlax-3d-point 0.0 0.0 0.0) (vlax-3d-point 0.0 (* -1.0 i tkolib:*spacing*) 0.0))
                (setq n (1+ n)))))))
      (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) acAllViewports)
      (princ (strcat "\nMoved " (itoa n) " objects onto the insertion point."
                     "\nNext: BEDIT TKO-3300DS, add the Flip parameter, Test Block, save."))))
  (princ))

(princ "\nTKOLIB loaded. Commands: TKOLIB, then TKOSTACK after the visibility states are set.")
(princ)
