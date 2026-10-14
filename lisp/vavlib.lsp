;;; VAVLIB / VAVSTACK - Build a size-switching Titus DESV VAV block (run once)
;;;
;;; VAVLIB   Creates block VAV-DESV with every DESV size drawn in plan, one size
;;;          per row, 150" apart (size 4 at the origin row). Each size has the
;;;          4D straight inlet run, inlet collar, casing, control enclosure,
;;;          door swing and NEC working clearance, right hand.
;;; Then, in the Block Editor (BEDIT VAV-DESV): add one Visibility parameter
;;;          with a state per size, hiding every other size in each state.
;;; VAVSTACK Moves every size back onto the insertion point, so all states
;;;          line up. Run it after the visibility states are saved.
;;; Then add a Flip parameter for left/right hand and save. No LISP needed after.
;;;
;;; Insertion point = where the supply duct meets the inlet collar, on the box
;;; centerline; airflow runs along +X; controls on the -Y side (right hand).
;;; The NEC 110.26 working clearance (30" x 36"; 208 V and 480 V Condition 1
;;; are the same depth, drawn dashed) and the control door swing share layer
;;; M-VAV-CLR, which does not plot - turn it on or off to show them.
;;; The "SIZE xx" label is on M-VAV-NPLT, which doesn't plot.
;;; Everything else is on layer 0 and takes the layer the block is inserted on.
;;; Drawing units are inches.

;; Titus DESV, from the ESV submittal (ESV-1.0, 02-02-2026). D rounded to the
;; nearest inch; size 40's inlet is 16 x 24 (24 wide in plan).
;;   (name  D  L  W  M(inlet collar)  G(panel overhang upstream of casing face))
(setq vavlib:*sizes*
  '(("04"  4.0 15.5 12.0 5.375 7.375)
    ("05"  5.0 15.5 12.0 5.375 7.375)
    ("06"  6.0 15.5 12.0 3.375 7.375)
    ("07"  7.0 15.5 12.0 3.375 7.375)
    ("08"  8.0 15.5 12.0 3.375 7.375)
    ("09"  9.0 15.5 14.0 3.375 5.375)
    ("10" 10.0 15.5 14.0 3.375 5.375)
    ("12" 12.0 15.5 16.0 3.375 5.375)
    ("14" 14.0 15.5 20.0 3.375 3.375)
    ("16" 16.0 15.5 24.0 3.375 3.375)
    ("40" 24.0 15.0 38.0 3.375 5.375)))

(setq vavlib:*ctl-len*   18.0     ; control enclosure length along the casing (Titus CL)
      vavlib:*ctl-out*    6.5     ; how far it sticks out from the casing (Titus CW)
      vavlib:*nec-wid*   30.0     ; NEC 110.26 working space width (min 30")
      vavlib:*nec-dep*   36.0     ; depth: 208 V, and 480 V Condition 1
      vavlib:*spacing*  150.0)    ; row spacing while the sizes are laid out

(defun vavlib:layer (name color lt plot)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0) (cons 62 color) (cons 6 lt)
                   (cons 290 (if plot 1 0))))))

(defun vavlib:line (x1 y1 x2 y2 lay)
  (entmake (list '(0 . "LINE") (cons 8 lay) (list 10 x1 y1 0.0) (list 11 x2 y2 0.0))))

(defun vavlib:rect (x1 y1 x2 y2 lay)
  (vavlib:rect-lt x1 y1 x2 y2 lay nil))

(defun vavlib:rect-lt (x1 y1 x2 y2 lay lt)
  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay))
                   (if lt (list (cons 6 lt)))
                   (list '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1)
                         (list 10 x1 y1) (list 10 x2 y1) (list 10 x2 y2) (list 10 x1 y2)))))

(defun vavlib:text (x y h str lay)
  (entmake (list '(0 . "TEXT") (cons 8 lay) (list 10 x y 0.0) (list 11 x y 0.0)
                 (cons 40 h) (cons 1 str) '(72 . 1) '(73 . 2))))

;; One size, right hand, with its centerline at y = dy
(defun vavlib:size (z dy / d l w m g run x1 xe ye cx yc)
  (setq d (nth 1 z) l (nth 2 z) w (nth 3 z) m (nth 4 z) g (nth 5 z)
        run (* 4.0 d)
        xe  (- m g)                                  ; enclosure upstream end
        ye  (- dy (/ w 2.0) vavlib:*ctl-out*)        ; enclosure outer face
        cx  (+ xe (/ vavlib:*ctl-len* 2.0)))
  ;; 4D straight inlet run
  (vavlib:line (- run) (+ dy (/ d 2.0)) 0.0 (+ dy (/ d 2.0)) "0")
  (vavlib:line (- run) (- dy (/ d 2.0)) 0.0 (- dy (/ d 2.0)) "0")
  (vavlib:line (- run) (+ dy (/ d 2.0)) (- run) (- dy (/ d 2.0)) "0")
  ;; inlet collar and casing
  (vavlib:rect 0.0 (- dy (/ d 2.0)) m (+ dy (/ d 2.0)) "0")
  (vavlib:rect m (- dy (/ w 2.0)) (+ m l) (+ dy (/ w 2.0)) "0")
  (vavlib:text (+ m (/ l 2.0)) (- dy 2.5) 2.0 (strcat "SIZE " (car z)) "M-VAV-NPLT")
  ;; control enclosure on the right (-Y) side
  (vavlib:rect xe ye (+ xe vavlib:*ctl-len*) (- dy (/ w 2.0)) "0")
  ;; door swing: hinged at the upstream end of the enclosure face, open 90 deg
  (vavlib:line xe ye xe (- ye vavlib:*ctl-len*) "M-VAV-CLR")
  (entmake (list '(0 . "ARC") '(8 . "M-VAV-CLR") (list 10 xe ye 0.0)
                 (cons 40 vavlib:*ctl-len*) (cons 50 (* 1.5 pi)) (cons 51 (* 2.0 pi))))
  ;; NEC 110.26 working clearance in front of the enclosure
  (setq x1 (- cx (/ (max vavlib:*nec-wid* vavlib:*ctl-len*) 2.0)))
  (vavlib:rect-lt x1 ye (+ x1 (max vavlib:*nec-wid* vavlib:*ctl-len*)) (- ye vavlib:*nec-dep*) "M-VAV-CLR"
                  (if (tblsearch "LTYPE" "HIDDEN") "HIDDEN"))
  (vavlib:text cx (- ye (/ vavlib:*nec-dep* 2.0)) 2.0 "NEC CLEARANCE" "M-VAV-CLR"))

(defun c:VAVLIB (/ i lts)
  (vl-load-com)
  (if (tblsearch "BLOCK" "VAV-DESV")
    (princ "\nBlock VAV-DESV already exists - rename or purge it first, then run VAVLIB again.")
    (progn
      (setq lts (vla-get-Linetypes (vla-get-ActiveDocument (vlax-get-acad-object))))
      (foreach f '("acad.lin" "acadiso.lin")
        (if (not (tblsearch "LTYPE" "HIDDEN"))
          (vl-catch-all-apply 'vla-Load (list lts "HIDDEN" f))))
      (vavlib:layer "M-VAV-CLR" 1 "Continuous" nil)        ; NEC clearance + door swing, never plotted
      (vavlib:layer "M-VAV-NPLT" 9 "Continuous" nil)     ; reference labels, never plotted
      (entmake '((0 . "BLOCK") (2 . "VAV-DESV") (70 . 2) (10 0.0 0.0 0.0)))
      ;; one tag for every size, inside the casing of all of them
      (entmake (list '(0 . "ATTDEF") '(8 . "0") '(10 12.0 2.5 0.0) '(11 12.0 2.5 0.0)
                     '(40 . 2.5) '(1 . "VAV-1") '(3 . "VAV tag") '(2 . "TAG")
                     '(70 . 0) '(72 . 1) '(74 . 2)))
      (setq i 0)
      (foreach z vavlib:*sizes*
        (vavlib:size z (* i vavlib:*spacing*))
        (setq i (1+ i)))
      (entmake '((0 . "ENDBLK")))
      (princ (strcat "\nBlock VAV-DESV created with " (itoa (length vavlib:*sizes*))
                     " sizes laid out " (rtos vavlib:*spacing* 2 0) "\" apart (size 04 at the bottom)."
                     "\nNext: BEDIT VAV-DESV and add the size visibility states."))))
  (princ))

;; Move every size row back onto the insertion point
(defun c:VAVSTACK (/ blk i cy mn mx n)
  (vl-load-com)
  (setq blk (vl-catch-all-apply 'vla-Item
              (list (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object))) "VAV-DESV"))
        n 0)
  (if (vl-catch-all-error-p blk)
    (princ "\nNo VAV-DESV block in this drawing - run VAVLIB first.")
    (progn
      (vlax-for o blk
        (if (and (vlax-method-applicable-p o 'GetBoundingBox)
                 (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list o 'mn 'mx)))))
          (progn
            (setq cy (/ (+ (cadr (vlax-safearray->list mn)) (cadr (vlax-safearray->list mx))) 2.0)
                  ;; row index; the clearance areas hang up to ~60" below a row's centerline
                  i  (fix (/ (+ cy 75.0) vavlib:*spacing*)))
            (if (> i 0)
              (progn
                (vla-Move o (vlax-3d-point 0.0 0.0 0.0) (vlax-3d-point 0.0 (* -1.0 i vavlib:*spacing*) 0.0))
                (setq n (1+ n)))))))
      (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) acAllViewports)
      (princ (strcat "\nMoved " (itoa n) " objects onto the insertion point."
                     "\nNext: BEDIT VAV-DESV, add the Flip parameter, Test Block, save."))))
  (princ))

(princ "\nVAVLIB loaded. Commands: VAVLIB, then VAVSTACK after the visibility states are set.")
(princ)
