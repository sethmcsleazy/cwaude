;;; VAV / VAVEDIT - Parametric single duct VAV box plan block (Titus sizes)
;;;
;;; VAV      Asks for a box tag (VAV-1, VAV-2...), opens a dialog (inlet size,
;;;          left/right hand, NEC working clearance, control door swing), then
;;;          inserts the plan block. Running it again with an existing tag
;;;          reopens that box's settings.
;;; VAVEDIT  Pick a placed VAV block to change its settings; every copy of that
;;;          tag is redrawn.
;;;
;;; Plan block (insertion point = upstream end of the straight inlet run, on
;;; the box centerline; airflow runs along +X):
;;;   - 4 inlet diameters of straight inlet duct, then the inlet collar
;;;   - casing, discharge at the far end
;;;   - control enclosure on the left or right side (hand = side the controls
;;;     are on, looking in the direction of airflow)
;;;   - optional NEC 110.26 working space in front of the control enclosure
;;;     (30" or enclosure width, whichever is wider; depth by voltage/condition)
;;;   - optional control enclosure door swing
;;; Drawing units are inches.
;;;
;;; ---------------------------------------------------------------------------
;;; SIZE TABLE - fill from the CURRENT Titus submittal (all values in inches)
;;; One list per inlet size:
;;;   (name  inlet-dia  inlet-collar-len  casing-len  casing-wid  casing-hgt
;;;          ctrl-len  ctrl-depth  ctrl-offset)
;;;   name             text shown in the size list, e.g. "08"
;;;   inlet-dia        round inlet diameter (for a rectangular inlet use its width)
;;;   inlet-collar-len inlet collar length beyond the casing
;;;   casing-len       casing length, inlet face to discharge
;;;   casing-wid       casing width
;;;   casing-hgt       casing height (not drawn in plan; kept for reference)
;;;   ctrl-len         control enclosure length along the casing side
;;;   ctrl-depth       how far the control enclosure sticks out from the casing
;;;   ctrl-offset      distance from the casing inlet face to the enclosure
;;; ---------------------------------------------------------------------------
(setq vav:*sizes*
  '(
    ;; Waiting for the current Titus DESV submittal dimension table.
  ))

;; NEC 110.26(A)(1) working space depth, inches: (condition-1 condition-2 condition-3)
(setq vav:*nec*  '(("208V" 36.0 36.0 36.0)       ; 0-150 V to ground (208Y/120)
                   ("480V" 36.0 42.0 48.0)))     ; 151-600 V to ground (480Y/277)
(setq vav:*necw* 30.0)                           ; minimum working space width

(setq vav:*clr*      '("NONE" "208V" "480V"))
(setq vav:*clrnames* '("None" "208 V" "480 V"))
(setq vav:*cond*     '("Condition 1" "Condition 2" "Condition 3"))

;; Settings: ("SIZE" . name) ("HAND" . "L"/"R") ("CLR" . "NONE"/"208V"/"480V")
;;           ("COND" . 1-3) ("DOOR" . T/nil)
(defun vav:p (k s) (cdr (assoc k s)))
(defun vav:size (name) (assoc name vav:*sizes*))
(defun vav:defaults ()
  (list (cons "SIZE" (car (nth (/ (length vav:*sizes*) 2) vav:*sizes*)))
        '("HAND" . "R") '("CLR" . "NONE") '("COND" . 1) '("DOOR")))

(defun vav:clean (str / out c)
  (setq out "")
  (foreach code (vl-string->list (strcase str))
    (setq c (chr code))
    (if (not (vl-string-search c "<>/\\\":;?*|,=`"))
      (setq out (strcat out c))))
  (vl-string-trim " " out))

(defun vav:bname (tag) (strcat "VAV_PLAN_" tag))

(defun vav:nexttag (/ n tags)
  (setq tags (mapcar 'car (vlax-ldata-list "VAV_PARAMS")) n 1)
  (while (member (strcat "VAV-" (itoa n)) tags) (setq n (1+ n)))
  (strcat "VAV-" (itoa n)))

;;; ---------------------------------------------------------------- geometry

(defun vav:hidden (/ lts)
  (if (not (tblsearch "LTYPE" "HIDDEN"))
    (progn
      (setq lts (vla-get-Linetypes (vla-get-ActiveDocument (vlax-get-acad-object))))
      (foreach f '("acad.lin" "acadiso.lin")
        (if (not (tblsearch "LTYPE" "HIDDEN"))
          (vl-catch-all-apply 'vla-Load (list lts "HIDDEN" f))))))
  (if (tblsearch "LTYPE" "HIDDEN") "HIDDEN"))

(defun vav:rect (x1 y1 x2 y2 lt)
  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "0"))
                   (if lt (list (cons 6 lt)))
                   (list '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1)
                         (list 10 x1 y1) (list 10 x2 y1) (list 10 x2 y2) (list 10 x1 y2)))))

(defun vav:line (x1 y1 x2 y2 lt)
  (entmake (append (list '(0 . "LINE") '(8 . "0"))
                   (if lt (list (cons 6 lt)))
                   (list (list 10 x1 y1 0.0) (list 11 x2 y2 0.0)))))

(defun vav:arc (cx cy r a1 a2)
  (entmake (list '(0 . "ARC") '(8 . "0") (list 10 cx cy 0.0) (cons 40 r)
                 (cons 50 a1) (cons 51 a2))))

(defun vav:text (x y h str)
  (entmake (list '(0 . "TEXT") '(8 . "0") (list 10 x y 0.0) (list 11 x y 0.0)
                 (cons 40 h) (cons 1 str) (cons 7 (getvar "TEXTSTYLE"))
                 '(72 . 1) '(73 . 2))))

;; Builds block VAV_PLAN_<tag>. The insertion point is the upstream end of
;; the straight run on the centerline.
(defun vav:makeblock (tag s / sz d col cl cw ctl ctd cto run x0 xc side y0 y1 h lt
                              clr dep wid cx sgn)
  (setq sz  (vav:size (vav:p "SIZE" s))
        d   (nth 1 sz) col (nth 2 sz) cl (nth 3 sz) cw (nth 4 sz)
        ctl (nth 6 sz) ctd (nth 7 sz) cto (nth 8 sz)
        run (* 4.0 d)                        ; 4 diameters of straight inlet
        x0  (+ run col)                      ; casing inlet face
        sgn (if (= "R" (vav:p "HAND" s)) -1.0 1.0)   ; right hand = -Y side
        h   (max 1.5 (* 0.12 cw))
        lt  (vav:hidden))
  (entmake (list '(0 . "BLOCK") (cons 2 (vav:bname tag)) '(70 . 0) '(10 0.0 0.0 0.0)))
  ;; straight inlet run (sides plus a centerline break at the upstream end)
  (vav:line 0.0 (/ d 2.0) run (/ d 2.0) nil)
  (vav:line 0.0 (/ d -2.0) run (/ d -2.0) nil)
  (vav:line 0.0 (/ d 2.0) 0.0 (/ d -2.0) nil)
  (vav:text (/ run 2.0) (+ (/ d 2.0) h) (* 0.8 h) "4D STRAIGHT")
  ;; inlet collar
  (vav:rect run (/ d -2.0) x0 (/ d 2.0) nil)
  ;; casing
  (vav:rect x0 (/ cw -2.0) (+ x0 cl) (/ cw 2.0) nil)
  (vav:text (+ x0 (/ cl 2.0)) (* 0.6 h) h tag)
  (vav:text (+ x0 (/ cl 2.0)) (* -0.9 h) (* 0.8 h) (strcat "SIZE " (vav:p "SIZE" s)))
  ;; control enclosure on the hand side
  (setq y0 (* sgn (/ cw 2.0))
        y1 (* sgn (+ (/ cw 2.0) ctd)))
  (vav:rect (+ x0 cto) (min y0 y1) (+ x0 cto ctl) (max y0 y1) nil)
  ;; door swing: hinged at the inlet end of the enclosure face, swinging open 90 deg
  (if (vav:p "DOOR" s)
    (progn
      (vav:line (+ x0 cto) y1 (+ x0 cto) (+ y1 (* sgn ctl)) nil)
      (if (> sgn 0)
        (vav:arc (+ x0 cto) y1 ctl 0.0 (/ pi 2.0))
        (vav:arc (+ x0 cto) y1 ctl (* 1.5 pi) (* 2.0 pi)))))
  ;; NEC 110.26 working space in front of the enclosure
  (setq clr (vav:p "CLR" s))
  (if (/= clr "NONE")
    (progn
      (setq dep (nth (vav:p "COND" s) (assoc clr vav:*nec*))
            wid (max vav:*necw* ctl)
            cx  (+ x0 cto (/ ctl 2.0)))
      (vav:rect (- cx (/ wid 2.0)) y1 (+ cx (/ wid 2.0)) (+ y1 (* sgn dep)) lt)
      (vav:text cx (+ y1 (* sgn (/ dep 2.0))) (* 0.8 h)
                (strcat "NEC CLEARANCE " (substr clr 1 3) "V"))))
  (entmake '((0 . "ENDBLK")))
  (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) acAllViewports))

;;; ------------------------------------------------------------------ dialog

(defun vav:writedcl (path / f)
  (setq f (open path "w"))
  (foreach ln
    '("vav : dialog {"
      "  label = \"VAV Box\";"
      "  : text { key = \"tag\"; }"
      "  : popup_list { key = \"size\"; label = \"Inlet size\"; width = 30; }"
      "  : radio_row { key = \"hand\";"
      "    : radio_button { key = \"L\"; label = \"Left hand\"; }"
      "    : radio_button { key = \"R\"; label = \"Right hand\"; }"
      "  }"
      "  : boxed_column { label = \"NEC 110.26 working clearance\";"
      "    : popup_list { key = \"clr\"; label = \"Voltage\"; width = 30; }"
      "    : popup_list { key = \"cond\"; label = \"Condition\"; width = 30; }"
      "  }"
      "  : toggle { key = \"door\"; label = \"Show control enclosure door swing\"; }"
      "  ok_cancel;"
      "}")
    (write-line ln f))
  (close f))

(defun vav:accept ()
  (setq *vav-result*
    (list (cons "SIZE" (car (nth (atoi (get_tile "size")) vav:*sizes*)))
          (cons "HAND" (if (= "1" (get_tile "L")) "L" "R"))
          (cons "CLR"  (nth (atoi (get_tile "clr")) vav:*clr*))
          (cons "COND" (1+ (atoi (get_tile "cond"))))
          (cons "DOOR" (= "1" (get_tile "door")))))
  (done_dialog 1))

(defun vav:dialog (tag s / path id)
  (setq path (vl-filename-mktemp "vav.dcl"))
  (vav:writedcl path)
  (setq id (load_dialog path) *vav-result* nil)
  (if (new_dialog "vav" id)
    (progn
      (set_tile "tag" (strcat "Tag: " tag))
      (start_list "size")
      (mapcar '(lambda (z) (add_list (strcat (car z) "\"  -  " (rtos (nth 3 z) 2 2) " x "
                                             (rtos (nth 4 z) 2 2) " casing")))
              vav:*sizes*)
      (end_list)
      (set_tile "size" (itoa (max 0 (vl-position (vav:size (vav:p "SIZE" s)) vav:*sizes*))))
      (set_tile (vav:p "HAND" s) "1")
      (start_list "clr") (mapcar 'add_list vav:*clrnames*) (end_list)
      (set_tile "clr" (itoa (vl-position (vav:p "CLR" s) vav:*clr*)))
      (start_list "cond") (mapcar 'add_list vav:*cond*) (end_list)
      (set_tile "cond" (itoa (1- (vav:p "COND" s))))
      (set_tile "door" (if (vav:p "DOOR" s) "1" "0"))
      (action_tile "accept" "(vav:accept)")
      (action_tile "cancel" "(done_dialog 0)")
      (start_dialog)))
  (unload_dialog id)
  (vl-file-delete path)
  *vav-result*)

(defun vav:edit (tag s)
  (if (setq s (vav:dialog tag s))
    (progn
      (vlax-ldata-put "VAV_PARAMS" tag s)
      (vlax-ldata-put "VAV_LAST" "LAST" s)
      (vav:makeblock tag s)
      s)))

;;; ---------------------------------------------------------------- commands

(defun c:VAV (/ *error* doc tag s pt rot sp)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (if (not vav:*sizes*)
    (princ "\nThe VAV size table in vav.lsp is empty - fill it from the Titus submittal first.")
    (progn
      (setq tag (vav:clean (getstring T (strcat "\nVAV tag <" (vav:nexttag) ">: "))))
      (if (= tag "") (setq tag (vav:nexttag)))
      (setq s (cond ((vlax-ldata-get "VAV_PARAMS" tag))
                    ((vlax-ldata-get "VAV_LAST" "LAST"))
                    ((vav:defaults))))
      (if (not (vav:size (vav:p "SIZE" s)))          ; size no longer in the table
        (setq s (subst (cons "SIZE" (car (car vav:*sizes*))) (assoc "SIZE" s) s)))
      (if (and (vav:edit tag s)
               (setq pt (getpoint "\nInsertion point (upstream end of inlet run): ")))
        (progn
          (setq rot (getangle pt "\nAirflow direction <0>: ")
                sp  (if (and (= 0 (getvar "TILEMODE")) (= 1 (getvar "CVPORT")))
                      (vla-get-PaperSpace doc)
                      (vla-get-ModelSpace doc)))
          (vla-InsertBlock sp (vlax-3d-point (trans pt 1 0)) (vav:bname tag)
                           1.0 1.0 1.0
                           (+ (if rot rot 0.0) (angle '(0 0 0) (getvar "UCSXDIR"))))))))
  (princ))

(defun c:VAVEDIT (/ *error* sel ed name tag s)
  (vl-load-com)
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (if (and (setq sel (entsel "\nSelect a VAV block: "))
           (= "INSERT" (cdr (assoc 0 (setq ed (entget (car sel))))))
           (wcmatch (setq name (strcase (cdr (assoc 2 ed)))) "VAV_PLAN_*"))
    (progn
      (setq tag (substr name 10)
            s   (cond ((vlax-ldata-get "VAV_PARAMS" tag)) ((vav:defaults))))
      (if (vav:edit tag s) (princ (strcat "\n" tag " updated."))))
    (princ "\nThat is not a VAV block."))
  (princ))

(princ "\nVAV loaded. Commands: VAV, VAVEDIT.")
(princ)
