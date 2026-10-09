;;; AHU / AHUEDIT - Parametric air handling unit blocks (plan and elevation)
;;;
;;; AHU      Asks for a unit tag (AHU-1, AHU-2...), opens a dialog with the unit
;;;          size and the supply (SA), return (RA) and outside air (OA)
;;;          openings, then inserts the plan or elevation block of that unit.
;;;          Running it again with an existing tag reopens that unit's values.
;;; AHUEDIT  Pick any placed AHU block (plan or elevation) to change its
;;;          values. Both views of that unit are redrawn everywhere they are
;;;          inserted.
;;;
;;; Each unit gets two blocks, AHU_PLAN_<tag> and AHU_ELEV_<tag>. The values are
;;; stored in the drawing (dictionary AHU_PARAMS), keyed by tag.
;;;
;;; Unit axes: Length runs left end -> right end, Width runs front side -> back
;;; side, Height runs bottom -> top. The insertion point is the front-left
;;; bottom corner. The elevation block is the view looking at the front side.
;;;
;;; Opening location and offsets:
;;;   Top / Bottom      Length along the unit length, Width across it.
;;;                     Offset 1 = from left end, Offset 2 = from front side.
;;;   Front / Back side Length horizontal, Width vertical.
;;;                     Offset 1 = from left end, Offset 2 = from bottom.
;;;   Left / Right end  Length horizontal (along the end), Width vertical.
;;;                     Offset 1 = from front side, Offset 2 = from bottom.
;;; Openings you look straight at are drawn as a box with a symbol (SA = X,
;;; RA/OA = one diagonal); openings on the far side are dashed (HIDDEN);
;;; openings on a side seen edge-on are drawn as a short duct collar.

(setq ahu:*faces*     '("NONE" "TOP" "BOTTOM" "FRONT" "BACK" "LEFT" "RIGHT"))
(setq ahu:*facenames* '("None" "Top" "Bottom" "Front side" "Back side" "Left end" "Right end"))

(defun ahu:p (k pr) (cdr (assoc k pr)))

(defun ahu:defaults ()
  '(("L" . 120.0) ("W" . 60.0) ("H" . 72.0)
    ("SA" "TOP"  24.0 18.0 12.0 21.0)
    ("RA" "TOP"  24.0 18.0 60.0 21.0)
    ("OA" "LEFT" 30.0 20.0 15.0 36.0)))

;; Tag usable in a block name
(defun ahu:clean (str / out c)
  (setq out "")
  (foreach code (vl-string->list (strcase str))
    (setq c (chr code))
    (if (not (vl-string-search c "<>/\\\":;?*|,=`"))
      (setq out (strcat out c))))
  (vl-string-trim " " out))

(defun ahu:bname (tag view) (strcat "AHU_" view "_" tag))

(defun ahu:nexttag (/ n tags)
  (setq tags (mapcar 'car (vlax-ldata-list "AHU_PARAMS")) n 1)
  (while (member (strcat "AHU-" (itoa n)) tags) (setq n (1+ n)))
  (strcat "AHU-" (itoa n)))

;; Number to text without trailing zeros (independent of DIMZIN)
(defun ahu:fmt (v / dz s)
  (setq dz (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq s (rtos v 2 4))
  (setvar "DIMZIN" dz)
  (if (vl-string-search "." s)
    (setq s (vl-string-right-trim "." (vl-string-right-trim "0" s))))
  s)

;;; ---------------------------------------------------------------- geometry

(defun ahu:hidden (/ lts)
  (if (not (tblsearch "LTYPE" "HIDDEN"))
    (progn
      (setq lts (vla-get-Linetypes (vla-get-ActiveDocument (vlax-get-acad-object))))
      (foreach f '("acad.lin" "acadiso.lin")
        (if (not (tblsearch "LTYPE" "HIDDEN"))
          (vl-catch-all-apply 'vla-Load (list lts "HIDDEN" f))))))
  (if (tblsearch "LTYPE" "HIDDEN") "HIDDEN"))

(defun ahu:rect (x1 y1 x2 y2 lt)
  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "0"))
                   (if lt (list (cons 6 lt)))
                   (list '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1)
                         (list 10 x1 y1) (list 10 x2 y1) (list 10 x2 y2) (list 10 x1 y2)))))

(defun ahu:line (x1 y1 x2 y2 lt)
  (entmake (append (list '(0 . "LINE") '(8 . "0"))
                   (if lt (list (cons 6 lt)))
                   (list (list 10 x1 y1 0.0) (list 11 x2 y2 0.0)))))

(defun ahu:text (x y h str)
  (entmake (list '(0 . "TEXT") '(8 . "0") (list 10 x y 0.0) (list 11 x y 0.0)
                 (cons 40 h) (cons 1 str) (cons 7 (getvar "TEXTSTYLE"))
                 '(72 . 1) '(73 . 2))))

;; Opening seen face-on: box, symbol and label
(defun ahu:face-on (kind x1 y1 x2 y2 lt h)
  (ahu:rect x1 y1 x2 y2 lt)
  (if (or (= kind "SA") (= kind "RA")) (ahu:line x1 y1 x2 y2 lt))
  (if (or (= kind "SA") (= kind "OA")) (ahu:line x1 y2 x2 y1 lt))
  (ahu:text (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0)
            (min h (* 0.35 (min (- x2 x1) (- y2 y1)))) kind))

;; Opening seen edge-on: duct collar with the label beside it
(defun ahu:collar (kind x1 y1 x2 y2 lx ly h)
  (ahu:rect x1 y1 x2 y2 nil)
  (ahu:text lx ly h kind))

;; Draws one opening. view is "PLAN" or "ELEV"; a/b are the drawing's
;; horizontal/vertical unit sizes (L/W in plan, L/H in elevation).
(defun ahu:opening (view kind o a b c h lt / face ol ow o1 o2 m)
  (setq face (car o) ol (nth 1 o) ow (nth 2 o) o1 (nth 3 o) o2 (nth 4 o))
  (if (= view "PLAN")
    (cond
      ((= face "TOP")    (ahu:face-on kind o1 o2 (+ o1 ol) (+ o2 ow) nil h))
      ((= face "BOTTOM") (ahu:face-on kind o1 o2 (+ o1 ol) (+ o2 ow) lt h))
      ((= face "FRONT")  (ahu:collar kind o1 (- c) (+ o1 ol) 0.0 (+ o1 (/ ol 2.0)) (- (+ c h)) h))
      ((= face "BACK")   (ahu:collar kind o1 b (+ o1 ol) (+ b c) (+ o1 (/ ol 2.0)) (+ b c h) h))
      ((= face "LEFT")   (ahu:collar kind (- c) o1 0.0 (+ o1 ol) (- (+ c (* 1.5 h))) (+ o1 (/ ol 2.0)) h))
      ((= face "RIGHT")  (ahu:collar kind a o1 (+ a c) (+ o1 ol) (+ a c (* 1.5 h)) (+ o1 (/ ol 2.0)) h)))
    (cond
      ((= face "FRONT")  (ahu:face-on kind o1 o2 (+ o1 ol) (+ o2 ow) nil h))
      ((= face "BACK")   (ahu:face-on kind o1 o2 (+ o1 ol) (+ o2 ow) lt h))
      ((= face "TOP")    (ahu:collar kind o1 b (+ o1 ol) (+ b c) (+ o1 (/ ol 2.0)) (+ b c h) h))
      ((= face "BOTTOM") (ahu:collar kind o1 (- c) (+ o1 ol) 0.0 (+ o1 (/ ol 2.0)) (- (+ c h)) h))
      ((= face "LEFT")   (ahu:collar kind (- c) o2 0.0 (+ o2 ow) (- (+ c (* 1.5 h))) (+ o2 (/ ow 2.0)) h))
      ((= face "RIGHT")  (ahu:collar kind a o2 (+ a c) (+ o2 ow) (+ a c (* 1.5 h)) (+ o2 (/ ow 2.0)) h)))))

;; (Re)define one view's block
(defun ahu:makeblock (tag pr view / a b c h lt)
  (setq a  (ahu:p "L" pr)
        b  (if (= view "PLAN") (ahu:p "W" pr) (ahu:p "H" pr))
        c  (min 6.0 (* 0.15 (min a b)))       ; collar depth
        h  (* 0.08 (min a b))                 ; text height
        lt (ahu:hidden))
  (entmake (list '(0 . "BLOCK") (cons 2 (ahu:bname tag view)) '(70 . 0) '(10 0.0 0.0 0.0)))
  (ahu:rect 0.0 0.0 a b nil)
  (ahu:text (/ a 2.0) (/ b 2.0) (* 1.25 h) tag)
  (foreach kind '("SA" "RA" "OA")
    (if (/= "NONE" (car (ahu:p kind pr)))
      (ahu:opening view kind (ahu:p kind pr) a b c h lt)))
  (entmake '((0 . "ENDBLK"))))

(defun ahu:build (tag pr)
  (ahu:makeblock tag pr "PLAN")
  (ahu:makeblock tag pr "ELEV")
  (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) acAllViewports))

;; Openings that run past the face they are on
(defun ahu:check (pr / msgs o face a b)
  (foreach kind '("SA" "RA" "OA")
    (setq o (ahu:p kind pr) face (car o))
    (cond
      ((member face '("TOP" "BOTTOM")) (setq a (ahu:p "L" pr) b (ahu:p "W" pr)))
      ((member face '("FRONT" "BACK")) (setq a (ahu:p "L" pr) b (ahu:p "H" pr)))
      ((member face '("LEFT" "RIGHT")) (setq a (ahu:p "W" pr) b (ahu:p "H" pr))))
    (if (and (/= face "NONE")
             (or (> (+ (nth 3 o) (nth 1 o)) (+ a 1e-6))
                 (> (+ (nth 4 o) (nth 2 o)) (+ b 1e-6))))
      (setq msgs (cons (strcat kind " opening runs past the edge of the "
                               (nth (vl-position face ahu:*faces*) ahu:*facenames*) ".")
                       msgs))))
  (reverse msgs))

;;; ------------------------------------------------------------------ dialog

(defun ahu:writedcl (path / f)
  (setq f (open path "w"))
  (foreach ln
    (append
      '("ahu : dialog {"
        "  label = \"AHU Block\";"
        "  : boxed_row { label = \"Unit\";"
        "    : text { key = \"tag\"; width = 14; }"
        "    : edit_box { key = \"l\"; label = \"Length\"; edit_width = 8; }"
        "    : edit_box { key = \"w\"; label = \"Width\"; edit_width = 8; }"
        "    : edit_box { key = \"h\"; label = \"Height\"; edit_width = 8; }"
        "  }")
      (apply 'append
        (mapcar
          '(lambda (k lab)
             (list (strcat "  : boxed_row { label = \"" lab "\";")
                   (strcat "    : popup_list { key = \"" k "_face\"; label = \"Location\"; width = 26; }")
                   (strcat "    : edit_box { key = \"" k "_len\"; label = \"Length\"; edit_width = 6; }")
                   (strcat "    : edit_box { key = \"" k "_wid\"; label = \"Width\"; edit_width = 6; }")
                   (strcat "    : edit_box { key = \"" k "_o1\"; label = \"Offset 1\"; edit_width = 6; }")
                   (strcat "    : edit_box { key = \"" k "_o2\"; label = \"Offset 2\"; edit_width = 6; }")
                   "  }"))
          '("sa" "ra" "oa")
          '("Supply air (SA)" "Return air (RA)" "Outside air (OA)")))
      '("  : boxed_column { label = \"Offsets\";"
        "    : text { label = \"Top / Bottom: Offset 1 = from left end, Offset 2 = from front side. Length runs along the unit.\"; }"
        "    : text { label = \"Front / Back side: Offset 1 = from left end, Offset 2 = from bottom. Width is the opening height.\"; }"
        "    : text { label = \"Left / Right end: Offset 1 = from front side, Offset 2 = from bottom. Width is the opening height.\"; }"
        "  }"
        "  ok_cancel;"
        "}"))
    (write-line ln f))
  (close f))

;; OK button: read and check every field
(defun ahu:accept (/ pr bad v face vals key)
  (foreach k '("L" "W" "H")
    (if (and (setq v (distof (get_tile (strcase k T)))) (> v 0))
      (setq pr (cons (cons k v) pr))
      (setq bad "Unit length, width and height must be numbers greater than 0.")))
  (foreach k '("SA" "RA" "OA")
    (setq key  (strcase k T)
          face (nth (atoi (get_tile (strcat key "_face"))) ahu:*faces*)
          vals nil)
    (foreach f '("_len" "_wid" "_o1" "_o2")
      (setq v (distof (get_tile (strcat key f))))
      (if (or (null v) (< v 0)
              (and (/= face "NONE") (member f '("_len" "_wid")) (<= v 0)))
        (setq bad (strcat k " opening: sizes must be greater than 0 and offsets 0 or more.")))
      (setq vals (cons (if v v 0.0) vals)))
    (setq pr (cons (cons k (cons face (reverse vals))) pr)))
  (if bad
    (alert bad)
    (progn
      (setq *ahu-result* (reverse pr))
      (done_dialog 1))))

;; Shows the dialog filled with pr; returns the new values or nil on Cancel
(defun ahu:dialog (tag pr / path id o key)
  (setq path (vl-filename-mktemp "ahu.dcl"))
  (ahu:writedcl path)
  (setq id (load_dialog path) *ahu-result* nil)
  (if (new_dialog "ahu" id)
    (progn
      (set_tile "tag" tag)
      (set_tile "l" (ahu:fmt (ahu:p "L" pr)))
      (set_tile "w" (ahu:fmt (ahu:p "W" pr)))
      (set_tile "h" (ahu:fmt (ahu:p "H" pr)))
      (foreach k '("SA" "RA" "OA")
        (setq o (ahu:p k pr) key (strcase k T))
        (start_list (strcat key "_face"))
        (mapcar 'add_list ahu:*facenames*)
        (end_list)
        (set_tile (strcat key "_face") (itoa (vl-position (car o) ahu:*faces*)))
        (set_tile (strcat key "_len") (ahu:fmt (nth 1 o)))
        (set_tile (strcat key "_wid") (ahu:fmt (nth 2 o)))
        (set_tile (strcat key "_o1")  (ahu:fmt (nth 3 o)))
        (set_tile (strcat key "_o2")  (ahu:fmt (nth 4 o))))
      (action_tile "accept" "(ahu:accept)")
      (action_tile "cancel" "(done_dialog 0)")
      (start_dialog)))
  (unload_dialog id)
  (vl-file-delete path)
  *ahu-result*)

;; Dialog, then store and rebuild. Returns the new values or nil.
(defun ahu:edit (tag pr / msgs)
  (if (setq pr (ahu:dialog tag pr))
    (progn
      (vlax-ldata-put "AHU_PARAMS" tag pr)
      (vlax-ldata-put "AHU_LAST" "LAST" pr)
      (ahu:build tag pr)
      (if (setq msgs (ahu:check pr))
        (alert (strcat "Check these openings:\n\n"
                       (apply 'strcat (mapcar '(lambda (m) (strcat m "\n")) msgs)))))
      pr)))

;;; ---------------------------------------------------------------- commands

(defun c:AHU (/ *error* doc tag pr view pt rot sp)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (setq tag (ahu:clean (getstring T (strcat "\nAHU tag <" (ahu:nexttag) ">: "))))
  (if (= tag "") (setq tag (ahu:nexttag)))
  (setq pr (cond ((vlax-ldata-get "AHU_PARAMS" tag))
                 ((vlax-ldata-get "AHU_LAST" "LAST"))
                 ((ahu:defaults))))
  (if (ahu:edit tag pr)
    (progn
      (initget "Plan Elevation")
      (setq view (if (= "Elevation" (getkword "\nInsert which view [Plan/Elevation] <Plan>: "))
                   "ELEV" "PLAN"))
      (if (setq pt (getpoint "\nInsertion point (front-left corner): "))
        (progn
          (setq rot (getangle pt "\nRotation <0>: ")
                sp  (if (and (= 0 (getvar "TILEMODE")) (= 1 (getvar "CVPORT")))
                      (vla-get-PaperSpace doc)
                      (vla-get-ModelSpace doc)))
          (vla-InsertBlock sp (vlax-3d-point (trans pt 1 0)) (ahu:bname tag view)
                           1.0 1.0 1.0
                           (+ (if rot rot 0.0) (angle '(0 0 0) (getvar "UCSXDIR"))))
          (princ (strcat "\n" tag " " (if (= view "PLAN") "plan" "elevation")
                         " inserted. Run AHU again with tag " tag " to place the other view."))))))
  (princ))

(defun c:AHUEDIT (/ *error* sel ed name tag pr)
  (vl-load-com)
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (if (and (setq sel (entsel "\nSelect an AHU block: "))
           (= "INSERT" (cdr (assoc 0 (setq ed (entget (car sel))))))
           (wcmatch (setq name (strcase (cdr (assoc 2 ed)))) "AHU_PLAN_*,AHU_ELEV_*"))
    (progn
      (setq tag (substr name 10)
            pr  (vlax-ldata-get "AHU_PARAMS" tag))
      (if (not pr)
        (progn
          (princ (strcat "\nNo stored values for " tag " in this drawing - starting from defaults."))
          (setq pr (ahu:defaults))))
      (if (ahu:edit tag pr)
        (princ (strcat "\n" tag " updated (plan and elevation)."))))
    (princ "\nThat is not an AHU block."))
  (princ))

(princ "\nAHU loaded. Commands: AHU, AHUEDIT.")
(princ)
