;;; MECHBLOCKS - Attribute-driven AHU and VAV blocks
;;;
;;; Every setting lives in an attribute on the block. Edit the attributes any
;;; way you like (double-click / EATTEDIT, ATTEDIT, the Properties palette) and
;;; the block redraws itself to match when the command ends. Edits made in the
;;; Properties palette are picked up at the end of the next command (e.g. RE),
;;; or run MBSYNC.
;;;
;;; Commands
;;;   AHU     Place an AHU plan or elevation. Placing a view of a TAG that
;;;           already exists copies that unit's values.
;;;   VAV     Place a single duct VAV box (Titus DESV sizes).
;;;   MBSYNC  Redraw selected AHU/VAV blocks from their attributes (Enter = all).
;;;
;;; Each placed block has its own block definition (AHU$P$<handle>,
;;; AHU$E$<handle>, VAV$P$<handle>), so copies can be edited independently.
;;; Blocks with the same TAG are kept in step: edit the plan of AHU-1 and the
;;; elevation of AHU-1 follows (and vice versa).
;;;
;;; Load this file in every drawing (Startup Suite or acaddoc.lsp) so the
;;; automatic redraw is active. Drawing units are inches.

(vl-load-com)

;;; ================================================================ shared

(defun mb:doc () (vla-get-ActiveDocument (vlax-get-acad-object)))

(defun mb:fmt (v / dz s)
  (setq dz (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq s (rtos v 2 4))
  (setvar "DIMZIN" dz)
  (if (vl-string-search "." s)
    (setq s (vl-string-right-trim "." (vl-string-right-trim "0" s))))
  s)

;; Text to number: plain numbers, or feet-inches like 10'-6"
(defun mb:num (s)
  (if (= 'STR (type s))
    (cond ((distof s 2)) ((distof s 4)) ((distof s)))))

(defun mb:val (k vals) (cdr (assoc k vals)))

;; Attribute values of a block reference: (("TAG" . "AHU-1") ...)
(defun mb:vals (ins / out)
  (foreach a (vlax-invoke ins 'GetAttributes)
    (setq out (cons (cons (strcase (vla-get-TagString a)) (vla-get-TextString a)) out)))
  (reverse out))

(defun mb:setvals (ins vals / v)
  (foreach a (vlax-invoke ins 'GetAttributes)
    (if (and (setq v (assoc (strcase (vla-get-TagString a)) vals))
             (/= (cdr v) (vla-get-TextString a)))
      (vla-put-TextString a (cdr v)))))

(defun mb:samevals (a b)
  (vl-every '(lambda (p) (= (cdr p) (mb:val (car p) b))) a))

(defun mb:hidden (/ lts)
  (if (not (tblsearch "LTYPE" "HIDDEN"))
    (progn
      (setq lts (vla-get-Linetypes (mb:doc)))
      (foreach f '("acad.lin" "acadiso.lin")
        (if (not (tblsearch "LTYPE" "HIDDEN"))
          (vl-catch-all-apply 'vla-Load (list lts "HIDDEN" f))))))
  (if (tblsearch "LTYPE" "HIDDEN") "HIDDEN"))

;; --- drawing into a block definition (layer 0 so the block takes its insert's layer)
(defun mb:prop (o lt)
  (vla-put-Layer o "0")
  (if lt (vla-put-Linetype o lt))
  o)

(defun mb:pts (lst / sa)
  (setq sa (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length lst)))))
  (vlax-safearray-fill sa lst)
  sa)

(defun mb:rect (blk x1 y1 x2 y2 lt / o)
  (setq o (vla-AddLightWeightPolyline blk (mb:pts (list x1 y1 x2 y1 x2 y2 x1 y2))))
  (vla-put-Closed o :vlax-true)
  (mb:prop o lt))

(defun mb:line (blk x1 y1 x2 y2 lt)
  (mb:prop (vla-AddLine blk (vlax-3d-point x1 y1 0.0) (vlax-3d-point x2 y2 0.0)) lt))

(defun mb:arc (blk cx cy r a1 a2)
  (mb:prop (vla-AddArc blk (vlax-3d-point cx cy 0.0) r a1 a2) nil))

(defun mb:text (blk x y h str / o)
  (setq o (vla-AddText blk str (vlax-3d-point x y 0.0) h))
  (vla-put-Alignment o acAlignmentMiddleCenter)
  (vla-put-TextAlignmentPoint o (vlax-3d-point x y 0.0))
  (mb:prop o nil))

;; --- block kinds
(defun mb:kind (name)
  (setq name (strcase name))
  (cond ((wcmatch name "AHU$P$*") "AHUP")
        ((wcmatch name "AHU$E$*") "AHUE")
        ((wcmatch name "VAV$P$*") "VAV")))

(defun mb:prefix (kind)
  (cdr (assoc kind '(("AHUP" . "AHU$P$") ("AHUE" . "AHU$E$") ("VAV" . "VAV$P$")))))

(defun mb:atts (kind) (if (= kind "VAV") vav:*atts* ahu:*atts*))

(defun mb:parse (kind vals) (if (= kind "VAV") (vav:parse vals) (ahu:parse vals)))

;; Block references whose block name matches pattern
(defun mb:refs (pattern / ss i out)
  (if (setq ss (ssget "_X" (list '(0 . "INSERT") (cons 2 pattern))))
    (repeat (setq i (sslength ss))
      (setq out (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) out))))
  out)

;; Clear a block definition and draw it from the parsed values
(defun mb:redraw (blk kind pr tag / objs h)
  (vlax-for o blk (setq objs (cons o objs)))
  (foreach o objs (vla-Delete o))
  (setq h (cond ((= kind "VAV")  (vav:draw blk pr tag))
                ((= kind "AHUP") (ahu:draw blk pr tag "PLAN"))
                (T               (ahu:draw blk pr tag "ELEV"))))
  ;; attribute definitions (all invisible; the drawing shows plain labels)
  (foreach s (mb:atts kind)
    (mb:prop (vla-AddAttribute blk h acAttributeModeInvisible (cadr s)
                               (vlax-3d-point 0.0 0.0 0.0) (car s) (caddr s))
             nil)))

;; Redraw one block reference from its attributes. With propagate, blocks
;; with the same TAG get the same values and are redrawn too.
(defun mb:sync (ins propagate / kind vals pr blks own old blk)
  (if (setq kind (mb:kind (vla-get-Name ins)))
    (progn
      (setq vals (mb:vals ins)
            pr   (mb:parse kind vals))
      (if (= 'STR (type pr))
        (princ (strcat "\n" (cond ((mb:val "TAG" vals)) ("Block")) ": " pr " - not redrawn."))
        (progn
          (setq blks (vla-get-Blocks (mb:doc))
                own  (strcat (mb:prefix kind) (vla-get-Handle ins))
                old  (vla-get-Name ins))
          ;; copies still sharing this definition get their own before it changes
          (if (= (strcase old) (strcase own))
            (foreach o (mb:refs own)
              (if (/= (vla-get-Handle o) (vla-get-Handle ins)) (mb:sync o nil))))
          (setq blk (vl-catch-all-apply 'vla-Item (list blks own)))
          (if (vl-catch-all-error-p blk)
            (setq blk (vla-Add blks (vlax-3d-point 0.0 0.0 0.0) own)))
          (mb:redraw blk kind pr (mb:val "TAG" vals))
          (if (/= (strcase old) (strcase own))
            (progn
              (vla-put-Name ins own)
              (if (not (mb:refs old))
                (vl-catch-all-apply 'vla-Delete (list (vla-Item blks old))))))
          (vla-Update ins)
          (if (= kind "VAV") (vav:warn pr (mb:val "TAG" vals)) (ahu:warn pr (mb:val "TAG" vals)))
          (if propagate (mb:propagate ins kind vals)))))))

(defun mb:propagate (ins kind vals / tag)
  (setq tag (strcase (mb:val "TAG" vals)))
  (foreach o (mb:refs (if (= kind "VAV") "VAV$*" "AHU$*"))
    (if (and (/= (vla-get-Handle o) (vla-get-Handle ins))
             (= tag (strcase (cond ((mb:val "TAG" (mb:vals o))) (""))))
             (not (mb:samevals vals (mb:vals o))))
      (progn
        (mb:setvals o vals)
        (mb:sync o nil)))))

;; Make the definition for a new block reference and insert it
(defun mb:place (kind vals pt rot / doc blks tmp blk sp ins)
  (setq doc  (mb:doc)
        blks (vla-get-Blocks doc)
        tmp  (strcat (mb:prefix kind) "NEW")
        blk  (vl-catch-all-apply 'vla-Item (list blks tmp)))
  (if (vl-catch-all-error-p blk)
    (setq blk (vla-Add blks (vlax-3d-point 0.0 0.0 0.0) tmp)))
  (mb:redraw blk kind (mb:parse kind vals) (mb:val "TAG" vals))
  (setq sp  (if (and (= 0 (getvar "TILEMODE")) (= 1 (getvar "CVPORT")))
              (vla-get-PaperSpace doc)
              (vla-get-ModelSpace doc))
        ins (vla-InsertBlock sp (vlax-3d-point (trans pt 1 0)) tmp 1.0 1.0 1.0
                             (+ (if rot rot 0.0) (angle '(0 0 0) (getvar "UCSXDIR")))))
  (mb:setvals ins vals)
  (mb:sync ins nil)                       ; gives it its own definition
  ins)

;; Open the attribute editor on a new block, then redraw from the result
(defun mb:edit-new (ins / *mb-busy*)
  (setq *mb-busy* nil)
  (vl-catch-all-apply '(lambda () (command "_.EATTEDIT" (vlax-vla-object->ename ins))))
  (mb:sync ins T))

;;; --- automatic redraw: note edited attributes, redraw when the command ends

(defun mb:onmod (reactor args / e ed own)
  (if (not *mb-busy*)
    (vl-catch-all-apply
      '(lambda ()
         (setq e (cadr args))
         (if (and (= 'ENAME (type e))
                  (setq ed (entget e))
                  (= "ATTRIB" (cdr (assoc 0 ed)))
                  (setq own (cdr (assoc 330 ed)))
                  (mb:kind (cdr (assoc 2 (entget own))))
                  (not (member own *mb-pending*)))
           (setq *mb-pending* (cons own *mb-pending*)))))))

(defun mb:oncmd (reactor args / todo)
  ;; undo/redo put the geometry back themselves
  (if (wcmatch (strcase (car args)) "U,UNDO,REDO,MREDO,OOPS")
    (setq *mb-pending* nil))
  (if (and *mb-pending* (not *mb-busy*))
    (progn
      (setq todo *mb-pending* *mb-pending* nil *mb-busy* T)
      (foreach e todo
        (vl-catch-all-apply
          '(lambda ()
             (if (entget e) (mb:sync (vlax-ename->vla-object e) T)))))
      (setq *mb-busy* nil))))

(if (not *mb-reactors*)
  (setq *mb-reactors*
    (list (vlr-acdb-reactor nil '((:vlr-objectModified . mb:onmod)))
          (vlr-command-reactor nil '((:vlr-commandEnded . mb:oncmd)
                                     (:vlr-commandCancelled . mb:oncmd))))))

;;; ================================================================ AHU
;;; Unit axes: Length = left end -> right end, Width = front side -> back side,
;;; Height = bottom -> top. Insertion point = front-left bottom corner.
;;; Elevation = the view looking at the front side.
;;; Openings (SA_, RA_, OA_):
;;;   LOC   TOP, BOTTOM, FRONT, BACK, LEFT, RIGHT or NONE
;;;   LEN   horizontal size (along the unit for TOP/BOTTOM/FRONT/BACK, along
;;;         the end for LEFT/RIGHT)
;;;   WID   the other size (across the unit for TOP/BOTTOM, vertical otherwise)
;;;   OFF1  TOP/BOTTOM/FRONT/BACK: from left end   LEFT/RIGHT: from front side
;;;   OFF2  TOP/BOTTOM: from front side            others: from bottom

(setq ahu:*atts*
  (append
    '(("TAG"    "Unit tag"                  "AHU-1")
      ("LENGTH" "Unit length"               "120")
      ("WIDTH"  "Unit width (front to back)" "60")
      ("HEIGHT" "Unit height"               "72"))
    (apply 'append
      (mapcar
        '(lambda (k loc len wid o1 o2)
           (list (list (strcat k "_LOC")  (strcat k " location: TOP BOTTOM FRONT BACK LEFT RIGHT NONE") loc)
                 (list (strcat k "_LEN")  (strcat k " length") len)
                 (list (strcat k "_WID")  (strcat k " width") wid)
                 (list (strcat k "_OFF1") (strcat k " offset 1 (from left end; LEFT/RIGHT: from front)") o1)
                 (list (strcat k "_OFF2") (strcat k " offset 2 (TOP/BOTTOM: from front; others: from bottom)") o2)))
        '("SA" "RA" "OA") '("TOP" "TOP" "LEFT") '("24" "24" "30") '("18" "18" "20")
        '("12" "60" "15") '("21" "21" "36")))))

(setq ahu:*locs* '("NONE" "TOP" "BOTTOM" "FRONT" "BACK" "LEFT" "RIGHT"))

(defun ahu:loc (s)
  (if (= 'STR (type s))
    (progn
      (setq s (strcase (vl-string-trim " " s)))
      (if (= s "") "NONE"
        (vl-some '(lambda (f) (if (wcmatch s (strcat f "*")) f)) ahu:*locs*)))))

;; Attribute values -> (("L" . n) ("W" . n) ("H" . n) ("SA" loc len wid o1 o2) ...)
;; or an error message
(defun ahu:parse (vals / pr bad v loc o)
  (foreach k '(("LENGTH" . "L") ("WIDTH" . "W") ("HEIGHT" . "H"))
    (if (and (setq v (mb:num (mb:val (car k) vals))) (> v 0))
      (setq pr (cons (cons (cdr k) v) pr))
      (setq bad (strcat (car k) " must be a number greater than 0"))))
  (foreach k '("SA" "RA" "OA")
    (if (not (setq loc (ahu:loc (mb:val (strcat k "_LOC") vals))))
      (setq bad (strcat k "_LOC must be TOP, BOTTOM, FRONT, BACK, LEFT, RIGHT or NONE")))
    (setq o (mapcar '(lambda (f) (mb:num (mb:val (strcat k f) vals)))
                    '("_LEN" "_WID" "_OFF1" "_OFF2")))
    (if (and loc (/= loc "NONE")
             (or (member nil o) (<= (nth 0 o) 0) (<= (nth 1 o) 0) (< (nth 2 o) 0) (< (nth 3 o) 0)))
      (setq bad (strcat k " sizes must be numbers greater than 0 and offsets 0 or more")))
    (setq pr (cons (cons k (cons loc (mapcar '(lambda (x) (if x x 0.0)) o))) pr)))
  (if bad bad (reverse pr)))

(defun ahu:p (k pr) (cdr (assoc k pr)))

;; Seen face-on: box, symbol (SA = X, RA/OA = one diagonal) and label
(defun ahu:face-on (blk kind x1 y1 x2 y2 lt h)
  (mb:rect blk x1 y1 x2 y2 lt)
  (if (or (= kind "SA") (= kind "RA")) (mb:line blk x1 y1 x2 y2 lt))
  (if (or (= kind "SA") (= kind "OA")) (mb:line blk x1 y2 x2 y1 lt))
  (mb:text blk (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0)
           (min h (* 0.35 (min (- x2 x1) (- y2 y1)))) kind))

;; Seen edge-on: duct collar with the label beside it
(defun ahu:collar (blk kind x1 y1 x2 y2 lx ly h)
  (mb:rect blk x1 y1 x2 y2 nil)
  (mb:text blk lx ly h kind))

(defun ahu:opening (blk view kind o a b c h lt / face ol ow o1 o2)
  (setq face (car o) ol (nth 1 o) ow (nth 2 o) o1 (nth 3 o) o2 (nth 4 o))
  (if (= view "PLAN")
    (cond
      ((= face "TOP")    (ahu:face-on blk kind o1 o2 (+ o1 ol) (+ o2 ow) nil h))
      ((= face "BOTTOM") (ahu:face-on blk kind o1 o2 (+ o1 ol) (+ o2 ow) lt h))
      ((= face "FRONT")  (ahu:collar blk kind o1 (- c) (+ o1 ol) 0.0 (+ o1 (/ ol 2.0)) (- (+ c h)) h))
      ((= face "BACK")   (ahu:collar blk kind o1 b (+ o1 ol) (+ b c) (+ o1 (/ ol 2.0)) (+ b c h) h))
      ((= face "LEFT")   (ahu:collar blk kind (- c) o1 0.0 (+ o1 ol) (- (+ c (* 1.5 h))) (+ o1 (/ ol 2.0)) h))
      ((= face "RIGHT")  (ahu:collar blk kind a o1 (+ a c) (+ o1 ol) (+ a c (* 1.5 h)) (+ o1 (/ ol 2.0)) h)))
    (cond
      ((= face "FRONT")  (ahu:face-on blk kind o1 o2 (+ o1 ol) (+ o2 ow) nil h))
      ((= face "BACK")   (ahu:face-on blk kind o1 o2 (+ o1 ol) (+ o2 ow) lt h))
      ((= face "TOP")    (ahu:collar blk kind o1 b (+ o1 ol) (+ b c) (+ o1 (/ ol 2.0)) (+ b c h) h))
      ((= face "BOTTOM") (ahu:collar blk kind o1 (- c) (+ o1 ol) 0.0 (+ o1 (/ ol 2.0)) (- (+ c h)) h))
      ((= face "LEFT")   (ahu:collar blk kind (- c) o2 0.0 (+ o2 ow) (- (+ c (* 1.5 h))) (+ o2 (/ ow 2.0)) h))
      ((= face "RIGHT")  (ahu:collar blk kind a o2 (+ a c) (+ o2 ow) (+ a c (* 1.5 h)) (+ o2 (/ ow 2.0)) h)))))

;; Draws a view into blk; returns the text height used
(defun ahu:draw (blk pr tag view / a b c h lt)
  (setq a  (ahu:p "L" pr)
        b  (if (= view "PLAN") (ahu:p "W" pr) (ahu:p "H" pr))
        c  (min 6.0 (* 0.15 (min a b)))          ; collar depth
        h  (* 0.08 (min a b))                    ; text height
        lt (mb:hidden))
  (mb:rect blk 0.0 0.0 a b nil)
  (mb:text blk (/ a 2.0) (/ b 2.0) (* 1.25 h) tag)
  (foreach kind '("SA" "RA" "OA")
    (if (/= "NONE" (car (ahu:p kind pr)))
      (ahu:opening blk view kind (ahu:p kind pr) a b c h lt)))
  h)

;; Warn about openings that run past the face they are on
(defun ahu:warn (pr tag / o face a b)
  (foreach kind '("SA" "RA" "OA")
    (setq o (ahu:p kind pr) face (car o))
    (cond
      ((member face '("TOP" "BOTTOM")) (setq a (ahu:p "L" pr) b (ahu:p "W" pr)))
      ((member face '("FRONT" "BACK")) (setq a (ahu:p "L" pr) b (ahu:p "H" pr)))
      ((member face '("LEFT" "RIGHT")) (setq a (ahu:p "W" pr) b (ahu:p "H" pr))))
    (if (and (/= face "NONE")
             (or (> (+ (nth 3 o) (nth 1 o)) (+ a 1e-6))
                 (> (+ (nth 4 o) (nth 2 o)) (+ b 1e-6))))
      (princ (strcat "\n" tag ": " kind " opening runs past the edge of the "
                     (strcase face T) " face.")))))

(defun ahu:nexttag (/ n tags)
  (setq tags (mapcar '(lambda (o) (strcase (cond ((mb:val "TAG" (mb:vals o))) ("")))) (mb:refs "AHU$*"))
        n 1)
  (while (member (strcat "AHU-" (itoa n)) tags) (setq n (1+ n)))
  (strcat "AHU-" (itoa n)))

(defun c:AHU (/ *error* tag view pt rot vals src ins)
  (defun *error* (msg)
    (setq *mb-busy* nil)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (setq tag (strcase (vl-string-trim " " (getstring T (strcat "\nAHU tag <" (ahu:nexttag) ">: ")))))
  (if (= tag "") (setq tag (ahu:nexttag)))
  (initget "Plan Elevation")
  (setq view (if (= "Elevation" (getkword "\nView [Plan/Elevation] <Plan>: ")) "AHUE" "AHUP"))
  ;; start from an existing unit with this tag, if there is one
  (setq src (vl-some '(lambda (o) (if (= tag (strcase (cond ((mb:val "TAG" (mb:vals o))) ("")))) o))
                     (mb:refs "AHU$*"))
        vals (if src
               (mb:vals src)
               (mapcar '(lambda (s) (cons (car s) (caddr s))) ahu:*atts*))
        vals (subst (cons "TAG" tag) (assoc "TAG" vals) vals))
  (if (setq pt (getpoint "\nInsertion point (front-left corner): "))
    (progn
      (setq rot (getangle pt "\nRotation <0>: "))
      (setq *mb-busy* T)
      (setq ins (mb:place view vals pt rot))
      (setq *mb-busy* nil)
      (if src
        (princ (strcat "\n" tag " placed with the values of the existing " tag "."))
        (mb:edit-new ins))))
  (princ))

;;; ================================================================ VAV
;;; Single duct VAV box (Titus DESV), plan view. Insertion point = upstream
;;; end of the straight inlet run on the box centerline; airflow along +X.
;;; Draws 4 inlet diameters of straight inlet duct, inlet collar, casing,
;;; control enclosure on the hand side (HAND = side the controls are on,
;;; looking in the direction of airflow), optional NEC 110.26 working space in
;;; front of the enclosure and optional enclosure door swing.
;;;
;;; SIZE TABLE - fill from the CURRENT Titus DESV submittal (inches)
;;;   (name  inlet-dia  inlet-collar-len  casing-len  casing-wid  casing-hgt
;;;          ctrl-len  ctrl-depth  ctrl-offset)
;;;   ctrl-len    control enclosure length along the casing side
;;;   ctrl-depth  how far the enclosure sticks out from the casing
;;;   ctrl-offset distance from the casing inlet face to the enclosure
(setq vav:*sizes*
  '(
    ;; Waiting for the current Titus DESV submittal dimension table.
  ))

;; NEC 110.26(A)(1) working space depth, inches, for conditions 1, 2, 3
(setq vav:*nec*  '(("208" 36.0 36.0 36.0)       ; 0-150 V to ground (208Y/120)
                   ("480" 36.0 42.0 48.0)))     ; 151-600 V to ground (480Y/277)
(setq vav:*necw* 30.0)                          ; minimum working space width

(setq vav:*atts*
  '(("TAG"           "Box tag"                                              "VAV-1")
    ("SIZE"          "Inlet size (Titus DESV)"                              "")
    ("HAND"          "Hand: LEFT or RIGHT (controls side, looking downstream)" "RIGHT")
    ("NEC_CLEARANCE" "NEC working clearance: NONE, 208 or 480"              "NONE")
    ("NEC_CONDITION" "NEC condition: 1, 2 or 3"                             "1")
    ("DOOR_SWING"    "Show control door swing: YES or NO"                   "NO")))

;; Size row for a SIZE value ("8" and "08" both match)
(defun vav:size (s / n)
  (if (= 'STR (type s))
    (progn
      (setq s (strcase (vl-string-trim " \"" s)) n (distof s 2))
      (vl-some '(lambda (z)
                  (if (or (= s (strcase (car z)))
                          (and n (distof (car z) 2) (equal n (distof (car z) 2) 1e-9)))
                    z))
               vav:*sizes*))))

(defun vav:parse (vals / sz hand clr cnd door s)
  (setq sz   (vav:size (mb:val "SIZE" vals))
        s    (strcase (cond ((mb:val "HAND" vals)) ("")))
        hand (cond ((wcmatch s "L*") "L") ((wcmatch s "R*") "R"))
        s    (strcase (cond ((mb:val "NEC_CLEARANCE" vals)) ("")))
        clr  (cond ((or (= s "") (wcmatch s "N*,0*")) "NONE")
                   ((wcmatch s "*208*") "208")
                   ((wcmatch s "*480*") "480"))
        cnd  (atoi (cond ((mb:val "NEC_CONDITION" vals)) ("1")))
        s    (strcase (cond ((mb:val "DOOR_SWING" vals)) ("")))
        door (wcmatch s "Y*,1,ON"))
  (cond
    ((not vav:*sizes*) "the VAV size table in mechblocks.lsp is empty")
    ((not sz)   (strcat "SIZE must be one of: "
                        (vl-string-right-trim ", " (apply 'strcat (mapcar '(lambda (z) (strcat (car z) ", ")) vav:*sizes*)))))
    ((not hand) "HAND must be LEFT or RIGHT")
    ((not clr)  "NEC_CLEARANCE must be NONE, 208 or 480")
    ((not (member cnd '(1 2 3))) "NEC_CONDITION must be 1, 2 or 3")
    (T (list sz hand clr cnd door))))

(defun vav:warn (pr tag) nil)

;; Draws the plan into blk; returns the text height used
(defun vav:draw (blk pr tag / sz hand clr cnd door d col cl cw ctl ctd cto run x0 sgn h lt y0 y1 dep wid cx)
  (setq sz   (nth 0 pr) hand (nth 1 pr) clr (nth 2 pr) cnd (nth 3 pr) door (nth 4 pr)
        d    (nth 1 sz) col (nth 2 sz) cl (nth 3 sz) cw (nth 4 sz)
        ctl  (nth 6 sz) ctd (nth 7 sz) cto (nth 8 sz)
        run  (* 4.0 d)                           ; 4 diameters of straight inlet
        x0   (+ run col)                         ; casing inlet face
        sgn  (if (= hand "R") -1.0 1.0)          ; right hand = -Y side
        h    (max 1.5 (* 0.12 cw))
        lt   (mb:hidden))
  ;; straight inlet run
  (mb:line blk 0.0 (/ d 2.0) run (/ d 2.0) nil)
  (mb:line blk 0.0 (/ d -2.0) run (/ d -2.0) nil)
  (mb:line blk 0.0 (/ d 2.0) 0.0 (/ d -2.0) nil)
  (mb:text blk (/ run 2.0) (+ (/ d 2.0) h) (* 0.8 h) "4D STRAIGHT")
  ;; inlet collar and casing
  (mb:rect blk run (/ d -2.0) x0 (/ d 2.0) nil)
  (mb:rect blk x0 (/ cw -2.0) (+ x0 cl) (/ cw 2.0) nil)
  (mb:text blk (+ x0 (/ cl 2.0)) (* 0.6 h) h tag)
  (mb:text blk (+ x0 (/ cl 2.0)) (* -0.9 h) (* 0.8 h) (strcat "SIZE " (car sz)))
  ;; control enclosure
  (setq y0 (* sgn (/ cw 2.0))
        y1 (* sgn (+ (/ cw 2.0) ctd)))
  (mb:rect blk (+ x0 cto) (min y0 y1) (+ x0 cto ctl) (max y0 y1) nil)
  ;; door swing, hinged at the inlet end of the enclosure face
  (if door
    (progn
      (mb:line blk (+ x0 cto) y1 (+ x0 cto) (+ y1 (* sgn ctl)) nil)
      (if (> sgn 0)
        (mb:arc blk (+ x0 cto) y1 ctl 0.0 (/ pi 2.0))
        (mb:arc blk (+ x0 cto) y1 ctl (* 1.5 pi) (* 2.0 pi)))))
  ;; NEC 110.26 working space
  (if (/= clr "NONE")
    (progn
      (setq dep (nth cnd (assoc clr vav:*nec*))
            wid (max vav:*necw* ctl)
            cx  (+ x0 cto (/ ctl 2.0)))
      (mb:rect blk (- cx (/ wid 2.0)) y1 (+ cx (/ wid 2.0)) (+ y1 (* sgn dep)) lt)
      (mb:text blk cx (+ y1 (* sgn (/ dep 2.0))) (* 0.8 h) (strcat "NEC CLEARANCE " clr "V"))))
  h)

(defun vav:nexttag (/ n tags)
  (setq tags (mapcar '(lambda (o) (strcase (cond ((mb:val "TAG" (mb:vals o))) ("")))) (mb:refs "VAV$*"))
        n 1)
  (while (member (strcat "VAV-" (itoa n)) tags) (setq n (1+ n)))
  (strcat "VAV-" (itoa n)))

(defun c:VAV (/ *error* tag pt rot vals ins)
  (defun *error* (msg)
    (setq *mb-busy* nil)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (if (not vav:*sizes*)
    (princ "\nThe VAV size table in mechblocks.lsp is empty - fill it from the Titus DESV submittal first.")
    (progn
      (setq tag (strcase (vl-string-trim " " (getstring T (strcat "\nVAV tag <" (vav:nexttag) ">: ")))))
      (if (= tag "") (setq tag (vav:nexttag)))
      (setq vals (mapcar '(lambda (s) (cons (car s) (caddr s))) vav:*atts*)
            vals (subst (cons "TAG" tag) (assoc "TAG" vals) vals)
            vals (subst (cons "SIZE" (car (car vav:*sizes*))) (assoc "SIZE" vals) vals))
      (if (setq pt (getpoint "\nInsertion point (upstream end of inlet run): "))
        (progn
          (setq rot (getangle pt "\nAirflow direction <0>: "))
          (setq *mb-busy* T)
          (setq ins (mb:place "VAV" vals pt rot))
          (setq *mb-busy* nil)
          (mb:edit-new ins)))))
  (princ))

;;; ================================================================ MBSYNC

(defun c:MBSYNC (/ *error* ss i n)
  (defun *error* (msg)
    (setq *mb-busy* nil)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (prompt "\nSelect AHU/VAV blocks to redraw <Enter for all>: ")
  (setq ss (cond ((ssget '((0 . "INSERT") (2 . "AHU$*,VAV$*"))))
                 ((ssget "_X" '((0 . "INSERT") (2 . "AHU$*,VAV$*"))))))
  (setq *mb-busy* T n 0)
  (if ss
    (repeat (setq i (sslength ss))
      (mb:sync (vlax-ename->vla-object (ssname ss (setq i (1- i)))) T)
      (setq n (1+ n))))
  (setq *mb-busy* nil *mb-pending* nil)
  (princ (strcat "\n" (itoa n) " block(s) redrawn."))
  (princ))

(princ "\nMECHBLOCKS loaded. Commands: AHU, VAV, MBSYNC. Edit the block attributes to change them.")
(princ)
