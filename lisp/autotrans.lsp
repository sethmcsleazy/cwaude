;;; AUTOTRANS - Angled transition between two runs of parallel lines
;;;
;;; Pick the two lines of the first run (the one you keep), then the two lines
;;; of the second run. All four lines must be parallel. From the end of each
;;; first-run line, a transition line is drawn at the set angle (measured from
;;; the run direction) over to the matching second-run line, and that line is
;;; trimmed or extended to meet it. A line is also drawn across the run at each
;;; end of the transition. Sides that are already in line are just
;;; trimmed/extended, so offset (eccentric) transitions work too.
;;;
;;; The angle works like the FILLET radius: choose the [Angle] option at the
;;; first prompt to change it. It defaults to 30 and is remembered between sessions.

(defun autotrans:angle (/ a)
  (if (and (setq a (getenv "AutoTransAngle"))
           (setq a (atof a))
           (< 0.0 a 90.0))
    a
    30.0))

(defun autotrans:setangle (/ a)
  (initget 6)
  (setq a (getreal (strcat "\nTransition angle in degrees <" (rtos (autotrans:angle) 2 2) ">: ")))
  (cond
    ((null a))
    ((>= a 90.0) (princ "\nAngle must be less than 90."))
    (T (setenv "AutoTransAngle" (rtos a 2 8)))))

(defun autotrans:pick (msg kw picked / sel ent done)
  (while (not done)
    (setvar "ERRNO" 0)
    (if kw (initget "Angle"))
    (setq sel (entsel (if kw
                        (strcat msg " or [Angle] <" (rtos (autotrans:angle) 2 2) " deg>: ")
                        (strcat msg ": "))))
    (cond
      ((= sel "Angle") (autotrans:setangle))
      ((and (null sel) (= 7 (getvar "ERRNO"))) (princ "\nMissed, try again."))
      ((null sel) (setq done T))
      ((/= "LINE" (cdr (assoc 0 (entget (car sel))))) (princ "\nThat is not a LINE."))
      ((member (car sel) picked) (princ "\nThat line is already selected."))
      (T (setq ent (car sel) done T))))
  ent)

(defun autotrans:2d (p) (list (car p) (cadr p)))

(defun autotrans:unit (v / len)
  (setq len (distance '(0.0 0.0) v))
  (if (> len 1e-12) (mapcar '(lambda (x) (/ x len)) v)))

(defun autotrans:dot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))

(defun autotrans:dir (e / ed)
  (setq ed (entget e))
  (autotrans:unit (mapcar '- (autotrans:2d (cdr (assoc 11 ed))) (autotrans:2d (cdr (assoc 10 ed))))))

;; Position of a line's midpoint along direction u, measured from origin o
(defun autotrans:station (e u o / ed)
  (setq ed (entget e))
  (autotrans:dot (mapcar '(lambda (a b c) (- (/ (+ a b) 2.0) c))
                         (autotrans:2d (cdr (assoc 10 ed)))
                         (autotrans:2d (cdr (assoc 11 ed)))
                         o)
                 u))

;; Returns (ename side near-code near-pt near-station far-pt far-station)
(defun autotrans:rec (e u n o / ed p10 p11 s10 s11)
  (setq ed  (entget e)
        p10 (cdr (assoc 10 ed))
        p11 (cdr (assoc 11 ed))
        s10 (autotrans:dot (mapcar '- (autotrans:2d p10) o) u)
        s11 (autotrans:dot (mapcar '- (autotrans:2d p11) o) u))
  (if (<= s10 s11)
    (list e (autotrans:dot (mapcar '- (autotrans:2d p10) o) n) 10 p10 s10 p11 s11)
    (list e (autotrans:dot (mapcar '- (autotrans:2d p10) o) n) 11 p11 s11 p10 s10)))

;; Point at a given station (along u) and side offset (along n)
(defun autotrans:at (o u n sta side z)
  (list (+ (car o) (* (car u) sta) (* (car n) side))
        (+ (cadr o) (* (cadr u) sta) (* (cadr n) side))
        z))

;; Works out the new geometry. Returns (mods lines) or an error string.
;;   mods  - list of (ename group-code new-point) for the second-run lines
;;   lines - list of (start end) for the new lines: the two angled lines plus
;;           an end line across each end of the transition
(defun autotrans:plan (lines ang / tol o u n v run1 run2 ra rb pa off len pb mods new pas sta-end err)
  (setq tol 1e-8
        o   (autotrans:2d (cdr (assoc 10 (entget (car lines)))))
        u   (autotrans:dir (car lines)))
  (foreach e lines
    (setq v (autotrans:dir e))
    (if (or (null u) (null v)
            (> (abs (- (* (car u) (cadr v)) (* (cadr u) (car v)))) 1e-6))
      (setq err "All four lines must be parallel.")))
  (if (not err)
    (progn
      ;; point u from the first run toward the second run
      (if (< (+ (autotrans:station (nth 2 lines) u o) (autotrans:station (nth 3 lines) u o))
             (+ (autotrans:station (nth 0 lines) u o) (autotrans:station (nth 1 lines) u o)))
        (setq u (mapcar '- u)))
      (setq n    (list (- (cadr u)) (car u))
            run1 (vl-sort (mapcar '(lambda (e) (autotrans:rec e u n o)) (list (nth 0 lines) (nth 1 lines)))
                          '(lambda (x y) (< (cadr x) (cadr y))))
            run2 (vl-sort (mapcar '(lambda (e) (autotrans:rec e u n o)) (list (nth 2 lines) (nth 3 lines)))
                          '(lambda (x y) (< (cadr x) (cadr y)))))
      (cond
        ((< (- (cadr (cadr run1)) (cadr (car run1))) tol)
         (setq err "The two first-run lines are on top of each other."))
        ((< (- (cadr (cadr run2)) (cadr (car run2))) tol)
         (setq err "The two second-run lines are on top of each other."))
        (T
         (foreach pair (list (list (car run1) (car run2)) (list (cadr run1) (cadr run2)))
           (setq ra  (car pair)
                 rb  (cadr pair)
                 pa  (nth 5 ra)                       ; far end of first-run line
                 off (- (cadr rb) (cadr ra))          ; sideways distance to cover
                 len (/ (abs off) (/ (sin ang) (cos ang)))
                 pb  (list (+ (car pa) (* (car u) len) (* (car n) off))
                           (+ (cadr pa) (* (cadr u) len) (* (cadr n) off))
                           (caddr pa))
                 mods    (cons (list (car rb) (nth 2 rb) pb) mods)
                 pas     (cons pa pas)
                 sta-end (if sta-end (max sta-end (+ (nth 6 ra) len)) (+ (nth 6 ra) len)))
           (if (> (abs off) tol)
             (setq new (cons (list pa pb) new))))
         (cond
           ((not new)
            (setq err "Both runs are already in line - there is nothing to transition."))
           ((or (<= (nth 6 (car run2)) (+ sta-end tol))
                (<= (nth 6 (cadr run2)) (+ sta-end tol)))
            (setq err "The second run is too short for the transition."))
           (T
            ;; end lines: across the end of the first run, and across the second
            ;; run where the transition finishes
            (setq new (append new
                              (list (list (car pas) (cadr pas))
                                    (list (autotrans:at o u n sta-end (cadr (car run2)) (caddr (car pas)))
                                          (autotrans:at o u n sta-end (cadr (cadr run2)) (caddr (car pas)))))))))))))
  (if err err (list mods new)))

(defun c:AUTOTRANS (/ *error* doc msgs picked e plan ed props)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (foreach x picked (redraw x 4))
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (setq msgs '("\nSelect first line of FIRST run (kept)"
               "\nSelect second line of FIRST run"
               "\nSelect first line of SECOND run (trimmed/extended)"
               "\nSelect second line of SECOND run"))
  (while (and msgs (setq e (autotrans:pick (car msgs) (null picked) picked)))
    (redraw e 3)
    (setq picked (append picked (list e))
          msgs   (cdr msgs)))
  (foreach x picked (redraw x 4))
  (if (= 4 (length picked))
    (if (= 'STR (type (setq plan (autotrans:plan picked (* pi (/ (autotrans:angle) 180.0))))))
      (princ (strcat "\n" plan))
      (progn
        (vla-StartUndoMark doc)
        ;; trim/extend the second-run lines
        (foreach m (car plan)
          (setq ed (entget (car m)))
          (entmod (subst (cons (cadr m) (caddr m)) (assoc (cadr m) ed) ed)))
        ;; draw the angled lines and end lines using the first-run line's properties
        (setq props (vl-remove-if-not '(lambda (x) (member (car x) '(6 8 48 62 370)))
                                      (entget (car picked))))
        (foreach l (cadr plan)
          (entmake (append (list '(0 . "LINE") (cons 10 (car l)) (cons 11 (cadr l))) props)))
        (vla-EndUndoMark doc)
        (princ (strcat "\nTransition drawn at " (rtos (autotrans:angle) 2 2) " degrees.")))))
  (princ))

(princ "\nAUTOTRANS loaded.")
(princ)
