;;; MTFORMAT - Center MTEXT inside the border around it and size its frame to fit
;;; For each selected MTEXT, finds the closed area it sits in (a box, a table
;;; cell made of lines, a title block cell, etc.), sets the justification to
;;; Middle Center, moves it to the center of that area, and sets the text's
;;; defined width and height to the border size so the grips sit on the
;;; border's corners. Paragraphs set to left/right/justified are centered too.
;;; The border must be visible on screen (same limitation as BOUNDARY/HATCH).
;;; Text on locked layers is skipped.

;; Set an MTEXT's defined height (DXF 46), adding the group after 41 if missing
;; Set an MTEXT's defined height (DXF 46), adding the group after 41 if
;; missing. entmod writes the contents back as plain text, so any fields are
;; restored from their field codes afterwards.
(defun mtformat:setheight (obj h / e ed out fc)
  (setq e  (vlax-vla-object->ename obj)
        fc (vla-FieldCode obj)
        ed (entget e))
  (if (assoc 46 ed)
    (setq out (subst (cons 46 h) (assoc 46 ed) ed))
    (foreach x ed
      (setq out (if (= 41 (car x)) (cons (cons 46 h) (cons x out)) (cons x out)))))
  (entmod (if (assoc 46 ed) out (reverse out)))
  (if (/= fc (vla-get-TextString obj))
    (vla-put-TextString obj fc)))

(defun mtformat:bbox (obj / mn mx)
  (vla-GetBoundingBox obj 'mn 'mx)
  (list (vlax-safearray->list mn) (vlax-safearray->list mx)))

;; Force every explicit paragraph alignment code (\p...;) to centered
;; Force every explicit paragraph alignment code (\p...;) to centered.
;; A \p preceded by an odd number of backslashes is typed text, not a code.
(defun mtformat:centerpara (s / i j k seg)
  (setq i 0)
  (while (setq i (vl-string-search "\\p" s i))
    (setq k 0)
    (while (and (> (- i k) 0) (= "\\" (substr s (- i k) 1))) (setq k (1+ k)))
    (if (and (= 0 (rem k 2)) (setq j (vl-string-search ";" s i)))
      (progn
        (setq seg (substr s (1+ i) (- (1+ j) i)))
        (foreach a '("ql" "qr" "qj" "qd")
          (setq seg (vl-string-subst "qc" a seg)))
        (setq s (strcat (substr s 1 i) seg (substr s (+ j 2)))
              i (1+ j)))
      (setq i (1+ i))))
  s)

;; Finds the closed area around pt (WCS) and measures it along the text
;; direction rot. Returns ((center-x center-y) width height), or nil.
(defun mtformat:border (pt rot / prev bset bp bo box mid res e new)
  (setq prev (entlast)
        bset (ssget "_X" (list '(0 . "LINE,ARC,CIRCLE,ELLIPSE,SPLINE,LWPOLYLINE,POLYLINE,INSERT")
                               ;; model space when working through a layout viewport
                               (cons 410 (if (and (= 0 (getvar "TILEMODE")) (= 1 (getvar "CVPORT")))
                                           (getvar "CTAB")
                                           "Model")))))
  (setq bp (vl-catch-all-apply 'bpoly (if bset (list (trans pt 0 1) bset) (list (trans pt 0 1)))))
  (if (and bp (not (vl-catch-all-error-p bp)))
    (progn
      (setq bo (vlax-ename->vla-object bp))
      ;; turn the temporary boundary so the text direction lines up with X
      (if (not (equal rot 0.0 1e-9))
        (vla-Rotate bo (vlax-3d-point pt) (- rot)))
      (setq box (mtformat:bbox bo)
            mid (list (- (/ (+ (car (car box)) (car (cadr box))) 2.0) (car pt))
                      (- (/ (+ (cadr (car box)) (cadr (cadr box))) 2.0) (cadr pt)))
            ;; turn the center back to where it really is
            mid (list (+ (car pt) (- (* (cos rot) (car mid)) (* (sin rot) (cadr mid))))
                      (+ (cadr pt) (* (sin rot) (car mid)) (* (cos rot) (cadr mid))))
            res (list mid
                      (- (car (cadr box)) (car (car box)))
                      (- (cadr (cadr box)) (cadr (car box)))))))
  ;; delete the temporary boundary object(s)
  (setq e (if prev (entnext prev) (entnext)))
  (while e
    (if (not (member (cdr (assoc 0 (entget e))) '("VERTEX" "SEQEND" "ATTRIB")))
      (setq new (cons e new)))
    (setq e (entnext e)))
  (foreach x new (entdel x))
  res)

(defun c:MTFORMAT (/ *error* doc ss i obj ed box ins ctr rot border str str2 clay relock undo ok fail)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (if relock (vla-put-Lock clay :vlax-true))
    (if undo (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (prompt "\nSelect MTEXT to center: ")
  (if (setq ss (ssget "_:L" '((0 . "MTEXT"))))
    (progn
      (vla-StartUndoMark doc)
      (setq undo T)
      ;; the temporary boundary goes on the current layer, which must be unlocked to delete it
      (setq clay (vla-Item (vla-get-Layers doc) (getvar "CLAYER")))
      (if (= :vlax-true (vla-get-Lock clay))
        (progn
          (vla-put-Lock clay :vlax-false)
          (setq relock T)))
      (setq i 0 ok 0 fail 0)
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i))
              ed  (entget (ssname ss i))
              i   (1+ i)
              box (mtformat:bbox obj)
              ctr (mapcar '(lambda (a b) (/ (+ a b) 2.0)) (car box) (cadr box))
              ;; text direction in WCS, from the MTEXT X-axis vector
              rot (if (assoc 11 ed)
                    (angle '(0.0 0.0 0.0) (cdr (assoc 11 ed)))
                    (vla-get-Rotation obj)))
        (if (setq border (mtformat:border ctr rot))
          (progn
            (setq ins  (vlax-get obj 'InsertionPoint)
                  ctr  (list (car (car border)) (cadr (car border)) (caddr ins))
                  str  (vla-FieldCode obj)              ; keeps fields as fields
                  str2 (mtformat:centerpara str))
            (vla-put-AttachmentPoint obj acAttachmentPointMiddleCenter)
            (if (/= str str2) (vla-put-TextString obj str2))
            (vla-put-InsertionPoint obj (vlax-3d-point ctr))
            (vla-put-Width obj (cadr border))
            (mtformat:setheight obj (caddr border))
            (setq ok (1+ ok)))
          (setq fail (1+ fail))))
      (if relock (vla-put-Lock clay :vlax-true))
      (setq relock nil)
      (vla-EndUndoMark doc)
      (setq undo nil)
      (princ (strcat "\n" (itoa ok) " MTEXT centered"
                     (if (> fail 0)
                       (strcat ", " (itoa fail) " skipped (no closed border found - AutoCAD circles any gap in red; REDRAW clears them. The border must also be on screen)")
                       "")))))
  (princ))

(princ "\nMTFORMAT loaded.")
(princ)
