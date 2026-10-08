;;; MTFORMAT - Center MTEXT inside the border around it and stretch its width to fit
;;; For each selected MTEXT, finds the closed area it sits in (a box, a table
;;; cell made of lines, a title block cell, etc.), sets the justification to
;;; Middle Center, moves it to the center of that area, and sets the text width
;;; so the grips sit on the left and right borders.
;;; The border must be visible on screen (same limitation as BOUNDARY/HATCH).

(defun mtformat:bbox (obj / mn mx)
  (vla-GetBoundingBox obj 'mn 'mx)
  (list (vlax-safearray->list mn) (vlax-safearray->list mx)))

;; Returns the bounding box of the closed area around pt (WCS), or nil
(defun mtformat:border (pt / prev bset bp box e new)
  (setq prev (entlast)
        bset (ssget "_X" (list '(0 . "LINE,ARC,CIRCLE,ELLIPSE,SPLINE,LWPOLYLINE,POLYLINE,INSERT")
                               (cons 410 (getvar "CTAB")))))
  (setq bp (vl-catch-all-apply 'bpoly (if bset (list (trans pt 0 1) bset) (list (trans pt 0 1)))))
  (if (and bp (not (vl-catch-all-error-p bp)))
    (setq box (mtformat:bbox (vlax-ename->vla-object bp))))
  ;; delete the temporary boundary object(s)
  (setq e (if prev (entnext prev) (entnext)))
  (while e
    (if (not (member (cdr (assoc 0 (entget e))) '("VERTEX" "SEQEND" "ATTRIB")))
      (setq new (cons e new)))
    (setq e (entnext e)))
  (foreach x new (entdel x))
  box)

(defun c:MTFORMAT (/ *error* doc ss i obj box ins ctr border width ok fail)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (vla-EndUndoMark doc)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (prompt "\nSelect MTEXT to center: ")
  (if (setq ss (ssget '((0 . "MTEXT"))))
    (progn
      (vla-StartUndoMark doc)
      (setq i 0 ok 0 fail 0)
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i))
              i   (1+ i)
              box (mtformat:bbox obj)
              ctr (mapcar '(lambda (a b) (/ (+ a b) 2.0)) (car box) (cadr box)))
        (if (setq border (mtformat:border ctr))
          (progn
            (setq ins   (vlax-get obj 'InsertionPoint)
                  ctr   (list (/ (+ (car (car border)) (car (cadr border))) 2.0)
                              (/ (+ (cadr (car border)) (cadr (cadr border))) 2.0)
                              (caddr ins))
                  ;; rotated 90/270 text spans the border's height instead
                  width (if (< (abs (sin (vla-get-Rotation obj))) 0.7071)
                          (- (car (cadr border)) (car (car border)))
                          (- (cadr (cadr border)) (cadr (car border)))))
            (vla-put-AttachmentPoint obj acAttachmentPointMiddleCenter)
            (vla-put-InsertionPoint obj (vlax-3d-point ctr))
            (vla-put-Width obj width)
            (setq ok (1+ ok)))
          (setq fail (1+ fail))))
      (vla-EndUndoMark doc)
      (princ (strcat "\n" (itoa ok) " MTEXT centered"
                     (if (> fail 0)
                       (strcat ", " (itoa fail) " skipped (no closed border found - make sure it is on screen)")
                       "")))))
  (princ))

(princ "\nMTFORMAT loaded.")
(princ)
