;;; DOUBLEOFFSET - Offset an object to both sides, half the entered distance each way
;;; Enter the total width once; it is remembered for the rest of the session,
;;; in every drawing. Keep picking objects; press Enter to finish. The original
;;; is kept. If only one side can be offset, nothing is added for that object.

;; Offset obj by d; returns the list of new objects, or nil if it failed
(defun doubleoffset:try (obj d / r)
  (setq r (vl-catch-all-apply 'vla-Offset (list obj d)))
  (if (not (vl-catch-all-error-p r))
    (setq r (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value r)))))
  (if (not (vl-catch-all-error-p r)) r))

(defun c:DOUBLEOFFSET (/ *error* doc dist sel obj half done undo a b)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (if undo (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  ;; each drawing has its own LISP variables - keep the width on the session-wide blackboard
  (setq *doubleoffset-dist* (vl-bb-ref '*doubleoffset-dist*))
  (initget (if *doubleoffset-dist* 6 7))
  (setq dist (getdist (if *doubleoffset-dist*
                        (strcat "\nTotal offset width <" (rtos *doubleoffset-dist*) ">: ")
                        "\nTotal offset width: ")))
  (if (not dist) (setq dist *doubleoffset-dist*))
  (setq *doubleoffset-dist* dist
        half (/ dist 2.0))
  (vl-bb-set '*doubleoffset-dist* dist)
  (vla-StartUndoMark doc)
  (setq undo T)
  (while (not done)
    (setvar "ERRNO" 0)
    (setq sel (entsel "\nSelect object to offset <exit>: "))
    (cond
      ((and (not sel) (= 7 (getvar "ERRNO"))) (princ "\nMissed, try again."))
      ((not sel) (setq done T))
      (T
       (setq obj (vlax-ename->vla-object (car sel))
             a   (doubleoffset:try obj half)
             b   (if a (doubleoffset:try obj (- half))))
       (if (not (and a b))
         (progn
           ;; don't leave a one-sided offset behind
           (foreach o a (vla-Delete o))
           (princ "\nCould not offset that object (too small for this distance, or not offsettable)."))))))
  (vla-EndUndoMark doc)
  (setq undo nil)
  (princ))

(princ "\nDOUBLEOFFSET loaded.")
(princ)
