;;; DOUBLEOFFSET - Offset an object to both sides, half the entered distance each way
;;; Enter the total width once; it is remembered for the rest of the session.
;;; Keep picking objects; press Enter to finish. The original is kept.

(defun c:DOUBLEOFFSET (/ *error* doc dist sel obj half done)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (vla-EndUndoMark doc)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (initget (if *doubleoffset-dist* 6 7))
  (setq dist (getdist (if *doubleoffset-dist*
                        (strcat "\nTotal offset width <" (rtos *doubleoffset-dist*) ">: ")
                        "\nTotal offset width: ")))
  (if (not dist) (setq dist *doubleoffset-dist*))
  (setq *doubleoffset-dist* dist
        half (/ dist 2.0))
  (vla-StartUndoMark doc)
  (while (not done)
    (setvar "ERRNO" 0)
    (setq sel (entsel "\nSelect object to offset <exit>: "))
    (cond
      ((and (not sel) (= 7 (getvar "ERRNO"))) (princ "\nMissed, try again."))
      ((not sel) (setq done T))
      (T
       (setq obj (vlax-ename->vla-object (car sel)))
       (if (or (vl-catch-all-error-p (vl-catch-all-apply 'vla-Offset (list obj half)))
               (vl-catch-all-error-p (vl-catch-all-apply 'vla-Offset (list obj (- half)))))
         (princ "\nCould not offset that object (too small for this distance, or not offsettable).")))))
  (vla-EndUndoMark doc)
  (princ))

(princ "\nDOUBLEOFFSET loaded.")
(princ)
