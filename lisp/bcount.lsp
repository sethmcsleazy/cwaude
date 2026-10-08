;;; BCOUNT - Count block references by name
;;; Uses the effective name, so dynamic blocks are grouped correctly.
;;; Press Enter at the selection prompt to count the whole drawing.

(defun c:BCOUNT (/ *error* ss i name pair counts)
  (vl-load-com)
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (prompt "\nSelect blocks to count <Enter for all>: ")
  (if (or (setq ss (ssget '((0 . "INSERT"))))
          (setq ss (ssget "_X" '((0 . "INSERT")))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq name (vla-get-EffectiveName (vlax-ename->vla-object (ssname ss i)))
              i (1+ i))
        (if (setq pair (assoc name counts))
          (setq counts (subst (cons name (1+ (cdr pair))) pair counts))
          (setq counts (cons (cons name 1) counts))))
      (setq counts (vl-sort counts '(lambda (a b) (< (strcase (car a)) (strcase (car b))))))
      (princ "\n\nBlock name                      Count\n------------------------------  -----")
      (foreach pair counts
        (princ (strcat "\n" (car pair)
                       (substr "                                " 1 (max 2 (- 32 (strlen (car pair)))))
                       (itoa (cdr pair)))))
      (princ (strcat "\n------------------------------  -----\nTotal: " (itoa (sslength ss))))
      (textscr))
    (princ "\nNo blocks found."))
  (princ))

(princ "\nBCOUNT loaded.")
(princ)
