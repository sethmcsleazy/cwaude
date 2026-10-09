;;; BLKCOUNT - Count block references by name
;;; Uses the effective name, so dynamic blocks are grouped correctly. MINSERT
;;; arrays count every copy. Choose Select to pick blocks, or press Enter to
;;; count the whole drawing. Anonymous blocks (associative arrays and other
;;; *U blocks that aren't dynamic blocks) are skipped and counted separately.
;;; (Named BLKCOUNT so it does not clash with the Express Tools BCOUNT command.)

(defun c:BLKCOUNT (/ *error* ss i obj name qty tot pair counts anon)
  (vl-load-com)
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (initget "Select All")
  (setq ss (if (= "Select" (getkword "\nCount blocks [Select/All] <All>: "))
             (progn
               (prompt "\nSelect blocks to count: ")
               (ssget '((0 . "INSERT"))))
             (ssget "_X" '((0 . "INSERT")))))
  (if ss
    (progn
      (setq i 0 tot 0 anon 0)
      (repeat (sslength ss)
        (setq obj  (vlax-ename->vla-object (ssname ss i))
              name (vla-get-EffectiveName obj)
              i    (1+ i))
        (if (wcmatch name "`**")
          (setq anon (1+ anon))
          (progn
            (setq qty (if (= "AcDbMInsertBlock" (vla-get-ObjectName obj))
                        (* (vla-get-Columns obj) (vla-get-Rows obj))
                        1)
                  tot (+ tot qty))
            (if (setq pair (assoc name counts))
              (setq counts (subst (cons name (+ qty (cdr pair))) pair counts))
              (setq counts (cons (cons name qty) counts))))))
      (setq counts (vl-sort counts '(lambda (a b) (< (strcase (car a)) (strcase (car b))))))
      (princ "\n\nBlock name                      Count\n------------------------------  -----")
      (foreach pair counts
        (princ (strcat "\n" (car pair)
                       (substr "                                " 1 (max 2 (- 32 (strlen (car pair)))))
                       (itoa (cdr pair)))))
      (princ (strcat "\n------------------------------  -----\nTotal: " (itoa tot)))
      (if (> anon 0)
        (princ (strcat "\n(" (itoa anon) " anonymous block(s) skipped, e.g. associative arrays.)")))
      (textscr))
    (princ "\nNo blocks found."))
  (princ))

(princ "\nBLKCOUNT loaded.")
(princ)
