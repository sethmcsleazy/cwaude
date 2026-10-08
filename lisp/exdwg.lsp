;;; EXDWG - Open Windows Explorer at the current drawing's folder, with the file selected

(defun c:EXDWG (/ folder)
  (setq folder (getvar "DWGPREFIX"))
  (if (= 1 (getvar "DWGTITLED"))
    (startapp "explorer" (strcat "/select,\"" folder (getvar "DWGNAME") "\""))
    (progn
      (princ "\nDrawing has not been saved yet - opening its default folder.")
      (startapp "explorer" (strcat "\"" (vl-string-right-trim "\\" folder) "\""))))
  (princ))

(princ "\nEXDWG loaded.")
(princ)
