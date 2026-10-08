;;; NUMINC - Place incrementing numbers
;;; Prompts for prefix, suffix, start, increment and height, then places
;;; middle-centered text at each picked point until you press Enter/Esc.

(defun c:NUMINC (/ *error* pre suf num inc ht pt str)
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (setq pre (getstring T "\nPrefix <none>: ")
        suf (getstring T "\nSuffix <none>: "))
  (initget 0)
  (setq num (getint "\nStart number <1>: "))
  (if (not num) (setq num 1))
  (initget 2)
  (setq inc (getint "\nIncrement <1>: "))
  (if (not inc) (setq inc 1))
  (initget 6)
  (setq ht (getdist (strcat "\nText height <" (rtos (getvar "TEXTSIZE")) ">: ")))
  (if (not ht) (setq ht (getvar "TEXTSIZE")))
  (while (setq pt (getpoint (strcat "\nPlace " (setq str (strcat pre (itoa num) suf)) " <Enter to finish>: ")))
    (setq pt (trans pt 1 0))
    (entmake (list '(0 . "TEXT")
                   (cons 10 pt)
                   (cons 11 pt)
                   (cons 40 ht)
                   (cons 1 str)
                   (cons 7 (getvar "TEXTSTYLE"))
                   '(72 . 1)    ; horizontal: center
                   '(73 . 2)))  ; vertical: middle
    (setq num (+ num inc)))
  (princ))

(princ "\nNUMINC loaded.")
(princ)
