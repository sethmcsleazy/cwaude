;;; FLOWTOTAL - Add up the numbers in selected text
;;; Works with TEXT, MTEXT and MLEADER text. The first number in each object is
;;; used (so "120 GPM" or "Q=1,250" both work). After each selection the running
;;; total is shown and you can keep selecting more; objects already counted are
;;; skipped. Press Enter when done to optionally place the total as text.

;; Strip MTEXT formatting codes so they are not mistaken for numbers
(defun flowtotal:strip (s / i n c out)
  (setq i 1 n (strlen s) out "")
  (while (<= i n)
    (setq c (substr s i 1))
    (cond
      ((= c "\\")
       (setq c (substr s (1+ i) 1)
             i (+ i 2))
       (cond
         ((member c '("P" "~")) (setq out (strcat out " ")))
         ((member c '("\\" "{" "}")) (setq out (strcat out c)))
         ((= c "U") (setq i (+ i 5)))   ; \U+XXXX unicode char
         ((member c '("A" "C" "c" "F" "f" "H" "Q" "T" "W" "p" "S"))
          (while (and (<= i n) (/= (substr s i 1) ";")) (setq i (1+ i)))
          (setq i (1+ i)))))
      ((member c '("{" "}")) (setq i (1+ i)))
      (T (setq out (strcat out c) i (1+ i)))))
  out)

(defun flowtotal:digit-p (c) (and (/= c "") (wcmatch c "#")))

;; Returns (value . decimal-places) for the first number in s, or nil
(defun flowtotal:number (s / i n c start str dot dec neg)
  (setq i 1 n (strlen s))
  (while (and (<= i n) (not start))
    (setq c (substr s i 1))
    (if (or (flowtotal:digit-p c)
            (and (= c ".") (flowtotal:digit-p (substr s (1+ i) 1))))
      (setq start i)
      (setq i (1+ i))))
  (if start
    (progn
      (setq neg (and (> start 1) (= "-" (substr s (1- start) 1)))
            str ""
            dec 0)
      (while (and (<= i n)
                  (progn
                    (setq c (substr s i 1))
                    (or (flowtotal:digit-p c)
                        (and (member c '("." ",")) (not dot) (flowtotal:digit-p (substr s (1+ i) 1))))))
        (cond
          ((= c ".") (setq dot T str (strcat str c)))
          ((= c ","))                     ; thousands separator
          (T (setq str (strcat str c))
             (if dot (setq dec (1+ dec)))))
        (setq i (1+ i)))
      (cons (* (if neg -1 1) (atof str)) dec))))

(defun c:FLOWTOTAL (/ *error* ss i e txt r used total dec cnt skip pt)
  (vl-load-com)
  (defun *error* (msg)
    (foreach x used (redraw x 4))
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (setq total 0.0 dec 0)
  (while (progn
           (prompt "\nSelect text to add <done>: ")
           (setq ss (ssget '((0 . "TEXT,MTEXT,MULTILEADER")))))
    (setq i 0 cnt 0 skip 0)
    (repeat (sslength ss)
      (setq e (ssname ss i) i (1+ i))
      (cond
        ((member e used))
        ((and (not (vl-catch-all-error-p
                     (setq txt (vl-catch-all-apply 'vla-get-TextString (list (vlax-ename->vla-object e))))))
              (setq r (flowtotal:number (flowtotal:strip txt))))
         (setq total (+ total (car r))
               dec   (max dec (cdr r))
               used  (cons e used)
               cnt   (1+ cnt)))
        (T (setq skip (1+ skip)))))
    (foreach x used (redraw x 3))
    (princ (strcat "\n  +" (itoa cnt) " value(s)"
                   (if (> skip 0) (strcat ", " (itoa skip) " without a number skipped") "")
                   "   >>  TOTAL = " (rtos total 2 dec)
                   "  (" (itoa (length used)) " values)")))
  (foreach x used (redraw x 4))
  (if used
    (progn
      (princ (strcat "\nFinal total = " (rtos total 2 dec)))
      (initget "Yes No")
      (if (and (= "Yes" (getkword "\nPlace total as text? [Yes/No] <No>: "))
               (setq pt (getpoint "\nText location: ")))
        (entmake (list '(0 . "TEXT")
                       (cons 10 (trans pt 1 0))
                       (cons 40 (getvar "TEXTSIZE"))
                       (cons 1 (rtos total 2 dec))
                       (cons 7 (getvar "TEXTSTYLE")))))))
  (princ))

(princ "\nFLOWTOTAL loaded.")
(princ)
