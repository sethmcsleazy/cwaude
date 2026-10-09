;;; FLOWTOTAL - Add up the numbers in selected text
;;; Works with TEXT, MTEXT and MLEADER text. The first number in each object is
;;; used (so "120 GPM", "Q=1,250", "3/4" and "1 1/2" all work; a dash inside a
;;; tag like "CHW-450" is not read as a minus sign). After each selection the running
;;; total is shown and you can keep selecting more; objects already counted are
;;; skipped. Press Enter when done to optionally place the total as text.

;; Strip MTEXT formatting codes so they are not mistaken for numbers
(defun flowtotal:strip (s / i n c out j)
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
         ((= c "S")   ; stacked text: keep fractions \S1/2; \S1#2;, drop ^ stacks
          (setq j i)
          (while (and (<= i n) (/= (substr s i 1) ";")) (setq i (1+ i)))
          (setq c (substr s j (- i j))
                i (1+ i))
          (if (not (vl-string-search "^" c))
            (setq out (strcat out " " (vl-string-translate "#" "/" c)))))
         ((member c '("A" "C" "c" "F" "f" "H" "Q" "T" "W" "p"))
          (while (and (<= i n) (/= (substr s i 1) ";")) (setq i (1+ i)))
          (setq i (1+ i)))))
      ((member c '("{" "}")) (setq i (1+ i)))
      (T (setq out (strcat out c) i (1+ i)))))
  out)

(defun flowtotal:digit-p (c) (and (/= c "") (wcmatch c "#")))

;; Run of digits in s starting at position i ("" if none)
(defun flowtotal:digits (s i / r)
  (setq r "")
  (while (flowtotal:digit-p (substr s i 1))
    (setq r (strcat r (substr s i 1))
          i (1+ i)))
  r)

;; Decimal places needed to show v (up to 4), used for fractions
(defun flowtotal:places (v / d)
  (setq d 0)
  (while (and (< d 4)
              (not (equal v (/ (fix (+ (* v (expt 10.0 d)) 0.5)) (expt 10.0 d)) 1e-9)))
    (setq d (1+ d)))
  d)

;; Returns (value . decimal-places) for the first number in s, or nil
(defun flowtotal:number (s / i n c start str dot dec neg val num den grp)
  (setq i 1 n (strlen s))
  (while (and (<= i n) (not start))
    (setq c (substr s i 1))
    (if (or (flowtotal:digit-p c)
            (and (= c ".") (flowtotal:digit-p (substr s (1+ i) 1))))
      (setq start i)
      (setq i (1+ i))))
  (if start
    (progn
      ;; a dash right after a letter or digit (CHW-450) is part of a tag, not a minus sign
      (setq neg (and (> start 1)
                     (= "-" (substr s (1- start) 1))
                     (or (= start 2) (not (wcmatch (substr s (- start 2) 1) "@,#"))))
            str ""
            dec 0)
      (while (and (<= i n)
                  (progn
                    (setq c (substr s i 1))
                    (or (flowtotal:digit-p c)
                        (and (member c '("." ",")) (not dot) (flowtotal:digit-p (substr s (1+ i) 1))))))
        (cond
          ;; 1,250 thousands separator: groups of 3 after 1-3 leading digits
          ;; (not 0,125 or 1250,125 - those are decimal commas)
          ((and (= c ",")
                (wcmatch (substr s (1+ i) 4) "###,###[~0-9]")
                (or grp (and (<= (strlen str) 3) (/= "0" (substr str 1 1)))))
           (setq grp T))
          ((member c '("." ",")) (setq dot T str (strcat str ".")))          ; 1.5 or 1,5 decimal
          (T (setq str (strcat str c))
             (if dot (setq dec (1+ dec)))))
        (setq i (1+ i)))
      (setq val (atof str))
      (if (not dot)
        (cond
          ((and (= "/" (substr s i 1))                                     ; 3/4
                (/= "" (setq den (flowtotal:digits s (1+ i))))
                (/= 0 (atoi den)))
           (setq val (/ val (atof den))
                 dec (max dec (flowtotal:places val))))
          ((and (member (substr s i 1) '(" " "-"))                       ; 1 1/2  or  1-1/2
                (/= "" (setq num (flowtotal:digits s (1+ i))))
                (= "/" (substr s (+ i 1 (strlen num)) 1))
                (/= "" (setq den (flowtotal:digits s (+ i 2 (strlen num)))))
                (/= 0 (atoi den)))
           (setq val (+ val (/ (atof num) (atof den)))
                 dec (max dec (flowtotal:places val))))))
      (cons (* (if neg -1 1) val) dec))))

;; rtos without DIMZIN dropping zeros
(defun flowtotal:fmt (v dec / dz r)
  (setq dz (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq r (rtos v 2 dec))
  (setvar "DIMZIN" dz)
  r)

;; Height for new text: the style's fixed height if it has one, else TEXTSIZE;
;; annotative styles are scaled up for model space
(defun flowtotal:height (/ sty h x)
  (setq sty (tblobjname "STYLE" (getvar "TEXTSTYLE"))
        h   (cdr (assoc 40 (entget sty))))
  (if (<= h 0.0) (setq h (getvar "TEXTSIZE")))
  (if (and (setq x (cadr (assoc -3 (entget sty '("AcadAnnotative")))))
           (= 1 (cdr (last (vl-remove-if-not '(lambda (p) (= 1070 (car p))) (cdr x)))))
           (or (= 1 (getvar "TILEMODE")) (/= 1 (getvar "CVPORT"))))
    (setq h (/ h (getvar "CANNOSCALEVALUE"))))
  h)

(defun c:FLOWTOTAL (/ *error* ss i e txt r used total dec cnt skip pt zdir sty)
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
                   "   >>  TOTAL = " (flowtotal:fmt total dec)
                   "  (" (itoa (length used)) " values)")))
  (foreach x used (redraw x 4))
  (if used
    (progn
      (princ (strcat "\nFinal total = " (flowtotal:fmt total dec)))
      (initget "Yes No")
      (if (and (= "Yes" (getkword "\nPlace total as text? [Yes/No] <No>: "))
               (setq pt (getpoint "\nText location: ")))
        (progn
          ;; place it flat in the current UCS, like the TEXT command
          (setq zdir (trans '(0 0 1) 1 0 T)
                sty  (tblsearch "STYLE" (getvar "TEXTSTYLE")))
          (entmake (list '(0 . "TEXT")
                         (cons 10 (trans pt 1 zdir))
                         (cons 40 (flowtotal:height))
                         (cons 1 (flowtotal:fmt total dec))
                         (cons 7 (getvar "TEXTSTYLE"))
                         (cons 41 (cdr (assoc 41 sty)))     ; style width factor
                         (cons 51 (cdr (assoc 50 sty)))     ; style oblique angle
                         (cons 71 (cdr (assoc 71 sty)))     ; backward / upside down
                         (cons 50 (angle '(0 0 0) (trans (getvar "UCSXDIR") 0 zdir T)))
                         (cons 210 zdir)))))))
  (princ))

(princ "\nFLOWTOTAL loaded.")
(princ)
