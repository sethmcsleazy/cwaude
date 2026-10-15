;;; SETUPALL - Make a named page setup the current page setup on the first
;;;            layout of every open drawing (other layouts are left alone)
;;; Pick a page setup from the current drawing. It is copied into each open
;;; drawing (created or overwritten there under the same name), then made the
;;; current page setup of that drawing's first layout tab only (falling back
;;; to -PLOT with "Save changes to page setup" = Yes and "Proceed with plot" =
;;; No, so nothing is plotted), and that layout is made current and zoomed to
;;; extents. That only works inside the drawing being changed, so SETUPALL
;;; switches through the other open drawings and does it inside each one, then
;;; switches back here (like XREFLOAD). Choose "Current" to only update this
;;; drawing. Drawings are not saved.

;; The per-drawing step, kept as data so it can also be written to a temp file
;; and loaded in the other drawings (each drawing has its own LISP variables).
;; For the drawing's first layout tab it copies the page setup's settings onto the layout and
;; writes the page setup name into it (what "Set Current" in the Page Setup
;; Manager does). That only works from inside the drawing being changed, which
;; is why it runs here and not from the starting drawing. If the layout still
;; doesn't show the page setup as current, -PLOT is tried (twice), and any
;; layout that still failed is reported (with the prompts AutoCAD showed) on
;; the session blackboard for the final summary.
(setq *setupall-here-src*
  '(progn
     ;; page setup name stored on a LAYOUT object (group 1 of AcDbPlotSettings)
     (defun setupall:psname (lo / sub res)
       (foreach x (entget (vlax-vla-object->ename lo))
         (if (and (not res) (= sub "AcDbPlotSettings") (= (car x) 1)) (setq res (cdr x)))
         (if (= (car x) 100) (setq sub (cdr x))))
       res)

     ;; copy the settings onto the layout and write the page setup name
     (defun setupall:direct (lo pc name / e out sub done)
       (vla-CopyFrom lo pc)
       (setq e (vlax-vla-object->ename lo))
       (foreach x (entget e)
         (cond
           ((and (not done) (= sub "AcDbPlotSettings") (= (car x) 1))
            (setq out (cons (cons 1 name) out) done T))
           ((and (not done) (= sub "AcDbPlotSettings") (= (car x) 100))
            ;; no name stored yet - add one at the end of the section
            (setq out (cons x (cons (cons 1 name) out)) done T sub (cdr x)))
           (T
            (if (= (car x) 100) (setq sub (cdr x)))
            (setq out (cons x out)))))
       (entmod (reverse out)))

     (defun setupall:here (name / doc echo n bad lays pc lo try seen p k cur first)
       (setq doc  (vla-get-ActiveDocument (vlax-get-acad-object))
             echo (getvar "CMDECHO")
             n    0
             lays (vla-get-Layouts doc)
             pc   (vl-catch-all-apply 'vla-Item (list (vla-get-PlotConfigurations doc) name)))
       ;; only the first layout tab (lowest tab order); the others are left alone
       (vlax-for l lays
         (if (and (= :vlax-false (vla-get-ModelType l))
                  (or (not first) (< (vla-get-TabOrder l) (vla-get-TabOrder first))))
           (setq first l)))
       (foreach lay (if first (list (vla-get-Name first)))
         (setq lo (vla-Item lays lay) seen nil try 0)
         (if (not (vl-catch-all-error-p pc))
           (vl-catch-all-apply 'setupall:direct (list lo pc name)))
         (setq cur (setupall:psname lo))
         ;; fallback: -PLOT with "Save changes to page setup" = Yes, nothing plotted
         (while (and (< try 2) (/= cur name))
           (setq try (1+ try) seen nil k 0)
           (setvar "CMDECHO" 1)                ; prompts must echo for LASTPROMPT
           (command "_.-PLOT" "_N" lay name)
           ;; answer the rest by reading each prompt, since they vary by plotter
           (while (and (> (getvar "CMDACTIVE") 0) (< (setq k (1+ k)) 20))
             (setq p    (strcase (getvar "LASTPROMPT"))
                   seen (cons (getvar "LASTPROMPT") seen))
             (cond
               ((wcmatch p "*SAVE CHANGES*") (command "_Y"))
               ((wcmatch p "*PROCEED WITH PLOT*") (command "_N"))
               ((wcmatch p "*WRITE THE PLOT TO A FILE*") (command "_N"))
               ;; second try: any other Yes/No question (e.g. after a warning) gets Yes
               ((and (= try 2) (wcmatch p "*`[YES/NO`]*")) (command "_Y"))
               (T (command ""))))
           (if (> (getvar "CMDACTIVE") 0) (command))
           (setvar "CMDECHO" echo)
           (setq cur (setupall:psname lo)))
         (if (= cur name)
           (setq n (1+ n))
           (setq bad (cons (strcat (getvar "DWGNAME") " / " lay
                                   (if seen
                                     (strcat " - last prompts: "
                                             (apply 'strcat (mapcar '(lambda (s) (strcat "[" s "] "))
                                                                    (reverse seen))))
                                     (if (vl-catch-all-error-p pc)
                                       " - the page setup was not copied into this drawing"
                                       "")))
                           bad))))
       ;; leave the drawing on its first layout, zoomed to extents in paper space
       (if first
         (progn
           (setvar "CTAB" (vla-get-Name first))
           (if (/= 1 (getvar "CVPORT")) (command "_.PSPACE"))
           (command "_.ZOOM" "_E")))
       (if bad (vl-bb-set '*setupall-fail* (append (vl-bb-ref '*setupall-fail*) (reverse bad))))
       (vla-Regen doc 1)                      ; all viewports
       (princ (strcat "\nSETUPALL: "
                      (cond ((not first) "no layouts in this drawing.")
                            ((= n 1) (strcat "\"" name "\" set current on layout \"" (vla-get-Name first) "\"."))
                            (T (strcat "could not set \"" name "\" current on layout \"" (vla-get-Name first) "\".")))))
       n)))
(eval *setupall-here-src*)

;; Copy the page setup into a drawing (ActiveX works across drawings)
(defun setupall:copyps (target src name / pcs pc)
  (setq pcs (vla-get-PlotConfigurations target)
        pc  (vl-catch-all-apply 'vla-Item (list pcs name)))
  (if (vl-catch-all-error-p pc)
    (setq pc (vla-Add pcs name :vlax-false)))
  (if (not (equal pc src))
    (vla-CopyFrom pc src))
  T)

;; Command-line text that runs the step in the drawing it is sent to, then
;; switches to the drawing at position idx
(defun setupall:remote (idx)
  (strcat "(progn (vl-load-com) (load " (vl-prin1-to-string *setupall-file*) ")"
          " (setupall:here " (vl-prin1-to-string *setupall-name*) ")"
          " (vl-load-com) (vla-Activate (vla-Item (vla-get-Documents (vlax-get-acad-object)) "
          (itoa idx) ")) (princ))\n"))

(defun setupall:goto (idx)
  (strcat "(progn (vl-load-com) (vla-Activate (vla-Item (vla-get-Documents (vlax-get-acad-object)) "
          (itoa idx) ")) (princ))\n"))

(defun setupall:stop ()
  (if *setupall-reactor* (vlr-remove *setupall-reactor*))
  (setq *setupall-reactor* nil
        *setupall-queue*   nil))

;; Runs every time a drawing becomes current
(defun setupall:onactivate (reactor args / doc hit)
  (setq doc (car args))
  (cond
    ;; the chain stalled (no switch for a minute) or you came back to the
    ;; starting drawing early: stop listening instead of acting later
    ((or (> (- (getvar "MILLISECS") *setupall-time*) 60000)
         (and *setupall-queue* (equal doc (cdr *setupall-home*))))
     (princ (strcat "\nSETUPALL: stopped before reaching "
                    (apply 'strcat (mapcar '(lambda (x) (strcat (vla-get-Name (cdr x)) " "))
                                           *setupall-queue*))
                    "- run SETUPALL again for those.\n"))
     (setupall:stop)
     (princ))
    ((setq hit (vl-some '(lambda (x) (if (equal (cdr x) doc) x)) *setupall-queue*))
     (setq *setupall-time*  (getvar "MILLISECS")
           *setupall-queue* (vl-remove hit *setupall-queue*)
           *setupall-count* (1+ *setupall-count*))
     (vla-SendCommand doc (setupall:remote (if *setupall-queue*
                                             (car (car *setupall-queue*))
                                             (car *setupall-home*)))))
    ((and (null *setupall-queue*) (equal doc (cdr *setupall-home*)))
     (setupall:stop)
     (setupall:summary))))

;; Final report, including any layouts that did not take the page setup
(defun setupall:summary (/ fails)
  (setq fails (vl-bb-ref '*setupall-fail*))
  (princ (strcat "\nSETUPALL: \"" *setupall-name* "\" processed in "
                 (itoa *setupall-count*) " drawing(s)"
                 (if fails (strcat ", " (itoa (length fails)) " layout(s) FAILED:") ".")))
  (foreach f fails (princ (strcat "\n  " f)))
  (princ "\n")
  (princ))

(defun c:SETUPALL (/ *error* acad doc names i num src scope queue f res)
  (vl-load-com)
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (setupall:stop)
  (setq acad (vlax-get-acad-object)
        doc  (vla-get-ActiveDocument acad))
  (vlax-for pc (vla-get-PlotConfigurations doc)
    (if (= :vlax-false (vla-get-ModelType pc))
      (setq names (cons (vla-get-Name pc) names))))
  (if (not names)
    (princ "\nNo layout page setups in this drawing. Create one with PAGESETUP or import one with PSETUPIN.")
    (progn
      (setq names (acad_strlsort names) i 0)
      (princ "\nPage setups:")
      (foreach nm names
        (princ (strcat "\n  " (itoa (setq i (1+ i))) ". " nm)))
      (initget 6)
      (setq num (getint "\nPage setup number <1>: "))
      (if (not num) (setq num 1))
      (if (> num (length names))
        (princ "\nInvalid number.")
        (progn
          (setq src (vla-Item (vla-get-PlotConfigurations doc) (nth (1- num) names))
                *setupall-name* (vla-get-Name src))
          (initget "Current All")
          (setq scope (getkword "\nApply to [Current drawing/All open drawings] <All>: "))
          ;; copy the page setup into the other open drawings, noting their positions
          (setq i 0)
          (vlax-for d (vla-get-Documents acad)
            (if (equal d doc)
              (setq *setupall-home* (cons i d))
              (if (/= scope "Current")
                (if (vl-catch-all-error-p
                      (setq res (vl-catch-all-apply 'setupall:copyps (list d src *setupall-name*))))
                  (princ (strcat "\n  " (vla-get-Name d) ": could not copy the page setup - "
                                 (vl-catch-all-error-message res)))
                  (setq queue (cons (cons i d) queue)))))
            (setq i (1+ i)))
          ;; this drawing
          (vl-bb-set '*setupall-fail* nil)
          (setq *setupall-count* 1)
          (setupall:here *setupall-name*)
          (if (not queue) (setupall:summary))
          (if queue
            (progn
              ;; the other drawings load the per-drawing step from a temp file
              (setq *setupall-file* (vl-filename-mktemp "setupall.lsp")
                    f (open *setupall-file* "w"))
              (write-line (vl-prin1-to-string *setupall-here-src*) f)
              (close f)
              (setq *setupall-queue*   (reverse queue)
                    *setupall-reactor* (vlr-docmanager-reactor
                                         nil
                                         '((:vlr-documentBecameCurrent . setupall:onactivate))))
              (vlr-set-notification *setupall-reactor* 'all-documents)
              (setq *setupall-time* (getvar "MILLISECS"))
              (princ (strcat "\nSwitching through " (itoa (length queue))
                             " other open drawing(s) to set the page setup there..."))
              (vla-SendCommand doc (setupall:goto (car (car *setupall-queue*))))))))))
  (princ))

(princ "\nSETUPALL loaded.")
(princ)
