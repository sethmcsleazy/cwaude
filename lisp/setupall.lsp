;;; SETUPALL - Make a named page setup the current page setup on every layout
;;;            in every open drawing
;;; Pick a page setup from the current drawing. It is copied into each open
;;; drawing (created or overwritten there under the same name), then made the
;;; current page setup of every layout. That last step uses -PLOT with "Save
;;; changes to page setup" = Yes and "Proceed with plot" = No, so nothing is
;;; plotted. A routine can only run commands in its own drawing, so SETUPALL
;;; switches through the other open drawings and does it inside each one, then
;;; switches back here (like XREFLOAD). Choose "Current" to only update this
;;; drawing. Drawings are not saved.

;; The per-drawing step, kept as data so it can also be written to a temp file
;; and loaded in the other drawings (each drawing has its own LISP variables).
(setq *setupall-here-src*
  '(defun setupall:here (name / echo n p k)
     (setq echo (getvar "CMDECHO") n 0)
     (setvar "CMDECHO" 1)                    ; prompts must echo for LASTPROMPT
     (foreach lay (layoutlist)
       (command "_.-PLOT" "_N" lay name)
       ;; answer the rest by reading each prompt, since they vary by plotter
       (setq k 0)
       (while (and (> (getvar "CMDACTIVE") 0) (< (setq k (1+ k)) 20))
         (setq p (strcase (getvar "LASTPROMPT")))
         (cond
           ((wcmatch p "*SAVE CHANGES*") (command "_Y"))
           ((wcmatch p "*PROCEED WITH PLOT*") (command "_N"))
           ((wcmatch p "*WRITE THE PLOT TO A FILE*") (command "_N"))
           (T (command ""))))
       (if (> (getvar "CMDACTIVE") 0) (command))
       (setq n (1+ n)))
     (setvar "CMDECHO" echo)
     (princ (strcat "\nSETUPALL: \"" name "\" set current on " (itoa n) " layout(s)."))
     n))
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
  (strcat "(progn (load " (vl-prin1-to-string *setupall-file*) ")"
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
    ((setq hit (vl-some '(lambda (x) (if (equal (cdr x) doc) x)) *setupall-queue*))
     (setq *setupall-queue* (vl-remove hit *setupall-queue*)
           *setupall-count* (1+ *setupall-count*))
     (vla-SendCommand doc (setupall:remote (if *setupall-queue*
                                             (car (car *setupall-queue*))
                                             (car *setupall-home*)))))
    ((and (null *setupall-queue*) (equal doc (cdr *setupall-home*)))
     (setupall:stop)
     (princ (strcat "\nSETUPALL: \"" *setupall-name* "\" set current in "
                    (itoa *setupall-count*) " drawing(s).\n"))
     (princ))))

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
          (setupall:here *setupall-name*)
          (if queue
            (progn
              ;; the other drawings load the per-drawing step from a temp file
              (setq *setupall-file* (vl-filename-mktemp "setupall.lsp")
                    f (open *setupall-file* "w"))
              (write-line (vl-prin1-to-string *setupall-here-src*) f)
              (close f)
              (setq *setupall-queue*   (reverse queue)
                    *setupall-count*   1
                    *setupall-reactor* (vlr-docmanager-reactor
                                         nil
                                         '((:vlr-documentBecameCurrent . setupall:onactivate))))
              (vlr-set-notification *setupall-reactor* 'all-documents)
              (princ (strcat "\nSwitching through " (itoa (length queue))
                             " other open drawing(s) to set the page setup there..."))
              (vla-SendCommand doc (setupall:goto (car (car *setupall-queue*))))))))))
  (princ))

(princ "\nSETUPALL loaded.")
(princ)
