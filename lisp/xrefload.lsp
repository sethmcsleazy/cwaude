;;; XREFLOAD - Reload every xref in every open drawing
;;; A routine can only run commands in the drawing it was started from, so
;;; XREFLOAD reloads this drawing, then switches to each other open drawing in
;;; turn and runs -XREF Reload * there, and finally switches back here. A small
;;; document reactor sends the reload to each drawing as it comes to the front.

;; Command-line text that switches to the drawing at position idx
(defun xrefload:goto (idx)
  (strcat "(progn (vl-load-com) (vla-Activate (vla-Item (vla-get-Documents (vlax-get-acad-object)) "
          (itoa idx)
          ")) (princ))\n"))

(defun xrefload:stop ()
  (if *xrefload-reactor* (vlr-remove *xrefload-reactor*))
  (setq *xrefload-reactor* nil
        *xrefload-queue*   nil))

;; Runs every time a drawing becomes current
(defun xrefload:onactivate (reactor args / doc hit)
  (setq doc (car args))
  (cond
    ;; the chain stalled (no switch for a minute) or you came back to the
    ;; starting drawing early: stop listening instead of acting later
    ((or (> (- (getvar "MILLISECS") *xrefload-time*) 60000)
         (and *xrefload-queue* (equal doc (cdr *xrefload-home*))))
     (princ (strcat "\nXREFLOAD: stopped before reaching "
                    (apply 'strcat (mapcar '(lambda (x) (strcat (vla-get-Name (cdr x)) " "))
                                           *xrefload-queue*))
                    "- run XREFLOAD again for those.\n"))
     (xrefload:stop)
     (princ))
    ;; a drawing still waiting: reload it there, then move on to the next one
    ((setq hit (vl-some '(lambda (x) (if (equal (cdr x) doc) x)) *xrefload-queue*))
     (setq *xrefload-time*  (getvar "MILLISECS")
           *xrefload-queue* (vl-remove hit *xrefload-queue*)
           *xrefload-count* (1+ *xrefload-count*))
     (vla-SendCommand doc
                      (strcat "_.-XREF _Reload * "
                              (xrefload:goto (if *xrefload-queue*
                                               (car (car *xrefload-queue*))
                                               (car *xrefload-home*))))))
    ;; back in the starting drawing with nothing left to do
    ((and (null *xrefload-queue*) (equal doc (cdr *xrefload-home*)))
     (xrefload:stop)
     (princ (strcat "\nXREFLOAD: xrefs reloaded in " (itoa *xrefload-count*) " drawing(s).\n"))
     (princ))))

(defun c:XREFLOAD (/ *error* acad home i queue oldecho)
  (vl-load-com)
  (setq acad    (vlax-get-acad-object)
        home    (vla-get-ActiveDocument acad)
        oldecho (getvar "CMDECHO")
        i       0)
  (defun *error* (msg)
    (setvar "CMDECHO" oldecho)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (xrefload:stop)
  (vlax-for d (vla-get-Documents acad)
    (if (equal d home)
      (setq *xrefload-home* (cons i d))
      (setq queue (cons (cons i d) queue)))
    (setq i (1+ i)))
  ;; this drawing
  (setvar "CMDECHO" 0)
  (command "_.-XREF" "_Reload" "*")
  (setvar "CMDECHO" oldecho)
  (if queue
    (progn
      (setq *xrefload-queue*   (reverse queue)
            *xrefload-count*   1
            *xrefload-reactor* (vlr-docmanager-reactor
                                 nil
                                 '((:vlr-documentBecameCurrent . xrefload:onactivate))))
      ;; the reactor must fire while the other drawings are in front
      (vlr-set-notification *xrefload-reactor* 'all-documents)
      (setq *xrefload-time* (getvar "MILLISECS"))
      (princ (strcat "\nXrefs reloaded here. Switching through " (itoa (length queue))
                     " other open drawing(s) to reload theirs..."))
      ;; switch after this command has finished
      (vla-SendCommand home (xrefload:goto (car (car *xrefload-queue*)))))
    (princ "\nXrefs reloaded (this is the only open drawing)."))
  (princ))

(princ "\nXREFLOAD loaded.")
(princ)
