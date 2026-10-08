;;; SETUPALL - Apply a named page setup to every layout in every open drawing
;;; Pick a page setup from the current drawing. It is copied into each open
;;; drawing (created or overwritten there under the same name), applied to all
;;; of that drawing's paper space layouts, and set as each layout's current
;;; page setup (as if you had picked it and clicked "Set Current" in the Page
;;; Setup Manager). Choose "Current" to only update the layouts in this drawing.
;;; Drawings are modified but not saved.

;; Returns the LAYOUT object data with its page setup name changed. The page
;; setup name is the group 1 in the AcDbPlotSettings section; the layout's own
;; name is a different group 1 further down, so it is left alone.
(defun setupall:with-psname (ed name / out sub done)
  (foreach x ed
    (cond
      ((and (not done) (= sub "AcDbPlotSettings") (= (car x) 1))
       (setq out  (cons (cons 1 name) out)
             done T))
      ((and (not done) (= sub "AcDbPlotSettings") (= (car x) 100))
       ;; no page setup name stored yet - add one at the end of the section
       (setq out  (cons x (cons (cons 1 name) out))
             done T
             sub  (cdr x)))
      (T
       (if (= (car x) 100) (setq sub (cdr x)))
       (setq out (cons x out)))))
  (reverse out))

;; Page setup name currently stored on a LAYOUT object's data
(defun setupall:psname (ed / sub res)
  (foreach x ed
    (if (and (not res) (= sub "AcDbPlotSettings") (= (car x) 1))
      (setq res (cdr x)))
    (if (= (car x) 100) (setq sub (cdr x))))
  res)

;; Copy the settings onto the layout and make the page setup its current one.
;; Returns T if the layout now shows the page setup as current.
(defun setupall:setlayout (lay pc name / res)
  (vla-CopyFrom lay pc)
  (setq res (vl-catch-all-apply
              (function
                (lambda (/ e)
                  (setq e (vlax-vla-object->ename lay))
                  (entmod (setupall:with-psname (entget e) name))
                  (= name (setupall:psname (entget e)))))
              nil))
  (and (not (vl-catch-all-error-p res)) res))

;; Returns (layouts-updated layouts-set-current)
(defun setupall:apply (target src name / pcs pc cnt cur res)
  (setq pcs (vla-get-PlotConfigurations target)
        pc  (vl-catch-all-apply 'vla-Item (list pcs name)))
  (if (vl-catch-all-error-p pc)
    (setq pc (vla-Add pcs name :vlax-false)))
  (if (not (equal pc src))
    (vla-CopyFrom pc src))
  (setq cnt 0 cur 0)
  (vlax-for lay (vla-get-Layouts target)
    (if (= :vlax-false (vla-get-ModelType lay))
      (progn
        (setq res (vl-catch-all-apply 'setupall:setlayout (list lay pc name)))
        (if (not (vl-catch-all-error-p res))
          (setq cnt (1+ cnt)))
        (if (= res T)
          (setq cur (1+ cur))))))
  (list cnt cur))

(defun c:SETUPALL (/ *error* acad doc names i num src scope docs res)
  (vl-load-com)
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
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
          (setq src (vla-Item (vla-get-PlotConfigurations doc) (nth (1- num) names)))
          (initget "Current All")
          (setq scope (getkword "\nApply to [Current drawing/All open drawings] <All>: "))
          (if (= scope "Current")
            (setq docs (list doc))
            (vlax-for d (vla-get-Documents acad) (setq docs (cons d docs))))
          (foreach d (reverse docs)
            (setq res (vl-catch-all-apply 'setupall:apply (list d src (vla-get-Name src))))
            (princ (strcat "\n  " (vla-get-Name d) ": "
                           (cond
                             ((vl-catch-all-error-p res)
                              (strcat "FAILED - " (vl-catch-all-error-message res)))
                             ((= (car res) (cadr res))
                              (strcat (itoa (car res)) " layout(s) set to \"" (vla-get-Name src) "\""))
                             (T
                              (strcat (itoa (car res)) " layout(s) updated, but only "
                                      (itoa (cadr res)) " could be set as the current page setup"))))))
          (vla-Regen doc acAllViewports)))))
  (princ))

(princ "\nSETUPALL loaded.")
(princ)
