;;; SETLAYOUT - Rename the layout to match the drawing file name
;;; Characters that are not allowed in layout names (< > / \ " : ; ? * | , = `)
;;; are removed. Renames each drawing's current layout tab; if a drawing is on
;;; the Model tab, its only layout is renamed (if it has more than one, switch
;;; that drawing to the one you want). Choose "Current" for just this drawing
;;; or "All" for every open drawing, each named after its own file.
;;; Drawings are not saved.

(defun setlayout:clean (str / out c)
  (setq out "")
  (foreach code (vl-string->list str)
    (setq c (chr code))
    (if (not (vl-string-search c "<>/\\\":;?*|,=`"))
      (setq out (strcat out c))))
  (setq out (vl-string-trim " " out))
  (if (> (strlen out) 255) (substr out 1 255) out))

;; Rename the layout in one drawing (works on any open drawing through
;; ActiveX). Returns a message describing what happened.
(defun setlayout:rename (doc / layouts name target paper existing old res)
  (setq layouts (vla-get-Layouts doc)
        name    (setlayout:clean (vl-filename-base (vla-get-Name doc))))
  (cond
    ((= "" (vla-get-FullName doc))
     "not saved yet - save it first so the layout can take its file name")
    ((= name "")
     "file name has no usable characters for a layout name")
    ((= (strcase name) "MODEL")
     "a layout cannot be named \"Model\"")
    (T
     (setq target (vla-get-ActiveLayout doc))
     (if (= :vlax-true (vla-get-ModelType target))
       (progn
         (setq target nil)
         (vlax-for lay layouts
           (if (= :vlax-false (vla-get-ModelType lay)) (setq paper (cons lay paper))))
         (if (= 1 (length paper)) (setq target (car paper)))))
     (cond
       ((not target)
        "more than one layout - switch that drawing to the layout tab you want and run SETLAYOUT again")
       ((= (vla-get-Name target) name)
        (strcat "layout is already named \"" name "\""))
       ((and (not (vl-catch-all-error-p (setq existing (vl-catch-all-apply 'vla-Item (list layouts name)))))
             (/= (strcase (vla-get-Name existing)) (strcase (vla-get-Name target))))
        (strcat "another layout is already named \"" name "\""))
       (T
        (setq old (vla-get-Name target))
        (if (vl-catch-all-error-p (setq res (vl-catch-all-apply 'vla-put-Name (list target name))))
          (strcat "could not rename \"" old "\" - " (vl-catch-all-error-message res))
          (strcat "renamed layout \"" old "\" to \"" name "\"")))))))

(defun c:SETLAYOUT (/ *error* acad doc scope)
  (vl-load-com)
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (setq acad (vlax-get-acad-object)
        doc  (vla-get-ActiveDocument acad))
  (initget "Current All")
  (setq scope (getkword "\nRename layouts in [Current drawing/All open drawings] <Current>: "))
  (if (= scope "All")
    (vlax-for d (vla-get-Documents acad)
      (princ (strcat "\n  " (vla-get-Name d) ": " (setlayout:rename d))))
    (princ (strcat "\n" (setlayout:rename doc) ".")))
  (princ))

(princ "\nSETLAYOUT loaded.")
(princ)
