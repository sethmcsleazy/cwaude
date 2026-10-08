;;; SETLAYOUT - Rename the layout to match the drawing file name
;;; Characters that are not allowed in layout names (< > / \ " : ; ? * | , = `)
;;; are removed. Renames the current layout tab; from the Model tab it renames
;;; the drawing's only layout (if there is more than one, switch to the one you want).

(defun setlayout:clean (str / out c)
  (setq out "")
  (foreach code (vl-string->list str)
    (setq c (chr code))
    (if (not (vl-string-search c "<>/\\\":;?*|,=`"))
      (setq out (strcat out c))))
  (setq out (vl-string-trim " " out))
  (if (> (strlen out) 255) (substr out 1 255) out))

(defun c:SETLAYOUT (/ *error* doc layouts name target paper existing)
  (vl-load-com)
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (setq doc     (vla-get-ActiveDocument (vlax-get-acad-object))
        layouts (vla-get-Layouts doc)
        name    (setlayout:clean (vl-filename-base (getvar "DWGNAME"))))
  (if (= 0 (getvar "DWGTITLED"))
    (princ "\nNote: drawing has not been saved yet, using its temporary name."))
  (cond
    ((= name "")
     (princ "\nFile name has no usable characters for a layout name."))
    ((= (strcase name) "MODEL")
     (princ "\nA layout cannot be named \"Model\"."))
    (T
     (if (/= (strcase (getvar "CTAB")) "MODEL")
       (setq target (vla-Item layouts (getvar "CTAB")))
       (progn
         (vlax-for lay layouts
           (if (= :vlax-false (vla-get-ModelType lay)) (setq paper (cons lay paper))))
         (if (= 1 (length paper)) (setq target (car paper)))))
     (cond
       ((not target)
        (princ "\nMore than one layout - switch to the layout tab you want to rename and run SETLAYOUT again."))
       ((= (vla-get-Name target) name)
        (princ (strcat "\nLayout is already named \"" name "\".")))
       ((and (not (vl-catch-all-error-p (setq existing (vl-catch-all-apply 'vla-Item (list layouts name)))))
             (/= (strcase (vla-get-Name existing)) (strcase (vla-get-Name target))))
        (princ (strcat "\nAnother layout is already named \"" name "\".")))
       (T
        (princ (strcat "\nRenamed layout \"" (vla-get-Name target) "\" to \"" name "\"."))
        (vla-put-Name target name)))))
  (princ))

(princ "\nSETLAYOUT loaded.")
(princ)
