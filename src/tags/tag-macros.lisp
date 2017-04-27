
(in-package :it.bese.yaclml)

;;;; ** Helper macro fer defining the tag macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-effective-attributes (attributes)
    (with-collector (attrs)
      (dolist (attr attributes)
        (case attr
          (:core  (attrs 'class 'id 'style 'title))
          (:i18n  (attrs 'dir 'lang))
          (:event (attrs 'onclick 'ondblclick
                         'onkeydown 'onkeypress
                         'onkeyup 'onmousedown
                         'onmousemove 'onmouseout
                         'onmouseover 'onmouseup))
          (t (attrs attr))))
      (attrs))))

(defmacro def-empty-html-tag (name &rest attributes)
  "Define a tag that has `End Tag` set to Forbidden and `Empty`
set to Empty according to:
http://www.w3.org/TR/1999/REC-html401-19991224/index/elements.html
used so generated XHTML would follow guidelines described in
http://www.w3.org/TR/xhtml1/#guidelines"
  (let ((effective-attributes (make-effective-attributes attributes)))
    (with-unique-names (custom-attributes)
      `(deftag ,name (&attribute ,@effective-attributes
                      &allow-custom-attributes ,custom-attributes)
         (emit-empty-tag ,(string-downcase (symbol-name name))
                         (list ,@(iter (for attr :in effective-attributes)
                                       (collect (string-downcase (symbol-name attr)))
                                       (collect attr)))
                         ,custom-attributes)))))

(defmacro def-html-tag (name &rest attributes)
  (let ((effective-attributes (make-effective-attributes attributes)))
    (with-unique-names (custom-attributes)
      `(deftag ,name (&attribute ,@effective-attributes
                      &allow-custom-attributes ,custom-attributes &body body)
         (emit-open-tag ,(string-downcase (symbol-name name))
                        (list ,@(iter (for attr :in effective-attributes)
                                      (collect (string-downcase (symbol-name attr)))
                                      (collect attr)))
                        ,custom-attributes)
         (emit-body body)
         (emit-close-tag ,(string-downcase (symbol-name name)))))))

(defun href (base &rest params)
  (with-output-to-string (href)
    (write-string base href)
    (when params
      (write-char #\? href)
      (loop
	 for (key value . rest) on params by #'cddr
	 do (etypecase key
              (string (write-string key href))
              (symbol (write-string (string-downcase key) href))) 
	 do (write-char #\= href)
	 do (princ value href)
	 when rest
	 do (write-char #\& href)))))
