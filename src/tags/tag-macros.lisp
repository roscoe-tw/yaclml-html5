
(in-package :it.bese.yaclml)

;;;; ** Helper macro fer defining the tag macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-effective-attributes (attributes)
    (with-collector (attrs)
      (dolist (attr attributes)
        (case attr
	  ;; 保留原設定搭配非 html5
          (:core  (attrs 'class 'id 'style 'title))
	  (:i18n  (attrs 'dir 'lang))
	  ;; 增加 global ，包含原設計, html5
	  ;;'data-* 未加入，因為它似乎是自定義屬性，目前未有方法使用它
	  ;; Used to store custom data private to the page or application
	  (:global (attrs 'accesskey 'class 'contenteditable
			  'contextmenu ;; 'data-*
			  'dir 'draggable 'dropzone 'hidden
			  'id 'lang 'spellcheck 'style
			  ;;'tabindex
			  'title 'translate))
          (:event (attrs 'onclick 'ondblclick
                         'onkeydown 'onkeypress
                         'onkeyup 'onmousedown
                         'onmousemove 'onmouseout
                         'onmouseover 'onmouseup
			 ;; 新增
			 'onafterprint 'onbeforeprint
			 'onbeforeuload 'onerror
			 'onhashchange
			 ;; 'onload
			 'onmessage 'onoffline
			 'ononline 'onpagehide
			 'onpageshow 'onpopstate
			 'onresize 'onstorge
			 ;; 'onunload
			 ))
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

(defmacro def-media-html-tag (name &rest attributes)
  (let ((effective-attributes (make-effective-attributes attributes)))
    (with-unique-names (custom-attributes)
      `(deftag ,name (&attribute ,@effective-attributes
				 &allow-custom-attributes ,custom-attributes &body body)
	 (incf %yaclml-indentation-depth% 2)
	 (emit-princ ,(strcat "<" (string-downcase (symbol-name name))))
	 (mapc #'emit-princ-attributes
	       (list (list ,@(iter (for attr :in effective-attributes)
				   (unless (equal (string-downcase (symbol-name attr)) "controls")
				     (collect (string-downcase (symbol-name attr)))
				     (collect attr))))))
	 ;; 屬性 :controls
	 (emit-princ " ")
	 (emit-princ "controls")
	 (cond ((listp controls)
		(emit-princ-attributes (list* controls)))
	       (t
		(emit-princ " ")
		(emit-princ controls)))

	 (emit-indentation)
	 (emit-princ ">")
	 (emit-body body)
	 (decf %yaclml-indentation-depth% 2)
	 (emit-princ "</" ,(string-downcase (symbol-name name)))
	 (emit-indentation)
	 (emit-princ ">")))))

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
