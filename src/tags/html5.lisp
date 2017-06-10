;; -*- lisp -*-

(in-package :it.bese.yaclml)

;;;; * YACLML tags mapping to HTML4 tags.

(defparameter +xhtml-strict-doctype+
  "\"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"")
(defparameter +xhtml-transitional-doctype+
  "\"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/transitional.dtd\"")
(defparameter +xhtml-frameset-doctype+
  "\"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\"")


;;;; * All HTML5 tags

;;;; This list taken from https://www.w3schools.com/tags/
(deftag <:!-- (&body contents)
  (emit-princ "<!-- ")
  (emit-body contents)
  (emit-princ (strcat " -->" ~%)))


(deftag <:!DOCTYPE (&attribute doctype)
  (if doctype
      (progn
	(awhen doctype
	  (emit-princ "<!DOCTYPE html PUBLIC ")
	  (emit-princ it)))
      (progn
	(emit-princ "<!DOCTYPE html")))
  (emit-princ (strcat ">" ~%)))

      
(def-html-tag <:a :global :event
              charset
              coords
	      download
              href
              hreflang
	      media
              name
              rel
              rev
              shape
              target
              type)

(def-html-tag <:abbr :core :event :i18n)

;; Not supported in HTML5. Use <abbr> instead.
;; Defines an acronym
(def-html-tag <:acronym :core :event :i18n)

(def-html-tag <:address :global :event)

;; Not supported in HTML5. Use <embed> or <object> instead.
;; Defines an embedded applet
;; (def-html-tag <:applet :core :event :i18n
;; 	      code
;; 	      object
;; 	      align
;; 	      alt
;; 	      archive
;; 	      codebase
;; 	      height
;; 	      hspace
;; 	      name
;; 	      vspace
;; 	      width)

(def-empty-html-tag <:area :global :event
                    alt
                    coords
		    download
                    href
		    hreflang
		    media
                    nohref
		    rel
                    shape
		    target
                    type)

(def-html-tag <:article :global :event)

(def-html-tag <:aside :global :event)

(def-html-tag <:audio :global :event
	      autoplay
	      controls
	      loop
	      muted
	      preload
	      src)
  
(def-html-tag <:b :global :event)

(def-empty-html-tag <:base :global
                    href
                    target)

;; The <basefont> tag is not supported in HTML5.
;; (def-html-tag <:basefont color face size)

(def-html-tag <:bdi :global :event)

(def-html-tag <:bdo :global :event)
	      ;; :global 已有 'dir 屬性
	      ;; dir


;; HTML <big> Tag. Not Supported in HTML5.
(def-html-tag <:big :core :event :i18n)

(def-html-tag <:blockquote :global :event
              cite)

(def-html-tag <:body :global :event
	      alink
	      background
	      bgcolor
	      link
	      text
	      vlink)

(def-empty-html-tag <:br :global :event)

(def-html-tag <:button :global :event
	      autofocus
	      disabled
	      form
	      formaction
	      formenctype
	      formmethod
	      formnovalidate
	      formtarget
	      name
	      type
	      value)

(def-html-tag <:canvas :global :event
	      height
	      width)

(def-html-tag <:caption :global :event
	      align)

;; HTML <center> Tag. Not Supported in HTML5.

(def-html-tag <:cite :global :event)

(def-html-tag <:code :global :event)

(def-empty-html-tag <:col :global :event
                    align
                    char
                    charoff
                    span
                    valign
                    width)

(def-html-tag <:colgroup :global :event
              align
              char
              charoff
              span
              valign
              width)

(def-html-tag <:data :global
  value)

(def-html-tag <:datalist :global :event)

(def-html-tag <:dd :global :event)

(def-html-tag <:del :global :event
              cite
              datetime)

(def-html-tag <:details :global :event
	      open)

(def-html-tag <:dfn :global :event)

(def-html-tag <:dialog :global :event
	      open)

;; HTML <dir> Tag. Not Supported in HTML5.

(def-html-tag <:div :global :event
	      align)

(def-html-tag <:dl :global :event)

(def-html-tag <:dt :global :event)
;; -*- lisp -*-

;; (in-package :it.bese.yaclml)

(def-html-tag <:em :global :event)

(def-html-tag <:embed :global :event
	      height
	      src
	      type
	      width)
   
(def-html-tag <:fieldset :global :event
	      disabled
	      form
	      name)

(def-html-tag <:figcaption :global :event)

(def-html-tag <:figure :global :event)

;; The <font> tag is not supported in HTML5. Use CSS instead.

(def-html-tag <:footer :global :event)

(def-html-tag <:form :global :event
              action
              accept-charset
	      autocomplete
              enctype
              method
              name
	      novalidate
              target)

;; The <frame> tag is not supported in HTML5.
(def-empty-html-tag <:frame :core
                    frameborder
                    longdesc
                    marginheight
                    marginwidth
		    name
                    noresize
                    scrolling
                    src)

;; HTML <frameset> Tag. Not Supported in HTML5.
(def-html-tag <:frameset :core
              cols
              ;;onload
              olunload
              rows)

(def-html-tag <:h1 :global :event
	      align)

(def-html-tag <:h2 :global :event
	      align)

(def-html-tag <:h3 :global :event
	      align)

(def-html-tag <:h4 :global :event
	      align)

(def-html-tag <:h5 :global :event
	      align)

(def-html-tag <:h6 :global :event
	      align)

(def-html-tag <:head :global
	      profile)

(def-html-tag <:header :global :event)
	      
(def-empty-html-tag <:hr :global :event
		    align
		    noshade
		    size
		    width)

(defmacro def-tag-html (&rest attributes)
  "設定 <:html 其中 attributes 搭配 :global, manifest xmlns... 設定"
  (let ((effective-attributes (make-effective-attributes attributes)))
    (with-unique-names (custom-attributes)
      `(deftag <:html (&attribute ,@effective-attributes
				  ;; (&attribute dir lang prologue doctype
				  &allow-custom-attributes ,custom-attributes
				  &body body)
	
	 (assert (or (and (not prologue)
			  (not doctype))
		     (xor prologue doctype)) () "You can only specify one of PROLOGUE or DOCTYPE")
	 ;; 如果傳來 "html" 預設為 html5 2017.05.02 L.S.K.
	 (when doctype
		  (emit-code `(awhen ,doctype
				(princ "<!DOCTYPE html" *yaclml-stream*)
				(unless (or (equal (string-downcase it) "html")
					    (equal it t)
					    (equal it "")
					    (equal (string-downcase it) "doctype"))
				  (princ " PUBLIC " *yaclml-stream*)
				  (princ it *yaclml-stream*))
				(princ (strcat ">" ~%) *yaclml-stream*))))
	 (when prologue
	   (emit-code `(awhen ,prologue
			 (princ it *yaclml-stream*))))
	 (emit-open-tag "html"
			(list ,@(iter (for attr :in (make-effective-attributes (list :global)))
				      (collect (string-downcase (symbol-name attr)))
				      (collect attr)))
			,custom-attributes)
	 (emit-body body)
	 (emit-close-tag "html")))))

;; 設定 <:html, :doctype "html" => <!DOCTYPE html>
(def-tag-html :global manifest xmlns prologue doctype)

(def-html-tag <:i :global :event)

(def-html-tag <:iframe :global :event
	      align
	      frameborder
	      height
	      longdesc
	      marginheight
	      marginwidth
	      name
	      sandbox
	      scrolling
	      src
	      srcdoc
	      width)

(def-empty-html-tag <:img :global :event
		    align
		    alt
		    border
		    crossorigin
		    height
		    hspace
		    ismap
		    longdesc
		    sizes
		    src
		    srcset
		    usemap
		    vspace
		    width)

(def-empty-html-tag <:input :global :event
		    accept
		    align
		    alt
		    autocomplete
		    autofocus
		    checked
		    dirname
		    disabled
		    form
		    formaction
		    formenctype
		    formmethod
		    formnovalidate
		    formtarget
		    height
		    list
		    max
		    maxlength
		    min
		    multiple
		    name
		    pattern
		    placeholder
		    readonly
		    required
		    size
		    src
		    step
		    type
		    value
		    width)

(def-html-tag <:ins :global :event
              cite
              datetime)

(def-html-tag <:kbd :global :event)

(def-html-tag <:keygen :global :event
	      autofocus
	      challenge
	      disabled
	      form
	      keytype
	      name)
;; -*- lisp -*-

;; (in-package :it.bese.yaclml)
	      
(def-html-tag <:label :global :event
	      for
	      form)

(def-html-tag <:legend :global :event
	      align)

(def-html-tag <:li :global :event
	      type
	      value)

(def-empty-html-tag <:link :global :event
		    charset
		    crossorigin
		    href
		    hreflang
		    media
		    rel
		    rev
		    sizes
		    target
		    type)

(def-html-tag <:main :global :event)
	      
(def-html-tag <:map :global :event
	      name)

(def-html-tag <:make :global :event)

;; Note: The <menu> tag is only supported in Firefox, and it only works for context menus.
(def-html-tag <:menu :global :event
	      label
	      type)

(def-html-tag <:menuitem :global :event
	      checked
	      command
	      default
	      disabled
	      icon
	      label
	      radiogroup
	      type)
	      
(def-empty-html-tag <:meta :global
                    charset
                    content
                    http-equiv
                    name
                    scheme)

(def-html-tag <:meter :global :event
	      form
	      high
	      low
	      max
	      min
	      optimum
	      value)

(def-html-tag <:nav :global :event)

;; The <noframes> tag is not supported in HTML5.
(def-html-tag <:noframes :core :event :i18n)

(def-html-tag <:noscript :global)

(def-html-tag <:object :global :event
	      align
	      archive
	      border
	      classid
	      codebase
	      codetype
	      data
	      declare
	      form
	      height
	      hspace
	      name
	      standby
	      type
	      usemap
	      vspace
	      width)


(def-html-tag <:ol :global :event
	      compact
	      reversed
	      start
	      type)

(def-html-tag <:optgroup :global :event
              disabled
	      label)

(def-html-tag <:option :global :event
              disabled
              label
              selected
              value)

(def-html-tag <:output :global :event
	      for
	      form
	      name)

(def-html-tag <:p :global :event
	      align)

;; 原來的 html+ 也有設定，要確認用哪一個 (2017.05.01) todo
(def-empty-html-tag <:param :global :event
                    name
                    type
                    value
                    valuetype)

(def-html-tag <:picture :global :event)

(def-html-tag <:pre :global :event
	      width)

(def-html-tag <:progress :global :event
	      max
	      value)

(def-html-tag <:q :global :event
              cite)

(def-html-tag <:rp :global :event)

(def-html-tag <:rt :global :event)

(def-html-tag <:ruby :global :event)

(def-html-tag <:s :global :event)

(def-html-tag <:samp :global :event)

(def-html-tag <:script :global
              async
	      charset
	      defer
	      src
	      type
	      xml:space)

(def-html-tag <:section :global :event)

(def-html-tag <:select :global :event
	      autofocus
              disabled
	      form
              multiple
              name
	      required
              size)

(def-html-tag <:small :global :event)

(def-empty-html-tag <:source :global :event
		    src
		    srcset
		    media
		    sizes
		    type)

(def-html-tag <:span :global :event)

;; The <strike> tag is not supported in HTML5. Use <del> or <s> instead.

(def-html-tag <:strong :global :event)

(def-html-tag <:style :global :event
              media
	      scoped
	      type)

(def-html-tag <:sub :global :event)

(def-html-tag <:summary :global :event)

(def-html-tag <:sup :global :event)
;; -*- lisp -*-

;; (in-package :it.bese.yaclml)

(def-html-tag <:table :global :event
	      align
	      bgcolor
	      border
	      cellpadding
	      cellspacing
	      frame
	      rules
	      sortable
	      summary
	      width)


(def-html-tag <:tbody :global :event
              align
              char
              charoff
              valign)

(def-html-tag <:td :global :event
              abbr
              align
              axis
	      bgcolor
              char
              charoff
              colspan
              headers
	      height
	      nowrap
              rowspan
              scope
              valign
              width)

(def-html-tag <:textarea :global :event
	      autofocus
	      cols
	      dirname
	      disabled
	      form
	      maxlength
	      name
	      placeholder
	      readonly
	      required
	      rows
	      wrap)

(def-html-tag <:tfoot :global :event
	      align
	      char
	      charoff
	      valign)

(def-html-tag <:th :global :event
              abbr
              align
              axis
	      bgcolor
              char
              charoff
              colspan
              headers
	      height
	      nowrap
              rowspan
              scope
	      sorted
              valign
	      width)

(def-html-tag <:thead :global :event
              align
              char
              charoff
              valign)

(def-html-tag <:time :global :event
	      datetime)

(def-html-tag <:title :global)

(def-html-tag <:tr :global :event
              align
	      bgcolor
              char
              charoff
              valign)

(def-html-tag <:track :global :event
	      default
	      kind
	      label
	      src
	      srclang)

;; The <tt> tag is not supported in HTML5.
(def-html-tag <:tt :core :event :i18n)

(def-html-tag <:u :global :event)

(def-html-tag <:ul :global :event
	      compact
	      type)

(def-html-tag <:var :global :event)

(def-html-tag <:video :global :event
	      autoplay
	      controls
	      height
	      loop
	      muted
	      poster
	      preload
	      src
	      width)

(def-html-tag <:wbr :global :event)

;; 使用上方設定的 <:embed
;; (deftag <:embed (&allow-other-attributes others)
;;   (emit-empty-tag "embed" others))

;; Copyright (c) 2002-2005, Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
