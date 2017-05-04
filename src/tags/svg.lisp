;; -*- lisp -*-

(in-package :it.bese.yaclml.svg)

;;;; * YACLML tags mapping to SVG tags.

;; TODO this file badly needs factoring, see def-html-tag for examples to factor out attribute groups
;; TODO find out something for the xml:foo and xlink:foo tags

;; ** Helper macros and function for defining the tag macros.

(defmacro def-svg-tag (name &rest attributes)
  ;; The tag names `symbol' and `use' are reserved for the cl core. Their names are prepended by 'svg-'.
  (case name
    (set (setf name 'svg-set))
    (symbol (setf name 'svg-symbol)))
  (let ((effective-attributes attributes)
        (tag-name (if (stringp name)
                      name
                      (string-downcase (symbol-name name)))))
    (labels ((gen-attr-var-name (attr)
               (if (stringp attr)
                   (intern (string-upcase (hyphenize attr)))
                   attr))
             (hyphenize (str)
               (coerce (iter (for c :in-vector str)
                             (if (upper-case-p c)
                                 (progn
                                   (collect #\-)
                                   (collect (char-downcase c)))
                                 (collect c)))
                       'string)))
      (let ((tag-symbol (intern (string-upcase (hyphenize tag-name)))))
        `(progn
          (export ',tag-symbol)
          (deftag ,tag-symbol
              (&attribute ,@(mapcar #'gen-attr-var-name effective-attributes)
                          &allow-custom-attributes custom-attributes
                          &body body)
            (emit-open-tag ,tag-name
                           (list* ,@(iter (for attr :in effective-attributes)
                                          (collect (if (stringp attr)
                                                       attr
                                                       (string-downcase (symbol-name attr))))
                                          (collect (gen-attr-var-name attr)))
                                  custom-attributes))
            (emit-body body)
            (emit-close-tag ,tag-name)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun concat-symbol (&rest args)
   "Concatenate symbols or strings to form an interned symbol"
   (intern (format nil "~{~a~}" args))))

(def-svg-tag altGlyph
             id
             xml:base
             xml:lang
             xml:space
             fill
             fill-rule
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-with
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourceRequired
             x
             y
             dx
             dy
             glyphRef
             format
             rotate)

(def-svg-tag altGlyphDef
             id
             xml:base
             xml:lang
             xml:space)

(def-svg-tag altGlyphItem
             id
             xml:base
             xml:lang
             xml:space)

(def-svg-tag a
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filter
             cursor
             flood-color
             flood-opacity
             lighting-color
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourceRequired
             transform
             target)

(def-svg-tag animate
             requiredFeatures
             requiredExtensions
             systemLanguage
             attributeName
             attributeType
             begin
             dur
             end
             min
             max
             restart
             repeatCount
             repeatDur
             fill
             calcMode
             values
             keyTimes
             keySplines
             from
             to
             by
             additive
             accumulate)

(def-svg-tag animateMotion
             id
             xml:base
             xml:lang
             xml:space
             onbegin
             onend
             onrepeat
             onload
             externalResourcesRequired
             xlink:type
             xlink:href
             xlink:role
             xlink:arcrole
             xlink:title
             xlink:show
             xlink:actuate
             calcMode
             values
             keyTimes
             keySplines
             from
             to
             by
             path
             keyPoints
             rotate
             origin)
             
(def-svg-tag animateColor
             requiredFeatures
             requiredExtensions
             systemLanguage
             attributeName
             attributeType
             begin
             dur
             end
             min
             max
             restart
             repeatCount
             repeatDur
             fill
             calcMode
             values
             keyTimes
             keySplines
             from
             to
             by
             additive
             accumulate)

(def-svg-tag animateTransform
             id
             xml:base
             xml:lang
             xml:space
             onbegin
             onend
             onrepeat
             onload
             externalResourcesRequired
             xlink:type
             xlink:href
             xlink:role
             xlink:arcrole
             xlink:title
             xlink:show
             xlink:actuate
	     ;; 因為和 xml:type 取 (symbol-name 'xlink-type) -> "type" 所以先取消它
             ;; type
	     )

(def-svg-tag circle
             id
             xml:base
             xml:lang
             xml:space
             fill
             fill-rule
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-width
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             cx
             cy
             r
             transform) 

(def-svg-tag color-profile
             id
             xml:base
             xml:lang
             xml:space
             local
             name
             rendering-intent)

(def-svg-tag clipPath
             id
             xml:base
             xml:lang
             xml:space
             fill
             fill-rule
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-width
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             externalResourcesRequired
             transform
             clipPathUnits)

(def-svg-tag cursor
             id
             xml:base
             xml:lang
             xml:space
             externalResourcesRequired
             x
             y)

(def-svg-tag defs
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             transform)

(def-svg-tag desc
             svg
             g
             defs
             symbol
             use
             switch
             image
             path
             rect
             circle
             line
             ellipse
             polyline
             polygon
             text
             tspan
             tref
             textPath
             marker
             color-profile
             linearGradient
             radialGradient
             pattern
             clipPath
             mask
             filter
             cursor
             a
             view
             animate
             set
             animateMotion
             animateColor
             animateTransform
             mpath
             font
             font-face
             glyph
             missing-glyph)
             

(def-svg-tag definition-src
             xlink:type
             xlink:href
             xlink:role
             xlink:arcrole
             xlink:title
             xlink:show
             xlink:actuate)

(def-svg-tag ellipse
             id
             xml:base
             xml:lang
             xml:space
             fill
             fill-rule
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-width
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             cx
             cy
             rx
             ry
             transform)

(def-svg-tag filter
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             externalResourcesRequired
             x
             y
             width
             height
             filterRes
             filterUnits
             primitiveUnits)
             
(def-svg-tag feBlend
             id
             xml:base
             xml:lang
             xml:space
             x
             y
             width
             height
             result
             in
             in2
             mode)

(def-svg-tag feColorMatrix
             id
             xml:base
             xml:lang
             xml:space
             x
             y
             width
             height
             result
             in
             type
             values)

(def-svg-tag feComponentTransfer
             color-interpolation-filters
             in)

(def-svg-tag feComposite
             id
             xml:base
             xml:lang
             xml:space
             x
             y
             width
             height
             result
             in
             in2
             operator
             k1
             k2
             k3
             k4)

(def-svg-tag feConvolveMatrix
             id
             xml:base
             xml:lang
             xml:space
             x
             y
             width
             height
             result
             in
             order
             kernelMatrix
             divisor
             bias
             targetX
             targetY
             edgeMode
             kernelUnitLength
             preserveAlpha)

(def-svg-tag feDiffuseLighting
             id
             xml:base
             xml:lang
             xml:space
             color
             color-interpolation
             color-rendering
             x
             y
             width
             height
             result
             in
             lighting-color
             surfaceScale
             diffuseConstant
             kernelUnitLength)

(def-svg-tag feDisplacementMap
             id
             xml:base
             xml:lang
             xml:space
             x
             y
             width
             height
             result
             in
             in2
             scale
             xChannelSelector
             yChannelSelector)

(def-svg-tag feFlood
             id
             xml:base
             xml:lang
             xml:space
             color
             color-interpolation
             color-rendering
             x
             y
             width
             height
             result
             in
             flood-color
             flood-opacity)

(def-svg-tag feGaussianBlur
             id
             xml:base
             xml:lang
             xml:space
             x
             y
             width
             height
             result
             in
             stdDeviation)

(def-svg-tag feImage
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             externalResourcesRequired
             preserveAspectRatio)

(def-svg-tag feMerge
             color-interpolation-filters
             x
             y
             width
             height
             result)

(def-svg-tag feMergeNode
             id
             xml:base
             xml:lang
             xml:space
             in)

(def-svg-tag feMorphology
             id
             xml:base
             xml:lang
             xml:space
             x
             y
             width
             height
             result
             in
             operator
             radius)

(def-svg-tag feOffset
             id
             xml:base
             xml:lang
             xml:space
             x
             y
             width
             height
             result
             in
             dx
             dy)

(def-svg-tag feSpecularLighting
             id
             xml:base
             xml:lang
             xml:space
             color
             color-interpolation
             color-rendering
             x
             y
             width
             height
             result
             in
             lighting-color
             surfaceScale
             specularConstant
             specularExponent
             kernelUnitLength)

(def-svg-tag feTile
             color-interpolation-filters
             in)

(def-svg-tag feTurbulence
             id
             xml:base
             xml:lang
             xml:space
             baseFrequency
             numOctaves
             seed
             stitchTiles
             type)

(def-svg-tag feDistantLight
             id
             xml:base
             xml:lang
             xml:space
             azimuth
             elevation)

(def-svg-tag fePointLight
             id
             xml:base
             xml:lang
             xml:space
             x
             y
             z)

(def-svg-tag feSpotLight
             id
             xml:base
             xml:lang
             xml:space
             x
             y
             z
             pointsAtX
             pointsAtY
             pointsAtZ
             specularExponent
             limitingConeAngle)

(def-svg-tag feFuncR
             id
             xml:base
             xml:lang
             xml:space
             type
             tableValues
             slope
             intercept
             amplitude
             exponent
             offset)

(def-svg-tag feFuncG
             id
             xml:base
             xml:lang
             xml:space
             type
             tableValues
             slope
             intercept
             amplitude
             exponent
             offset)

(def-svg-tag feFuncB
             id
             xml:base
             xml:lang
             xml:space
             type
             tableValues
             slope
             intercept
             amplitude
             exponent
             offset)

(def-svg-tag feFuncA
             id
             xml:base
             xml:lang
             xml:space
             type
             tableValues
             slope
             intercept
             amplitude
             exponent
             offset)

(def-svg-tag font
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             externalResourcesRequired
             horiz-origin-x
             horiz-origin-y
             horiz-adv-x
             vert-origin-x
             vert-origin-y
             vert-adv-y)

(def-svg-tag font-face
             id
             xml:base
             xml:lang
             xml:space
             font-family
             font-style
             font-variant
             font-weight
             font-stretch
             font-size
             unicode-range
             units-per-em
             panose-1
             stemv
             stemh
             slope
             cap-height
             x-height
             accent-height
             ascent
             descent
             widths
             bbox
             ideographic
             alphabetic
             mathematical
             hanging
             v-ideographic
             v-alphabetic
             v-mathematical
             v-hanging
             underline-position
             underline-thickness
             strikethrough-position
             strikethrough-thickness
             overline-position
             overline-thickness)

(def-svg-tag font-face-src
             id
             xml:base
             xml:lang
             xml:space)

(def-svg-tag font-face-uri
             xlink:type
             xlink:href
             xlink:role
             xlink:arcrole
             xlink:title
             xlink:show
             xlink:actuate)

(def-svg-tag font-face-format
             id
             xml:base
             xml:lang
             xml:space
             string)

(def-svg-tag font-face-name
             id
             xml:base
             xml:lang
             xml:space
             name)

(def-svg-tag foreignObject
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             x
             y
             width
             height
             transform)
;; -*- lisp -*-

;;(in-package :it.bese.yaclml.svg)

(def-svg-tag g
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             transform)

(def-svg-tag glyphRef
             id
             xml:base
             xml:lang
             xml:space
             x
             y
             dx
             dy
             glyphRef
             format)

(def-svg-tag glyph
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             unicode
             glyph-name
             d
             orientation
             arabic-form
	     ;; 因為和 xml:lang 取 (symbol-name 'xml:lang) -> "LANG" 所以先取消它
             ;; lang
             horiz-adv-x
             vert-origin-x
             vert-origin-y
             vert-adv-y)

(def-svg-tag hkern
             id
             xml:base
             xml:lang
             xml:space
             u1
             g1
             u2
             g2
             k)

(def-svg-tag image
             id
             xml:base
             xml:lang
             xml:space
             clip
             overflow
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             x
             y
             width
             height
             preserveAspectRatio
             transform)

(def-svg-tag line
             id
             xml:base
             xml:lang
             xml:space
             fill
             fill-rule
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-width
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             x1
             y1
             x2
             y2
             transform)

(def-svg-tag "linearGradient"
             id
             xml:base
             xml:lang
             xml:space
             color
             color-interpolation
             color-rendering
             "externalResourcesRequired"
             x1
             y1
             x2
             y2
             "gradientUnits"
             "gradientTransform"
             "spreadMethod")

(def-svg-tag metadata
             svg
             g
             defs
             symbol
             use
             switch
             image
             path
             rect
             circle
             line
             ellipse
             polyline
             polygon
             text
             tspan
             tref
             textPath
             marker
             color-profile
             linearGradient
             radialGradient
             pattern
             clipPath
             mask
             filter
             cursor
             a
             view
             animate
             set
             animateMotion
             animateColor
             animateTransform
             mpath
             font
             ce
             glyph
             missing-glyph)

(def-svg-tag marker
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-width
             fill
             flood-color
             flood-opacity
             lighting-color
             externalResourcesRequired
             refX
             refY
             markerUnits
             markerWidth
             markerHeight
             orient
             viewBox
             preserveAspectRatio)

(def-svg-tag mask
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             externalResourcesRequired
             x
             y
             width
             height
             maskUnits
             maskContentUnits)

(def-svg-tag mpath
             xlink:type
             xlink:href
             xlink:role
             xlink:arcrole
             xlink:title
             xlink:show
             xlink:actuate)

(def-svg-tag missing-glyph
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             d
             horiz-adv-x
             vert-origin-x
             vert-origin-y
             vert-adv-y)

(def-svg-tag path
             id
             xml:base
             xml:lang
             xml:space
             fill
             fill-rule
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-width
             marker-start
             marker-mid
             marker-end
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             d
             pathLength
             transform)

(def-svg-tag polyline
             id
             xml:base
             xml:lang
             xml:space
             fill
             fill-rule
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-width
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             points
             transform)

(def-svg-tag polygon
             id
             xml:base
             xml:lang
             xml:space
             fill
             fill-rule
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-width
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             points
             transform)

(def-svg-tag pattern
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             externalResourcesRequired
             x
             y
             width
             height
             patternUnits
             patternContentUnits
             patternTransform
             viewBox
             preserveAspectRatio)

(def-svg-tag rect
             id
             xml:base
             xml:lang
             xml:space
             fill
             fill-rule
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-width
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             x
             y
             width
             height
             rx
             ry
             transform)

(def-svg-tag radialGradient
             id
             xml:base
             xml:lang
             xml:space
             color
             color-interpolation
             color-rendering
             externalResourcesRequired
             cx
             cy
             r
             fx
             fy
             gradientUnits
             gradientTransform
             spreadMethod)

(def-svg-tag svg
             id
             xml:base
             xml:lang
             xml:space
             xmlns
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             onunload
             onabort
             onerror
             onresize
             onscroll
             onzoom
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             x
             y
             width
             height
             viewBox
             preserveAspectRatio
             zoomAndPan
             version
             baseProfile
             contentScriptType
             contentStyleType)

(def-svg-tag symbol
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             viewBox
             preserveAspectRatio)

(def-svg-tag switch
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             transform)

(def-svg-tag style
             xml:space
             type
             media
             title)

(def-svg-tag stop
             id
             xml:base
             xml:lang
             xml:space
             color
             color-interpolation
             color-rendering
             offset
             stop-color
             stop-opacity)

(def-svg-tag script
             id
             xml:base
             xml:lang
             xml:space
             externalResourcesRequired
             type)

(def-svg-tag set
             id
             xml:base
             xml:lang
             xml:space
             onbegin
             onend
             onrepeat
             onload
             externalResourcesRequired
             xlink:type
             xlink:href
             xlink:role
             xlink:arcrole
             xlink:title
             xlink:show
             xlink:actuate
             to)

(def-svg-tag title
             style
             class)

(def-svg-tag text
             id
             xml:base
             xml:lang
             xml:space
             font-family
             font-size
             font-size-adjust
             font-stretch
             font-style
             font-variant
             font-weight
             fill
             fill-rule
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-width
             style
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             x
             y
             dx
             dy
             rotate
             textLength
             lengthAdjust
             transform)

(def-svg-tag tspan
             id
             xml:base
             xml:lang
             xml:space
             fill
             fill-rule
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-width
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             x
             y
             dx
             dy
             rotate
             textLength
             lengthAdjust)

(def-svg-tag tref
             id
             xml:base
             xml:lang
             xml:space
             fill
             fill-rule
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-width
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             x
             y
             dx
             dy
             rotate
             textLength
             lengthAdjust)

(def-svg-tag textPath
             id
             xml:base
             xml:lang
             xml:space
             fill
             fill-rule
             stroke
             stroke-dasharray
             stroke-dashoffset
             stroke-linecap
             stroke-linejoin
             stroke-miterlimit
             stroke-width
             color
             color-interpolation
             color-rendering
             opacity
             fill-opacity
             stroke-opacity
             display
             image-rendering
             pointer-events
             shape-rendering
             text-rendering
             visibility
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             startOffset
             textLength
             lengthAdjust
             method
             spacing)

(def-svg-tag use
             id
             xml:base
             xml:lang
             xml:space
             writing-mode
             alignment-baseline
             baseline-shift
             direction
             dominant-baseline
             glyph-orientation-horizontal
             glyph-orientation-vertical
             kerning
             letter-spacing
             text-anchor
             text-decoration
             unicode-bidi
             word-spacing
             font-family
             font-size
             just
             font-stretch
             font-style
             font-variant
             font-weight
             marker-start
             marker-mid
             marker-end
             color-profile
             stop-color
             stop-opacity
             clip-path
             clip-rule
             mask
             filter
             color-interpolation-filters
             cursor
             flood-color
             flood-opacity
             lighting-color
             onfocusin
             onfocusout
             onactivate
             onclick
             onmousedown
             onmouseup
             onmouseover
             onmousemove
             onmouseout
             onload
             externalResourcesRequired
             x
             y
             width
             height
             transform)

(def-svg-tag view
             id
             xml:base
             xml:lang
             xml:space
             externalResourcesRequired
             viewBox
             preserveAspectRatio
             zoomAndPan
             viewTarget)

(def-svg-tag vkern
             id
             xml:base
             xml:lang
             xml:space
             u1
             g1
             u2
             g2
             k)

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
