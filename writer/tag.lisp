(uiop:define-package :xmllib-cl/writer/tag
  (:documentation
   "Read and check headers of mp3 files.")
  (:use :cl)
  (:export #:start-tag
           #:end-tag
           #:content
           #:tag
           #:simple-tag
           #:xml-header-tag
           #:write-xml))
(in-package :xmllib-cl/writer/tag)

(defvar *indent-level* 0)
(defvar *output-stream* nil)
(defvar *indentp* t)

(defun indent (&key
                 (stream *output-stream*)
                 (indent-level *indent-level*))
  "Write as many tabs as specified in INDENT-LEVEL to STREAM."
  (format stream "~v@{~C~:*~}" indent-level #\Tab))

(defun start-tag (name
                  &key
                    attrs
                    closep
                    (stream *output-stream*)
                    (indentp *indentp*))
  "Write a start tag NAME to STREAM with attributes ATTRS.
ATTRS is a LIST of LIST with 2 items, the name and value of an attribute.
If CLOSEP is non-nil the start tag will end with '/>'.
If INDENTP is non-nil the tag will be indented."
  (when indentp
    (indent :stream stream))
  (format stream
          "<~A~:{ ~A=\"~A\"\~}~@[ /~]>"
          name
          attrs
          closep))

(defun end-tag (name
                &key
                  (stream *output-stream*)
                  (indentp *indentp*))
  "Write a end tag NAME to STREAM.
If INDENTP is non-nil the tag will be indented."
  (when indentp
    (indent :stream stream))
  (format stream "</~A>" name))

(defun content (content
                &key
                  (stream *output-stream*)
                  (indentp *indentp*))
  "Write CONTENT to STREAM.
If INDENTP is non-nil the tag will be indented."
  (when indentp
    (indent :stream stream))
  (format stream "~A" content))

(defmacro let-or-not-let (let-it var val &body body)
  "If LET-IT is non-nil put BODY inside a let with VAR set to VAL.
Otherwise, put BODY inside a progn."
  (if let-it
      `(let ((,var ,val))
         ,@body)
      `(progn ,@body)))

(defmacro tag ((name
                &key
                  attrs
                  simplep
                  (stream *output-stream* streamp))
               &body body)
  "Write a tag NAME with attributes ATTRS with its content being BODY to STREAM.
If SIMPLEP is non-nil writes a simple tag without newlines nor tabs."
  `(let-or-not-let
       ,streamp *output-stream* ,stream
     (fresh-line *output-stream*)
     (start-tag ,name :attrs ,attrs)
     ,(unless simplep
        '(fresh-line *output-stream*))
     (let ((*indent-level* ,(if simplep
                                0
                                '(1+ *indent-level*))))
       ,@body)
     ,(unless simplep
        '(fresh-line *output-stream*))
     (end-tag ,name :indentp ,(not simplep))))

(defun xml-header-tag (name
                       &key
                         attrs
                         (stream *output-stream*))
  "Write a xml header tag NAME to STREAM with attributes ATTRS.
ATTRS is a LIST of LIST with 2 items, the name and value of an attribute."
  (format stream
          "<?~A~:{ ~A=\"~A\"\~}?>"
          name
          attrs))

(defmacro write-xml ((stream
                      &key
                        stylesheet
                        (version "1.0" versionp)
                        (encoding "UTF-8" encodingp))
                     &body body)
  "Write a xml structure to STREAM.
STYLESHEET is a STRING specifying the stylesheet linked to the xml if non-nil.
VERSION is a STRING specifying the version of the xml.
ENCODING is a STRING specifying the encoding of the xml."
  `(let ((*output-stream* ,stream))
     (xml-header-tag "xml"
                     :attrs '(("version" ,(if versionp
                                              version
                                              "1.0"))
                              ("encoding" ,(if encodingp
                                               encoding
                                               "UTF-8"))))
     (fresh-line *output-stream*)
     ,(when stylesheet
        `(xml-header-tag "xml-stylesheet"
                         :attrs '(("href" ,stylesheet)
                                  ("type" "text/xsl"))))
     ,@body))
