(uiop:define-package :xmllib-cl/writer/tag
  (:documentation
   "Read and check headers of mp3 files.")
  (:use :cl)
  (:export #:start-tag
           #:end-tag
           #:content
           #:tag))
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
