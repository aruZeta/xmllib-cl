(uiop:define-package :xmllib-cl/writer/xml
  (:documentation
   "Read and check headers of mp3 files.")
  (:use :cl)
  (:import-from :xmllib-cl/writer/tag
                #:*output-stream*)
  (:export #:xml-header-tag
           #:write-xml))
(in-package :xmllib-cl/writer/xml)

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
