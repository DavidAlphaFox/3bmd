(in-package #:3bmd)

(defvar *md-prefix*)
(defvar *md-list-item*)
(defvar *md-in-block*)
(defvar *md-block-seen-p*)

(defmacro with-md-indent ((indent) &body body)
  `(let ((*md-prefix* (concatenate 'string *md-prefix*
                                   (make-string ,indent
                                                :initial-element #\space))))
     ,@body))

(defmacro with-md-prefix ((prefix) &body body)
  `(let ((*md-prefix* (concatenate 'string *md-prefix* ,prefix)))
     ,@body))

(defun md-indent (stream)
  (write-string *md-prefix* stream)
  (setq *md-in-block* :right-after-indent))

(defun ensure-block (stream)
  (unless *md-in-block*
    (when *md-block-seen-p*
      (format stream "~%"))
    (setq *md-in-block* t)
    (md-indent stream)
    (setq *md-block-seen-p* t)))

(defun end-block (stream)
  (when *md-in-block*
    (format stream "~%")
    (setq *md-in-block* nil)))

;;; These are some of the 3BMD-GRAMMAR::SPECIAL-CHARs. The ! character
;;; is not necessary to escape if [ is. Similary, no need to escape >
;;; if < is. Backslash should never be escaped.
(defparameter *md-default-block-chars-to-escape* "#")
(defparameter *md-default-inline-chars-to-escape* "*_`&[]<")

(defvar *md-block-chars-to-escape*)
(defvar *md-inline-chars-to-escape*)

(defun chars-to-escape-with-extensions (default extension-to-md-chars-to-escape)
  (let ((strings (list default)))
    (maphash (lambda (extension-flag chars)
               (when (symbol-value extension-flag)
                 (push (coerce chars 'string) strings)))
             extension-to-md-chars-to-escape)
    (apply #'concatenate 'string strings)))

(defmacro with-md-escapes (&body body)
  `(let ((*md-block-chars-to-escape*
           (chars-to-escape-with-extensions
            *md-default-block-chars-to-escape*
            *extension-to-md-block-chars-to-escape*))
         (*md-inline-chars-to-escape*
           (chars-to-escape-with-extensions
            *md-default-inline-chars-to-escape*
            *extension-to-md-inline-chars-to-escape*)))
     ,@body))

(defun print-md-escaped (string stream)
  (loop for char across string
        do (when (and (not *in-code*)
                      (or (find char *md-inline-chars-to-escape*)
                          (and (eq *md-in-block* :right-after-indent)
                               (find char *md-block-chars-to-escape*))))
             ;; TODO: The escaping is overeager. For example, there is
             ;; no need for the escapes in "\\<->" and "\\&KEY " due
             ;; to how the parser works, but this needs information
             ;; about what follows STRING in the parse tree.
             (write-char #\\ stream))
           (when (eq *md-in-block* :right-after-indent)
             (setq *md-in-block* t))
           (write-char char stream)
           (when (char= char #\Newline)
             (md-indent stream))))

(defun print-md (string stream)
  (loop for char across string
        do (write-char char stream)
           (when (eq *md-in-block* :right-after-indent)
             (setq *md-in-block* t))
           (when (char= char #\Newline)
             (md-indent stream))))

(defmethod print-md-tagged-element ((tag (eql :heading)) stream rest)
  (ensure-block stream)
  (loop repeat (getf rest :level) do (write-char #\# stream))
  (write-char #\Space stream)
  (dolist (a (getf rest :contents)) (print-md-element a stream))
  (end-block stream))

(defmethod print-md-tagged-element ((tag (eql :paragraph)) stream rest)
  (ensure-block stream)
  (dolist (a rest) (print-md-element a stream))
  (end-block stream))

(defmethod print-md-tagged-element ((tag (eql :block-quote)) stream rest)
  (with-md-prefix ("> ")
    (dolist (a rest)
      (print-md-element a stream))))

(defmethod print-md-tagged-element ((tag (eql :plain)) stream rest)
  (ensure-block stream)
  (dolist (a rest) (print-md-element a stream)))

(defmethod print-md-tagged-element ((tag (eql :emph)) stream rest)
  (format stream "*")
  (dolist (a rest) (print-md-element a stream))
  (format stream "*"))

(defmethod print-md-tagged-element ((tag (eql :strong)) stream rest)
  (format stream "**")
  (dolist (a rest) (print-md-element a stream))
  (format stream "**"))

(defmethod print-md-tagged-element ((tag (eql :link)) stream rest)
  (format stream "<")
  (dolist (a rest) (print-md-element a stream))
  (format stream ">"))

(defmethod print-md-tagged-element ((tag (eql :mailto)) stream rest)
  (format stream "<")
  (dolist (a rest)
    (if (and (stringp a)
             (alexandria:starts-with-subseq "mailto:" a))
        (print-md-element (subseq a (length "mailto:")) stream)
        (print-md-element a stream)))
  (format stream ">"))

(defmethod print-md-tagged-element ((tag (eql :explicit-link)) stream rest)
  (format stream "[")
  (dolist (a (getf rest :label)) (print-md-element a stream))
  (format stream "](~a~@[ ~s~])" (getf rest :source) (getf rest :title)))

(defmethod print-md-tagged-element ((tag (eql :reference-link)) stream rest)
  (format stream "[")
  (dolist (a (getf rest :label)) (print-md-element a stream))
  (format stream "][")
  (dolist (a (getf rest :definition)) (print-md-element a stream))
  (format stream "]"))

(defmethod print-md-tagged-element ((tag (eql :image)) stream rest)
  (format stream "!")
  (dolist (a rest)
    (print-md-element a stream)))

(defmethod print-md-tagged-element ((tag (eql :counted-list)) stream rest)
  (let ((*md-list-item* 1))
    (dolist (a rest) (print-md-element a stream))))

(defmethod print-md-tagged-element ((tag (eql :bullet-list)) stream rest)
  (let ((*md-list-item* "-"))
    (dolist (a rest) (print-md-element a stream))))

(defmethod print-md-tagged-element ((tag (eql :list-item)) stream rest)
  (ensure-block stream)
  (cond ((numberp *md-list-item*)
         (format stream "~D. " *md-list-item*)
         (incf *md-list-item*))
        (t
         (format stream "~a " *md-list-item*)))
  (print-md-element (first rest) stream)
  (with-md-indent (4)
    (dolist (a (rest rest)) (print-md-element a stream)))
  (end-block stream))

(defmethod print-md-tagged-element ((tag (eql :line-break)) stream rest)
  (print-md-escaped "  
" stream))

(defmethod print-md-tagged-element ((tag (eql :horizontal-rule)) stream rest)
  (ensure-block stream)
  (print-md-escaped "---" stream)
  (end-block stream))

(defmethod print-md-tagged-element ((tag (eql :html)) stream rest)
  (format stream "~{~a~}" rest))

(defmethod print-md-tagged-element ((tag (eql :raw-html)) stream rest)
  (format stream "~{~a~}" rest))

(defmethod print-md-tagged-element ((tag (eql :entity)) stream rest)
  (format stream "~{~a~}" rest))

(defun remove-ending-newline (string)
  (if (alexandria:ends-with #\Newline string)
      (subseq string 0 (1- (length string)))
      string))

(defmethod print-md-tagged-element ((tag (eql :verbatim)) stream rest)
  (with-md-indent (4)
    (ensure-block stream)
    (dolist (a (butlast rest)) (print-md a stream))
    (print-md (remove-ending-newline (first (last rest))) stream)
    (end-block stream)))

;;; :UNESCAPED-STRING is not produced by the parser, but applications
;;; that manipulate the parse tree might find it convenient.
(defmethod print-md-tagged-element ((tag (eql :unescaped-string)) stream rest)
  (dolist (a rest) (print-md a stream)))

(defun max-n-consecutive-char (char string)
  (let ((n 0)
        (max 0))
    (loop for char-2 across string
          do (cond ((char= char char-2)
                    (incf n)
                    (setq max (max n max)))
                   (t
                    (setq n 0))))
    max))

(defun max-n-consecutive-backticks (parse-tree)
  (let ((n 0))
    (labels ((foo (parse-tree)
               (if (stringp parse-tree)
                   (setq n (max n (max-n-consecutive-char #\` parse-tree)))
                   (map nil #'foo parse-tree))))
      (foo parse-tree))
    n))

(defmethod print-md-tagged-element ((tag (eql :code)) stream rest)
  (let ((n (max-n-consecutive-backticks rest)))
    (loop repeat (1+ n) do (write-char #\` stream))
    (let ((*in-code* t))
      (dolist (a rest) (print-md-element a stream)))
    (loop repeat (1+ n) do (write-char #\` stream))))

(defmacro define-smart-quote-md-translation (name replacement)
  `(defmethod print-md-tagged-element ((tag (eql ,name)) stream rest)
     (write-string ,replacement stream)))

(define-smart-quote-md-translation :single-quoted "'")
(define-smart-quote-md-translation :double-quoted "\"")
(define-smart-quote-md-translation :en-dash "--")
(define-smart-quote-md-translation :em-dash "---")
(define-smart-quote-md-translation :left-right-single-arrow "<->")
(define-smart-quote-md-translation :left-single-arrow "<-")
(define-smart-quote-md-translation :right-single-arrow "->")
(define-smart-quote-md-translation :left-right-double-arrow "<=>")
(define-smart-quote-md-translation :left-double-arrow "<=")
(define-smart-quote-md-translation :right-double-arrow "=>")
(define-smart-quote-md-translation :ellipsis "...")

(defmethod print-md-tagged-element ((tag (eql :reference)) stream rest)
  (format stream "~%[")
  (dolist (a (getf rest :label)) (print-md-element a stream))
  (format stream "]: ~a~@[ ~s~]~%"
          (getf rest :source) (getf rest :title)))

(defmethod print-md-element ((elem (eql :apostrophe)) stream)
  (write-string "'" stream))

(defmethod print-md-element ((elem string) stream)
  (print-md-escaped elem stream))

(defmethod print-md-element ((elem cons) stream)
  (if (symbolp (car elem))
      (print-md-tagged-element (car elem) stream (cdr elem))
      (error "unknown cons? ~s" elem)))

(defmethod print-doc-to-stream-using-format (doc stream
                                             (format (eql :markdown)))
  (let ((*references* (extract-refs doc))
        (*md-prefix* "")
        (*md-in-block* nil)
        (*md-block-seen-p* nil))
    (with-md-escapes
        (dolist (element doc)
          (print-md-element element stream)))))

#|

(defparameter *test-cases*
  '("\\*notemph\\* \\_notemph2\\_ \\ \\`notcode\\`
"
    "### heading

- outer item

    dsfdsf *emph* **strong** ***strongemph***
    kjdsf [asdf](#xxx \"*title*\") <http://quotenil.com> <mega@retes.hu>

    - b1

        inside `````co````de`````

            codeblock 234
            sdfkj kjsdf

    - b2

    dsfdsf
    llll

    1. xxx

    2. yyy

    ---

    para [ref-link][sfd] ![image](#dsfa \"img\") ![img][32]
    sddsf <a href=\"#xxx\">ddd</a> <!-- comment -->

em-dash -- en-dash --- lrsa <-> <- -> <=> <= => ... foo's

[id1]: http://some.link.com/ \"with some title\"

plain
"))

(defun check-roundtrip (original)
  (let* ((reconstruction (with-output-to-string (out)
                           (let ((3bmd-grammar:*smart-quotes* t))
                             (3bmd:parse-string-and-print-to-stream
                              original out :format :markdown))))
         (position (mismatch reconstruction original)))
    (when position
      (format t "Mismatch found. Common part:~%~S~%-- Original:~%~S~%~
                -- Reconstruction:~%~S~%"
              (subseq original 0 position)
              (subseq original position)
              (subseq reconstruction position))
      (error "Mismatch found at position ~S." position))))

(map nil #'check-roundtrip *test-cases*)

(let ((3bmd-grammar:*smart-quotes* t))
  (3bmd:parse-string-and-print-to-stream "\\'" *standard-output*))

(3bmd-grammar:parse-doc "[id1]: http://some.link.com/ \"with some title\"")

|#
