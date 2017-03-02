(defpackage foonline
  (:export start-foonline stop-foonline)
  (:use cl cl4l-html cl4l-utils lifoo))

(in-package foonline)

(defparameter *port* 8080)
(defparameter *root* "~/Andreas/Dev/Lisp/foonline/www/")

(defvar *server*)
(defvar *docs* (make-hash-table :test 'equal))

(hunchentoot:define-easy-handler (demo :uri "/") ()
  (let* ((doc (html-doc :title "foonline"
                        :dynamic? t
                        :call-url "cl4l"))
         (body (html-body doc)))

    (html-script doc :src "jquery.js")
    (html-script doc :src "cl4l.js")

    (html-link doc :rel :stylesheet
                   :type :text/css
                   :href "reset.css")
    (html-link doc :rel :stylesheet
                   :type :text/css
                   :href "foonline.css")
    
    (let* ((repl (html-div body :id :repl))
           (input (html-textarea repl :id :input))
           (output (html-textarea (html-br repl) :id :output)))
      (setf
       (html-attr input :rows) 5
       (html-attr output :readonly) t))

    (let ((canvas (html-div body :id :canvas)))
      (declare (ignore canvas)))
    
    (setf (gethash (html-doc-id doc) *docs*) doc)
    (setf (hunchentoot:content-type*) "text/html")
    (to-html doc)))

(hunchentoot:define-easy-handler (cl4l :uri "/cl4l") ()
  (let* ((doc-id (hunchentoot:parameter "cl4l-doc"))
         (doc (gethash doc-id *docs*)))

    (html-call doc
               (mapcar (lambda (arg)
                         (cons (keyword! (first arg)) (rest arg)))
                       (hunchentoot:post-parameters*)))

    (setf (hunchentoot:content-type*) "application/javascript")
    (html-update-script doc)))

(defun start-foonline ()
  (setf *server*
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-acceptor
                        :port *port*
                        :document-root *root*))))

(defun stop-foonline ()
  (hunchentoot:stop *server*))
