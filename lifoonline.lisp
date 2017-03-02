(defpackage lifoonline
  (:export start-lifoonline stop-lifoonline)
  (:use cl cl4l-html cl4l-utils lifoo))

(in-package lifoonline)

(defparameter *port* 8080)
(defparameter *root* "~/Andreas/Dev/Lisp/lifoonline/www/")

(defvar *server*)
(defvar *docs* (make-hash-table :test 'equal))

(hunchentoot:define-easy-handler (demo :uri "/demo") ()
  (let* ((doc (html-doc :dynamic? t :call-url "cl4l"))
         (body (html-body doc))
         (input (html-input body :type :text)))

    (html-script doc :src "jquery.js")
    (html-script doc :src "cl4l.js")

    (html-button body
                 :body "Greet"
                 :onclick
                 (lambda ()
                   (html body (format nil "Hello ~a!"
                                      (html-attr input :value)))
                   (html-br body)))
    
    (html-br body)
    
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

(defun start-lifoonline ()
  (setf *server*
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-acceptor
                        :port *port*
                        :document-root *root*))))

(defun stop-lifoonline ()
  (hunchentoot:stop *server*))
