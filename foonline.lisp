(defpackage foonline
  (:export start-foonline stop-foonline)
  (:use cl cl4l-html cl4l-trans cl4l-utils lifoo))

(in-package foonline)

(defparameter *port* 8080)
(defparameter *root* "~/Andreas/Dev/Lisp/foonline/www/")

(defvar *server*)
(defvar *docs* (make-hash-table :test 'equal))

(hunchentoot:define-easy-handler (main :uri "/") ()
  (let* ((doc (html-doc :title "Foonline"
                        :dynamic? t
                        :call-url "cl4l"))
         (body (html-body doc))
         (exec (lifoo-init nil :exec (make-lifoo))))

    (html-script doc :src "jquery.js")
    (html-script doc :src "cl4l.js")

    (html-link doc :rel :stylesheet
                   :type :text/css
                   :href "reset.css")
    (html-link doc :rel :stylesheet
                   :type :text/css
                   :href "foonline.css")
    
    (define-lisp-word :document () (:exec exec)
      (lifoo-push doc))

    (let* ((repl (html-div body :id :repl))
           (input (html-textarea repl :id :input))
           (output (html-textarea (html-br repl) :id :output))
           undo-stack)
      (define-lisp-word :input () (:exec exec)
        (lifoo-push input))
      
      (define-lisp-word :output () (:exec exec)
        (lifoo-push output))
      
      (setf
       (html-attr input :rows) 5
       (html-attr output :readonly) :true)

      (html output "Welcome to Foonline,")
      (html output "press Ctrl+Enter to evaluate")
      (html output "")
      (html-focus input)
      
      (html-onkeydown
       input
       (lambda ()
         (cond
           ((and
             (string= "true" (html-param :cl4l-shift-key))
             (string= "true" (html-param :cl4l-ctrl-key))
             (string= "90" (html-param :cl4l-key)))
            (when undo-stack
              (handler-case
                  (progn
                    (rollback :trans (pop undo-stack))
                    (html output "undo!\\n"))
                (error (e)
                  (html output (format nil "~a\\n" e)))))
            (drop-html-event doc))
           
           ((and
             (string= "true" (html-param :cl4l-ctrl-key))
             (string= "13" (html-param :cl4l-key)))
             (lifoo-reset :exec exec)
             (let ((expr (html-value input)))
               (with-input-from-string (in expr)
                 (let ((line))
                   (do-while ((setf line (read-line in nil)))
                     (html output (format nil "~a\\n" line)))
                   (html output "\\n")))
               
               (with-input-from-string (in expr)
                 (with-lifoo (:exec exec)
                   (let ((out
                           (with-output-to-string (out)
                             (setf (lifoo-output) out)
                             (push (lifoo-push-trans)
                                   undo-stack)
                             (handler-case
                                 (lifoo-eval
                                  (lifoo-read :in in))
                               (error (e)
                                 (html output (format nil "~a\\n"
                                                      e))))
                             (lifoo-pop-trans))))
                     (unless (string= "" out)
                       (html output out))))))
             (html output
                   (with-output-to-string (out)
                     (lifoo-print (lifoo-pop :exec exec) :out out)
                     (write-string "\\n\\n" out)))
             (html-scroll-bottom output)
             (html-select-all input)
             (drop-html-event doc))))))
    
    (let ((canvas (html-div body :id :canvas)))
      (define-lisp-word :canvas () (:exec exec)
        (lifoo-push canvas)))
    
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
