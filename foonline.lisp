(defpackage foonline
  (:export foonline start-foonline stop-foonline)
  (:use cl cl4l cl4l-html cl4l-utils lifoo lifoo-thread))

(in-package foonline)

(defvar *server*)
(defvar *docs* (make-hash-table :test 'equal))

(define-fn eval-input (expr
                       &key exec
                       console)
    ()
  (with-input-from-string (in expr)
    (with-lifoo (:exec exec)
      (let ((out
              (with-output-to-string (out)
                (setf (lifoo-output) out)
                (lifoo-reset)
                (handler-case
                    (lifoo-eval
                     (lifoo-read :in in))
                  (error (e)
                    (html console (format nil "~a\\n"
                                          e)))))))
        (unless (string= "" out)
          (html console out)))))
  (html console
        (with-output-to-string (out)
          (lifoo-print (lifoo-pop :exec exec) :out out)
          (write-string "\\n\\n" out))))

(define-fn handle-input (key
                         &key alt? ctrl? shift?
                         exec
                         input
                         console)
    ()
  (declare (ignore alt? shift?))
  (cond
    ((and ctrl? (= 13 key))
     (lifoo-reset :exec exec)
     (let ((expr (html-value input)))
       (with-input-from-string (in expr)
         (let ((line))
           (do-while ((setf line (read-line in nil)))
             (html console (format nil "~a\\n" line)))
           (html console "\\n")))
       (eval-input expr :exec exec :console console))
     (html-scroll-bottom console)
     (html-select-all input)
     t)
    (t nil)))

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
           (console (html-textarea (html-br repl) :id :console)))
      (define-lisp-word :console () (:exec exec)
        (lifoo-push console))
      
      (setf (html-attr console :readonly) :true)

      (html console "Welcome to Foonline,")
      (html console "Ctrl-Enter evaluates")
      (html console "")
      (focus-html input)
      
      (html-onkeydown
       input
       (lambda ()
         (when (handle-input
                (parse-integer (html-param :cl4l-key))
                :alt? (parse-bool (html-param :cl4l-alt-key))
                :ctrl? (parse-bool (html-param :cl4l-ctrl-key))
                :shift? (parse-bool (html-param :cl4l-shift-key))
                :exec exec
                :input input
                :console console)
           (drop-html-event doc)))))
    
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

(define-fn start-foonline (&key port root) ()
  (setf *server*
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-acceptor
                        :access-log-destination nil
                        :port port
                        :document-root root))))

(define-fn stop-foonline () ()
  (hunchentoot:stop *server*))

(define-fn foonline (&key (root "www/")) ()
  (write-string "Welcome to Foonline,")
  (terpri)
  (write-string "please specify server http-port: ")
  (force-output)
  (let* ((line (read-line)))
    (unless (string= "" line)
      (start-foonline :port (parse-integer line)
                      :root root)))
  (terpri)
  (write-string "Foonline is waiting for your call,")
  (terpri)
  (write-string "press Enter to stop server and exit")
  (force-output)
  (read-line)
  (stop-foonline))
