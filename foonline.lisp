(defpackage foonline
  (:export foonline start-foonline stop-foonline)
  (:use cl cl4l cl4l-html cl4l-index cl4l-utils lifoo
        lifoo-thread))

(in-package foonline)

(defvar *server*)
(defvar *docs* (make-hash-table :test 'equal))

(defstruct (repl)
  exec input console
  (history)
  (history-len 0)
  (history-pos 0))

(define-lifoo-init (:foonline) ()
  (define-word :words () ()
    canvas table
    :width style "100%" set drop
    :margin-top style "0.5em" set drop
    caption "words" text drop
    tr
    th "name" text
    :width style "50%" set drop drop
    th "args" text drop 
    th "macro?" text drop
    drop
    :words (:w swap set drop tr 
               td :w id str down swap drop text drop
               td :w args 1 list str down swap drop text drop
               td :w macro? "yes" "no" if swap drop text drop 
               drop)@ each
    $ (:w (:words (lifoo-push
                   (mapcar #'rest 
                           (index-first (lifoo-words)))) 
                  lisp)) 
    let))

(define-fn load-history (repl) ()
  (when (probe-file "history")
    (with-open-file (in "history")
      (let (str)
        (do-while ((setf str (read in nil)))
          (push str (repl-history repl))
          (incf (repl-history-len repl)))))))

(define-fn write-history (expr) ()
  (with-open-file (out "history"
                       :if-exists :append
                       :if-does-not-exist :create
                       :direction :output)
    (write expr :stream out)
    (terpri out)))

(define-fn eval-input (repl expr)
    ()
  (let ((ex (string-right-trim '(#\NewLine) expr)))
    (push ex (repl-history repl))
    (incf (repl-history-len repl))
    (setf (repl-history-pos repl) 0)
    (write-history ex))
  
  (with-input-from-string (in expr)
    (with-lifoo (:exec (repl-exec repl))
      (let ((out
              (with-output-to-string (out)
                (setf (lifoo-output) out)
                (lifoo-reset)
                (handler-case
                    (lifoo-eval
                     (lifoo-read :in in))
                  (error (e)
                    (html (repl-console repl)
                          (format nil "~a\\n" e)))))))
        (unless (string= "" out)
          (html (repl-console repl) out)))))
  (html (repl-console repl)
        (with-output-to-string (out)
          (lifoo-print (lifoo-pop :exec (repl-exec repl)) :out out)
          (write-string "\\n\\n" out))))

(define-fn handle-input (repl key
                         &key alt? ctrl? shift?)
    ()
  (declare (ignore shift?))
  (cond
    ((and ctrl? (char= #\Return (code-char key)))
     (lifoo-reset :exec (repl-exec repl))
     (let ((expr (html-value (repl-input repl))))
       (with-input-from-string (in expr)
         (let ((line))
           (do-while ((setf line (read-line in nil)))
             (html (repl-console repl) (format nil "~a\\n" line)))
           (html (repl-console repl) "\\n")))
       (eval-input repl expr))
     (html-scroll-bottom (repl-console repl))
     (html-select-all (repl-input repl))
     t)
    ((and alt? (char= #\n (char-downcase (code-char key))))
     (let ((pos
             (if (zerop (repl-history-pos repl))
                 (setf (repl-history-pos repl)
                       (1- (repl-history-len repl)))
                 (decf (repl-history-pos repl)))))
       (setf (html-value (repl-input repl))
             (nth pos (repl-history repl)))
       (html-select-all (repl-input repl)))
     t)
    ((and alt? (char= #\p (char-downcase (code-char key))))
     (let ((pos
             (if (< (repl-history-pos repl)
                    (1- (repl-history-len repl)))
                 (incf (repl-history-pos repl))
                 (setf (repl-history-pos repl) 0))))
       (setf (html-value (repl-input repl))
             (nth pos (repl-history repl)))
       (html-select-all (repl-input repl)))
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

    (let* ((repl-div (html-div body :id :repl))
           (input (html-textarea repl-div :id :input))
           (console (html-textarea (html-br repl-div)
                                   :id :console))
           (repl (make-repl :exec exec
                            :input input
                            :console console)))
      (load-history repl)
      
      (define-lisp-word :console () (:exec exec)
        (lifoo-push console))
      
      (setf (html-attr console :readonly) :true)

      (html console "Welcome to Foonline!")
      (html console "Ctrl-Enter evaluates, and Alt-p/n navigates history")
      (html console "")
      (focus-html input)
      
      (html-onkeydown
       input
       (lambda ()
         (in-package foonline)
         (when (handle-input
                repl
                (parse-integer (html-param :cl4l-key))
                :alt? (parse-bool (html-param :cl4l-alt-key))
                :ctrl? (parse-bool (html-param :cl4l-ctrl-key))
                :shift? (parse-bool (html-param :cl4l-shift-key)))
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
  (write-string "Please specify http-server port: ")
  (force-output)
  (let* ((line (read-line)))
    (unless (string= "" line)
      (start-foonline :port (parse-integer line)
                      :root root)))
  (terpri)
  (write-string "Foonline is waiting for your call,")
  (terpri)
  (write-string "press Enter to stop server and exit.")
  (force-output)
  (read-line)
  (stop-foonline))
