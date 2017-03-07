(asdf:defsystem foonline
  :description "a web-based Lifoo IDE"
  :author "Andreas <codr4life@gmail.com>"
  :license "MIT"
  :depends-on (:hunchentoot :lifoo)
  :serial t
  :components ((:file "foonline")))
