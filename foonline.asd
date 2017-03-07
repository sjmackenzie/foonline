(asdf:defsystem foonline
  :description "a web-based development environment based on Lifoo"
  :author "Andreas <codr4life@gmail.com>"
  :license "MIT"
  :depends-on (:hunchentoot :lifoo)
  :serial t
  :components ((:file "foonline")))
