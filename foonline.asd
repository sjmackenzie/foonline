(asdf:defsystem foonline
  :description "Lifoo online"
  :author "Andreas <codr4life@gmail.com>"
  :license "MIT"
  :depends-on (:hunchentoot :lifoo)
  :serial t
  :components ((:file "foonline")))
