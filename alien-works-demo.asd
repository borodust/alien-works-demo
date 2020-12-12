(asdf:defsystem :alien-works-demo
  :description "alien-works demo"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :pathname "src/"
  :depends-on (:alexandria :flexi-streams :static-vectors :alien-works :float-features :dissect)
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "light")
               (:file "resource")
               (:file "demo")))


(asdf:defsystem :alien-works-demo/support
  :description "alien-works demo"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :pathname "src/support"
  :depends-on (:alien-works-demo :alien-works/support)
  :serial t
  :components ((:file "packages")
               (:file "texture")
               (:file "converter")))
