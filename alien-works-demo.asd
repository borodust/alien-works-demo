(asdf:defsystem :alien-works-demo
  :description "alien-works demo"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :pathname "src/"
  :depends-on (:alexandria :flexi-streams :static-vectors :alien-works :alien-works/tools :float-features :dissect)
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "light")
               (:file "resource")

               (:module support
                :serial t
                :components ((:file "packages")
                             (:file "texture")
                             (:file "converter")
                             (:file "banner")))

               (:file "demo")))
