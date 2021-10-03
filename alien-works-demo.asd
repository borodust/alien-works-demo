(asdf:defsystem :alien-works-demo
  :description "alien-works demo"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (#:alien-works-demo/app #:alien-works-demo/tools))


(asdf:defsystem :alien-works-demo/app
  :description "alien-works demo"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (#:alien-works
               #:uiop
               #:alexandria
               #:static-vectors
               #:cffi
               #:float-features
               #:dissect
               #:flexi-streams
               #:bodge-blobs-support)
  :serial t
  :pathname "app/"
  :components ((:file "packages")
               (:file "utils")
               (:file "light")
               (:file "resource")
               (:file "tools")
               (:file "demo")))


(asdf:defsystem :alien-works-demo/tools
  :description "alien-works demo"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (#:alien-works-demo/app
               #:alien-works/tools)
  :serial t
  :pathname "tools/"
  :components ((:file "packages")
               (:file "texture")
               (:file "converter")
               (:file "banner")
               (:file "demo-support")
               (:file "ui")))


(asdf:defsystem :alien-works-demo/bundle
  :description "Bundle for alien-works demo"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (#:alien-works-demo/app #:alien-works-delivery)
  :serial t
  :pathname "bundle/"
  :components ((:file "bundle")))
