(cl:defpackage :alien-works-demo.bundle
  (:use :cl)
  (:local-nicknames (:awd :alien-works-delivery))
  (:export))
(cl:in-package :alien-works-demo.bundle)


(awd:defbundle :alien-works-demo
  :system :alien-works-demo/app
  :entry-point (:alien-works :run)
  :assets ((:file (:system "assets/helmet.bin"))
           (:file (:system "assets/skybox.bin"))
           (:file (:system "assets/indirect.bin"))
           (:file (:system "assets/audio.bin"))))
