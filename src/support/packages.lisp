(cl:defpackage :alien-works-demo.support
  (:local-nicknames (:aw :alien-works)
                    (:aws :alien-works.support)
                    (:cref :cffi-c-ref))
  (:use :cl)
  (:export #:load-cubemap
           #:make-banner
           #:banner-entity
           #:banner-texture))
