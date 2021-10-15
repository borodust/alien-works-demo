(cl:in-package :alien-works-demo)

(declaim (special *engine*))

(defvar *renderables* nil)
(defvar *banner* nil)
(defvar *sun* nil)
(defvar *lights* nil)
(defvar *booo* nil)
(defvar *music* nil)

(defvar *scene* nil)
(defvar *environment* nil)
(defvar *skybox* nil)
(defvar *audio* nil)


(defun shout (control &rest args)
  (terpri t)
  (apply #'format t control args)
  (finish-output t))


(defvar *unloaded-foreign-libraries* nil)


(defun unload-foreign-libraries ()
  (bodge-blobs-support:close-foreign-libraries)
  (handler-bind ((style-warning #'muffle-warning))
    (loop for lib in (cffi:list-foreign-libraries :loaded-only t)
          do (progn
               (pushnew (cffi:foreign-library-name lib) *unloaded-foreign-libraries*
                        :test #'equal)
               (cffi:close-foreign-library lib)))))


(defun reload-foreign-libraries ()
  (bodge-blobs-support:load-foreign-libraries)
  (loop for lib-name in *unloaded-foreign-libraries*
        do (cffi:load-foreign-library lib-name))
  (setf *unloaded-foreign-libraries* nil))


(uiop:register-image-dump-hook 'unload-foreign-libraries)
