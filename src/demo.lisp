(cl:in-package :alien-works-demo)


(defun shout (control &rest args)
  (terpri)
  (apply #'format t control args)
  (finish-output t))

;;;
;;; DEMO
;;;
(defun load-scene ()
  (let* ((resources (load-resources *scene*))
         (forged (forge-resources resources))
         (renderables (loop for (kind nil resource) in forged
                            when (eq :renderable kind)
                              collect resource)))
    (loop for renderable in renderables
          do (aw:add-scene-entity *engine* renderable)
             (push renderable *renderables*))))


(defun init-loop ()
  (setf *renderables* nil
        *sun* nil
        *lights* nil)
  (loop for renderable in (load-scene)
        do (aw:add-scene-entity *engine* renderable)
        do (push renderable *renderables*))

  (aw:with-vec3 (dir :y -1)
    (setf *sun* (add-sun dir)))

  (flet ((%load-cubemap (source)
           (loop for (kind nil resource) in (forge-resources (load-resources source))
                 when (eq kind :texture)
                   return resource)))
    (let ((cubemap (%load-cubemap *skybox*)))
      (setf (aw:skybox *engine*) (aw:make-cubemap-skybox *engine* cubemap)))
    (let ((cubemap (%load-cubemap *environment*)))
      (add-indirect-light cubemap))))


(defun destroy-loop ()
  (loop for renderable in *renderables*
        do (aw:destroy-renderable *engine* renderable))
  (aw:destroy-light *engine* *sun*)
  (setf *renderables* nil
        *sun* nil))


(defun handle-event (event)
  (when (and event (eq (aw:event-type event) :quit))
    (throw 'quit nil)))


(defun real-time-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second) 0f0))


(defun handle-loop ()
  (flet ((%handle-event (event)
           (handle-event event)))
    (aw:handle-events #'%handle-event))
  (aw:with-mat4* (transform0
                  transform1)
    (aw:with-vec3* ((rotation :y 1 :x 0 :z 0)
                    (translation :x 0 :y 0 :z 3)
                    (scale :x 1 :y 1 :z 1))
      (aw:rotate-mat4 transform0 transform1 (/ (real-time-seconds) 5) rotation)
      (aw:translate-mat4 transform1 transform0 translation)
      (aw:scale-mat4 transform0 transform1 scale))
    (aw:transform-camera *engine* transform0))

  (aw:with-mat4* (transform0
                  transform1)
    (aw:with-vec3* ((rotation :y 0 :x 1 :z 0)
                    (translation :x 0 :y 0 :z 0)
                    (scale :x 1 :y 1 :z 1))
      (aw:translate-mat4 transform0 transform1 translation)
      (aw:rotate-mat4 transform1 transform0 (+ (/ pi 2)
                                               (* 0.5 (cos (/ (real-time-seconds) 3))))
                      rotation)
      (aw:scale-mat4 transform0 transform1 scale))
    (loop for renderable in *renderables*
          do (aw:transform-entity *engine* renderable transform0)))

  (aw:render-frame *engine*)
  (sleep 0.015))


(defun run (scene skybox environment)
  (setf *scene* scene
        *skybox* skybox
        *environment* environment)
  (handler-bind ((serious-condition (lambda (c)
                                      (format *error-output* "~%Unhandled serious condition:~%")
                                      (dissect:present c *error-output*))))
    (dissect:with-capped-stack ()
      (float-features:with-float-traps-masked t
        (aw:with-window (win)
          (aw:with-engine (engine :surface (aw:window-surface win)
                                  :width (aw:window-width win)
                                  :height (aw:window-height win))
            (let ((*engine* engine))
              (init-loop)
              (unwind-protect
                   (catch 'quit
                     (shout "Looping")
                     (loop (handle-loop)))
                (destroy-loop)))))))))


(aw:definit main ()
  (flet ((%asset-path (asset-name)
           (if (member :android *features*)
               asset-name
               (asdf:system-relative-pathname :alien-works-demo
                                              (merge-pathnames asset-name "local/")))))
    (run (%asset-path "helmet.bin")
         (%asset-path "skybox.bin")
         (%asset-path "indirect.bin"))))
