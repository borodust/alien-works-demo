(cl:in-package :alien-works-demo)


(defun shout (control &rest args)
  (terpri)
  (apply #'format t control args)
  (finish-output t))

;;;
;;; DEMO
;;;
(defun load-banner ()
  (let* ((banner (alien-works-demo.support:make-banner))
         (entity (alien-works-demo.support:banner-entity banner)))
    (aw:add-scene-entity *engine* entity)
    (setf *banner* entity)
    banner))


(defun update-banner-texture (banner surface)
  (let ((tex (alien-works.graphics::buffered-surface-acquire surface)))
    (unwind-protect
         (progn
           (setf (alien-works-demo.support:banner-texture banner) tex))
      (alien-works.graphics::buffered-surface-release surface))))


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
  #++(progn
       (loop for renderable in *renderables*
             do (aw:destroy-renderable *engine* renderable))
       (aw:destroy-light *engine* *sun*))
  (setf *renderables* nil
        *sun* nil))


(defun handle-event (event)
  (when (and event (eq (aw:event-type event) :quit))
    (throw 'quit nil)))


(defun real-time-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second) 0f0))


(defmacro with-transform ((transform &rest operations) &body body)
  (alexandria:with-gensyms (transform0 transform1 vec)
    (flet ((%expand-transform (result source operation-desc)
             (let* ((operation (first operation-desc))
                    (vec-config (if (eq operation :rotation)
                                    (cddr operation-desc)
                                    (rest operation-desc))))
               (destructuring-bind (&key x y z) vec-config
                 `(aw:with-vec3 (,vec
                                 ,@(when x `(:x ,x))
                                 ,@(when y `(:y ,y))
                                 ,@(when z `(:z ,z)))
                    ,(ecase operation
                       (:rotation `(aw:rotate-mat4 ,result ,source ,(second operation-desc) ,vec))
                       (:translation `(aw:translate-mat4 ,result ,source ,vec))
                       (:scale `(aw:scale-mat4 ,result ,source ,vec))))))))
      `(aw:with-mat4* (,transform0
                       ,transform1)
         ,@(loop with result = transform0 and source = transform1
                 for operation in operations
                 collect (prog1 (%expand-transform result source operation)
                           (rotatef result source))
                   into transforms
                 finally (return (append transforms
                                         `((let ((,transform ,source))
                                             ,@body)))))))))


(defun handle-loop ()
  (flet ((%handle-event (event)
           (handle-event event)))
    (aw:handle-events #'%handle-event))

  (with-transform (transform
                   (:rotation (/ (real-time-seconds) 5) :x 0 :y 1 :z 0)
                   (:translation :x 0 :y 0 :z 3)
                   (:scale :x 1 :y 1 :z 1))
    (aw:transform-camera *engine* transform))

  (with-transform (transform
                   (:translation :x -0.5 :y -0.5 :z -0.5)
                   #++(:rotation (+ (/ pi 2)
                                    (* 0.5 (cos (/ (real-time-seconds) 3))))
                       :x 0 :y 1 :z 0)
                   (:scale :x 1 :y 1 :z 1))
    (loop for renderable in *renderables*
          do (aw:transform-entity *engine* renderable transform))
    (when *banner*
      (aw:transform-entity *engine* *banner* transform)))

  (aw:render-frame *engine*)
  (sleep 0.015))


(defvar *surface* nil)

(defun run (scene skybox environment)
  (setf *scene* scene
        *skybox* skybox
        *environment* environment)
  (handler-bind ((serious-condition (lambda (c)
                                      (format *error-output* "~%Unhandled serious condition:~%")
                                      (dissect:present c *error-output*))))
    (dissect:with-capped-stack ()
      (float-features:with-float-traps-masked t
        (aw:with-window (win :context context)
          (let* ((width (aw:window-width win))
                 (height (aw:window-height win)))
            (aw:with-engine (engine :surface (aw:window-surface win)
                                    :shared-context context
                                    :width width
                                    :height height)
              (let* ((*engine* engine)
                     (surface (alien-works.graphics::make-buffered-surface engine width height))
                     (banner (load-banner))
                     (stopped-p nil))
                (aw:make-shared-context-thread
                 win
                 (lambda ()
                   (alien-works.graphics::init-shared-context-thread
                    surface
                    (lambda () stopped-p))))
                (init-loop)
                (unwind-protect
                     (catch 'quit
                       (shout "Looping")
                       (loop (handle-loop)
                             (update-banner-texture banner surface)))
                  (setf stopped-p t)
                  (sleep 1) ;; FIXME: i know i know
                  (destroy-loop))))))))))


(defun asset-path (asset-name)
  (if (member :android *features*)
      asset-name
      (asdf:system-relative-pathname :alien-works-demo
                                     (merge-pathnames asset-name "assets/"))))


(aw:definit main ()
  (flet ()
    (run (asset-path "helmet.bin")
         (asset-path "skybox.bin")
         (asset-path "indirect.bin"))))


(defun convert-helmet ()
  (let ((resources (alien-works-demo.support::parse-gltf
                    (asset-path "helmet/DamagedHelmet.gltf"))))
    (apply #'alien-works-demo::save-resources (asset-path "helmet.bin") resources)))
