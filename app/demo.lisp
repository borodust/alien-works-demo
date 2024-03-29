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


(defun load-audio ()
  (let* ((resources (load-resources *audio*)))
    (loop for (nil name rsc) in (forge-resources resources)
          do (a:switch (name :test #'string=)
               ("booo" (setf *booo* rsc))
               ("theme" (setf *music* rsc))))))


(defun play-boo ()
  (bt:make-thread
   (lambda () (aw:play-audio *booo*))))


(defun play-music ()
  (bt:make-thread
   (lambda () (aw:play-audio *music*))))


(defun init-loop ()
  (setf *renderables* nil
        *sun* nil
        *lights* nil
        *booo* nil)

  (load-scene)
  (load-audio)

  (aw:with-vec3 (dir :y -1)
    (setf *sun* (add-sun dir)))

  (flet ((%load-cubemap (source)
           (loop for (kind nil resource) in (forge-resources (load-resources source))
                 when (eq kind :texture)
                   return resource)))
    (let ((cubemap (%load-cubemap *skybox*)))
      (setf (aw:skybox *engine*) (aw:make-cubemap-skybox *engine* cubemap)))
    (let ((cubemap (%load-cubemap *environment*)))
      (add-indirect-light cubemap)))
  (init-tools *tools*))


(defun destroy-loop ()
  (progn
    (loop for renderable in *renderables*
          do (aw:destroy-renderable *engine* renderable))
    (aw:destroy-light *engine* *sun*))
  (setf *renderables* nil
        *sun* nil))


(defun handle-event (event)
  (when event
    (case (aw:event-type event)
      (:quit (throw 'quit nil)))
    (handle-tool-event *tools* event)))


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

  (update-tools *tools* 0.015)

  (with-transform (transform
                   (:rotation (/ (real-time-seconds) 5) :x 0 :y 1 :z 0)
                   (:rotation (/ (real-time-seconds) 10) :x 0 :y 1 :z 1)
                   (:translation :x 0 :y 0 :z 3)
                   (:scale :x 1 :y 1 :z 1))
    (aw:transform-camera *engine* transform))

  (with-transform (transform
                   (:translation :x 0 :y 0 :z 0)
                   (:rotation (+ (/ pi 2)
                                 (* 0.5 (cos (/ (real-time-seconds) 3))))
                    :x 0 :y 1 :z 0)
                   (:rotation (/ pi 2) :x 1 :y 0 :z 0)
                   (:scale :x 1 :y 1 :z 1))
    (loop for renderable in *renderables*
          do (aw:transform-entity *engine* renderable transform)))

  (with-transform (transform
                   (:translation :x -1.0 :y -1 :z -1)
                   (:rotation (/ pi 2) :x 1 :y 0 :z 0)
                   (:scale :x 2 :y 2 :z 2))
    (when *banner*
      (aw:transform-entity *engine* *banner* transform)))

  (aw:in-frame (*engine*)
    (render-tools *tools*))

  (sleep 0.015))


(defvar *surface* nil)

(defun run (scene skybox environment audio game-controller-db)
  (setf *scene* scene
        *skybox* skybox
        *environment* environment
        *audio* audio)
  (handler-bind ((serious-condition (lambda (c)
                                      (format *error-output* "~%Unhandled serious condition:~%")
                                      (dissect:present c *error-output*))))
    (dissect:with-capped-stack ()
      (float-features:with-float-traps-masked t
        (print "Initializing host")
        (aw:with-window (win :context context)
          (let* ((width (aw:window-width win))
                 (height (aw:window-height win)))
            (shout "Initializing audio")
            (aw:with-audio ()
              (shout "Initializing renderer")
              (aw:with-engine (engine :surface (aw:window-surface win)
                                      :shared-context context
                                      :width width
                                      :height height)
                (shout "Alien-Works ready")
                (let* ((*engine* engine))
                  (with-tools (:alien-works-demo :engine engine
                                                 :controller-db-path game-controller-db)
                    (init-loop)
                    (shout "Demo ready")
                    (unwind-protect
                         (catch 'quit
                           (shout "Looping")
                           (loop
                             (tagbody start
                                (restart-case
                                    (handle-loop)
                                  (restart-loop ()
                                    :report "Restart alien-works demo loop"
                                    (go start))))))
                      (destroy-loop))))))))))))


(defun asset-path (asset-name)
  (cond
    ((member :android *features*) asset-name)
    ((member :appimage *features*) (merge-pathnames asset-name (merge-pathnames
                                                                "usr/share/app/"
                                                                (aw:working-directory))))
    ((or (member :msix *features*)
         (member :awd-archive *features*))
     (merge-pathnames asset-name (merge-pathnames
                                  "rsc/"
                                  (aw:working-directory))))
    (t (asdf:system-relative-pathname :alien-works-demo
                                      (merge-pathnames asset-name "assets/")))))

(aw:definit main ()
  (reload-foreign-libraries)
  (run (asset-path "helmet.bin")
       (asset-path "skybox.bin")
       (asset-path "indirect.bin")
       (asset-path "audio.bin")
       (asset-path "gamecontrollerdb.txt")))


;; to call from native SDL2 loop, e.g. on android
(cffi:defcallback %run-alien-works :void ()
  (print "Callback called")
  (alien-works:run))


(defun alien-works-runner-pointer ()
  (cffi:pointer-address (cffi:callback %run-alien-works)))
