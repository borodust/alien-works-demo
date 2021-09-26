(cl:in-package :alien-works-demo)


(defun asset-image (asset-path &key name)
  (let ((name (if name name (file-namestring asset-path))))
    (alien-works.tools:load-image name (asset-path asset-path))))

;;;
;;; DEMO SUPPORT
;;;
(defun load-banner ()
  (let* ((banner (alien-works-demo.tools:make-banner))
         (entity (alien-works-demo.tools:banner-entity banner)))
    (aw:add-scene-entity *engine* entity)
    (setf *banner* entity)
    banner))


(defun update-banner-texture (banner surface)
  (let ((tex (alien-works.graphics::buffered-surface-acquire surface)))
    (unwind-protect
         (progn
           (setf (alien-works-demo.tools:banner-texture banner) tex))
      (alien-works.graphics::buffered-surface-release surface))))


(defun convert-helmet ()
  (let ((resources (alien-works-demo.tools::parse-gltf
                    (asset-path "src/helmet/DamagedHelmet.gltf"))))
    (apply #'alien-works-demo::save-resources (asset-path "helmet.bin") resources)))


(defun convert-mountain-to-cubemap ()
  (alien-works.tools:images-to-cubemap-cross
   (asset-path "src/skybox/posx.jpg")
   (asset-path "src/skybox/negx.jpg")
   (asset-path "src/skybox/posy.jpg")
   (asset-path "src/skybox/negy.jpg")
   (asset-path "src/skybox/posz.jpg")
   (asset-path "src/skybox/negz.jpg")
   (asset-path "local/skybox.png")))


(defun generate-indirect-light-map (&key (cmgen "cmgen"))
  (uiop:run-program `(,cmgen
                      ,(format nil "--deploy=~A" (namestring (asset-path "src/indirect/")))
                      "--format=png"
                      ,(namestring (asset-path "local/skybox.png")))
                    :force-shell t
                    :error-output *error-output*))


(defun convert-skybox ()
  (let ((resources (alien-works-demo.tools::cubemap->resources
                    "skybox"
                    (asset-image "src/skybox/posx.jpg")
                    (asset-image "src/skybox/negx.jpg")
                    (asset-image "src/skybox/posy.jpg")
                    (asset-image "src/skybox/negy.jpg")
                    (asset-image "src/skybox/posz.jpg")
                    (asset-image "src/skybox/negz.jpg"))))
    (apply #'alien-works-demo::save-resources (asset-path "skybox.bin") resources)))

(defun convert-indirect ()
  (let ((resources (alien-works-demo.tools::cubemap->resources
                    "indirect"
                    (asset-image "src/indirect/skybox/m1_px.png")
                    (asset-image "src/indirect/skybox/m1_nx.png")
                    (asset-image "src/indirect/skybox/m1_py.png")
                    (asset-image "src/indirect/skybox/m1_ny.png")
                    (asset-image "src/indirect/skybox/m1_pz.png")
                    (asset-image "src/indirect/skybox/m1_nz.png"))))
    (apply #'alien-works-demo::save-resources (asset-path "indirect.bin") resources)))


(defun convert-audio ()
  (alexandria:with-input-from-file (in (asset-path "src/audio/sample.raw")
                                       :element-type '(signed-byte 16))
    (apply #'alien-works-demo::save-resources (asset-path "audio.bin")
           (alien-works-demo.tools::pcm->resources "booo" in))))


(defun update-assets ()
  (convert-helmet)
  (convert-skybox)
  (convert-indirect)
  (convert-audio))
