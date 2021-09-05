(cl:in-package :alien-works-demo)


;;;
;;; DEMO SUPPORT
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


(defun convert-helmet ()
  (let ((resources (alien-works-demo.support::parse-gltf
                    (asset-path "helmet/DamagedHelmet.gltf"))))
    (apply #'alien-works-demo::save-resources (asset-path "helmet.bin") resources)))
