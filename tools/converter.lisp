(cl:in-package :alien-works-demo.tools)


(defun parse-material-resource (name source &optional base-path)
  (multiple-value-bind (data size)
      (awt:parse-material source base-path)
    (alien-works-demo::make-material-resource name data size)))


(defun format-gltf-material-source (&key albedo
                                      emissive
                                      metallic
                                      roughness
                                      normal
                                      ambient-occlusion
                                      transparent)
  (let (params
        invocations
        normal-invocation)
    (flet ((%add-sampler-parameter (name)
             (pushnew (format nil "{ type: sampler2d, name: ~A }" name) params :test #'string=))
           (%add-invocation (invocation name)
             (push (format nil invocation name) invocations)))
      (when albedo
        (%add-sampler-parameter albedo)
        (%add-invocation "material.baseColor = texture(materialParams_~A, getUV0());" albedo))

      (when emissive
        (%add-sampler-parameter emissive)
        (%add-invocation "material.baseColor.rgb += texture(materialParams_~A, getUV0()).rgb;" emissive))

      (when metallic
        (%add-sampler-parameter metallic)
        (%add-invocation "material.metallic = texture(materialParams_~A, getUV0()).b;"
                         metallic))

      (when roughness
        (%add-sampler-parameter roughness)
        (%add-invocation "material.roughness = texture(materialParams_~A, getUV0()).g;"
                         roughness))

      (when normal
        (%add-sampler-parameter normal)
        (setf
         normal-invocation
         (format nil "material.normal = texture(materialParams_~A, getUV0()).xyz * 2.0 - 1.0;"
                 normal)))

      (when ambient-occlusion
        (%add-sampler-parameter ambient-occlusion)
        (%add-invocation "material.ambientOcclusion = texture(materialParams_~A, getUV0()).r;"
                         ambient-occlusion))

      (let ((name (format nil "~{~A~}" (list albedo
                                             emissive
                                             metallic
                                             roughness
                                             normal
                                             ambient-occlusion
                                             transparent))))
        (values
         (with-output-to-string (out)
           (format out "material { name: ~A, parameters: [ ~{~A~^, ~} ], requires: [ uv0 ], shadingModel: lit, blending: ~A, transparency: twoPassesOneSide }"
                   name
                   (nreverse params)
                   (if transparent
                       "transparent"
                       "opaque"))

           (format out "
fragment {
    void material(inout MaterialInputs material) {
~@[~A~]
        prepareMaterial(material);
~{~A~^~%~}
    }
}
"
                   normal-invocation
                   (nreverse invocations)))
         name)))))


(defun cubemap->resources (name px-image nx-image py-image ny-image pz-image nz-image)
  (let* ((images (list px-image nx-image py-image ny-image pz-image nz-image))
         (width (awt:image-width (first images)))
         (height (awt:image-height (first images)))
         (channels (awt:image-channels (first images)))
         (sizes (loop for image in images
                      unless (and (= (awt:image-width image) width)
                                  (= (awt:image-height image) height)
                                  (= (awt:image-channels image) channels))
                        do (error "Cubemap face image with wrong dimensions found")
                      collect (* (awt:image-width image)
                                 (awt:image-height image)
                                 (awt:image-channels image))))
         (total-size (reduce #'+ sizes))
         (total-data (let ((data (cffi:foreign-alloc :char :count total-size)))
                       (loop with offset = 0
                             for size in sizes
                             for image in images
                             do (aw:memcpy (cffi:inc-pointer data offset) (awt:image-data image) size)
                                (incf offset size))
                       data))
         (pixel-buffer-name (format nil "pb:~A" name)))
    (list (alien-works-demo::make-pixel-buffer-resource pixel-buffer-name
                                      `((:ubyte ,channels))
                                      total-data
                                      total-size)
          (alien-works-demo::make-texture-resource name
                                 width
                                 height
                                 (ecase channels
                                   (1 :r8)
                                   (2 :rg8)
                                   (3 :rgb8)
                                   (4 :rgba8))
                                 :pixel-buffer pixel-buffer-name
                                 :sampler :cubemap))))


(defun image->resources (image)
  (let* ((name (awt:image-name image))
         (width (awt:image-width image))
         (height (awt:image-height image))
         (channels (awt:image-channels image))
         (size (* width height channels))
         (pixel-buffer-name (format nil "pb:~A" name)))
    (list (alien-works-demo::make-pixel-buffer-resource pixel-buffer-name
                                                        `((:ubyte ,channels))
                                                        (awt:image-data image)
                                                        size)
          (alien-works-demo::make-texture-resource name
                                                   width
                                                   height
                                                   (ecase channels
                                                     (1 :r8)
                                                     (2 :rg8)
                                                     (3 :rgb8)
                                                     (4 :rgba8))
                                                   :pixel-buffer pixel-buffer-name
                                                   :sampler :2d))))


(defun vertex-buffer->resources (vbuf name)
  (list (alien-works-demo::make-vertex-buffer-resource name
                                     (awt:buffer-descriptor vbuf)
                                     (awt:buffer-data vbuf)
                                     (awt:buffer-size vbuf))))


(defun index-buffer->resources (ibuf name)
  (list (alien-works-demo::make-index-buffer-resource name
                                                      (awt:buffer-descriptor ibuf)
                                                      (awt:buffer-data ibuf)
                                                      (awt:buffer-size ibuf))))


(defun parse-gltf (source-path)
  (let ((material-table (make-hash-table :test 'equal))
        (image-table (make-hash-table :test 'equal))
        resources)
    (labels ((add-resources (&rest rscs)
               (loop for resource in rscs
                     if (listp resource)
                       do (apply #'add-resources resource)
                     else
                       do (push resource resources)))
             (texture-name (mesh kind num)
               (alexandria:when-let ((tex (awt:material-texture (awt:mesh-material mesh) kind num)))
                 (awt:texture-name tex)))
             (texture-with-alpha-p (mesh kind num)
               (alexandria:when-let ((name (texture-name mesh kind num)))
                 (alexandria:when-let ((image (gethash name image-table)))
                   (= 4 (awt:image-channels image)))))
             (%add-material-resource (transparent samplers)
               (multiple-value-bind (material-source material-name)
                   (apply #'format-gltf-material-source :transparent transparent samplers)
                 (unless (gethash material-name material-table)
                   (let ((material (parse-material-resource material-name material-source)))
                     (setf (gethash material-name material-table) material)
                     (add-resources material)))
                 material-name))
             (%parse-material-config (mesh)
               (multiple-value-bind (samplers config)
                   (loop for (param-name gltf-kind material-kind)
                           in '(("albedo" :diffuse :albedo)
                                ("metallic" :unknown :metallic)
                                ("roughness" :unknown :roughness)
                                ("normal" :normals :normal)
                                ("ao" :lightmap :ambient-occlusion)
                                ("emissive" :emissive :emissive))
                         for tex-name = (texture-name mesh gltf-kind 0)
                         when tex-name
                           append (list material-kind param-name) into samplers
                           and collect (list param-name tex-name) into config
                         finally (return (values samplers config)))
                 (let ((transparent-p (texture-with-alpha-p mesh :diffuse 0)))
                   (values (%add-material-resource transparent-p samplers) config)))))
      (let ((scene (awt:parse-scene source-path)))
        (loop for image in (awt:scene-images scene)
              do (add-resources (image->resources image))
                 (setf (gethash (awt:image-name image) image-table) image))
        (loop for mesh in (awt:scene-meshes scene)
              for mesh-idx from 0
              for vbuf = (awt:mesh-vertex-buffer mesh)
              for ibufs = (awt:mesh-index-buffers mesh)
              for vbuf-name = (format nil "mesh.~A.vb" mesh-idx)
              for converted-vbuf = (vertex-buffer->resources vbuf vbuf-name)
              for converted-ibufs = (loop for ibuf in ibufs
                                          for ibuf-idx from 0
                                          for ibuf-name = (format nil "mesh.~A.ib.~A"
                                                                  mesh-idx ibuf-idx)
                                          collect (list
                                                   ibuf-name
                                                   (index-buffer->resources ibuf ibuf-name)))
              for ibuf-names = (mapcar #'first converted-ibufs)
              for geometry = (loop for ibuf-name in ibuf-names
                                   collect `(,vbuf-name ,ibuf-name))
              do (add-resources converted-vbuf (mapcar #'second converted-ibufs))
                 (multiple-value-bind (material-name material-config)
                     (%parse-material-config mesh)
                   (add-resources (alien-works-demo::make-renderable-resource
                                   (format nil "mesh.~A.renderable" mesh-idx)
                                   material-name
                                   geometry
                                   material-config))))))
    (nreverse resources)))


(defun convert-cubemap (target px-path nx-path py-path ny-path pz-path nz-path)
  (let ((rsc (apply #'cubemap->resources (file-namestring target)
                    (loop for path in (list px-path nx-path py-path ny-path pz-path nz-path)
                          collect (awt:load-image (file-namestring path) path)))))
    (apply #'alien-works-demo::save-resources target rsc)))
