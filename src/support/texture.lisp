(cl:in-package :alien-works-demo.support)


(defun load-cubemap (px-path nx-path py-path ny-path pz-path nz-path)
  (let* ((images (loop for path in (list px-path nx-path py-path ny-path pz-path nz-path)
                       collect (aws:load-image (file-namestring path) path)))
         (width (aws:image-width (first images)))
         (height (aws:image-height (first images)))
         (channels (aws:image-channels (first images)))
         (sizes (loop for image in images
                      unless (and (= (aws:image-width image) width)
                                  (= (aws:image-height image) height)
                                  (= (aws:image-channels image) channels))
                        do (error "Cubemap face image with wrong dimensions found")
                      collect (* (aws:image-width image)
                                 (aws:image-height image)
                                 (aws:image-channels image))))
         (total-size (reduce #'+ sizes))
         (data (let ((data (cffi:foreign-alloc :char :count total-size)))
                 (loop with offset = 0
                       for size in sizes
                       for image in images
                       do (aw:memcpy (cffi:inc-pointer data offset) (aws:image-data image) size)
                          (incf offset size))
                 data))
         (pixel-buffer (aw:make-pixel-buffer data
                                             total-size
                                             (ecase channels
                                               (1 :r)
                                               (2 :rg)
                                               (3 :rgb)
                                               (4 :rgba))
                                             :ubyte
                                             (lambda () (cffi:foreign-free data))))
         (texture (aw:make-texture alien-works-demo::*engine*
                                   (aw:.width width)
                                   (aw:.height height)
                                   (aw:.format (ecase channels
                                                 (1 :r8)
                                                 (2 :rg8)
                                                 (3 :rgb8)
                                                 (4 :rgba8)))
                                   (aw:.sampler :cubemap))))
    (unwind-protect
         (progn
           (apply #'aw:update-cubemap-images alien-works-demo::*engine* texture 0 pixel-buffer sizes)
           (aw:generate-texture-mipmaps alien-works-demo::*engine* texture))
      (aw:destroy-pixel-buffer pixel-buffer))
    texture))
