(cl:in-package :alien-works-demo.support)

(defparameter *banner-material-source*
"
material {
    name : banner,
    parameters: [
      { type: sampler2d, name: banner }
    ],
    requires : [
      uv0
    ],
    shadingModel : unlit,
    culling : none,
    blending : transparent
}
fragment {
    void material(inout MaterialInputs material) {
        prepareMaterial(material);
        material.baseColor = textureLod(materialParams_banner, getUV0(), 0.0);
    }
}
")


(cffi:defcstruct banner-vertex
  (pos :float :count 2)
  (uv :float :count 2))


(cffi:defcstruct banner-data
  (vertices (:struct banner-vertex) :count 4)
  (indices :uint16 :count 6))


(defun make-banner-data ()
  (flet ((init-vertex (vertex x y)
           (cref:c-val ((vertex (:struct banner-vertex)))
             (setf (vertex :pos 0) (float x 0f0)
                   (vertex :pos 1) (float y 0f0)

                   (vertex :uv 0) (float x 0f0)
                   (vertex :uv 1) (float y 0f0)))))
    (cref:c-let ((data (:struct banner-data) :alloc t))
      (init-vertex (data :vertices 0 &)
                   0 0)
      (init-vertex (data :vertices 1 &)
                   1 0)
      (init-vertex (data :vertices 2 &)
                   1 1)
      (init-vertex (data :vertices 3 &)
                   0 1)

      (setf (data :indices 0) 0
            (data :indices 1) 1
            (data :indices 2) 2

            (data :indices 3) 0
            (data :indices 4) 2
            (data :indices 5) 3)
      (data &))))


(defun make-banner-material ()
  (aws:make-material alien-works-demo::*engine* *banner-material-source*))


(defclass banner ()
  ((sampler)
   (data)
   (mat)
   (mat-instance)
   (renderable)
   (vbuf)
   (ibuf)))


(defun (setf banner-texture) (texture banner)
  (with-slots (mat-instance sampler) banner
    (setf (aw:material-instance-parameter-sampler mat-instance "banner" texture) sampler)))


(defmethod initialize-instance :after ((this banner) &key)
  (with-slots (data entity vbuf ibuf mat renderable sampler mat-instance) this
    (let ((vertex-size (cffi:foreign-type-size '(:struct banner-vertex)))
          (index-size (cffi:foreign-type-size :uint16))
          (pos-size (* 2 (cffi:foreign-type-size :float))))
      (setf vbuf (aw:make-vertex-buffer alien-works-demo::*engine* 4
                                        (aw:.attribute :position :float2 0 vertex-size)
                                        (aw:.attribute :uv0 :float2 pos-size vertex-size))
            ibuf (aw:make-index-buffer alien-works-demo::*engine* 6
                                       (aw:.type :ushort))
            data (make-banner-data)
            mat (make-banner-material)
            sampler (aw:make-sampler)
            mat-instance (aw:make-material-instance mat))
      (cref:c-val ((data (:struct banner-data)))
        (aw:fill-vertex-buffer alien-works-demo::*engine* vbuf (data :vertices &)
                               (* 4 vertex-size))
        (aw:fill-index-buffer alien-works-demo::*engine* ibuf
                              (data :indices &)
                              (* 6 index-size)))
      (setf renderable (aw:make-renderable alien-works-demo::*engine* 1
                                           (aw:.bounding-box 0 0 0 1 1 1)
                                           (aw:.culling nil)
                                           (aw:.receive-shadows nil)
                                           (aw:.cast-shadows nil)

                                           (aw:.geometry 0 :triangles vbuf ibuf)
                                           (aw:.material 0 mat-instance))))))

(defun banner-entity (banner)
  (slot-value banner 'renderable))

(defun make-banner ()
  (make-instance 'banner))
