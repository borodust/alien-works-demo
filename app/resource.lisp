(cl:in-package :alien-works-demo)

;;;
;;; RESOURCE TABLE
;;;
(declaim (special *resource-table*))


(defun find-resource (name)
  (alexandria:if-let (resource (gethash name *resource-table*))
    (values (third resource) (fourth resource))
    (error "Resource ~A not found" name)))


;;;
;;; RESOURCE
;;;
(defclass resource ()
  ((kind :initarg :kind :initform (error ":kind missing") :reader resource-kind)
   (name :initarg :name :initform (error ":name missing") :reader resource-name)))


(defgeneric decode-resource (kind name descriptor data-ptr data-size))
(defgeneric encode-resource (resource))
(defgeneric forge-resource (resource))


(defclass blob-resource (resource)
  ((data :initarg :data :initform (error ":data missing"))
   (size :initarg :size :initform (error ":size missing"))))


(defun make-blob-resource (class name data-ptr data-size)
  (make-instance class :name name :data data-ptr :size data-size))


(defmethod encode-resource ((this blob-resource))
  (with-slots (data size) this
    (values nil data size)))


;;;
;;; MESH
;;;
(defmacro do-buffer-descriptor (((kind type size &rest keys) descriptor) &body body)
  (alexandria:with-gensyms (el)
    `(loop for ,el in ,descriptor
           do (destructuring-bind (,kind ,type ,size &key ,@keys &allow-other-keys) ,el
                ,@body))))


(defun parse-buffer-descriptor (descriptor)
  (flet ((%type-size (type)
           (ecase type
             ((:byte :ubyte) 1)
             ((:ushort :short) 2)
             (:float 4)
             ((:uint :int) 4)))
         (%convert-type (type size)
           (alexandria:eswitch ((list type size) :test #'equal)
             ('(:byte 1) :byte)
             ('(:byte 2) :byte2)
             ('(:byte 3) :byte3)
             ('(:byte 4) :byte4)
             ('(:ubyte 1) :ubyte)
             ('(:ubyte 2) :ubyte2)
             ('(:ubyte 3) :ubyte3)
             ('(:ubyte 4) :ubyte4)
             ('(:short 1) :short)
             ('(:short 2) :short2)
             ('(:short 3) :short3)
             ('(:short 4) :short4)
             ('(:ushort 1) :ushort)
             ('(:ushort 2) :ushort2)
             ('(:ushort 3) :ushort3)
             ('(:ushort 4) :ushort4)
             ('(:int 1) :int)
             ('(:int 2) :int2)
             ('(:int 3) :int3)
             ('(:int 4) :int4)
             ('(:uint 1) :uint)
             ('(:uint 2) :uint2)
             ('(:uint 3) :uint3)
             ('(:uint 4) :uint4)
             ('(:float 1) :float)
             ('(:float 2) :float2)
             ('(:float 3) :float3)
             ('(:float 4) :float4))))
    (let ((total-byte-size 0))
      (do-buffer-descriptor ((kind type size) descriptor)
        (declare (ignore kind))
        (incf total-byte-size (* (%type-size type) size)))
      (let ((current-offset 0)
            (opts)
            (uv-counter 0))
        (flet ((%opt (el)
                 (push el opts)))
          (do-buffer-descriptor ((kind type size) descriptor)
            (let ((complex-type (%convert-type type size))
                  (byte-size (* (%type-size type) size)))
              (case kind
                (:position (%opt (aw:.attribute :position complex-type current-offset total-byte-size)))
                (:tangent
                 (%opt (aw:.attribute :tangents complex-type current-offset total-byte-size))
                 (%opt (aw:.normalized :tangents t)))
                (:color
                 (%opt (aw:.attribute :color complex-type current-offset total-byte-size))
                 (%opt (aw:.normalized :color t)))
                (:uv
                 (let ((uv (ecase uv-counter
                             (0 :uv0)
                             (1 :uv1))))
                   (%opt (aw:.attribute uv complex-type current-offset total-byte-size))
                   (incf uv-counter)))
                (:index
                 (%opt (aw:.type complex-type))))
              (incf current-offset byte-size))))
        (values (nreverse opts) total-byte-size)))))


(defclass buffer-resource (resource)
  ((descriptor :initarg :descriptor)
   (data :initarg :data)
   (size :initarg :size :reader buffer-resource-size)))


(defmethod encode-resource ((this buffer-resource))
  (with-slots (descriptor data size) this
    (values descriptor data size)))


(defun make-buffer (class name descriptor data-ptr data-size)
  (make-instance class
                 :name name
                 :descriptor descriptor
                 :data data-ptr
                 :size data-size))


(defclass vertex-buffer-resource (buffer-resource) ()
  (:default-initargs :kind :vertex-buffer))


(defun make-vertex-buffer-resource (name descriptor data-ptr data-size)
  (make-buffer 'vertex-buffer-resource name descriptor data-ptr data-size))


(defmethod decode-resource ((this (eql :vertex-buffer)) name descriptor data-ptr data-size)
  (make-vertex-buffer-resource name descriptor data-ptr data-size))


(defmethod forge-resource ((this vertex-buffer-resource))
  (with-slots (descriptor data size) this
    (multiple-value-bind (opts vertex-size)
        (parse-buffer-descriptor descriptor)
      (let ((buf (apply #'aw:make-vertex-buffer
                        *engine*
                        (/ size vertex-size)
                        opts)))
        (aw:fill-vertex-buffer *engine* buf data size)
        buf))))


(defclass index-buffer-resource (buffer-resource) ()
  (:default-initargs :kind :index-buffer))


(defun make-index-buffer-resource (name descriptor data-ptr data-size)
  (make-buffer 'index-buffer-resource name descriptor data-ptr data-size))


(defmethod decode-resource ((this (eql :index-buffer)) name descriptor data-ptr data-size)
  (make-index-buffer-resource name descriptor data-ptr data-size))


(defmethod forge-resource ((this index-buffer-resource))
  (with-slots (descriptor data size) this
    (multiple-value-bind (opts index-size)
        (parse-buffer-descriptor descriptor)
      (let ((buf (apply #'aw:make-index-buffer
                        *engine*
                        (/ size index-size)
                        opts)))
        (aw:fill-index-buffer *engine* buf data size)
        buf))))

;;;
;;; TEXTURE
;;;
(defclass pixel-buffer-resource (buffer-resource) ()
  (:default-initargs :kind :pixel-buffer))


(defun make-pixel-buffer-resource (name descriptor data-ptr data-size)
  (make-buffer 'pixel-buffer-resource name descriptor data-ptr data-size))


(defmethod decode-resource ((this (eql :pixel-buffer)) name descriptor data-ptr data-size)
  (make-pixel-buffer-resource name descriptor data-ptr data-size))


(defmethod forge-resource ((this pixel-buffer-resource))
  (with-slots (descriptor data size) this
    (aw:make-pixel-buffer data
                          size
                          (ecase (second (first descriptor))
                            (1 :r)
                            (2 :rg)
                            (3 :rgb)
                            (4 :rgba))
                          :ubyte)))


(defclass texture-resource (resource)
  ((width :initarg :width)
   (height :initarg :height)
   (format :initarg :format)
   (sampler :initarg :sampler :initform :2d)
   (pixel-buffer :initarg :pixel-buffer :initform nil))
  (:default-initargs :kind :texture))


(defun make-texture-resource (name width height format &key pixel-buffer sampler)
  (make-instance 'texture-resource :name name
                                   :width width
                                   :height height
                                   :format format
                                   :sampler (or sampler :2d)
                                   :pixel-buffer pixel-buffer))


(defmethod encode-resource ((this texture-resource))
  (with-slots (width height format pixel-buffer sampler) this
    (values `(:width ,width
              :height ,height
              :format ,format
              ,@(when pixel-buffer
                  `(:pixel-buffer ,pixel-buffer))
              ,@(when sampler
                  `(:sampler ,sampler)))
            nil 0)))


(defmethod decode-resource ((this (eql :texture)) name descriptor data-ptr data-size)
  (declare (ignore data-ptr data-size))
  (destructuring-bind (&key width height format pixel-buffer sampler) descriptor
    (make-texture-resource name width height format :pixel-buffer pixel-buffer
                                                    :sampler sampler)))


(defmethod forge-resource ((this texture-resource))
  (with-slots (descriptor width height format pixel-buffer sampler) this
    (let ((tex (aw:make-texture *engine*
                                (aw:.width width)
                                (aw:.height height)
                                (aw:.format format)
                                (aw:.sampler sampler))))
      (when pixel-buffer
        (multiple-value-bind (forged-pb resource-pb) (find-resource pixel-buffer)
          (ecase sampler
            (:2d (aw:update-texture-image *engine* tex 0 forged-pb))
            (:cubemap (aw:update-cubemap-images *engine* tex 0 forged-pb
                                                (/ (buffer-resource-size resource-pb) 6))))
          (aw:generate-texture-mipmaps *engine* tex)))
      tex)))

;;;
;;; MATERIAL
;;;
(defclass material-resource (blob-resource) ()
  (:default-initargs :kind :material))


(defun make-material-resource (name data size)
  (make-blob-resource 'material-resource name data size))


(defmethod decode-resource ((kind (eql :material)) name descriptor data-ptr data-size)
  (declare (ignore descriptor))
  (make-material-resource name data-ptr data-size))


(defmethod forge-resource ((this material-resource))
  (with-slots (data size) this
    (aw:make-material-from-memory *engine* data size)))



;;;
;;; RENDERABLE
;;;
(defclass renderable-resource (resource)
  ((material :initarg :material)
   (samplers :initarg :samplers)
   (geometry :initarg :geometry))
  (:default-initargs :kind :renderable))


(defun make-renderable-resource (name material geometry samplers)
  (make-instance 'renderable-resource :name name
                                      :material material
                                      :geometry geometry
                                      :samplers samplers))


(defmethod encode-resource ((this renderable-resource))
  (with-slots (material samplers geometry) this
    (values `(:material ,material :samplers ,samplers :geometry ,geometry) nil 0)))


(defmethod decode-resource ((this (eql :renderable)) name descriptor data-ptr data-size)
  (declare (ignore data-ptr data-size))
  (destructuring-bind (&key material samplers geometry) descriptor
    (make-renderable-resource name material geometry samplers)))


(defmethod forge-resource ((this renderable-resource))
  (with-slots (material samplers geometry) this
    (let* ((mat-instance (aw:make-material-instance (find-resource material)))
           (sampler (aw:make-sampler))
           renderable-opts)
      (flet ((%add-renderable-opts (&rest opts)
               (loop for opt in opts
                     do (push opt renderable-opts))))
        (loop for (name texture) in samplers
              do (setf
                  (aw:material-instance-parameter-sampler mat-instance
                                                          name
                                                          (find-resource texture))
                  sampler))
        (loop for geometry-desc in geometry
              for idx from 0
              do (destructuring-bind (vertex-buffer index-buffer) geometry-desc
                   (%add-renderable-opts (aw:.geometry idx :triangles
                                                       (find-resource vertex-buffer)
                                                       (find-resource index-buffer))
                                         (aw:.material idx
                                                       mat-instance))))
        (apply #'aw:make-renderable *engine* (length geometry)
               (aw:.culling nil)
               (aw:.receive-shadows nil)
               (aw:.cast-shadows nil)
               renderable-opts)))))


;;;
;;; RESOURCE PACKAGE
;;;
(defun save-resources (path &rest resources)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
    (let ((total-size 0)
          preamble
          data-list)
      (loop for resource in resources
            do (multiple-value-bind (descriptor data size) (encode-resource resource)
                 (push (nconc
                        (list (resource-kind resource)
                              (resource-name resource))
                        (when (and size (> size 0))
                          (list :size size))
                        (when descriptor
                          (list :descriptor descriptor)))
                       preamble)
                 (push (cons data size) data-list)
                 (incf total-size size)))
      (alexandria:nreversef preamble)
      (alexandria:nreversef data-list)
      (let ((preamble-out (flexi-streams:make-flexi-stream out :external-format :utf8)))
        (prin1 preamble preamble-out))
      (when (> total-size 0)
        (static-vectors:with-static-vector (total-data total-size :element-type '(unsigned-byte 8))
          (loop with ptr = (static-vectors:static-vector-pointer total-data)
                for (data . size) in data-list
                when (> size 0)
                  do (when data
                       (aw:memcpy ptr data size))
                     (cffi:incf-pointer ptr size))
          (write-sequence total-data out)
          (finish-output out))))))


(defun read-into-foreign-memory (dst-ptr size octet-stream)
  (let ((buffer-size (* 1024 1024)))
    (static-vectors:with-static-vector (buffer buffer-size :element-type '(unsigned-byte 8))
      (let ((buffer-ptr (static-vectors:static-vector-pointer buffer)))
        (loop with size-read = 0
              with current-ptr = (cffi:make-pointer (cffi:pointer-address dst-ptr))
              while (< size-read size)
              do (let* ((bytes-to-read (min (- size size-read) buffer-size))
                        (bytes-read (read-sequence buffer octet-stream :start 0 :end bytes-to-read)))
                   (aw:memcpy current-ptr buffer-ptr bytes-read)
                   (incf size-read bytes-read)
                   (cffi:incf-pointer current-ptr bytes-read)))))))


(defun load-resources (path)
  (aw:with-open-host-file (out path :direction :input)
    (let* ((preamble-out (flexi-streams:make-flexi-stream out :external-format :utf8))
           (preamble (let ((*read-eval* nil))
                       (read-preserving-whitespace preamble-out))))
      (loop for encoded-resource in preamble
            collect (destructuring-bind (kind name &key size descriptor) encoded-resource
                      (let ((data-size 0)
                            data-ptr)
                        (when (and size (> size 0))
                          (setf data-ptr (cffi:foreign-alloc :uint8 :count size)
                                data-size size)
                          (read-into-foreign-memory data-ptr size out))
                        (decode-resource kind name
                                         descriptor
                                         data-ptr
                                         data-size)))))))


(defun forge-resources (resources)
  (let ((*resource-table* (make-hash-table :test 'equal)))
    (loop for resource in resources
          for name = (resource-name resource)
          for forged = (list (resource-kind resource) name (forge-resource resource) resource)
          collect (setf (gethash name *resource-table*) forged))))

;;;
;;; AUDIO RESOURCE
;;;
(defclass audio-resource (resource)
  ((encoded :initarg :encoded :initform (error ":encoded missing")))
  (:default-initargs :kind :audio))


(defun make-audio-resource (name encoded)
  (make-instance 'audio-resource
                 :name name
                 :encoded (static-vectors:make-static-vector
                           (length encoded)
                           :element-type '(unsigned-byte 8)
                           :initial-contents encoded)))


(defmethod encode-resource ((resource audio-resource))
  (with-slots (encoded) resource
    (values nil
            (static-vectors:static-vector-pointer encoded)
            (length encoded))))


(defmethod decode-resource ((kind (eql :audio)) name descriptor data-ptr data-size)
  (declare (ignore kind descriptor))
  (let ((data (static-vectors:make-static-vector data-size
                                                 :element-type '(unsigned-byte 8))))
    (aw:memcpy (static-vectors:static-vector-pointer data)
               data-ptr
               data-size)
    (make-instance 'audio-resource
                   :name name
                   :encoded data)))


(defmethod forge-resource ((resource audio-resource))
  (with-slots (encoded) resource
    (flex:with-input-from-sequence (in encoded)
      (aw:decode-audio in))))
