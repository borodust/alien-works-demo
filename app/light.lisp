(cl:in-package :alien-works-demo)


(defun add-light (position color)
  (let ((light (aw:make-light *engine* :point
                              (aw:.position position)
                              (aw:.color color)
                              (aw:.falloff 1000f0)
                              (aw:.intensity 100000f0))))
    (aw:add-scene-entity *engine* light)
    (push light *lights*)))


(defun add-sun (direction)
  (let ((sun (aw:make-light *engine* :sun
                            (aw:.direction direction))))
    (aw:add-scene-entity *engine* sun)
    sun))


(defun add-indirect-light (cubemap)
  (let ((light (aw:make-indirect-light *engine*
                                       (aw:.reflections cubemap)
                                       (aw:.intensity 35000f0))))
    (setf (aw:indirect-light *engine*) light)))
