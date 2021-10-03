(cl:in-package :alien-works-demo)


(declaim (special *ui*))

(defclass demo-tools ()
  ((ui :initarg :ui)
   (debug-window-visible :initform t)
   (last-time-delta :initform 0)))


(defmethod make-tools ((name (eql :alien-works-demo)) &key engine)
  (make-instance 'demo-tools
                 :ui (alien-works.tools:make-ui engine)))


(defmethod destroy-tools ((tools demo-tools))
  (with-slots (ui) tools
    (alien-works.tools:destroy-ui ui)))


(defmethod call-with-tools ((this demo-tools) body)
  (with-slots (ui) this
    (let ((*ui* ui))
      (funcall body))))


(defmethod handle-tool-event ((this demo-tools) event)
  (alien-works.tools:handle-ui-event *ui* event))


(defmethod update-tools (tools time-delta)
  (with-slots (last-time-delta) tools
    (setf last-time-delta time-delta))
  (alien-works.tools:update-ui-input *ui*))


(defun display-debug-panel ()
  (when (alien-works.tools:button "BOOO")
    (play-boo)))


(defmethod render-tools (tools)
  (with-slots (last-time-delta debug-window-visible) tools
    (alien-works.tools:ui (*ui* 1280 960 last-time-delta)
      (flet ((hide-debug-window ()
               (setf debug-window-visible nil)))
        (when debug-window-visible
          (%alien-works.tools.imgui::show-demo-window)
          (alien-works.tools:with-panel ("YO" :on-close #'hide-debug-window)
            (display-debug-panel)))))

    (setf last-time-delta 0)))
