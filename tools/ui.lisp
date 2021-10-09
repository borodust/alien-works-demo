(cl:in-package :alien-works-demo.tools)


(declaim (special *ui*
                  *tools*))



(defclass demo-tools ()
  ((ui :initarg :ui)
   (windows :initform (list :debug))
   (controller-table :initform (make-hash-table :test 'equal))
   (keyboard-button-bag :initform nil)
   (controller-button-bag :initform nil)
   (mouse-button-bag :initform nil)
   (last-time-delta :initform 0)))


(defmethod alien-works-demo::make-tools ((name (eql :alien-works-demo)) &key engine controller-db-path)
  (aw:with-open-host-file (file controller-db-path)
    (aw:load-gamepad-mappings-from-host-file file))
  (make-instance 'demo-tools
                 :ui (awt:make-ui engine)))


(defmethod alien-works-demo::destroy-tools ((tools demo-tools))
  (with-slots (ui) tools
    (awt:destroy-ui ui)))


(defmethod alien-works-demo::call-with-tools ((this demo-tools) body)
  (with-slots (ui) this
    (let ((*ui* ui))
      (funcall body))))


(defmethod alien-works-demo::handle-tool-event ((this demo-tools) event)
  (awt:handle-ui-event *ui* event))


(defmethod alien-works-demo::update-tools (tools time-delta)
  (with-slots (last-time-delta) tools
    (setf last-time-delta time-delta))
  (awt:update-ui-input *ui*))


(defun display-controller-panel (controller)
  (awt:label "Power Level:")
  (awt:same-line)
  (awt:label (string (aw:game-controller-power-level controller)))
  (loop for button-id below (aw:game-controller-button-count controller)
        do (awt:checkbox (format nil "Button #~A" button-id) (aw:game-controller-button-pressed-p controller button-id)))
  (loop for axis-id below (aw:game-controller-axes-count controller)
        do (awt:label "Axis #~A" (format nil "~A" axis-id))
           (awt:progress-bar (/ (1+ (aw:game-controller-axis-float-value controller axis-id)) 2)))
  (loop for hat-id below (aw:game-controller-hat-count controller)
        do (awt:label "Hat #~A:" (format nil "~A" hat-id))
           (awt:same-line)
           (awt:label (string (aw:game-controller-hat-value controller hat-id))))
  (aw:with-vec2 (ball-pos)
    (loop for ball-id below (aw:game-controller-ball-count controller)
          do (aw:game-controller-ball-value controller ball-id ball-pos)
             (awt:label "Ball #~A:" (format nil "~A" ball-id))
             (awt:same-line)
             (awt:label (format nil "(~A, ~A)" (aw:vec2 ball-pos 0) (aw:vec2 ball-pos 1))))))


(defun display-gamepad-panel (gamepad)
  (awt:label "Power Level:")
  (awt:same-line)
  (awt:label (string (aw:gamepad-power-level gamepad)))
  (when (aw:gamepad-haptic-p gamepad)
    (when (awt:button "Rumble")
      (bt:make-thread
       (lambda ()
         (let ((haptic (aw:grab-gamepad-haptic-device gamepad)))
           (unwind-protect
                (progn
                  (aw:add-rumble haptic)
                  (aw:play-rumble haptic 0.5 1000)
                  (sleep 1))
             (aw:release-haptic-device haptic)))))))
  (loop for button in '(:a :b :x :y
                        :back :guide :start
                        :leftstick :rightstick :leftshoulder :rightshoulder
                        :dpad-up :dpad-down :dpad-left :dpad-right
                        :misc1 :touchpad
                        :paddle1 :paddle2 :paddle3 :paddle4)
        do (awt:checkbox (string button) (aw:gamepad-button-pressed-p gamepad button)))
  (loop for stick in '(:leftx :lefty :rightx :righty)
        do (awt:label (string stick))
           (awt:progress-bar (/ (1+ (aw:gamepad-axis-float-value gamepad stick)) 2)))
  (loop for trigger in '(:triggerleft :triggerright)
        do (awt:label (string trigger))
           (awt:progress-bar (aw:gamepad-axis-float-value gamepad trigger))))


(defun request-tool-window (panel-id)
  (with-slots (windows) *tools*
    (pushnew panel-id windows)))


(defmacro with-tool-window ((panel-id panel-name &key on-close) &body body)
  (a:with-gensyms (result)
    (a:once-only (panel-id panel-name)
      `(with-slots (windows) *tools*
         (when (member ,panel-id windows :test #'equal)
           (awt:with-panel (,panel-name
                            :on-close (lambda (,result)
                                        (declare (ignore ,result))
                                        ,@(when on-close
                                            `((funcall ,on-close)))
                                        (a:deletef windows ,panel-id :test #'equal)))
             ,@body))))))


(defun display-audio-section ()
  (when (awt:button "Play")
    (alien-works-demo::play-boo)))


(defun display-game-controllers-section ()
  (with-slots (controller-table) *tools*
    (awt:with-tree-node ("Generic")
      (aw:do-game-controller-ids (cid)
        (let ((controller-name (aw:game-controller-name-by-id cid)))
          (when (awt:selectable controller-name)
            (setf (gethash (list :controller cid) controller-table)  (aw:grab-game-controller cid))
            (request-tool-window (list :controller cid)))

          (a:when-let (controller (gethash (list :controller cid) controller-table))
            (with-tool-window ((list :controller cid) controller-name
                               :on-close (lambda () (aw:release-game-controller controller)))
              (display-controller-panel controller))))))

    (awt:with-tree-node ("Gamepads")
      (aw:do-gamepad-ids (gid)
        (let ((gamepad-name (aw:gamepad-name-by-id gid)))
          (when (awt:selectable gamepad-name)
            (setf (gethash (list :gamepad gid) controller-table) (aw:grab-gamepad gid))
            (request-tool-window (list :gamepad gid)))

          (a:when-let (gamepad (gethash (list :gamepad gid) controller-table))
            (with-tool-window ((list :gamepad gid) gamepad-name
                               :on-close (lambda () (aw:release-gamepad gamepad)))
              (display-gamepad-panel gamepad))))))))


(defun display-misc-panel ()
  (with-slots (windows) *tools*
    (when (awt:button "Demo UI")
      (pushnew :imgui windows))

    (when (and (member :imgui windows)
               (%alien-works.tools.imgui::show-demo-window))
      (a:deletef windows :imgui))))


(defun display-debug-panel ()
  (when (awt:collapsing-header "Audio")
    (display-audio-section))

  (when (awt:collapsing-header "Game Controllers")
    (display-game-controllers-section))

  (when (awt:collapsing-header "Misc")
    (display-misc-panel)))


(defmethod alien-works-demo::render-tools (tools)
  (with-slots (last-time-delta windows) tools
    (let ((*tools* tools))
      (awt:ui (*ui* 1280 960 last-time-delta)
        (with-tool-window (:debug "Alien-Works Demo")
                          (display-debug-panel))))

    (setf last-time-delta 0)))
