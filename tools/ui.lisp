(cl:in-package :alien-works-demo.tools)


(declaim (special *ui*
                  *tools*))


(defun format-vec2 (vec2)
  (format nil "(~,2F, ~,2F)" (aw:vec2 vec2 0) (aw:vec2 vec2 1)))


(defun format-vec3 (vec3)
  (format nil "(~,2F, ~,2F, ~,2F)" (aw:vec3 vec3 0) (aw:vec3 vec3 1) (aw:vec3 vec3 2)))


(defclass demo-tools ()
  ((ui :initarg :ui)
   (windows :initform (list :debug))
   (controller-table :initform (make-hash-table :test 'equal))
   (button-bag :initform nil)
   (mouse-state :initform (aw:make-mouse-state))
   (booo-source)
   (music-source)
   (last-time-delta :initform 0)))


(defmethod alien-works-demo::make-tools ((name (eql :alien-works-demo)) &key engine controller-db-path)
  (aw:with-open-host-file (file controller-db-path )
    (aw:load-gamepad-mappings-from-host-file file))

  (make-instance 'demo-tools
                 :ui (awt:make-ui engine)))


(defmethod alien-works-demo::init-tools ((tools demo-tools))
  (with-slots (booo-source music-source) tools
    (setf booo-source (aw:make-audio-source alien-works-demo::*booo*)
          music-source (aw:make-audio-source alien-works-demo::*music*))))


(defmethod alien-works-demo::destroy-tools ((tools demo-tools))
  (with-slots (ui booo-source music-source) tools
    (awt:destroy-ui ui)
    (aw:destroy-audio-source booo-source)
    (aw:destroy-audio-source music-source)))


(defmethod alien-works-demo::call-with-tools ((this demo-tools) body)
  (with-slots (ui) this
    (let ((*ui* ui))
      (funcall body))))


(defmethod alien-works-demo::handle-tool-event ((this demo-tools) event)
  (with-slots (button-bag windows) this
    (awt:handle-ui-event *ui* event)
    (flet ((%add (btn)
             (pushnew btn button-bag :test #'equal))
           (%del (btn)
             (a:deletef button-bag btn :test #'equal)))
      (case (aw:event-type event)
        (:keyboard-button-down
         (when (eq :escape (aw:event-key-scan-code event))
           (pushnew :debug windows))
         (%add (aw:event-key-scan-code event)))
        (:keyboard-button-up (%del (aw:event-key-scan-code event)))
        (:mouse-button-down (%add (aw:event-mouse-button event)))
        (:mouse-button-up (%del (aw:event-mouse-button event)))
        ((:game-controller-button-down :gamepad-button-down)
         (%add (if (eq (aw:event-type event) :game-controller-button-down)
                   (list :controller
                         (aw:event-game-controller-id event)
                         (aw:event-game-controller-button event))
                   (list :gamepad
                         (aw:event-gamepad-id event)
                         (aw:event-gamepad-button event)))))
        ((:game-controller-button-up :gamepad-button-up)
         (%del (if (eq (aw:event-type event) :game-controller-button-up)
                   (list :controller
                         (aw:event-game-controller-id event)
                         (aw:event-game-controller-button event))
                   (list :gamepad
                         (aw:event-gamepad-id event)
                         (aw:event-gamepad-button event)))))))))


(defmethod alien-works-demo::update-tools (tools time-delta)
  (with-slots (last-time-delta mouse-state) tools
    (setf last-time-delta time-delta)
    (aw:mouse-state mouse-state))
    (awt:update-ui-input *ui*))


(defun display-controller-panel (controller)
  (awt:text "Power Level:")
  (awt:same-line)
  (awt:text (string (aw:game-controller-power-level controller)))
  (loop for button-id below (aw:game-controller-button-count controller)
        do (awt:checkbox (format nil "Button #~A" button-id) (aw:game-controller-button-pressed-p controller button-id)))
  (loop for axis-id below (aw:game-controller-axes-count controller)
        do (awt:text "Axis #~A" (format nil "~A" axis-id))
           (awt:progress-bar (/ (1+ (aw:game-controller-axis-float-value controller axis-id)) 2)))
  (loop for hat-id below (aw:game-controller-hat-count controller)
        do (awt:text "Hat #~A:" (format nil "~A" hat-id))
           (awt:same-line)
           (awt:text (string (aw:game-controller-hat-value controller hat-id))))
  (aw:with-vec2 (ball-pos)
    (loop for ball-id below (aw:game-controller-ball-count controller)
          do (aw:game-controller-ball-value controller ball-id ball-pos)
             (awt:text "Ball #~A:" (format nil "~A" ball-id))
             (awt:same-line)
             (awt:text (format-vec2 ball-pos)))))


(defun display-gamepad-panel (gamepad)
  (awt:text "Power Level:")
  (awt:same-line)
  (awt:text (string (aw:gamepad-power-level gamepad)))
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
                        :left-stick :right-stick :left-shoulder :right-shoulder
                        :dpad-up :dpad-down :dpad-left :dpad-right
                        :misc1 :touchpad
                        :paddle1 :paddle2 :paddle3 :paddle4)
        do (awt:checkbox (string button) (aw:gamepad-button-pressed-p gamepad button)))
  (loop for stick in '(:left-x :left-y :right-x :right-y)
        do (awt:text (string stick))
           (awt:progress-bar (/ (1+ (aw:gamepad-axis-float-value gamepad stick)) 2)))
  (loop for trigger in '(:trigger-left :trigger-right)
        do (awt:text (string trigger))
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


(defun display-input-section ()
  (with-slots (mouse-state button-bag) *tools*
    (awt:text "Mouse Position:")
    (awt:same-line)
    (awt:text (format nil "(~A, ~A)" (aw:mouse-state-x mouse-state) (aw:mouse-state-y mouse-state)))

    (awt:text "Pressed Buttons:")
    (awt:same-line)
    (awt:text (format nil "~{~A~^, ~}" button-bag))))


(defun display-audio-source (source)
  (when (awt:button "Play")
    (aw:play-audio-source source))
  (awt:same-line)
  (when (awt:button "Pause")
    (aw:pause-audio-source source))
  (awt:same-line)
  (when (awt:button "Stop")
    (aw:stop-audio-source source))

  (awt:text "State:")
  (awt:same-line)
  (awt:text (string (aw:audio-source-state source)))

  (awt:text "Offset:")
  (awt:same-line)
  (awt:text (format nil "~,1F seconds" (aw:audio-source-offset source)))

  (awt:text "Reference Distance:")
  (awt:same-line)
  (multiple-value-bind (new-value changed-p)
      (awt:float-input "##srdistance" (aw:audio-source-reference-distance source))
    (when changed-p
      (setf (aw:audio-source-reference-distance source) new-value)))

  (awt:text "Rolloff:")
  (awt:same-line)
  (multiple-value-bind (new-value changed-p)
      (awt:float-input "##srolloff" (aw:audio-source-rolloff source) :step 0.01 :step-fast 0.1)
    (when changed-p
      (setf (aw:audio-source-rolloff source) new-value)))


  (awt:text "Max Distance:")
  (awt:same-line)
  (multiple-value-bind (new-value changed-p)
      (awt:float-input "##smdistance" (aw:audio-source-max-distance source))
    (when changed-p
      (setf (aw:audio-source-max-distance source) new-value)))

  (awt:text "Gain:")
  (multiple-value-bind (new-value changed-p) (awt:float-slider "##sgain" (aw:audio-source-gain source))
    (when changed-p
      (setf (aw:audio-source-gain source) new-value)))
  (awt:text "Pitch:")
  (multiple-value-bind (new-value changed-p) (awt:float-slider "##spitch" (aw:audio-source-pitch source) :max 2)
    (when changed-p
      (setf (aw:audio-source-pitch source) new-value)))

  (aw:with-vec3* (vec)
    (awt:text "Position:")
    (awt:same-line)
    (aw:audio-source-position source vec)
    (awt:text (format-vec3 vec))
    (awt:same-line)
    (awt:button "Drag##pos")
    (when (awt:item-active-p)
      (aw:with-vec2 (pos)
        (awt:mouse-drag-delta :left pos)
        (setf (aw:vec3 vec 0) (aw:vec2 pos 0)
              (aw:vec3 vec 1) (aw:vec2 pos 1)))
      (setf (aw:audio-source-position source) vec))
    (awt:same-line)
    (when (awt:button "Reset##pos")
      (setf (aw:vec3 vec 0) 0
            (aw:vec3 vec 1) 0
            (aw:vec3 vec 2) 0)
      (setf (aw:audio-source-position source) vec))

    (awt:text "Velocity:")
    (awt:same-line)
    (aw:audio-source-velocity source vec)
    (awt:text (format-vec3 vec))
    (awt:same-line)
    (awt:button "Drag##vel")
    (when (awt:item-active-p)
      (aw:with-vec2 (vel)
        (awt:mouse-drag-delta :left vel)
        (setf (aw:vec3 vec 0) (aw:vec2 vel 0)
              (aw:vec3 vec 1) (aw:vec2 vel 1)))
      (setf (aw:audio-source-velocity source) vec))
    (awt:same-line)
    (when (awt:button "Reset##vel")
      (setf (aw:vec3 vec 0) 0
            (aw:vec3 vec 1) 0
            (aw:vec3 vec 2) 0)
      (setf (aw:audio-source-velocity source) vec))

    (awt:text "Direction:")
    (awt:same-line)
    (aw:audio-source-direction source vec)
    (awt:text (format-vec3 vec))
    (awt:same-line)
    (awt:button "Drag##dir")
    (when (awt:item-active-p)
      (aw:with-vec2 (dir)
        (awt:mouse-drag-delta :left dir)
        (setf (aw:vec3 vec 0) (aw:vec2 dir 0)
              (aw:vec3 vec 1) (aw:vec2 dir 1)))
      (setf (aw:audio-source-direction source) vec))
    (awt:same-line)
    (when (awt:button "Reset##dir")
      (setf (aw:vec3 vec 0) 0
            (aw:vec3 vec 1) 0
            (aw:vec3 vec 2) 0)
      (setf (aw:audio-source-direction source) vec))))


(defun display-audio-section ()
  (with-slots (booo-source music-source) *tools*
    (awt:with-tree-node ("Devices")
      (aw:do-output-audio-devices (device-name)
        (awt:selectable device-name)))
    (awt:with-tree-node ("Listener")
      (awt:text "Gain:")
      (multiple-value-bind (new-value changed-p) (awt:float-slider "##lgain" (aw:audio-listener-gain))
        (when changed-p
          (setf (aw:audio-listener-gain) new-value)))
      (aw:with-vec3* (vec0 vec1)
        (awt:text "Position:")
        (awt:same-line)
        (aw:audio-listener-position vec0)
        (awt:text (format-vec3 vec0))

        (awt:text "Velocity:")
        (awt:same-line)
        (aw:audio-listener-velocity vec0)
        (awt:text (format-vec3 vec0))

        (awt:text "Orientation:")
        (aw:audio-listener-orientation vec0 vec1)
        (awt:indent)
        (awt:text (format nil "At: ~A" (format-vec3 vec0)))
        (awt:text (format nil "Up: ~A" (format-vec3 vec1)))
        (awt:unindent)))

    (awt:with-tree-node ("Sources")
      (when (awt:selectable "Booo")
        (request-tool-window (list :source "booo")))
      (with-tool-window ((list :source "booo") "Audio Source: Booo")
        (display-audio-source booo-source))
      (when (awt:selectable "Theme")
        (request-tool-window (list :source "theme")))
      (with-tool-window ((list :source "theme") "Audio Source: Theme")
        (display-audio-source music-source)))))


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
  (when (awt:collapsing-header "Input")
    (display-input-section))

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
