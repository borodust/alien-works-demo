(cl:in-package :alien-works-demo)


(declaim (special *tools*))


(defgeneric make-tools (name &key &allow-other-keys)
  (:method (name &key)
    (declare (ignore name))
    nil))


(defgeneric destroy-tools (tools)
  (:method (tools)
    (declare (ignore tools))))


(defgeneric call-with-tools (tools body)
  (:method (tools body)
    (declare (ignore tools))
    (funcall body)))


(defgeneric handle-tool-event (tools event)
  (:method (tools event)
    (declare (ignore tools event))))


(defgeneric render-tools (tools)
  (:method (tools)
    (declare (ignore tools))))


(defgeneric update-tools (tools time-delta)
  (:method (tools time-delta)
    (declare (ignore tools time-delta))))


(defmacro with-tools ((name &rest args &key &allow-other-keys) &body body)
  `(let ((*tools* (make-tools ,name ,@args)))
     (unwind-protect
          (call-with-tools *tools* (lambda () ,@body))
       (destroy-tools *tools*))))
