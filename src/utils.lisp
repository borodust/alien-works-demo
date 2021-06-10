(cl:in-package :alien-works-demo)

(declaim (special *engine*))

(defvar *renderables* nil)
(defvar *banner* nil)
(defvar *sun* nil)
(defvar *lights* nil)

(defvar *scene* nil)
(defvar *environment* nil)
(defvar *skybox* nil)
