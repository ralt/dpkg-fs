(in-package :dpkg-fs)

(defgeneric is-directory (split-path type &key)
  (:documentation "Determines if a file is a directory."))

(defmethod is-directory (path (type (eql :root)) &key)
  (unless path
    (return-from is-directory t))
  (let ((folder (first path)))
    (cond ((string= folder "installed") (is-directory (rest path) :installed))
          ((string= folder "index") (is-directory (rest path) :index))
          ((string= folder "sync") nil))))

(defmethod is-directory (path (type (eql :installed)) &key)
  (unless path
    (return-from is-directory t))
  (when (package-exists (first path))
    (is-directory (rest path) :package-info)))

(defmethod is-directory (path (type (eql :index)) &key)
  (unless path
    (return-from is-directory t))
  (when (package-available (first path))
    (is-directory (rest path) :package-info)))

(defmethod is-directory (path (type (eql :package-info)) &key)
  (unless path
    (return-from is-directory t))
  (cond ((member (first path) '("name" "version" "desc") :test #'string=) nil)
        ((string= (first path) "deps") (is-directory (rest path) :deps))))

(defmethod is-directory (path (type (eql :deps)) &key)
  (unless path
    (return-from is-directory t))
  nil)

(defmethod is-directory (path (type (eql :index)) &key)
  (unless path
    (return-from is-directory t)))
