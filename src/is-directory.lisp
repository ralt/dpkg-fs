(in-package :dpkg-fs)

(defgeneric is-directory (split-path type &key)
  (:documentation "Determines if a file is a directory."))

(defmethod is-directory (path (type (eql :root)) &key)
  (unless path
    (return-from is-directory t))
  (let ((folder (first path)))
    (cond ((string= folder "installed") (is-directory (rest path) :installed))
          ((string= folder "index") (is-directory (rest path) :index))
          (t nil))))

(defmethod is-directory (path (type (eql :installed)) &key)
  (unless path
    (return-from is-directory t))
  (when (package-exists (first path))
    (is-directory (rest path) :package-info)))

(defmethod is-directory (path (type (eql :index)) &key)
  (unless path
    (return-from is-directory t))
  (when (package-available (first path))
    (is-directory (rest path) :package-info :package (first path))))

(defmethod is-directory (path (type (eql :package-info)) &key package)
  (unless path
    (return-from is-directory t))
  (cond ((string= (first path) "dependencies") (is-directory (rest path) :deps))
        ((string= (first path) "files") (is-directory (rest path) :files :package package))
        (t nil)))

(defmethod is-directory (path (type (eql :deps)) &key)
  (unless path
    (return-from is-directory t))
  nil)

(defmethod is-directory (path (type (eql :files)) &key package)
  (unless path
    (return-from is-directory t))
  (is-dir (sb-posix:stat-mode (sb-posix:stat (cat "/" (join path "/"))))))
