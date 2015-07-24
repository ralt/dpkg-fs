(in-package :dpkg-fs)

(defgeneric symlink (path type &key)
  (:documentation "Returns the target of a symbolic link."))

(defmethod symlink (path (type (eql :root)) &key)
  (unless path
    (return-from symlink nil))
  (let ((folder (first path)))
    (cond ((string= folder "installed") (symlink (rest path) :installed))
          ((string= folder "index") (symlink (rest path) :index))
          ((string= folder "sync") nil))))

(defmethod symlink (path (type (eql :installed)) &key)
  (unless path
    (return-from symlink nil))
  (when (package-exists (first path))
    (symlink (rest path) :package-info)))

(defmethod symlink (path (type (eql :package-info)) &key)
  (unless path
    (return-from symlink nil))
  (cond ((member (first path) '("name" "version" "desc") :test #'string=) nil)
        ((string= (first path) "deps") (symlink (rest path) :deps))))

(defmethod symlink (path (type (eql :deps)) &key)
  (unless path
    (return-from symlink nil))
  (cat "../../" (first path)))
