(in-package :dpkg-fs)

(defgeneric symlink (path type &key)
  (:documentation "Returns the target of a symbolic link."))

(defmethod symlink (path (type (eql :root)) &key)
  (unless path
    (return-from symlink nil))
  (let ((folder (first path)))
    (cond ((string= folder "installed") (symlink (rest path) :installed))
          ((string= folder "index") (symlink (rest path) :index)))))

(defmethod symlink (path (type (eql :installed)) &key)
  (use-dpkg-cache
      `("symlink" ,@path)
    (unless path
      (return-from symlink nil))
    (when (package-exists (first path))
      (symlink (rest path) :package-info))))

(defmethod symlink (path (type (eql :package-info)) &key)
  (unless path
    (return-from symlink nil))
  (cond ((string= (first path) "dependencies") (symlink (rest path) :deps))
        ((string= (first path) "files") (symlink (rest path) :files))))

(defmethod symlink (path (type (eql :files)) &key)
  (unless path
    (return-from symlink nil))
  (let ((full-path (cat "/" (join path "/"))))
    (unless (is-dir (sb-posix:stat-mode (sb-posix:stat full-path)))
      full-path)))

(defmethod symlink (path (type (eql :deps)) &key)
  (unless path
    (return-from symlink nil))
  (cat "../../" (first path)))

(defmethod symlink (path (type (eql :index)) &key)
  (use-apt-cache
      `("symlink" ,@path)
    (unless path
      (return-from symlink nil))
    (when (package-available (first path))
      (symlink (rest path) :package-info))))
