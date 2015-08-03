(in-package :dpkg-fs)

(defgeneric dir-content (path type &key)
  (:documentation "List a directory content."))

(defmethod dir-content (path (type (eql :root)) &key)
  (unless path
    (return-from dir-content '("installed" "index" "sync" "upgrade")))
  (let ((folder (first path)))
    (cond ((string= folder "installed") (dir-content (rest path) :installed))
          ((string= folder "index") (dir-content (rest path) :index)))))

(defmethod dir-content (path (type (eql :installed)) &key)
  (use-dpkg-cache
      `("dir-content-installed" ,@path)
    (unless path
      (return-from dir-content (installed-packages)))
    (when (package-exists (first path))
      (dir-content (rest path) :package-info :package (first path)))))

(defmethod dir-content (path (type (eql :package-info)) &key package)
  (unless path
    (return-from dir-content '("name" "version" "description" "dependencies" "uninstall" "files" "size")))
  (cond ((string= (first path) "dependencies") (dir-content (rest path)
                                                            :deps
                                                            :package package))
        ((string= (first path) "files") (dir-content (rest path)
                                                     :files
                                                     :package package))))

(defmethod dir-content (path (type (eql :deps)) &key package)
  (unless path
    (return-from dir-content (package-deps package))))

(defmethod dir-content (path (type (eql :files)) &key package)
  (gethash (cat "/" (join path "/")) (package-files package)))

(defmethod dir-content (path (type (eql :index)) &key)
  (use-apt-cache
      `("dir-content-index" ,@path)
    (unless path
      (return-from dir-content (all-packages)))
    (when (package-available (first path))
      (dir-content (rest path) :package-index-info :package (first path)))))

(defmethod dir-content (path (type (eql :package-index-info)) &key package)
  (unless path
    (return-from dir-content '("name" "version" "description" "dependencies" "install" "size")))
  (when (string= (first path) "deps")
    (dir-content (rest path) :index-deps :package package)))

(defmethod dir-content (path (type (eql :index-deps)) &key package)
  (unless path
    (return-from dir-content (package-index-deps package))))
