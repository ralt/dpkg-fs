(in-package :dpkg-fs)

(defgeneric read-file (path type &key)
  (:documentation "Reads a single file."))

(defmethod read-file (path (type (eql :root)) &key)
  (let ((file (first path)))
    (cond ((string= file "installed") (read-file (rest path) :installed))
          ((string= file "index") (read-file (rest path) :index))
          ((string= file "sync") (read-file nil :sync))
          ((string= file "upgrade") (read-file nil :upgrade))
          ((string= file "moo") (run "apt-get moo")))))

(defmethod read-file (path (type (eql :sync)) &key)
  "#!/bin/bash
apt-get update
")

(defvar *upgrade-script* (alexandria:read-file-into-string
                          (asdf:system-relative-pathname :dpkg-fs "scripts/upgrade")))

(defmethod read-file (path (type (eql :upgrade)) &key)
  *upgrade-script*)

(defmethod read-file (path (type (eql :index)) &key)
  (with-apt-cache ("read-file-index" path)
    (let ((folder (first path)))
      (when (package-available folder)
        (read-file (rest path) :package-index-info :package folder)))))

(defmethod read-file (path (type (eql :package-index-info)) &key package)
  (let ((file (first path)))
    (cond ((string= file "name") (read-file nil :package-name :package package))
          ((string= file "version") (read-file nil :package-index-version :package package))
          ((string= file "description") (read-file nil :package-index-desc :package package))
          ((string= file "install") (read-file nil :package-install :package package))
          ((string= file "size") (read-file nil :package-size :package package)))))

(defmethod read-file (path (type (eql :package-install)) &key package)
  (format nil "#!/bin/bash
apt-get install ~A
" package))

(defmethod read-file (path (type (eql :package-index-version)) &key package)
  (format nil "~A~%" (package-index-version package)))

(defmethod read-file (path (type (eql :package-index-desc)) &key package)
  (format nil "~A~%" (package-index-desc package)))

(defmethod read-file (path (type (eql :installed)) &key)
  (with-dpkg-cache ("read-file-installed" path)
    (let ((folder (first path)))
      (when (package-exists folder)
        (read-file (rest path) :package-info :package folder)))))

(defmethod read-file (path (type (eql :package-info)) &key package)
  (let ((file (first path)))
    (cond ((string= file "name") (read-file nil :package-name :package package))
          ((string= file "version") (read-file nil :package-version :package package))
          ((string= file "description") (read-file nil :package-desc :package package))
          ((string= file "uninstall") (read-file nil :package-uninstall :package package))
          ((string= file "size") (read-file nil :package-size :package package)))))

(defmethod read-file (path (type (eql :package-size)) &key package)
  (format nil "~A~%" (package-size package)))

(defmethod read-file (path (type (eql :package-uninstall)) &key package)
  (format nil "#!/bin/bash
apt-get remove ~A
" package))

(defmethod read-file (path (type (eql :package-name)) &key package)
  (format nil "~A~%" package))

(defmethod read-file (path (type (eql :package-version)) &key package)
  (format nil "~A~%" (package-version package)))

(defmethod read-file (path (type (eql :package-desc)) &key package)
  (format nil "~A~%" (package-desc package)))
