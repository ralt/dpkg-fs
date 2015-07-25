(in-package :dpkg-fs)

(defgeneric read-file (path size offset fh type &key)
  (:documentation "Reads a single file."))

(defmethod read-file (path size offset fh (type (eql :root)) &key)
  "")
