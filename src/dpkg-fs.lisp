(in-package :cl-user)
(defpackage dpkg-fs
  (:use :cl))
(in-package :dpkg-fs)

(annot:enable-annot-syntax)

(defun cat (&rest args)
  (apply #'concatenate 'string args))

@export
(defun disable-debugger ()
  (labels
      ((exit (c h)
         (declare (ignore h))
         (format t "~A~%" c)
         (sb-ext:exit)))
    (setf *debugger-hook* #'exit)))

(defmethod dir-content (path (type (eql :root)))
  (unless path
    (return-from dir-content '("installed" "remote" "sync")))
  (let ((folder (first path)))
    (cond ((string= folder "installed") (dir-content (rest path) :installed))
          ((string= folder "remote") (dir-content (rest path) :remote)))))

(defmethod dir-content (path (type (eql :installed)))
  (unless path
    (return-from dir-content (installed-packages)))
  (when (package-exists (first path))
    (dir-content (rest path) :package-info)))

(defmethod dir-content (path (type (eql :package-info)))
  (unless path
    (return-from dir-content '("name" "version" "desc"))))

(defmethod dir-content (path (type (eql :remote))))

(defun directory-content (split-path &optional (type :root))
  (log:debug "directory-content: ~A" split-path)
  (dir-content split-path type))

(defun directoryp (split-path)
  (log:debug "directoryp: ~A" split-path)
  (unless split-path
    (return-from directoryp t))
  (not (string= (first split-path) "sync")))

(defun file-read (split-path)
  (log:debug "file-read: ~A" split-path)
  t)

(defun file-size (split-path)
  (log:debug "file-size: ~A" split-path)
  0)

(defun symlink-target (split-path)
  (log:debug "symlink-target: ~A" split-path)
  "")

@export
(defun main (args)
  (log:config :debug)
  (log:debug "fuse-run")
  (cl-fuse:fuse-run `("none" ,(second args) "-d")
                    :directory-content 'directory-content
                    :directoryp 'directoryp
                    :file-read 'file-read
                    :file-size 'file-size
                    :symlink-target 'symlink-target))

(defun installed-packages ()
  (cl-ppcre:split " "
                  (let ((s (make-string-output-stream)))
                    (uiop:run-program "dpkg-query --showformat='${Package} ' --show" :output s)
                    (get-output-stream-string s))))

(defun package-exists (name)
  (string= "install ok installed"
           (let ((s (make-string-output-stream)))
             (uiop:run-program (cat "dpkg-query --showformat='${Status}' --show " name) :output s :ignore-error-status t)
             (get-output-stream-string s))))
