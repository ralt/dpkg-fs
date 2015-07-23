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
  nil)

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
