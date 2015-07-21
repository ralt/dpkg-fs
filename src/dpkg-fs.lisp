(in-package :cl-user)
(defpackage dpkg-fs
  (:use :cl))
(in-package :dpkg-fs)

(annot:enable-annot-syntax)

@export
(defun disable-debugger ()
  (labels
      ((exit (c h)
         (declare (ignore h))
         (format t "~A~%" c)
         (sb-ext:exit)))
    (setf *debugger-hook* #'exit)))

(defun directory-content (split-path)
  (log:debug "directory-content: ~A" split-path)
  (get-packages))

(defun is-directory (split-path)
  (log:debug "is-directory: ~A" split-path)
  t)

@export
(defun main (args)
  (log:config :debug)
  (log:debug "fuse-run")
  (cl-fuse:fuse-run `("none" ,(second args) "-d")
                    :directory-content 'directory-content
                    :directoryp 'is-directory))


(defun get-packages ()
  (cl-ppcre:split " "
                  (let ((s (make-string-output-stream)))
                    (uiop:run-program "dpkg-query --showformat='${Package} ' --show" :output s)
                    (get-output-stream-string s))))
