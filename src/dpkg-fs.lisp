(in-package :cl-user)
(defpackage dpkg-fs
  (:use :cl)
  (:export :main :disable-debugger))
(in-package :dpkg-fs)

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

(defun main (args)
  (log:config :debug)
  (log:debug "fuse-run")
  (cl-fuse:fuse-run `("none" ,(second args) "-d")
                    :directory-content 'directory-content
                    :directoryp 'is-directory))


(defun get-packages ()
  (loop
     :for line in (nthcdr 5
                          (cl-ppcre:split #\Newline
                                          (let ((s (make-string-output-stream)))
                                            (uiop:run-program "dpkg-query -l"
                                                              :output s)
                                            (get-output-stream-string s))))
     :collect (first (cl-ppcre:split ":" (second (cl-ppcre:split "\\s+" line))))))
