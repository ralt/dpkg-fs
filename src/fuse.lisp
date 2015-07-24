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

(defn directory-content (list -> list) (split-path)
  (log:debug "directory-content: ~A" split-path)
  (dir-content split-path :root))

(defn directoryp (list -> boolean) (split-path)
  (log:debug "directoryp: ~A" split-path)
  (unless split-path
    (return-from directoryp t))
  (not (string= (first split-path) "sync")))

(defn file-read (list -> string) (split-path)
  (log:debug "file-read: ~A" split-path)
  "")

(defn file-size (list -> integer) (split-path)
  (log:debug "file-size: ~A" split-path)
  0)

(defn symlink-target (list -> string) (split-path)
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
