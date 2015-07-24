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
  (is-directory split-path :root))

(defn file-read (list -> string) (split-path)
  (log:debug "file-read: ~A" split-path)
  "")

(defn file-size (list -> integer) (split-path)
  (log:debug "file-size: ~A" split-path)
  0)

(defn symlink-target (list -> string) (split-path)
  (log:debug "symlink-target: ~A" split-path)
  (symlink split-path :root))

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
