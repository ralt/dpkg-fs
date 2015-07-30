(in-package :dpkg-fs)

(defun run (command)
  (let ((s (make-string-output-stream)))
    (uiop:run-program command :ignore-error-status t :output s)
    (get-output-stream-string s)))

(defn installed-packages (list) ()
  (cl-ppcre:split " " (run "dpkg-query --showformat='${Package} ' --show")))

(defn package-exists (string -> boolean) (name)
  (string= "install ok installed"
           (run (cat "dpkg-query --showformat='${Status}' --show " name))))

(defn package-deps (string -> list) (name)
  ;; @todo handle OR dependencies correctly, e.g. foo | bar
  ;; means "use and install foo if possible, but use bar
  ;; if it's already there".
  (mapcar
   #'(lambda (dep)
       (first (cl-ppcre:split " " dep)))
   (cl-ppcre:split
    ", "
    (run (cat "dpkg-query --showformat='${Depends}' --show " name)))))

(defn package-version (string -> string) (name)
  (run (cat "dpkg-query --showformat='${Version}' --show " name)))

(defn package-desc (string -> string) (name)
  (run (cat "dpkg-query --showformat='${Description}' --show " name)))
