(in-package :dpkg-fs)

(defn installed-packages (list) ()
  (cl-ppcre:split " "
                  (let ((s (make-string-output-stream)))
                    (uiop:run-program "dpkg-query --showformat='${Package} ' --show" :output s)
                    (get-output-stream-string s))))

(defn package-exists (string -> boolean) (name)
  (string= "install ok installed"
           (let ((s (make-string-output-stream)))
             (uiop:run-program (cat "dpkg-query --showformat='${Status}' --show " name) :output s :ignore-error-status t)
             (get-output-stream-string s))))

(defn package-deps (string -> list) (name)
  ;; @todo handle OR dependencies correctly, e.g. foo | bar
  ;; means "use and install foo if possible, but use bar
  ;; if it's already there".
  (mapcar
   #'(lambda (dep)
       (first (cl-ppcre:split " " dep)))
   (cl-ppcre:split
    ", "
    (let ((s (make-string-output-stream)))
      (uiop:run-program (cat "dpkg-query --showformat='${Depends}' --show " name)
                        :output s :ignore-error-status t)
      (get-output-stream-string s)))))
