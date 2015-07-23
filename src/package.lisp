(in-package :dpkg-fs)

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

(defun package-deps (name)
  (mapcar
   #'(lambda (dep)
       (first (cl-ppcre:split " " dep)))
   (cl-ppcre:split
    ", "
    (let ((s (make-string-output-stream)))
      (uiop:run-program (cat "dpkg-query --showformat='${Depends}' --show " name)
                        :output s :ignore-error-status t)
      (get-output-stream-string s)))))
