(in-package :dpkg-fs)

(defun run (command)
  (let ((s (make-string-output-stream)))
    (uiop:run-program command :ignore-error-status t :output s)
    (get-output-stream-string s)))

(defun join (list delimiter)
  (format nil (cat "~{~A~^" delimiter "~}") list))

(defn installed-packages (list) ()
  (let ((installed-packages
         (mapcar #'(lambda (line)
                     (first (last (cl-ppcre:split " " line))))
                 (remove-if-not #'package-line-is-installed
                                (cl-ppcre:split #\Newline (run "dpkg-query --showformat='${Status} ${Package}\\n' --show"))))))
    ;; Now, fill in the dpkg-cache for is-directory.
    ;; Since we know the packages exist, we can already add them to the cache.
    ;; This has the added benefit of avoiding a call to dpkg-query for
    ;; every folder to check if it's a directory.
    ;; In practice, it reduces the time for an "ls /pkg/installed"
    ;; from ~1 minute to 1 second on a cold cache.
    (dolist (package installed-packages)
      (with-dpkg-cache ("is-directory-installed" (list package))
        t))
    installed-packages))

(defn all-packages (list) ()
  (let ((all-packages
         (mapcar #'(lambda (item)
                     (first (cl-ppcre:split " " item)))
                 (cl-ppcre:split #\Newline (run "apt-cache search .")))))
    ;; Same as in installed-packages, fill in the is-directory cache.
    (dolist (package all-packages)
      (with-apt-cache ("is-directory-index" (list package))
        t))
    all-packages))

(defn package-line-is-installed (string -> boolean) (line)
  (= 0
     (or (search "install ok installed" line)
         -1)))

(defn package-exists (string -> boolean) (name)
  (package-line-is-installed (run (cat "dpkg-query --showformat='${Status}' --show " name))))

(defn package-available (string -> boolean) (name)
  (> (length (cl-ppcre:split #\Newline (run (cat "apt-cache search " name))))
     0))

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

(defn package-index-deps (string -> list) (name)
  ;; @todo handle OR dependencies, or virtual packages.
  (remove-if #'is-empty-string
             (mapcar
              #'get-dependency
              (remove-if-not #'is-depends-line
                             (cl-ppcre:split #\Newline (run (cat "apt-cache depends " name)))))))

(defn get-dependency (string -> string) (line)
  (multiple-value-bind (_ name)
      (cl-ppcre:scan-to-strings "^\\s+Depends:\\s+([a-zA-Z0-9-_.]+)$" line)
    (declare (ignore _))
    (if name
        (elt name 0)
        "")))

(defn is-empty-string (string -> boolean) (str)
  (string= str ""))

(defn is-depends-line (string -> boolean) (line)
  (if (cl-ppcre:scan "^\\s+Depends:.*$" line)
      t
      nil))

(defn package-version (string -> string) (name)
  (run (cat "dpkg-query --showformat='${Version}' --show " name)))

(defn package-desc (string -> string) (name)
  (run (cat "dpkg-query --showformat='${Description}' --show " name)))

(defn package-index-version (string -> string) (name)
  (multiple-value-bind (_ matches)
      (cl-ppcre:scan-to-strings "Version: (.*)" (run (cat "apt-cache show " name)))
    (declare (ignore _))
    (elt matches 0)))

(defn package-index-desc (string -> string) (name)
  (multiple-value-bind (_ matches)
      (cl-ppcre:scan-to-strings "Description-\\w+:\\s(.*)" (run (cat "apt-cache show " name)))
    (declare (ignore _))
    (elt matches 0)))

(defn package-files (string -> hash-table) (name)
  "Returns a hash table of this form (using pseudo-JSON):
{
    '/': ['usr'],
    '/usr': ['share'],
    '/usr/share': ['foo'],
    '/usr/share/foo': ['bar', 'baz']
}

This way, getting the list of files in a folder is very simple
with the right key."
  (let ((files (make-hash-table :test #'equal)))
    (loop
       :for file in
       ;; The first is always "/.", so ignore it
       (rest
        (cl-ppcre:split #\Newline (run (cat "dpkg -L " name))))
       :when (is-dir (sb-posix:stat-mode (sb-posix:stat file)))
       :do (setf (gethash file files) (or (gethash file files)
                                          nil))
       :do (let* ((split-path (cl-ppcre:split #\/ file))
                  (folder (join (butlast split-path) "/"))
                  (file (first (last split-path))))
             (push file (gethash (if (string= folder "") "/" folder) files))))
    files))

(defn package-size (string -> integer) (name)
  (multiple-value-bind (_ matches)
      (cl-ppcre:scan-to-strings "Installed-Size: (\\d+)" (run (cat "apt-cache show " name)))
    (declare (ignore _))
    (parse-integer (elt matches 0))))
