(in-package :cl-user)
(defpackage dpkg-fs-test
  (:use :cl
        :dpkg-fs
        :prove))
(in-package :dpkg-fs-test)

;; NOTE: To run this test file, execute `(asdf:test-system :dpkg-fs)' in your Lisp.

(bordeaux-threads:make-thread #'(lambda ()
                                  (main '(nil "/pkg"))))

(defun is-executable (file)
  (not (= 0
          (logand (sb-posix:stat-mode (sb-posix:stat file)) #o100))))

(defun test-directories (dirs)
  (dolist (dir dirs)
    (ok (cl-fad:directory-exists-p (pathname dir)))))

(defun test-files (files)
  (dolist (file files)
    (ok (probe-file (pathname file)))))

(defun test-exec (files)
  (dolist (file files)
    (ok (is-executable (pathname file)))))

(defun test-symlinks (links))

(defun i-p (package path)
  "Returns the installed path for a file in a package."
  (cat "/pkg/installed/" package "/" path))

(defun r-p (package path)
  "Returns the index (remote) path for a file in a package."
  (cat "/pkg/index/" package "/" path))

(defun cat (&rest args)
  (apply #'concatenate 'string args))

(plan 23)

;; root
(test-directories '("/pkg/installed" "/pkg/index"))
(test-files '("/pkg/upgrade" "/pkg/sync"))
(test-exec '("/pkg/upgrade" "/pkg/sync"))

;; installed
(defvar *p* "sbcl")
(test-directories (list (i-p *p* "")
                        (i-p *p* "dependencies")
                        (i-p *p* "files")))
(test-files (list (i-p *p* "name")
                  (i-p *p* "description")
                  (i-p *p* "version")
                  (i-p *p* "size")
                  (i-p *p* "uninstall")))
(test-exec (list (i-p *p* "uninstall")))
(test-symlinks nil)

;; index
(defvar *r* "sbcl")
(test-directories (list (r-p *r* "")
                        (r-p *r* "dependencies")))
(test-files (list (r-p *p* "name")
                  (r-p *p* "description")
                  (r-p *p* "version")
                  (r-p *p* "size")
                  (r-p *p* "install")))
(test-exec (list (r-p *p* "install")))

(finalize)
