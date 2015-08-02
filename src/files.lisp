(in-package :dpkg-fs)

;;;; C macros translations
(defconstant +--s-ifmt+ #o0170000)
(defun is-type (mode mask)
  (= (logand mode +--s-ifmt+) mask))

(defmacro define-file-type (type mask)
  `(defun ,(intern (string-upcase
                    (concatenate 'string "is-" (symbol-name type)))) (mode)
     (is-type mode ,mask)))

;; Creates the is-dir function
(define-file-type dir #o0040000)
