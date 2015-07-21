#|
  This file is a part of dpkg-fs project.
|#

(in-package :cl-user)
(defpackage dpkg-fs-test-asd
  (:use :cl :asdf))
(in-package :dpkg-fs-test-asd)

(defsystem dpkg-fs-test
  :author ""
  :license ""
  :depends-on (:dpkg-fs
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "dpkg-fs"))))
  :description "Test system for dpkg-fs"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
