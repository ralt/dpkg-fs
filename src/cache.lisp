(in-package :dpkg-fs)

#|

mtime-based cache.

apt and dpkg both touch files when doing an
action. dpkg touches /var/lib/dpkg/lock when doing anything like
install/remove (which includes upgrade), and apt touches
/var/cache/apt/pkgcache.bin when an apt-get update is run.

So we'll base the cache of dpkg-fs on these files, so that we know
we're always up-to-date with regards to the package manager's state.

The part in /pkg/index will base its cache on apt's, and
/pkg/installed will base its cache on dpkg's.

Basically, anytime the cache is fetched, it will first check the mtime
of these files, and will refresh the cache if the last cache was
before it.

The cache itself is a property list, of the following form:

(:foo (:mtime 1234 :value "bar")
 :baz (:mtime 4321 :value "qux"))

|#

;; A macro to avoid scope issues.
(defmacro cache-set (cache key value)
  `(progn
     (setf (getf ,cache ,key) (list
                               :mtime (local-time:timestamp-to-unix
                                       (local-time:now))
                               :value ,value))
     ,value))

(defn hash (list -> keyword) (list)
      (intern (format nil "~A-~{~A~^-~}"
                      (first list)
                      (second list)) :keyword))

(defmacro define-cache (name cache cache-file)
  `(defun ,name (path fn &rest args)
     (let* ((key (hash path))
            (cache-entry (getf ,cache key)))
       (unless cache-entry
         (return-from ,name (cache-set ,cache key (apply fn args))))
       (let ((mtime (sb-posix:stat-mtime (sb-posix:stat ,cache-file))))
         (if (> (getf cache-entry :mtime) mtime)
             (getf cache-entry :value)
             (cache-set ,cache key (apply fn args)))))))

(defvar *apt-cache-file* "/var/cache/apt/pkgcache.bin")
(defvar *dpkg-cache-file* "/var/lib/dpkg/lock")

(defvar *apt-cache* nil)
(defvar *dpkg-cache* nil)

(define-cache apt-cache *apt-cache* *apt-cache-file*)
(define-cache dpkg-cache *dpkg-cache* *dpkg-cache-file*)

(defmacro with-apt-cache (key &body body)
  `(apt-cache (list ,(first key) ,(second key)) #'(lambda () ,@body)))

(defmacro with-dpkg-cache (key &body body)
  `(dpkg-cache (list ,(first key) ,(second key)) #'(lambda () ,@body)))
