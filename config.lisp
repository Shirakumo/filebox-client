#|
 This file is a part of Filebox-Client
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.filebox.client)

(defvar *config-file* #p"~/.shirakumo/filebox-client")
(defvar *config* ())

(defun conf (&rest branches)
  (if branches
      (let ((*config* (getf *config* (car branches))))
        (apply #'conf (cdr branches)))
      *config*))

(defun (setf conf) (value &rest branches)
  (labels ((setc (branches)
             (cond
               (branches
                (setf (getf *config* (car branches))
                      (let ((*config* (getf *config* (car branches))))
                        (setc (cdr branches))))
                *config*)
               (T value))))
    (setc branches))
  value)

(defmacro conf-default (branches default)
  `(or (conf ,@branches) (setf (conf ,@branches) ,default)))

(defun set-defaults ()
  (conf-default (:urls :base) "http://filebox.tymoon.eu/api")
  (conf-default (:urls :login) (format NIL "~a/simple-auth/login" (conf :urls :base)))
  (conf-default (:urls :logout) (format NIL "~a/simple-auth/logout" (conf :urls :base)))
  (conf-default (:urls :upload) (format NIL "~a/filebox/upload" (conf :urls :base)))
  (conf-default (:urls :delete) (format NIL "~a/filebox/delete" (conf :urls :base)))
  (conf-default (:directory) #p"~/filebox/"))

(defun load-config (&optional (file *config-file*))
  (format T "Loading configuration from ~s~%" file)
  (with-open-file (stream file :direction :input :if-does-not-exist NIL)
    (when stream
      (setf *config* (read stream)))
    (set-defaults))
  *config*)

(defun save-config (&optional (config *config*) (file *config-file*))
  (format T "Saving configuration to ~s~%" file)
  (ensure-directories-exist file)
  (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((*print-readably* T)
          (*print-pretty* T))
      (print config stream)))
  config)
