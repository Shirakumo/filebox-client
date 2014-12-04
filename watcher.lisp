#|
 This file is a part of Filebox-Client
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.filebox.client)
(named-readtables:in-readtable :qtools)

(defvar *shared* (make-hash-table :test 'eql))
(defvar *lock* (bt:make-lock))

(defun shared (field)
  (bt:with-lock-held (*lock*)
    (gethash field *shared*)))

(defun (setf shared) (value field)
  (bt:with-lock-held (*lock*)
    (setf (gethash field *shared*) value)))

(defun start-watcher ()
  (when (shared :watcher) (error "Watcher already running!"))
  (format T "Starting watcher...~%")
  (setf (shared :run) T
        (shared :watcher)
        (bt:make-thread
         #'(lambda ()
             (watch-for-uploads
              :on-upload #'file-uploaded
              :on-error #'file-upload-failed
              :condition #'(lambda () (shared :run)))
             (setf (shared :watcher) NIL
                   (shared :run) NIL))
         :initial-bindings `((*shared* . ,*shared*)
                             (*lock* . ,*lock*)
                             (*standard-output* . ,*standard-output*)))))

(defun stop-watcher ()
  (unless (shared :watcher) (error "No watcher running!"))
  (format T "Stopping watcher...~%")
  (setf (shared :run) NIL)
  (or (loop repeat 10
            do (sleep 1)
            thereis (not (shared :watcher)))
      (bt:destroy-thread (shared :watcher)))
  (setf (shared :watcher) NIL)
  T)


(defun restart-watcher ()
  (format T "Restarting watcher...~%")
  (when (shared :watcher)
    (stop-watcher))
  (start-watcher))
