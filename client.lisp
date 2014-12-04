#|
 This file is a part of Filebox-Client
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.filebox.client)

(defvar *cookies* (make-instance 'drakma:cookie-jar))
(defvar *login* NIL)

(defun request (url &optional params)
  (format T "Request: ~a ~a~%" url params)
  (let* ((drakma:*text-content-types* (list* '("text"."json") '("application"."json") drakma:*text-content-types*))
         (data (drakma:http-request url :cookie-jar *cookies* :method :post :parameters params)))
    (format T "Response: ~a~%" data)
    (with-input-from-string (stream data)
      (let* ((request (cl-json:decode-json stream))
             (status (cdr (assoc :status request)))
             (message (cdr (assoc :message request)))
             (data (cdr (assoc :data request))))
        (if (= status 200)
            data
            (error message))))))

(defun login (&optional (username (conf :username)) (password (conf :password)))
  (assert (and (not (null username)) (not (null password))) ()
          "Username and password cannot be null!")
  (format T "Logging in for ~a/~a~%" username password)
  (request (conf :urls :login) `(("username" . ,username)
                                 ("password" . ,password)))
  (setf *login* T))

(defun logout ()
  (format T "Logging out~%")
  (request (conf :urls :logout))
  (setf *login* NIL))

(defun upload (file &key attrs name password)
  (assert (not (null *login*)) () "You are not logged in!")
  (format T "Uploading ~s~%" file)
  (let ((data (request (conf :urls :upload)
                       `(("file" . ,file)
                         ("attrs" . ,(etypecase attrs
                                       (list (format NIL "~{~a~^ ~}" attrs))
                                       (string attrs)
                                       (null "")))
                         ("name" . ,(or name ""))
                         ("password" . ,(or password ""))))))
    (values
     (cdr (assoc :url data))
     (cdr (assoc :id data))
     (cdr (assoc :name data))
     (cdr (assoc :type data))
     (cdr (assoc :time data)))))

(defun delete (id)
  (assert (not (null *login*)) () "You are not logged in!")
  (format T "Deleting ~a~%" id)
  (request (conf :urls :delete) `(("file" . ,id))))

(defun watch-directory (dir func &optional (cooldown 5))
  (with-simple-restart (abort "Abort watching.")
    (loop with prev = ()
          for files = (uiop:directory-files dir)
          do (dolist (file files)
               (unless (find file prev :test #'equal)
                 (with-simple-restart (continue "Continue watching.")
                   (when (funcall func file)
                     (push file prev)))))           
             (when cooldown
               (sleep cooldown)))))

(defun do-nothing (&rest args)
  (declare (ignore args)))

(defun watch-for-uploads (&key (dir (conf :directory)) (on-upload #'do-nothing) (on-error #'do-nothing) (condition (constantly T)))
  (ensure-directories-exist dir :verbose T)
  (block exit
    (watch-directory
     dir #'(lambda (file)
             (unless (funcall condition)
               (return-from exit))
             (when *login*
               (let ((url (handler-bind ((error on-error))
                            (upload file))))
                 (funcall on-upload file url))
               T)))))
