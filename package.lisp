#|
 This file is a part of Filebox-Client
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:filebox-client
  (:use #:cl #:qtools #:qt)
  (:nicknames #:org.shirakumo.filebox.client)
  (:shadow #:delete)
  ;; client.lisp
  (:export
   #:*login*
   #:request
   #:login
   #:logout
   #:upload
   #:delete
   
   #:*config-file*
   #:conf
   #:load-config
   #:save-config

   #:*main*
   #:main))
