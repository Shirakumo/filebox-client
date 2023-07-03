(in-package #:cl-user)
(defpackage #:filebox-client
  (:use #:cl+qt)
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
