#|
 This file is a part of Filebox-Client
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:filebox-client
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :version "1.0.0"
  :description "Local client that automatically pushes files from a folder to the filebox."
  :homepage "https://github.com/Shirakumo/filebox"
  :components ((:file "package")
               (:file "config")
               (:file "client")
               (:file "watcher")
               (:file "ui"))
  :depends-on (:qtools
               :drakma
               :cl-json
               :bordeaux-threads))