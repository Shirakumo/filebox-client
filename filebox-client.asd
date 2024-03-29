(asdf:defsystem #:filebox-client
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :version "1.0.1"
  :description "Local client that automatically pushes files from a folder to the filebox."
  :homepage "https://github.com/Shirakumo/filebox"
  :components ((:file "package")
               (:file "config")
               (:file "client")
               (:file "ui"))
  :depends-on (:qtcore
               :qtgui
               :dexador
               :cl-json
               :bordeaux-threads
               :dissect)
  :defsystem-depends-on (:qtools)
  :build-operation "qt-program-op"
  :build-pathname "filebox-client"
  :entry-point "org.shirakumo.filebox.client::cmd-start")
