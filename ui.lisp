#|
 This file is a part of Filebox-Client
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.filebox.client)
(named-readtables:in-readtable :qtools)

(defvar *main* NIL)

(with-widget-environment
  (define-widget file-chooser (QWidget)
    ((file :initarg :file :initform NIL :accessor file)))

  (define-signal file-changed (string))
    
  (define-subwidget path (#_new QLineEdit widget)
    (#_setReadOnly path T)
    (#_setText path "No file chosen."))

  (define-initializer widget 100
    (when file
      (setf (file widget) file)))

  (define-subwidget button (#_new QPushButton "Browse.." widget))

  (define-layout layout (#_new QHBoxLayout widget)
    (#_setSizePolicy widget (#_QSizePolicy::Maximum) (#_QSizePolicy::Maximum))
    (#_setMargin layout 0)
    (#_setSpacing layout 0)
    (#_addWidget layout path)
    (#_addWidget layout button))

  (defmethod (setf file) :around (value (widget file-chooser))
    (etypecase value
      (string)
      (pathname (setf value (uiop:native-namestring value))))
    (#_setText (slot-value widget 'path) value)
    (call-next-method)
    (signal! widget file-changed (value string)))

  (defmethod file :around ((widget file-chooser))
    (uiop:parse-native-namestring (call-next-method) :ensure-directory T))

  (define-slot on-click (widget)
    (declare (connected button (clicked)))
    (let ((dir (#_QFileDialog::getExistingDirectory widget "Find Directory..." (#_QDir::currentPath))))
      (unless (string= dir "")
        (setf (file widget)
              (uiop:parse-native-namestring dir :ensure-directory T))))))

(with-widget-environment
  (define-widget settings (QDialog)
    ())

  (define-subwidget username (#_new QLineEdit widget)
    (#_setText username (or (conf :username) "")))

  (define-subwidget password (#_new QLineEdit widget)
    (#_setEchoMode password (#_QLineEdit::Password))
    (#_setText password (or (conf :password) "")))

  (define-subwidget folder (make-instance 'file-chooser :file (conf :directory)))

  (define-subwidget save (#_new QPushButton "&Save"))
  
  (define-subwidget cancel (#_new QPushButton "&Cancel"))

  (define-layout layout (#_new QGridLayout widget)
    (#_addWidget layout (#_new QLabel "Username:") 0 0 1 1)
    (#_addWidget layout username 0 1 1 1)
    (#_addWidget layout (#_new QLabel "Password:") 1 0 1 1)
    (#_addWidget layout password 1 1 1 1)
    (#_addWidget layout (#_new QLabel "Folder:") 2 0 1 1)
    (#_addWidget layout folder 2 1 1 1)

    (let ((sublayout (#_new QHBoxLayout)))
      (#_addWidget sublayout save)
      (#_addWidget sublayout cancel)
      (#_addLayout layout sublayout 3 0 1 2)))

  (define-initializer widget 100
    (#_setWindowTitle widget "Filebox Client Settings")
    (#_adjustSize widget))

  (define-slot save (widget)
    (declare (connected save (clicked)))
    (setf (conf :username) (#_text username))
    (setf (conf :password) (#_text password))
    (setf (conf :directory) (file folder))
    (save-config)
    (ignore-errors (logout))
    (handler-case
        (login)
      (error (err)
        (#_QMessageBox::critical widget "Failed to login" (princ-to-string err)))
      (:no-error (err)
        (declare (ignore err))
        (#_close widget))))

  (define-slot cancel (widget)
    (declare (connected cancel (clicked)))
    (#_close widget)))

(with-widget-environment
  (define-widget main (QMainWindow)
    ((known-files :initform ())))

  (define-initializer widget 0
    (setf *main* widget))

  (define-signal show-tray (string string))

  (define-subwidget settings (#_new QAction "&Settings" widget))
  
  (define-subwidget quit (#_new QAction "&Quit" widget))

  (define-subwidget watcher (#_new QFileSystemWatcher widget))

  (define-subwidget menu (#_new QMenu)
    (#_addAction menu settings)
    (#_addSeparator menu)
    (#_addAction menu quit))

  (define-subwidget tray (#_new QSystemTrayIcon widget)
    (#_setToolTip tray (format NIL "~:[Not logged in!~;Logged in as ~a~]" *login* (conf :username)))
    (#_setContextMenu tray menu)
    (#_setIcon tray (#_new QIcon (uiop:native-namestring
                                  (asdf:system-relative-pathname :filebox-client "icon.png"))))
    (#_show tray))

  (define-initializer widget 100
    (if (and (conf :username) (conf :password))
        (progn
          (or (ignore-errors
               (or *login* (login)) T)
              (signal! widget show-tray ("Login failed!" string)
                       ("Failed to automatically login, watcher not running." string)))
          (when (conf :directory)
            (#_addPath watcher (uiop:native-namestring (conf :directory)))
            (signal! widget show-tray ("Welcome to Filebox Client" string)
                     ((format NIL "Now watching over ~a" (uiop:native-namestring (conf :directory))) string))))
        (#_trigger settings)))

  (define-slot settings (widget)
    (declare (connected settings (triggered)))
    (#_exec (make-widget 'settings (widget)))
    (#_removePaths watcher (#_files watcher))
    (when (conf :directory)
      (#_addPath watcher (uiop:native-namestring (conf :directory)))))

  (define-slot quit (widget)
    (declare (connected quit (triggered)))
    (#_hide tray)
    (#_exit *qapplication*))

  (define-slot dir-changed (widget (file string))
    (declare (connected watcher (directory-changed string)))
    (declare (ignore file))
    (let ((files (uiop:directory-files (conf :directory))))
      (dolist (file files)
        (unless (find file known-files :test #'equal)
          (handler-case
              (let ((url (upload file)))
                (uiop:delete-file-if-exists file)
                (signal! widget show-tray ("File uploaded." string)
                         ((format NIL "~a~@[.~a~] has been uploaded to ~s" (pathname-name file) (pathname-type file) url) string))
                (#_setText (#_QApplication::clipboard) url))
            (error (err)
              (push file known-files)
              (signal! widget show-tray ("Uploaded failed!" string)
                       ((format NIL "~a~@[.~a~] has failed to upload: ~a" (pathname-name file) (pathname-type file) err) string))))))))

  (define-slot show-tray (widget (title string) (message string))
    (declare (connected widget (show-tray string string)))
    (format T "Showing in tray: ~a~%" message)
    (#_showMessage (slot-value widget 'tray) title message (#_QSystemTrayIcon::NoIcon) 5000)))

(defun main ()
  (load-config)
  (make-qapplication)
  (#_setQuitOnLastWindowClosed *qapplication* NIL)
  (let ((*main* NIL))
    (make-instance 'main)
    (#_exec *qapplication*)))
