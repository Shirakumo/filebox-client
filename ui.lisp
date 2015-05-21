#|
 This file is a part of Filebox-Client
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.filebox.client)
(named-readtables:in-readtable :qtools)

(defvar *main* NIL)

(define-widget file-chooser (QWidget)
  ((file :initarg :file :initform NIL :accessor file)))

(define-signal (file-chooser file-changed) (string))

(define-subwidget (file-chooser path) (q+:make-qlineedit file-chooser)
  (setf (q+:read-only path) T)
  (setf (q+:text path) "No file chosen."))

(define-initializer (file-chooser setup)
  (when file
    (setf (file file-chooser) file)))

(define-subwidget (file-chooser button) (q+:make-qpushbutton "Browse.." file-chooser))

(define-subwidget (file-chooser layout) (q+:make-qhboxlayout file-chooser)
  (setf (q+:size-policy file-chooser) (values (q+:qsizepolicy.maximum) (q+:qsizepolicy.maximum)))
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0)
  (q+:add-widget layout path)
  (q+:add-widget layout button))

(defmethod (setf file) :around (value (file-chooser file-chooser))
  (etypecase value
    (string)
    (pathname (setf value (uiop:native-namestring value))))
  (setf (q+:text (slot-value file-chooser 'path)) value)
  (call-next-method)
  (signal! file-chooser (file-changed string) value))

(defmethod file :around ((file-chooser file-chooser))
  (uiop:parse-native-namestring (call-next-method) :ensure-directory T))

(define-slot (file-chooser on-click) ()
  (declare (connected button (clicked)))
  (let ((dir (q+:qfiledialog-get-existing-directory file-chooser "Find Directory..." (q+:qdir-current-path))))
    (unless (string= dir "")
      (setf (file file-chooser)
            (uiop:parse-native-namestring dir :ensure-directory T)))))

(define-widget settings (QDialog)
  ())

(define-subwidget (settings username) (q+:make-qlineedit settings)
  (setf (q+:text username) ""))

(define-subwidget (settings password) (q+:make-qlineedit settings)
  (setf (q+:echo-mode password) (q+:qlineedit.password))
  (setf (q+:text password) ""))

(define-subwidget (settings folder) (make-instance 'file-chooser :file (conf :directory)))

(define-subwidget (settings save) (q+:make-qpushbutton "&Login && Save"))

(define-subwidget (settings cancel) (q+:make-qpushbutton "&Cancel")
  (setf (q+:default cancel) T))

(define-subwidget (settings layout) (q+:make-qgridlayout settings)
  (q+:add-widget layout (q+:make-qLabel "Username:") 0 0 1 1)
  (q+:add-widget layout username 0 1 1 1)
  (q+:add-widget layout (q+:make-qLabel "Password:") 1 0 1 1)
  (q+:add-widget layout password 1 1 1 1)
  (q+:add-widget layout (q+:make-qLabel "Folder:") 2 0 1 1)
  (q+:add-widget layout folder 2 1 1 1)

  (let ((sublayout (q+:make-qhboxlayout)))
    (q+:add-widget sublayout save)
    (q+:add-widget sublayout cancel)
    (q+:add-layout layout sublayout 3 0 1 2)))

(define-initializer (settings setup)
  (setf (q+:window-title settings) "Filebox Client Settings")
  (q+:adjust-size settings))

(define-slot (settings save) ()
  (declare (connected save (clicked)))
  (setf (conf :directory) (file folder))
  (save-config)
  (unless (or (string= (q+:text username) "")
              (string= (q+:text password) ""))
    (handler-case
        (progn
          (ignore-errors (logout))
          (login (q+:text username) (q+:text password))
          (setf (conf :session) (session))
          (save-config)
          (q+:close settings))
      (error (err)
        (q+:qmessagebox-critical settings "Failed to login" (princ-to-string err))))))

(define-slot (settings cancel) ()
  (declare (connected cancel (clicked)))
  (q+:close settings))

(define-widget main (QMainWindow)
  ((known-files :initform ())))

(define-initializer (main register-self 100)
  (setf *main* main))

(define-signal (main show-tray) (string string))

(define-subwidget (main settings) (q+:make-qaction "&Settings" main))

(define-subwidget (main quit) (q+:make-qaction "&Quit" main))

(define-subwidget (main watcher) (q+:make-qfilesystemwatcher main))

(define-subwidget (main menu) (q+:make-qmenu)
  (q+:add-action menu settings)
  (q+:add-separator menu)
  (q+:add-action menu quit))

(define-subwidget (main tray) (q+:make-qsystemtrayicon main)
  (setf (q+:tool-tip tray) (format NIL "~:[Not logged in!~;Logged in as ~a~]" *login* (conf :username)))
  (setf (q+:context-menu tray) menu)
  (setf (q+:icon tray) (q+:make-qicon (uiop:native-namestring
                                      (asdf:system-relative-pathname :filebox-client "icon.png"))))
  (q+:show tray))

(define-initializer (main setup)
  (if (conf :session)
      (progn
        (setf (session) (conf :session))
        (when (conf :directory)
          (ensure-directories-exist (conf :directory))
          (q+:add-path watcher (uiop:native-namestring (conf :directory)))))
      (q+:trigger settings)))

(define-slot (main settings) ()
  (declare (connected settings (triggered)))
  (q+:exec (make-instance 'settings))
  (q+:remove-paths watcher (q+:files watcher))
  (when (conf :directory)
    (ensure-directories-exist (conf :directory))
    (q+:add-path watcher (uiop:native-namestring (conf :directory)))))

(define-slot (main quit) ()
  (declare (connected quit (triggered)))
  (q+:hide tray)
  (q+:exit *qapplication*))

(define-slot (main dir-changed) ((file string))
  (declare (connected watcher (directory-changed string)))
  (declare (ignore file))
  (let ((files (uiop:directory-files (conf :directory))))
    (dolist (file files)
      (unless (find file known-files :test #'equal)
        (handler-case
            (let ((url (upload file)))
              (uiop:delete-file-if-exists file)
              (signal! main (show-tray string string) "File uploaded."
                       (format NIL "~a~@[.~a~] has been uploaded to ~s" (pathname-name file) (pathname-type file) url))
              (setf (q+:text (q+:qapplication-clipboard)) url))
          (error (err)
            (push file known-files)
            (signal! main (show-tray string string) "Uploaded failed!"
                     (format NIL "~a~@[.~a~] has failed to upload: ~a" (pathname-name file) (pathname-type file) err))))))))

(define-slot (main show-tray) ((title string) (message string))
  (declare (connected main (show-tray string string)))
  (format T "Showing in tray: ~a~%" message)
  (q+:show-message (slot-value main 'tray) title message (q+:qsystemtrayicon.no-icon) 5000))

(defun main ()
  (format T "Starting up...~%")
  (load-config)
  (format T "Launching Qt GUI...~%")
  (make-qapplication)
  (format T "QApplication launched...~%")
  (q+:qapplication-set-quit-on-last-window-closed NIL)
  (let ((*main* NIL))
    (make-instance 'main)
    (q+:exec *qapplication*))
  (format T "Done. ~%"))

(defun cmd-start (&rest args)
  (declare (ignore args))
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "disable_lossage_handler" (function sb-alien:void)))
  (unwind-protect
       (handler-case
           (progn
             (qt::load-libcommonqt)
             (qt::reload)
             (main))
         (T (error)
           (dissect:present error T)))
    (finish-output)
    (sb-ext:exit :timeout 1)))
