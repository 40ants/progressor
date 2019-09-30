(defpackage #:progressor/server
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:destroy-thread
                #:make-thread)
  (:import-from #:jsonrpc
                #:make-server
                #:server-listen)
  (:import-from #:progressor/models
                #:get-default-ttl
                #:expired-p
                #:make-progress)
  (:import-from #:alexandria
                #:make-keyword)
  (:export
   #:start
   #:stop))
(in-package progressor/server)


(defvar *server* nil)
(defvar *server-thread* nil)
(defvar *commands* nil)

(defvar *progresses* nil)


(defun find-progress-by-id (id)
  (find-if (lambda (progress)
             (string-equal (progressor/models:get-id progress)
                           id))
           *progresses*))


(defun remove-expired ()
  (setf *progresses*
        (remove-if #'expired-p *progresses*)))


(defun expose-commands ()
  (when *server*
    (loop for (command func) in *commands*
          do (jsonrpc:expose *server* command func))))


(defmacro defcommand ((rpc-name func-name) (&rest args) &body body)
  `(progn (defun ,func-name (args)
            (check-type args (or hash-table
                                 null))
            (unless args
              (setf args
                    (make-hash-table)))
            
            (destructuring-bind ,args
                (loop for key being the hash-keys in args
                      using (hash-value value)
                      appending (list (make-keyword (string-upcase key))
                                      value))
              (remove-expired)
              ,@body))
          (pushnew (list ,rpc-name ',func-name) *commands*
                   :test #'equal)
          (expose-commands)))


(defcommand ("create" create-progress) (&key
                                        id
                                        total
                                        description
                                        (current 0)
                                        (ttl (get-default-ttl)))
  (check-type current (or integer
                          null))

  (when (null current)
    (setf current 0))

  (when (< current 0)
    (setf current 0))
  
  (when (find-progress-by-id id)
    (error 'jsonrpc:jsonrpc-error 
           :code 1
           :message (format nil "Progress with id \"~A\" already exists" id)))
  
  (let ((progress (make-progress id
                                 :total total
                                 :current current
                                 :description description
                                 :ttl ttl)))
    (push progress
          *progresses*)
    progress))


(defcommand ("delete" delete-progress) (&key id)
  (when (find-progress-by-id id)
    (setf *progresses*
          (remove-if (lambda (progress)
                       (string-equal (progressor/models:get-id progress)
                                     id))
                     *progresses*))
    t))


(defcommand ("increment" increment-progress) (&key
                                              id
                                              (value 1))
  (let ((progress (find-progress-by-id id)))
    (unless progress
      (error 'jsonrpc:jsonrpc-error 
             :code 1
             :message (format nil "Unable to find progress with id \"~A\"" id)))
    
    (progressor/models:increment progress value)))


(defcommand ("list" list-progresses) ()
  *progresses*)


(defcommand ("hello" hello) ()
  "Привет мир!")


(defcommand ("clear" clear-progresses) ()
  (setf *progresses* nil))


(defun start (&key (port 7890) (in-thread))
  (unless *server-thread*
    (setf *server* (make-server))

    (expose-commands)
    
    (setf *server-thread*
          (make-thread (lambda ()
                         (server-listen *server*
                                        :port port
                                        :mode :tcp))
                       :name "JSON RPC Server"))))


(defun stop ()
  (when *server-thread*
    (destroy-thread *server-thread*)
    (jsonrpc/main:server-listen)))

