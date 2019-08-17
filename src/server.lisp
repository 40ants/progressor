(defpackage #:progressor/server
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:destroy-thread
                #:make-thread)
  (:import-from #:jsonrpc
                #:make-server
                #:server-listen))
(in-package progressor/server)


(defvar *server* nil)
(defvar *server-thread* nil)


(defun sum (args)
  (reduce #'+ args))


(defun sum (args)
  (reduce #'+ args))


(defun start (&key (port 7890))
  (unless *server-thread*
    (setf *server* (make-server))
  
    (jsonrpc:expose *server* "sum" 'sum)
    
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
