(defpackage #:progressor/client
  (:use #:cl)
  (:import-from #:jsonrpc
                #:client-connect
                #:make-client)
  (:export
   #:create-progress
   #:list-progresses))
(in-package progressor/client)


(defun restore-lisp-object (type obj)
  (apply #'make-instance
         type
         (loop for key being the hash-keys in obj
                 using (hash-value value)
               appending
               (list (alexandria:make-keyword (string-upcase key))
                     value))))


(defun make-call (method &rest args)
  "Makes an RPC call with keyword arguments."
  (let ((client (make-client)))
    (client-connect client
                    :url "http://localhost:7890"
                    :mode :tcp)
    (jsonrpc:call client method (alexandria:plist-hash-table
                                 (loop for (key value) on args by #'cddr
                                       appending (list (symbol-name key)
                                                       value))))))


(defun create-progress (id &key total
                             (current 0)
                             description)
  (check-type id string)
  
  (when (string= id "")
    (error "Id should be a non empty string"))
  
  (restore-lisp-object
   'progressor/models:progress
   (make-call "create"
              :id id
              :total total
              :current current
              :description description)))


(defun increment-progress (id &optional (value 1))
  (check-type id string)
  
  (when (string= id "")
    (error "Id should be a non empty string"))
  
  (restore-lisp-object
   'progressor/models:progress
   (make-call "increment"
              :id id
              :value value)))

(defun delete-progress (id)
  (check-type id string)
  
  (when (string= id "")
    (error "Id should be a non empty string"))
  
  (make-call "delete"
             :id id))


(defun list-progresses ()
  (mapcar (alexandria:curry #'restore-lisp-object
                            'progressor/models:progress)
          (make-call "list")))


(defun clear-progresses ()
  (make-call "clear"))
