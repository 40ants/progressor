(defpackage #:progressor/models
  (:use #:cl)
  (:import-from #:local-time
                #:now
                #:adjust-timestamp
                #:timestamp<=
                #:timestamp>=)
  (:export
   #:make-progress
   #:increment
   #:print-progress
   #:get-id
   #:get-description
   #:get-total
   #:get-current
   #:progress
   #:expired-p
   #:get-ttl
   #:get-last-update-at
   #:get-default-ttl))
(in-package progressor/models)


(defun get-default-ttl ()
  (* 10 60))


(defclass progress ()
  ((id :type string
       :initarg :id
       :accessor get-id)
   (description :type (or string
                          null)
                :initarg :description
                :initform nil
                :accessor get-description)
   (total :type (or integer
                    null)
          :initarg :total
          :initform nil
          :accessor get-total)
   (current :type integer
            :initarg :current
            :initform 0
            :accessor get-current)
   (last-update-at :type integer
                   :documentation "A timestamp of last `increment' call."
                   :initarg :last-update-at
                   :initform (local-time:now)
                   :accessor get-last-update-at)
   (ttl :type integer
        :documentation "A number of seconds after the `last-update-at' when a progress will be removed from the list."
        :initarg :ttl
        :initform (get-default-ttl)
        :accessor get-ttl)))


(defun make-progress (id &key total
                           (current 0)
                           description
                           (ttl (get-default-ttl)))
  (check-type id string)
  
  (when (string= id "")
    (error "Id should be a non empty string"))
  
  (when (and total
             (<= total 0))
    (error "Total should be a positive integer or nil."))

  (make-instance 'progress
                 :id id
                 :description description
                 :total total
                 :current current
                 :ttl ttl))


(defun print-progress (progress &key (stream *standard-output*)
                                     (width 40)
                                     (char #\=))
  (cond
    ((get-total progress)
     (let* ((ratio (/ (get-current progress)
                      (get-total progress)))
            (current-width (min (ceiling (* width ratio))
                                width)))
       (write-char #\[
                   stream)
       (loop repeat current-width
             do (write-char char
                            stream))
       (loop repeat (- width current-width)
             do (write-char #\Space
                            stream))
       (format stream "] ~F%"
               (coerce (* ratio 100)
                       'float))))
    (t (format stream "~A"
               (get-current progress))))
  (values))


(defmethod print-object ((progress progress) stream)
  (print-unreadable-object (progress stream :type t)
    (format stream "~A~@[ ~S~] ["
            (get-id progress)
            (get-description progress))
    (print-progress progress :stream stream)
    (format stream "]")))


(defun increment (progress &optional (value 1))
  (incf (get-current progress)
        value)
  (setf (get-last-update-at progress)
        (local-time:now))
  progress)


(defun expired-p (progress)
  (timestamp<=
   (adjust-timestamp (get-last-update-at progress)
     (offset :sec (get-ttl progress)))
   (now)))


(defmethod yason:encode ((progress progress) &optional stream)
  (yason:encode-plist
   (list :|id| (get-id progress)
         :|description| (get-description progress)
         :|total| (get-total progress)
         :|current| (get-current progress))
   stream))
