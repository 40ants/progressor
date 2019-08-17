(defpackage #:progressor/models
  (:use #:cl)
  (:export
   #:make-progress
   #:increment
   #:print-progress))
(in-package progressor/models)


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
            :accessor get-current)))


(defun make-progress (id &key total
                              (current 0)
                              description)
  (when (and total
             (<= total 0))
    (error "Total should be a positive integer or nil."))

  (make-instance 'progress
                 :id id
                 :description description
                 :total total
                 :current current))


(defun print-progress (progress &key (stream t)
                                     (width 40)
                                     (char #\=))
  (cond
    ((get-total progress)
     (let* ((ratio (/ (get-current progress)
                      (get-total progress)))
            (current-width (min (ceiling (* width ratio))
                                width)))
       (loop repeat current-width
             do (write-char char
                            stream))
       (loop repeat (- width current-width)
             do (write-char #\Space
                            stream))
       (format stream " ~F%"
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
  progress)
