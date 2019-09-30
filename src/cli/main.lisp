(defpackage #:progressor/cli/main
  (:use #:cl #:defmain)
  (:import-from #:progressor/client
                #:list-progresses)
  (:import-from #:progressor/models
                #:get-description
                #:print-progress)
  (:shadow #:list)
  (:export
   #:main))
(in-package progressor/cli/main)


(defcommand (main list) ()
  (handler-case
      (let ((progresses (list-progresses)))
        (cond
          (progresses
           (loop for progress in progresses
                 do (format *standard-output*
                            "~A~%"
                            (get-description progress))
                    (print-progress progress :stream *standard-output*)
                    (format *standard-output*
                            "~%")))
          (t (format t "No jobs.~%"))))
    (usocket:connection-refused-error ()
      (format *error-output* "Unable to connect to the server.")
      (values 1))))


(defcommand (main server) ()
  (format t "TODO: implement a server command."))


(defmain main (&subcommand))
