;; Main app entry-point  for my app.
;; Manage command-line file argument inserted from here,
;; check input validation, etc.

(in-package #:bf-interpreter)


;; File to read, use as test. Hardcoded for the moment.
(defparameter input-file  "./repeat.bf")

(defun run-interpreter (source)
  "TODO: add here docstring"
  (let ((state (make-bf-state :array (make-array 30000 :initial-element 0)
                              :p-counter 0
                              :a-counter 0
                              :jump-table (pre-parse source))))
    (loop while (< (bf-state-p-counter state) (length source)) do
      (execute (char source (bf-state-p-counter state)) state)
      (incf (bf-state-p-counter state)))))

(defun main ()  ;; TODO: here use my main loop
  "My main function in which I encapsulate my interpreted main loop"
   ; (let ((src (uiop:read-file-string input-file)))  ; Read file source code into a string
  )
  


