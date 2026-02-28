;; Main app entry-point  for my app.
;; Manage command-line file argument inserted from here,
;; check input validation, etc.

(in-package #:bf-interpreter)


;; File to read as test. Hardcoded for the moment.
(defparameter *input-file* "./repeat.bf")


(defun run-interpreter (source)
  "TODO: add here docstring"
  (let ((state (make-bf-state
		:array (make-array 30000 :element-type '(unsigned-byte 8) :initial-element 0)
                :p-counter 0
                :a-counter 0
                :jump-table (pre-parse source))))
    (loop while (< (bf-state-p-counter state) (length source)) do
      (execute (char source (bf-state-p-counter state)) state)
      (incf (bf-state-p-counter state)))))


(defun main ()
  "Main program entry point: reads the filename from command line arguments and runs the interpreter."
  (let ((args (uiop:command-line-arguments)))  ; read provided arguments
    (cond
      ((null args)  ; check is input is null
       (format t "Usage: bf-interpreter <filename.bf>~%"))
      ((not (uiop:file-exists-p (first args)))  ; check if file exists
       (format t "Error: File ~A does not exist.~%" (first args)))
      (t  ; read source code
       (let ((source-code (uiop:read-file-string (first args))))
         (run-interpreter source-code))))))


