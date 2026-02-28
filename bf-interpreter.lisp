;; Simple interpreter to try a little more serious program in CL
;; The BF inerpreter it's gonna use an integer array of len 30000
;; and two pointers:
;; - PC: program counter
;; - AC: Array counter


;; TODO:
;; 1. make that can read bigger file with no max pre-set len
;; 2. take the input from user on which file to read, now hard-coded

;; TEST:
;; simple-test: ,>,[<.>-]

;; (defpackage :bf-interpreter
;;   (:nicknames :brainfuck :bf)
;;   ;(:use :cl)
;;   (:export :main))

(in-package :bf-interpreter)

;; OLD GLOBAL VAR, NOW USING A DEFSTRUCT
;; ;; Default to length of 30000 elements to follow the original interpreter
;; (defparameter *array* (make-array 30000 :element-type '(unsigned-byte 8)  :initial-element 0))
;; ;; Program counter index
;; (defparameter *p-counter* 0)
;; ;; Array counter index
;; (defparameter *a-counter* 0)
;; ;; Jump table to preparse for better efficiency
;; (defparameter *jump-table* nil)

(defstruct bf-state
  "Encapsulate all datastructure used for interpreter state
  array: Array to save interpreter global state
  p-counter: Index for the program counter
  a-counter: Index for the array pointer
  jump-table: Hash-table to save the square brakets indexes  
"
  array
  p-counter
  a-counter
  jump-table)



;; Function to preparse source code and get a jumo table
(defun pre-parse (code-string)
  "This function pre-parse the BF code to make a lookup table of square brackets.
   The idea is to read it like like a stack:
   - If a [ is read, add it to stack
   - If a ] is read, pop from stack and add the 2 references to the table
"
  (let ((jump-table (make-hash-table))  ; hash-table to return with stored indexes
	(stack '()))  ; temporary stack to use while looping
    (loop for x across code-string
	  for y from 0 do (case x
			    (#\[ (push y stack))  ;; found the [
			    (#\] (let ((k (pop stack)))  ; get the [ index
				   (setf (gethash y jump-table) k)
				   (setf (gethash k jump-table) y)))))  ; added last parens
    ;; TODO: Here add an check, if stack not nil rise error else return jump table
    jump-table))


;; TEST
;; Use this function to test if the parsing was done right
;; Expected (3 . 8) and (8 . 3) for my test file
(defun test-parse (hs)
  "Little utility function to test if the source code it's parsed correctly"
  (maphash (lambda (key val)
	     (format t "key: ~A value: ~A~%" key val))
	   hs))


;; Utility wrapper function to limit cell value range [0, 255]
(defun wrap-add (val)
  "Wrapper function to get the correct value on state array cells,
   simulating unsigned byte integer."
  (cond ((> val 255) 0)
	((< val 0) 255)
	(t val)))


;; Read file to interprete
(defun execute (instruction state)
  "Function that execute the main loop command, executing the interpreter logic
   - Instruction is a character in this set |8|: < > + - . , [ ]
   - State is a struct of type bf-state
"
  (let ((current-cell-value (aref (bf-state-array state) (bf-state-a-counter state))))
    (case instruction  ; each instruction it's associated to a certain character
      ;; Instructions to Increment or decrement cells value
      (#\+ (incf (aref (bf-state-array state) (bf-state-a-counter state)))) 
      (#\- (decf (aref (bf-state-array state) (bf-state-a-counter state)))) 
      ;; Instructions to read or print cell values
      (#\. (write-char (code-char current-cell-value)))
      (#\, (setf (aref (bf-state-array state) (bf-state-a-counter state))
		 (char-code (read-char))))
      ;; Instructions to move array pointer by +1 or -1
      (#\< (if (> (bf-state-a-counter state) 0) (decf (bf-state-a-counter state)) 0))
      (#\> (incf (bf-state-a-counter state)))  ; TODO: add check or data chenge data structure
      ;; Instructions implementing the jumps
      (#\[ (when (zerop current-cell-value)
             (setf (bf-state-p-counter state) 
                   (gethash (bf-state-p-counter state) (bf-state-jump-table state)))))
      (#\] (unless (zerop current-cell-value)
             ;; Jump backward to the matching [
             (setf (bf-state-p-counter state) 
                   (gethash (bf-state-p-counter state) (bf-state-jump-table state))))))))


  
