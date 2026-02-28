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
			    (#\] (if (null stack)  ; check if there is a corresponding [ indexed
				     (error "Syntax Error: Unmatched ']' at position ~A" y)
				     (let ((k (pop stack)))
				       (setf (gethash y jump-table) k)
				       (setf (gethash k jump-table) y)))))
          ;; After executing all the loop, return the jump-table
          finally (if (not (null stack))  ; Check if all [ had a matching ]
                      (error "Syntax Error: Unmatched '[' at positions ~A" stack)
                      (return jump-table)))))


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
  ;; Get the state field values here for better accessibility
  (let* ((tape (bf-state-array state))
         (a-ptr (bf-state-a-counter state))
         (p-ptr (bf-state-p-counter state))
         (current-val (aref tape a-ptr)))
    (case instruction  ; each instruction it's associated to a certain character
      ;; Instructions to Increment or decrement cells value
      (#\+ (setf (aref tape a-ptr) (mod (1+ (aref tape a-ptr)) 256)))
      (#\- (setf (aref tape a-ptr) (mod (1- (aref tape a-ptr)) 256)))
      ;;OLD CODE
      ;; (#\+ (if (= a-ptr 255) 0 (incf (aref tape a-ptr))))
      ;; (#\- (if (= a-ptr 0) 255 (decf (aref tape a-ptr))))
      ;; Instructions to read or print cell values
      (#\. (progn
             (write-char (code-char current-val))
             (finish-output))) ;; Critical: Forces the character to show in terminal
      (#\, (setf (aref tape a-ptr)
                 (char-code (read-char)))) ;; Pauses and waits for user keyboard input
      ;; Instructions to move array pointer by +1 or -1
      (#\< (if (> a-ptr 0)
               (decf (bf-state-a-counter state))
               (error "Pointer Underflow: Attempted to move pointer below 0")))
      (#\> (if (< a-ptr (1- (length tape)))
	       (incf (bf-state-a-counter state))
	       (error "Pointer Overflow: Attempted to point past length: ~A" (length tape))))
      ;; Instructions implementing the jumps
      (#\[ (when (zerop current-val)
             (setf (bf-state-p-counter state)
                   (gethash p-ptr (bf-state-jump-table state)))))
      (#\] (unless (zerop current-val)
	     ;; Jump backward to the matching [
             (setf (bf-state-p-counter state)
                   (gethash p-ptr (bf-state-jump-table state))))))))


  
