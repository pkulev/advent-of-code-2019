;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun get-input ()
  (with-temp-buffer
    (insert-file-contents "./input")
    (buffer-string)))

(defun parse-program (program)
  (map 'vector #'string-to-number (split-string program ",")))

(cl-defstruct virtual-machine
  (tape (error "tape must be set"))
  (pc -1)
  (eof nil))

(defun virtual-machine-dump-tape (vm)
  (string-join (mapcar #'number-to-string (virtual-machine-tape vm)) ","))

(defconst virtual-machine-opmap
  '((1  op-plus        2)
    (2  op-multiply    2)
    (99 op-program-end 0))
  "VM operation mapping (OPCODE OPERATION-FUNCTION ARGNUM).
ARGNUM says how much elements we must skip before set new PC.")

(defun op-plus (vm)
  (let* ((*lop (virtual-machine-advance% vm))
         (*rop (virtual-machine-advance% vm))
         (retaddr (virtual-machine-advance% vm))
         (lop (virtual-machine-deref vm *lop))
         (rop (virtual-machine-deref vm *rop)))
    (virtual-machine-write vm retaddr (+ lop rop))))

(defun op-multiply (vm)
  (let* ((*lop (virtual-machine-advance% vm))
         (*rop (virtual-machine-advance% vm))
         (retaddr (virtual-machine-advance% vm))
         (lop (virtual-machine-deref vm *lop))
         (rop (virtual-machine-deref vm *rop)))
    (virtual-machine-write vm retaddr (* lop rop))))

(defun op-program-end (vm)
  (setf (virtual-machine-eof vm) t))

(defun virtual-machine-do-forever (tape)
  (let ((vm (make-virtual-machine :tape (copy-seq tape))))
    (while (not (virtual-machine-eof vm))
      (virtual-machine-advance vm))
    vm))

(defun virtual-machine-advance% (vm)
  "Advance program counter of VM and return corresponding value."
  (setf (virtual-machine-pc vm) (+ 1 (virtual-machine-pc vm)))
  (virtual-machine-current-cell vm))

(defun virtual-machine-advance (vm)
  (let* ((opcode (virtual-machine-advance% vm))
         (operation (cadr (assoc opcode virtual-machine-opmap))))
    (funcall operation vm)))

(defun virtual-machine-current-cell (vm)
  (elt (virtual-machine-tape vm) (virtual-machine-pc vm)))

(defun virtual-machine-write (vm pos data)
  (setf (elt (virtual-machine-tape vm) pos) data))

(defun virtual-machine-deref (vm addr)
  (elt (virtual-machine-tape vm) addr))

(defun solve-1 (tape)
  (let ((vm (virtual-machine-do-forever tape)))
    (message "tape: %s\nfirst: %s"
             (virtual-machine-dump-tape vm)
             (elt (virtual-machine-tape vm) 0))))

(defun check-combination (x y tape)
  "Substitute X and Y values in TAPE and return result of program execution."
  (setf (elt tape 1) x)
  (setf (elt tape 2) y)
  (let ((vm (virtual-machine-do-forever tape)))
    (elt (virtual-machine-tape vm) 0)))

(defun bruteforce (tape expected-result)
  (cl-loop for x to 99 append
           (cl-loop for y to 99
                    when (= (check-combination x y tape) expected-result)
                    return (values x y))))

(defun solve-2 (tape expected-result)
  (multiple-value-bind (x y) (bruteforce tape expected-result)
    (message "\nsecond: %s" (+ (* 100 x) y))))

(defun solve ()
  (let ((tape (parse-program (get-input))))
    ;; Required by task description for final result
    (setf (elt tape 1) 12)
    (setf (elt tape 2) 2)
    (solve-1 tape)
    (solve-2 tape 19690720)))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
