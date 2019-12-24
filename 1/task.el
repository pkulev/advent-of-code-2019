;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun fuel-required (mass)
  (let ((fuel (- (round (/ mass 3)) 2)))
    (if (>= fuel 0) fuel 0)))

(cl-defun fuel-required-rec (mass &optional (acc 0))
  (if (zerop mass)
      acc
    (let ((fuel (fuel-required mass)))
      (fuel-required-rec fuel (+ acc fuel)))))

(defun solve (input)
  (cl-reduce #'+ (mapcar 'fuel-required input)))

(defun solve-including-fuel-mass (input)
  (cl-reduce #'+ (mapcar 'fuel-required-rec input)))

(let ((input (with-temp-buffer
               (insert-file-contents "./input")
               (mapcar #'string-to-number (split-string (buffer-string))))))
  (message "fuel for modules: %s \nfuel including fuel mass: %s"
           (solve input) (solve-including-fuel-mass input)))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
