;; copyright (c) 2024 OKABE Gota

(defvar should-quit nil)
(defun start-repl (&optional path)
  "Start simple REPL."
  (when path
    (format t "~%Loading: ~a~%" path)
    (if (probe-file path)
        (load path)
        (format t "Error: File ~a does not exist.~%" path)))
  (loop
    (format t "~%> ")
    (let ((input (ignore-errors (read))))
      (if (eq input ':exit) ; :exitで終了
        (return T)
        (if input
            (handler-case
                (print (eval input))
              (error (e)
                (format t "Error: ~A~%" e)))
            (format t "Error: Invalid input.~%"))))))
