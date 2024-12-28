;; copyright (c) 2024 OKABE Gota

; (defun start-repl (&optional path)
;   "Start a REPL. If PATH is provided, load the Lisp file at PATH before starting the REPL."
;   (when path
;     (if (probe-file path)
;         (load path)
;         (format t "Error: File ~a does not exist.~%" path)))
;   (format t "Starting REPL...~%")
;   (loop
;     (format t "> ")
;     (let ((input (read-line)))
;       (unless (string= input "exit")
;         (handler-case
;             (format t "~a~%" (eval (read-from-string input)))
;           (error (e) (format t "Error: ~a~%" e)))
;         (restart-case
;             (return-from start-repl nil)
;           (continue () (format t "Continuing...~%")))))))

(defun start-repl (&optional path)
  "Start simple REPL."
  (when path
    (if (probe-file path)
        (load path)
        (format t "Error: File ~a does not exist.~%" path)))
  (format t "Welcome to LCL REPL!~%")
  (loop
    (format t "~%> ")
    (let ((input (ignore-errors (read))))
      (unless (eq input ':exit) ; :exitで終了
        (if input
            (handler-case
                (print (eval input))
              (error (e)
                (format t "Error: ~A~%" e)))
            (format t "Error: Invalid input.~%"))))))
(start-repl)
