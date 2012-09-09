; Loops to prompt users to add however many CDs they want
(defun add-cds()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

; The cd database
(defvar *db* nil)

; Dumps the contents of the cd database
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

; Adds a record to the DB
(defun add-record (cd)
  (push cd *db*))

; Function to create a new p-list representing a CD
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

; read prompt to input full cd
(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0) ; Default to 0, if no integer can be parsed
    (y-or-n-p "Ripped [y/n]")))  ; simple helper for "boolean" input

; read prompt for cd attribute
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))
