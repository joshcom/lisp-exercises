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

; Save the state of the database to a file
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

; Loads the database from a file
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

; Function to create a new p-list representing a CD
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

; select implemented 
; e.g. (select (where :rating 7 :artist "Pink Floyd"))
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

; update implemented
; e.g. (update (where :title "The Mollusk") :rating 10)
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
          #'(lambda (row)
                   (when (funcall selector-fn row)
                     (if title    (setf (getf row :title) title))
                     (if artist   (setf (getf row :artist) artist))
                     (if rating   (setf (getf row :rating) rating))
                     (if ripped-p (setf (getf row :ripped) ripped)))
                   row) *db*)))

; Remove records from the database
; (delete-rows (where :Artist "Ween"))
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

; where implemented
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
        (if title    (equal (getf cd :title) title) t)
        (if artist   (equal (getf cd :artist) artist) t)
        (if rating   (equal (getf cd :rating) rating) t)
        (if ripped-p (equal (getf cd :ripped) ripped) t))))

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
