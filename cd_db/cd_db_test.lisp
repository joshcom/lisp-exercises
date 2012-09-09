(load "cd_db.lisp")
(add-record (make-cd "Wish You Were Here" "Pink Floyd" 7 nil))
(add-record (make-cd "The Mollusk" "Ween" 8 t))
(add-record (make-cd "Chocolate and Cheese" "Ween" 7 t))
(dump-db)

; Prompt the user for a new cd
(add-cds)
(dump-db)
