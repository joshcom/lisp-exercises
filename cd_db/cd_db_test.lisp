(load "cd_db.lisp")
(load-db "my-cds.db")
(dump-db)

; Prompt the user for a new cd
; (add-cds)
(dump-db)
(save-db "my-cds.db")

; (select (where :rating 7 :artist "Pink Floyd"))
; (update (where :title "The Mollusk") :rating 10)
; (delete-rows (where :Artist "Ween"))
