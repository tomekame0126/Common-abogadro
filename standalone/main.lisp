;; Common-abogadro
;;--------------------------
(defun main ()

;; Load SDL
;;--------------------------
(cffi:define-foreign-library sdl
  (t (:default "SDL")))
(cffi:use-foreign-library sdl)

;; Run the game
;;--------------------------
(sdl:with-init ()
  (game:Common-abogadro))

;; Quit
;;--------------------------
#+sbcl (sb-ext:exit)
#-sbcl (quit))
