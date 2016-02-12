;;;; package.lisp
(defpackage :game
 (:use :common-lisp :lispbuilder-sdl
       :sprite-sheets :map-list :enemy-map-list :audio-list :move-pattern :shot-data)
 (:nicknames :shooting)
 (:export #:Common-abogadro))

