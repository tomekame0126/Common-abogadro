;;;; The Common-Abogadro
;;; step1  <Game Frame> <Sprite Sheets> <Define Package> <Macro> <Character Object> <Draw>
;;;        <Initialize> <Key State> <Game Field>
      
;; step1 <Sprite Sheets>
;; -----------------------------------------------------------------------------------------------
(load "C:\\work\\sprite-sheets.lisp")

;; step1 <Define Package>
;; -----------------------------------------------------------------------------------------------
(defpackage :game
  (:use :common-lisp :lispbuilder-sdl :sprite-sheets)
  (:nicknames :shooting)
  (:export #:Common-abogadro))
(in-package :game)

;; step1 <Macro>
;; -----------------------------------------------------------------------------------------------
(defmacro define-class (name superclasses slots form)
  `(defclass ,name ,superclasses
    ,(mapcar (lambda (slot)
               (let ((keyword (intern (symbol-name slot) :keyword)))
               `(,slot :initarg ,keyword :initform ,form :accessor ,slot)))
              slots)))

;;step1 <Character Object>
;; -----------------------------------------------------------------------------------------------
(define-class object ()
  (id x y width height) 0)
 ; id      graphic id in imageid
 ; x       upper left corner
 ; y       upper left corner
 ; width   from upper left corner
 ; height  from upper left corner

(define-class entity (object)
  (dx dy explode-cnt state) 0)
 ; dx          x direction speed
 ; dy          y direction speed
 ; explode-cnt explosion counter(wait) 
 ; state       ship  0:dead 1:alive 2:explosion 3:revival 
 ;             enemy 0:dead 1:alive 2:damage    3:explosion

;; step1 <Draw Images>
;; -----------------------------------------------------------------------------------------------  
(defun Draw (obj)
  "character draw"
  (sdl:draw-surface-at-* *images* (x obj) (y obj) :cell (id obj)))

;; step1 <Initialize>
;; -----------------------------------------------------------------------------------------------  
(defun Initialize ()
  "graphics initialize"
  (setf (sdl:frame-rate) 60)                      ; frame rate set
  (setf *random-state* (make-random-state t))     ; random set
  (Set-imageid)                                   ; imageid set
  (sdl:show-cursor nil))                          ; cursor not show

;; step1 <Update Key State>
;; -----------------------------------------------------------------------------------------------
(define-class keystate ()
  (right left up down z lshift) nil)
 ; right  right-key
 ; left   left-key
 ; up     up-key
 ; down   down-key
 ; z      z-key
 ; lshift lshift-key

(defgeneric Update-keystate (key boolean keystate))
(defmethod Update-keystate (key boolean keystate)  
  (cond ((sdl:key= key :SDL-KEY-RIGHT)  (setf (right  keystate) boolean))
        ((sdl:key= key :SDL-KEY-LEFT)   (setf (left   keystate) boolean))
        ((sdl:key= key :SDL-KEY-UP)     (setf (up     keystate) boolean))
        ((sdl:key= key :SDL-KEY-DOWN)   (setf (down   keystate) boolean))
        ((sdl:key= key :SDL-KEY-Z)      (setf (z      keystate) boolean))
        ((sdl:key= key :SDL-KEY-LSHIFT) (setf (lshift keystate) boolean))))

;; step 1 <Move Ship>
;; -----------------------------------------------------------------------------------------------
(defgeneric Move-ship (ship keystate))
(defmethod Move-ship (ship keystate)
  (when (or (= (state ship) 1)                                 ; When ship is alive or revival
            (= (state ship) 3))
    (cond ((right keystate) (progn (incf (x ship) (dx ship))   ; set ship id 1 (right turn)
				   (setf (id ship) 1)))
          ((left  keystate) (progn (decf (x ship) (dx ship))   ; set ship id 2 (left turn)
				   (setf (id ship) 2)))
          ((up    keystate)  (decf (y ship) (dy ship)))
          ((down  keystate)  (incf (y ship) (dy ship))))))

;; step1 <Fix Ship Position>
;; -----------------------------------------------------------------------------------------------
(define-class game-field ()
  (field-x field-y width height) 0)
; field-x  game field upper left x
; field-y  game field upper left y
; width    game field width
; height   game field height

(defgeneric Fix-ship-position (ship game-field))
(defmethod Fix-ship-position (ship game-field)
  "ship always inside game-field"
  (when (< (x ship) (field-x game-field))       (setf (x ship) (field-x game-field)))
  (when (< (y ship) (field-y game-field))       (setf (y ship) (field-y game-field)))
  (when (> (x ship) (- (width game-field) 32))  (setf (x ship) (- (width game-field) 32)))
  (when (> (y ship) (- (height game-field) 32)) (setf (y ship) (- (height game-field) 32))))
                                      
;; step1 <Game Frame>
;; -----------------------------------------------------------------------------------------------
(defun Common-abogadro ()
  "main routine"
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio) ; use video and audio
    (sdl:window 640 480 :position #(192 50)              ; size 640*480, position x(192) y(50)
                        :title-caption "ABOGADRO"
                        :icon-caption  "ABOGADRO"
                        :double-buffer t
                        :fullscreen nil)
    ; <Initialize>
      (Initialize)                                       ; graphics initialize

    ; <Charactor Object>
      (let((ship (make-instance       'entity :id 0 :x 304 :y 416 :width 32 :height 32 :dx 4 :dy 4 :state 1))
           (keystate (make-instance   'keystate))         
           (game-field (make-instance 'game-field :field-x 160 :field-y 16 :width 480 :height 464)))

      (sdl:update-display)
      (sdl:with-events (:poll)
        (:quit-event ()
          t)

      ; <Update Key State> 
        (:key-down-event (:key key)
          (if (sdl:key= key :SDL-KEY-ESCAPE)
              (sdl:push-quit-event)
	  (Update-keystate key t keystate)))
        (:key-up-event (:key key)
          (Update-keystate key nil keystate)
          (setf (id ship) 0))                            ; set ship id 0 (normal position)  

        (:idle ()
        ; <Clear Display>
          (sdl:clear-display sdl:*black*)

        ; <Move Ship> 
	  (Move-ship ship keystate)

        ; <Fix Ship Position>
	  (Fix-ship-position ship game-field)
       
        ; <Draw Images>
          (when (= (state ship) 1)
            (Draw ship))                                 ; draw ship

          (sdl:update-display))))))

(Common-abogadro)
