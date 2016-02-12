;;;; The Common-Abogadro
;;; step1  <Game Frame> <Sprite Sheets> <Define Package> <Macro> <Character Object> <Draw>
;;;        <Initialize>
      
;; step1 <Sprite Sheets>
;; -----------------------------------------------------------------------------------------------
(load "C:\\work\\sprite-sheets.lisp")

;; step2 <Enemy Map List>
;; -----------------------------------------------------------------------------------------------
(load "C:\\work\\enemy-map-list.lisp")

;; step2 <Enemy Map List>
;; -----------------------------------------------------------------------------------------------
(load "C:\\work\\move-pattern.lisp")

;; step1 <Define Package>
;; -----------------------------------------------------------------------------------------------
(defpackage :game
  (:use :common-lisp :lispbuilder-sdl :sprite-sheets :enemy-map-list :move-pattern)
  (:nicknames :shooting)
  (:export #:Common-abogadro))
(in-package :game)

;; step2 <Macro>
;; -----------------------------------------------------------------------------------------------
(defmacro define-class (name superclasses slots form)
  `(defclass ,name ,superclasses
    ,(mapcar (lambda (slot)
               (let ((keyword (intern (symbol-name slot) :keyword)))
               `(,slot :initarg ,keyword :initform ,form :accessor ,slot)))
              slots)))

;;step2 <Character Object>
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

(define-class foe (entity)
  (move-cnt damage-cnt life-cnt kind) 0)
 ; move-cnt   moving counter      (distance)  
 ; damage-cnt enemy damage counter(wait)
 ; life-cnt   enemy life counter  (life time)
 ; kind       kind of enemy

(define-class enemy-manager ()
  (enemy-list) nil)
 ; enemy-list list of enemy

(define-class game-field ()
  (field-x field-y width height) 0)
; field-x  game field upper left x
; field-y  game field upper left y
; width    game field width
; height   game field height

;; step3 <Draw Images>
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

(defvar *scroll-cnt* 0)                           ; scroll counter
(defvar *enemy-map-pointer* 64)

(defgeneric Generate-enemy (map enemy-manager))
(defmethod Generate-enemy (map enemy-manager)
  (when (and (= (mod *scroll-cnt* 64) 0)
                (<= *scroll-cnt* 4096))    
      (dotimes (j 10)
        (when (/= (aref map *enemy-map-pointer* j) -1)
          (case (aref map *enemy-map-pointer* j)
                 ((7 50 80)                              ; when id is 7 or 50 or 80
                   (let ((enemy (make-instance 'foe      ; small class enemy generate
                                 :id (aref map *enemy-map-pointer* j)
                                 :x (+ 160 (* j 32)) :y 0 :kind 1 :state 1)))
                   (push enemy (enemy-list enemy-manager))))

                 ((76 78)                                ; when id is 76 or 78
                   (let ((enemy (make-instance 'foe      ; middle class enemy generate
                                 :id (aref map *enemy-map-pointer* j)
                                 :x (+ 160 (* j 32)) :y 0 :kind 2 :state 1)))
                   (push enemy (enemy-list enemy-manager))))

                 ((70 71 72)                             ; when id is 70 or 71 or 72
                   (let ((enemy (make-instance 'foe      ; large class enemy generate
                                  :id (aref map *enemy-map-pointer* j)
                                  :x (+ 160 (* j 32)) :y 0 :kind 3 :state 1)))
                   (push enemy (enemy-list enemy-manager))))

		 ((17 18)                                ; when id is 17 or 18
                   (let ((enemy (make-instance 'foe      ; item generate
                                  :id (aref map *enemy-map-pointer* j)
                                  :x (+ 160 (* j 32)) :y 0  :dx 0 :dy 1 :kind 4 :state 1)))
                   (push enemy (enemy-list enemy-manager)))))))
      (decf *enemy-map-pointer*)))

(defgeneric Move-enemy (enemy-manager game-field))
(defmethod Move-enemy (enemy-manager game-field)
  (dolist (enemy (enemy-list enemy-manager))              
    (when (= (state enemy) 1)
      (case (id enemy)
        ((7 8 9 80 81 82)    ; id 7 8 9 or id 80 81 82  small size enemy
          (let((row (mod (move-cnt enemy) 16)))   ; row from 0 to 15
            (case (id enemy)
              ((7 8 9)       ; id 7 8 9(yellow)
              (setf (dx enemy) (aref *enemy-move-pattern1* row 0)
                    (dy enemy) (aref *enemy-move-pattern1* row 1)))
              ((80 81 82)    ; id 80 81 82(purple)
              (setf (dx enemy) (aref *enemy-move-pattern3* row 0)
                    (dy enemy) (aref *enemy-move-pattern3* row 1))))))    
        ((50 52)             ; id 50 52(blue)                    
          (let((row (mod (move-cnt enemy) 12)))   ; row from 0 to 11
            (setf (dx enemy) (aref *enemy-move-pattern2* row 0)
                  (dy enemy) (aref *enemy-move-pattern2* row 1)))) 
        ((76 78)             ; id 76 78 middle size enemy         
           (if (= (id enemy) 76)
             (setf (dx enemy) (first *enemy-move-pattern4*)
                   (dy enemy) (second *enemy-move-pattern4*))
             (setf (dx enemy) (first *enemy-move-pattern5*)
                   (dy enemy) (second *enemy-move-pattern5*))))
        ((70 71)             ; id 70 71 large size enemy          
          (let((row (mod (move-cnt enemy) 32)))   ; row from 0 to  31
            (if (= (id enemy) 70)
              (setf (dx enemy) (aref *enemy-move-pattern6* row 0)
                    (dy enemy) (aref *enemy-move-pattern6* row 1))
              (setf (dx enemy) (aref *enemy-move-pattern7* row 0)
                    (dy enemy) (aref *enemy-move-pattern7* row 1)))))
        ((72)                ; id 72 large size enemy         
          (setf (dx enemy) (first *enemy-move-pattern8*)
                (dy enemy) (second *enemy-move-pattern8*))))
      (incf (x enemy) (dx enemy))
      (incf (y enemy) (dy enemy))
      (incf (move-cnt enemy) 1)
      (when (or (>= (y enemy) (height game-field))                           ; bottom of game field
                (>= (x enemy) (width game-field))                            ; right of game field   
                (<= (x enemy) (- (field-x game-field) (* 32 (kind enemy))))) ; left of game field
        (setf (state enemy) 0)))))

(defgeneric Change-id (enemy))
(defmethod Change-id (enemy)
  (case (id enemy)
    ((7 8 9)
       (case (mod (floor (move-cnt enemy) 4) 4)             ; enemy id change
         (0 (setf (id enemy) 7))                            ; change pattern --> 0000, 1111 , 2222 , 3333
         (1 (setf (id enemy) 8))                            ;                    id7 , id8 ,  id9  , id8
         (2 (setf (id enemy) 9))
         (3 (setf (id enemy) 8))))
    ((50 52)
       (case (mod (floor (move-cnt enemy) 2) 2)             ; enemy id change
         (0 (setf (id enemy) 50))                           ; change pattern --> 0000, 1111
         (1 (setf (id enemy) 52))))                         ;                    id50 , id52
    ((80 81 82)
       (case (mod (floor (move-cnt enemy) 4) 4)             ; enemy id change
         (0 (setf (id enemy) 80))                           ; change pattern --> 0000, 1111 , 2222 , 3333
         (1 (setf (id enemy) 81))                           ;                    id80 ,id81 , id82 , id81
         (2 (setf (id enemy) 82))
         (3 (setf (id enemy) 81))))))

(defgeneric Remove-enemy (enemy-manager))
(defmethod Remove-enemy (enemy-manager)
  (setf (enemy-list enemy-manager) 
	(delete-if #'(lambda (enemy) (= (state enemy) 0)) (enemy-list enemy-manager))))

(defgeneric Remove-all-enemy (enemy-manager))
(defmethod Remove-all-enemy (enemy-manager)
  (when (> *scroll-cnt* 4096)
    (setf (enemy-list enemy-manager) nil)))

(defgeneric Enemy-draw (enemy-manager))
(defmethod Enemy-draw (enemy-manager)
  (dolist (enemy (enemy-list enemy-manager))
    (Change-id enemy)
    (Draw enemy)))

(defun Scroll-counter ()
  (incf *scroll-cnt*))
                                 
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

      (let((enemy-manager (make-instance 'enemy-manager))
           (game-field (make-instance    'game-field :field-x 160 :field-y 16 :width 480 :height 464)))
 
      (sdl:update-display)
      (sdl:with-events (:poll)
        (:quit-event ()
          t)

        (:idle ()
        ; <Clear Display>
          (sdl:clear-display sdl:*black*)

          (Move-enemy enemy-manager game-field)

          (Generate-enemy *enemy-map2* enemy-manager)

          (Enemy-draw enemy-manager)

          (Remove-enemy enemy-manager)
        ; (Remove-all-enemy enemy-manager)

          (Scroll-counter)       

          (sdl:update-display))))))

(Common-abogadro)

