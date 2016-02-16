;;;; The Common-abogadro
;;; step1  <Game Frame> <Sprite Sheets> <Define Package> <Macro> <Character Object> <Draw Images>
;;;        <Initialize> <Update Key State> <Fix Ship Position> <Move Ship>
;;; step2  <Map> <Scroll> 
;;; step3  <Font> <Stage Class> <Start Stage Message>
;;; step4  <Game Start Message> <Game Over Message> <Judge Game Over> <Set Screen Mode>
;;; step5  <Score Panel>
;;; step6  <Move Shot> <Set Shot> <Remove Dead Shot> <Draw Shot>
;;; step7  <Generate Enemy> <Judge Stage End> <Move Enemy> <Change Id> <Remove Enemy>
;;;        <Draw Enemy> <Reset Variables> <Rotate Map Pointer> 
;;; step8  <Move Balloon> <Generate Balloon> <Draw Balloon> <Move Item> <Remove Item>
;;;        <Hit Item> <Draw Item> 
;;; step9  <Move Enemy Shot> <Set Enemy Shot> <Remove Dead Enemy Shot>
;;; step10 <Audio> <Enemy Hit P> <Damage Counter> <Explode Enemy> <Score Up> <Change Damaged Id>
;;;        <Set Reset Id>
;;; step11 <Set Bomb Key> <Set Bomb> <Explode Bomb> <Remove Explode Bomb> <Draw Bomb> 
;;; step12 <Ship Hit P> <Explode Counter> <Revive Counter> <Draw Ship Explosion> <Set Explosion>
;;;        <Explode Large Enemy> <Remove Explode Large Enemy> <Draw Explosion Large Enemy>
;;;        <N-ship Zero P>
;;; step13 <Enemy Shot Data> <Enemy Shot Pattern> <Set Enemy Shot> <Set Enemy Center>
;;;        <Set Enemy Variables> <Set Enemy Shot Direction> <Set Repeat Flag OFF>
;;;        <Set Enemy Shot Timing> <Set Enemy Shot Angle> <Charge Enemy Shot> 
;;;      
;; step1 <Sprite Sheets>
;; -----------------------------------------------------------------------------------------------
;(load "C:\\work\\sprite-sheets.lisp")

;; step2 <Map>
;; -----------------------------------------------------------------------------------------------  
;(load "C:\\work\\map-list.lisp")

;; step7 <Enemy Map List>
;; -----------------------------------------------------------------------------------------------
;(load "C:\\work\\enemy-map-list.lisp")

;; step7 <Enemy Map List>
;; -----------------------------------------------------------------------------------------------
;(load "C:\\work\\move-pattern.lisp")

;; step10 <Audio>
;; -----------------------------------------------------------------------------------------------  
;(load "C:\\work\\audio-list.lisp")

;; step13 <Enemy Shot Data>
;; -----------------------------------------------------------------------------------------------  
;(load "C:\\work\\shot-data.lisp")

;; step1 <Define Package>
;; -----------------------------------------------------------------------------------------------
;(defpackage :game
;  (:use :common-lisp :lispbuilder-sdl :sprite-sheets :map-list :enemy-map-list :move-pattern
;   :audio-list :shot-data)
;  (:nicknames :shooting)
;  (:export #:Common-abogadro))
(in-package :game)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

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
  (dx dy explode-cnt state revival-cnt) 0)     ; + ship explosion revival
 ; dx          x direction speed
 ; dy          y direction speed
 ; explode-cnt explosion counter(wait) 
 ; state       ship  0:dead 1:alive 2:explosion 3:revival 
 ;             enemy 0:dead 1:alive 2:damage    3:explosion

;;step13 <Enemy Shot Pattern>
;; -----------------------------------------------------------------------------------------------
(define-class shotpattern ()
  (timing angle-store shot-counter beforetime pattern-number pattern-number-store 
   pattern-cnt pattern-cnt-store repeat-flag count-flag first-x first-y first-angle) 0)
 ; timing                shottiming
 ; angle-store           angle-store
 ; shot-counter          number of shot per battery 
 ; beforetime            interval before shot
 ; pattern-number        shotpattern0-13
 ; pattern-number-store  store pattern number
 ; pattern-cnt           number of shotpattern
 ; pattern-cnt-store     store pattern counter
 ; repeat-flag           0:once 1:repeat
 ; count-flag            repeat count
 ; first-x               x position of first shot
 ; first-y               y position of first shot
 ; first-angle           angle of first shot

(define-class foe (entity shotpattern)
  (move-cnt damage-cnt life-cnt kind) 0)
 ; move-cnt   moving counter      (distance)  
 ; damage-cnt enemy damage counter(wait)
 ; life-cnt   enemy life counter  (life time)
 ; kind       kind of enemy

(define-class enemy-manager ()
  (enemy-list enemy-shot-list) nil)
 ; enemy-list       enemy list
 ; enemy-shot-list  enemy shot list

(define-class shot-manager ()
  (shot-list) nil)
 ; shot-list      4 shot store

(define-class balloon-manager ()
  (balloon-list balloon-cnt) nil)
 ; balloon-list    list of balloon       
 ; balloon-cnt     max 2 balloon   

(define-class item-manager ()
  (item-list item-flag) nil)
 ; item-list  list of item
 ; item-flag  item get flag

(define-class score ()
  (score highscore oneup n-ship n-bomb) 0)
 ; score       score counter       
 ; highscore   hight score
 ; oneup       plus one ship
 ; n-ship      number of ship
 ; n-bomb      number of bomb

(define-class explosion-manager ()
  (bomb-list bomb-cnt bomb-number) nil)
; bomb-list    list of bomb       
; bomb-cnt     60 times waiting
; bomb-number  max 5

;; step1 <Draw Images>
;; -----------------------------------------------------------------------------------------------  
(defun Draw (obj)
  "character draw"
  (sdl:draw-surface-at-* *images* (round (x obj)) (round (y obj)) :cell (id obj)))

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

;; step3 <Font>
;; -----------------------------------------------------------------------------------------------
(defparameter *path-font16* "C:\\WINDOWS\\Fonts\\msmincho.ttc")
(defparameter *font16* (make-instance 'sdl:ttf-font-definition
                                :size 16
                                :filename (sdl:create-path *path-font16*)))
(defvar *menu-font*)                                     ; menu font

(defun Set-font ()
  (setf *menu-font*  (sdl:initialise-font *font16*)))

;; Step3 <Stage Class>
;; -----------------------------------------------------------------------------------------------
(define-class stage ()
  (stage-flag stage-number title-loop ending-loop start) t)
 ; stage-flag        on-stage or not
 ; stage-number      map change
 ; title-loop        waiting for input-key
 ; ending-loop       waiting for input-key
 ; start             game start

;; Step3 <Start Stage Message>
;; -----------------------------------------------------------------------------------------------
(defvar *atlas*)                                           ; map set
(defvar *enemymap*)                                        ; enemy map set
(defvar *BGM*)
(defvar *enemy-generate-flag* nil)
(defvar *rotation* '(0 1 2 3 4 5 6 7))
(defvar *bossbgm-on-air* nil)                              ; now BossBGM playing

(defgeneric Stage-start-message (stage keystate))
(defmethod Stage-start-message (stage keystate)            ; stage start message
  "Draw stage start message and set game parameters"
  (when (eql (stage-flag stage) t)
    (setf (stage-flag stage) nil
          (z keystate) nil                                 ; z-key reset
	  (lshift keystate) nil                            ; lshift-key reset
          *rotation* '(0 1 2 3 4 5 6 7)                    ; boss stage set   
          *enemy-generate-flag* t                            
          *bossbgm-on-air* nil)
    (incf (stage-number stage) 1)
    (case (stage-number stage)
      (1 (setf *atlas* *map1*	      
               *BGM* *samplebgm1*
               *enemymap* *enemy-map1*))
      (2 (setf *atlas* *map2*
               *BGM* *samplebgm2*
               *enemymap* *enemy-map2*))
      (3 (setf *atlas* *map3*
               *BGM* *samplebgm3* 
               *enemymap* *enemy-map3*)))
    (sdl:clear-display sdl:*black*)
    (sdl:draw-string-solid-* 
         (format nil "S T A G E  ~d" (stage-number stage)) 272 208 :color sdl:*white* :font *menu-font*)
    (sdl:update-display)
    (sleep 3)
    (Play-music *BGM*)))

;; step 2 <Scroll>
;; -----------------------------------------------------------------------------------------------  
(defvar *scroll-cnt* 0)                                  ; scroll counter
(defvar *map-pointer* 64)                                ; map start line
(defvar *draw-position-y* 0)                             ; y-axis start position
(defvar *repeat* nil)                                    ; scroll repeat flag
(defvar *BGM-change-flag* nil)                           ; BGM --> Boss BGM flag

(defun Rotate-map-pointer ()
  "Move last element to front of the list"
  (let ((x *rotation*))                                  ; list rotation right
    (setf *rotation* (append (last x) (butlast x)))))    ; --> (7 0 1 2 3 4 5 6)

(defun BossBGM-set ()
  (cond ((and (= *map-pointer* 4)                        ; when *map-pointer* is 4
	      (eql *bossbgm-on-air* nil))                ; BGM stop
          (Stop-sound))
        ((and (= *map-pointer* 0)                        ; when *map-pointer* is 0
              (eql *bossbgm-on-air* nil))                ;
          (setf *BGM* *bossbgm*                          ; BossBGM play     
                *BGM-change-flag* t))))                  ; BGM changeflag on

(defun BossBGM-change ()
  (when (eql *BGM-change-flag* t)
    (setf *BGM-change-flag* nil
          *bossbgm-on-air* t)
    (Play-music *BGM*)))                                 ;;;;;Do not use midi file. Delay Happend ! ;;;;;

(defun Set-map-pointer ()
  "Set map pointer"
  (incf *scroll-cnt* 2)                                  ; 2 dot scroll
  (when (eql (mod *scroll-cnt* 64) 0)                    ; mapchip draw position
    (setf *draw-position-y* 0)
    (decf *map-pointer*)                                 ; else scroll-line -1
    (BossBGM-set)
    (BossBGM-change)    
    (when (< *map-pointer* 0)                            ; when scroll-line is 0 (end line)      
      (setf *repeat* t))))                               ; map repeat flag on

(defun Scroll-background (map)
  "draw background"  
    (setf *draw-position-y* (+ -48 (mod *scroll-cnt* 64))) ; scroll start from y(-48) to y(16)
    (cond ((eql *repeat* nil)                              ; when map repeat flag off
            (dotimes (i 8)                                 ; 8 row
              (dotimes (j 5)                               ; 5 column 
                (sdl:draw-surface-at-* *images* (+ 160 (* j 64)) (+ *draw-position-y* (* i 64))
                    :cell (aref map (+ *map-pointer* i) j)))))
          ((eql *repeat* t)                                ; when map repeat flag on
            (when (= (mod *scroll-cnt* 64) 0)              ; when 64 dot scroll
              (Rotate-map-pointer))                        ; map rotate
            (dotimes (i 8)
              (setf *map-pointer* (nth i *rotation*))      ; read element of the list
              (dotimes (j 5)
                (sdl:draw-surface-at-* *images* (+ 160 (* j 64)) (+ *draw-position-y* (* i 64))
                    :cell (aref map *map-pointer* j)))))))

(defun Scroll-mask ()
  (sdl:draw-box-*   0   0 160 480 :color sdl:*black*)    ; mask scroll left  side
  (sdl:draw-box-* 160   0 320  16 :color sdl:*black*)    ; mask scroll upper side
  (sdl:draw-box-* 160 464 320 480 :color sdl:*black*)    ; mask scroll lower side
  (sdl:draw-box-* 480   0 640 480 :color sdl:*black*))   ; mask scroll right side

;; Step4 <Game Start Message>
;; -----------------------------------------------------------------------------------------------
(defvar *screen-mode* 1)        ; screen mode toggle switch  0:fullscreen 1:windowed
(defvar *switch* nil)           ; screen flag

(defgeneric Game-start-message (pointer character stage keystate))
(defmethod Game-start-message (pointer character stage keystate)   ; game start message
  "Draw game opening message"
  (sdl:clear-display sdl:*black*)
  (Stop-sound)                                                     ; stop ending BGM if playing
 ; title
  (dotimes (i 8)                                                   ; show title
    (setf (x character) (+ 192 (* i 32))
          (id character) (+ 19 i))
    (Draw character))
 ; memu
  (sdl:draw-string-solid-* "S T A R T" 224 328 :color sdl:*white* :font *menu-font*) ; show menu
  (if (= *screen-mode* 0)
    (sdl:draw-string-solid-* "S C R E E N : F U L L" 224 360 :color sdl:*white* :font *menu-font*)
    (sdl:draw-string-solid-* "S C R E E N : W I N D O W" 224 360 :color sdl:*white* :font *menu-font*))
  (sdl:draw-string-solid-* "E X I T" 224 392 :color sdl:*white* :font *menu-font*)
 ; select menu
  (cond ((up keystate)                                            ; select menu
         (decf (y pointer) 32) 
         (setf (up keystate) nil) 
         (when (<= (y pointer) 328)                               ; y:328 is START menu position
                (setf (y pointer) 328
                      (start stage) t)))
        ((down keystate)
          (incf (y pointer) 32)
          (setf (down keystate) nil)          
          (when (>= (y pointer) 392)                              ; y:392 is EXIT menu position
            (setf (y pointer) 392
                  (start stage) nil)))
        ((lshift keystate)
          (when (= (y pointer) 360)
            (if (= *screen-mode* 1)                               ; screen-mode toggle switch                   
              (setf *screen-mode* 0)                              ; 0:fullscreen  1:windowed
              (setf *screen-mode* 1))
            (setf *switch* t))
          (setf (lshift keystate) nil)))                     
 ; show pointer
  (sdl:draw-string-solid-* ">" (x pointer) (y pointer) :color sdl:*white* :font *menu-font*)
 ; game start or exit
  (cond ((and (z keystate) (eql (start stage) t) (= (y pointer) 328))              ; input z-key on start menu
          (setf (title-loop stage) nil
                (z keystate) nil))                                                 ; z key state reset 
        ((and (z keystate) (eql (start stage) nil) (= (y pointer) 392))            ; input z-key on exit menu  
          (sdl:push-quit-event)))
  (sdl:update-display))

;; Step12 <N-ship Zero P>
;; -----------------------------------------------------------------------------------------------
(defvar *interval* 0)

(defgeneric N-ship-zero-p (score stage))
(defmethod N-ship-zero-p (score stage)
  (when (= (n-ship score) 0)
    (incf *interval*)
    (stop-sound)
    (when (= *interval* 200)
      (setf *interval* 0
	    (ending-loop stage) t))))

;; Step7 <Reset Variables>
;; -----------------------------------------------------------------------------------------------
(defvar *enemy-map-pointer* 64)
(defvar *stage-end-flag* nil)
(defvar *bossbgm-flag* nil)

(defgeneric Reset-variables (stage enemy-manager balloon-manager score ship shot-manager))
(defmethod Reset-variables (stage enemy-manager balloon-manager score ship shot-manager)
  "reset variables" 
  (setf (title-loop stage) t
        (stage-flag stage) t
	*bossbgm-flag* nil
	*stage-end-flag* nil
        *scroll-cnt* 0
        *draw-position-y* 0
        *map-pointer* 64
        *repeat* nil
        *enemy-map-pointer* 64
        (shot-list shot-manager) nil
	(enemy-list enemy-manager) nil
        (enemy-shot-list enemy-manager) nil
        (stage-number stage) 0
        (balloon-cnt balloon-manager) 0
	(balloon-list balloon-manager) nil
        (score score) 0
        (n-ship score) 3
        (n-bomb score) 3
        (x ship) 304
        (y ship) 416
        (state ship) 1))

;; Step4 <Game Over Message> + score + ship explosion
;; -----------------------------------------------------------------------------------------------
(defgeneric Game-over-message (stage enemy-manager score balloon-manager ship shot-manager keystate))
(defmethod Game-over-message (stage enemy-manager score balloon-manager ship shot-manager keystate)
  "Draw game ending" 
  (case (n-ship score)
    ((0)
      ; game over message
      (sdl:draw-string-solid-* "GAME OVER" 284 200  :color sdl:*white* :font *menu-font*)
      (sdl:update-display))
    (otherwise
      (sdl:clear-display sdl:*black*)
      ; congratulations message
      (sdl:draw-string-solid-* "君の活躍によりアボガドロ軍は撤退した。"
                                          178 64  :color sdl:*white* :font *menu-font*)
      ; (sdl:draw-string-solid-* "THE ABOGADRO FORCES WITHDREW BY YOUR SUCCESS"
      ;                                      150 64  :color sdl:*white* :font *menu-font*)
      (sdl:draw-string-solid-* "C O N G R A T U L A T I O N S"
                                             208 96  :color sdl:*white* :font *menu-font*)
      (sdl:draw-string-solid-* "Y O U R S C O R E"
                                             224 160 :color sdl:*white* :font *menu-font*)
      (sdl:draw-string-solid-* (format nil "~5,'0d" (score score))
                                             380 160 :color sdl:*white* :font *menu-font*)
      (sdl:draw-string-solid-* "H I G H S C O R E"      
                                             224 192 :color sdl:*white* :font *menu-font*)
      (sdl:draw-string-solid-* (format nil "~d" (highscore score))        
                                             380 192 :color sdl:*white* :font *menu-font*)
      (sdl:update-display)))

    (when (eql *bossbgm-flag* nil)
      (setf *bossbgm-flag* t 
            *BGM* *endbgm*)             ; set ending BGM
      (sleep 2)                         ; 2 seconds waiting before play ending BGM 
      (Play-music-once *BGM*))          ; BGM play once   
    (when (or (z keystate)                       ; push z key   
	      (not (sdl-mixer:music-playing-p))) ; or BGM end
      (setf (title-loop stage) t                 ; GAME TITLE flag   ON
            (ending-loop stage) nil              ; GAME OVER flag    OFF
            (z keystate) nil)
      (Reset-variables stage enemy-manager balloon-manager score ship shot-manager)))

;; Step4 <Set Screen Mode>
;; -----------------------------------------------------------------------------------------------
(defun Set-screen-mode()
  (when (eql *switch* t) 
    (if (= *screen-mode* 0)                    ; fullscreen-mode on 
      (sdl:resize-window 640 480 :sw t :fullscreen t)
      (sdl:resize-window 640 480 :sw t))       
    (setf *switch* nil)))                      ; twice executing prevent

;; step5 <Score Panel>
;; -----------------------------------------------------------------------------------------------
(defgeneric Score-panel (score score-ship score-bomb))
(defmethod Score-panel (score score-ship score-bomb)
  "draw score and ships of rest"
  (sdl:draw-string-solid-* "SCORE:"       160 16 :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* "HIGH-SCORE:"  320 16 :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* (format nil "~5,'0d" (score score))     208 16 :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* (format nil     "~d" (highscore score)) 408 16 :color sdl:*white* :font *menu-font*)
  (dotimes (i (- (n-ship score) 1))
    (setf (x score-ship) (+ 160 (* 16 i)))
    (Draw score-ship))
  (dotimes (i (n-bomb score))
    (setf (x score-bomb) (+ 160 (* 16 i)))
    (Draw score-bomb)))

;; step6 <Move Shot>
;; -----------------------------------------------------------------------------------------------
(defgeneric Move-shot (shot-manager))
(defmethod Move-shot (shot-manager)
  "shot move"
  (dolist (shot (shot-list shot-manager))               
    (when (= (state shot) 1)                ; shot is alive
      (decf (y shot) (dy shot)))            ; shot 16 dot up 
    (when (< (y shot) -16)                  ; out of screen 
      (setf (state shot) 0))))              ; shot is dead

;; step6 <Set Shot> + Balloon Shot + shot
;; -----------------------------------------------------------------------------------------------
(defgeneric Set-shot (shot-manager ship keystate balloon-manager))
(defmethod Set-shot (shot-manager ship keystate balloon-manager)
  "set shot"
  (when (and (/= (state ship) 2)                          ; if ship is not explode
	     (eql (mod *scroll-cnt* 8) 0)                 ; 1 shot / 8 loop          
             (eql (z keystate) t)                         ; and set z key
             (eql *stage-end-flag* nil))                  ; not stage end
    (when (< (length (shot-list shot-manager)) (* 4 (+ (balloon-cnt balloon-manager) 1)))
      (Play-sample *shot*)
      (let ((shot (make-instance 'entity :id 3 :width 4 :height 16 :dx 0 :dy 16 :state 1)))
        (setf (y shot) (- (y ship) (height shot))         ; set shot position 
              (x shot) (+ (x ship) 14))
          (push shot (shot-list shot-manager)))
       (when (/= (balloon-cnt balloon-manager) 0)
          (dolist (balloon (balloon-list balloon-manager))
            (let ((shot (make-instance 'entity :id 3 :width 4 :height 16 :dx 0 :dy 16 :state 1)))          
              (setf (y shot) (- (y balloon) (height shot)); set balloon shot position
                    (x shot) (+ (x balloon) 14))	 
              (push shot (shot-list shot-manager))))))))

;; step6 <Remove Dead Shot>
;; -----------------------------------------------------------------------------------------------
(defgeneric Remove-dead-shot (shot-manager))
(defmethod Remove-dead-shot (shot-manager)
  "dead shot remove from list"
  (setf (shot-list shot-manager) 
	(delete-if #'(lambda (shot) (= (state shot) 0)) (shot-list shot-manager))))

;; step6 <Draw Shot>
;; -----------------------------------------------------------------------------------------------
(defgeneric Draw-shot (shot-manager))
(defmethod Draw-shot (shot-manager)
  (dolist (shot (shot-list shot-manager))
    (when (= (state shot) 1)
      (Draw shot))))                             ; draw shot

;; step7 <Generate Enemy> + Enemy Shot
;; -----------------------------------------------------------------------------------------------
(defgeneric Generate-enemy-item (map enemy-manager item-manager))
(defmethod Generate-enemy-item (map enemy-manager item-manager)
  (when (and (eql *enemy-generate-flag* t)
	     (= (mod *scroll-cnt* 64) 0)
             (<= (length (enemy-list enemy-manager)) 10)); max 10 enemy
     (dotimes (j 10)
       (when (/= (aref map *enemy-map-pointer* j) -1)
         (case (aref map *enemy-map-pointer* j)
                 ((7)                              ; when id is 7
                   (let ((enemy (make-instance 'foe      ; small class yellow enemy generate
                                 :id (aref map *enemy-map-pointer* j)
                                 :x (+ 160 (* j 32)) :y 0 :width 32 :height 32
                                 :life-cnt 3 :kind 1 :state 1
				 :beforetime 8 :pattern-number 0 :pattern-cnt 1 :repeat-flag 1)))
                   (push enemy (enemy-list enemy-manager))))
	         ((80)                              ; when id is 80
                   (let ((enemy (make-instance 'foe      ; small class purple enemy generate
                                 :id (aref map *enemy-map-pointer* j)
                                 :x (+ 160 (* j 32)) :y 0 :width 32 :height 32
                                 :life-cnt 3 :kind 1 :state 1
		                 :beforetime 8 :pattern-number 1 :pattern-cnt 1 :repeat-flag 1)))
                   (push enemy (enemy-list enemy-manager))))
		 ((50)                              ; when id is 50
                   (let ((enemy (make-instance 'foe      ; small class blue enemy generate
                                 :id (aref map *enemy-map-pointer* j)
                                 :x (+ 160 (* j 32)) :y 0 :width 32 :height 32
                                 :life-cnt 3 :kind 1 :state 1
		                 :beforetime 16 :pattern-number 2 :pattern-cnt 1 :repeat-flag 1)))
                   (push enemy (enemy-list enemy-manager))))		
                 ((76)                                ; when id is 76
                   (let ((enemy (make-instance 'foe      ; middle class gray enemy generate
                                 :id (aref map *enemy-map-pointer* j)
                                 :x (+ 160 (* j 32)) :y 0 :width 64 :height 64
                                 :life-cnt 20 :kind 2 :state 1
				 :beforetime 32 :pattern-number 3 :pattern-cnt 2 :repeat-flag 1)))
                   (push enemy (enemy-list enemy-manager))))
		 ((78)                                ; when id is 78
                   (let ((enemy (make-instance 'foe      ; middle class green enemy generate
                                 :id (aref map *enemy-map-pointer* j)
                                 :x (+ 160 (* j 32)) :y 0 :width 64 :height 64
                                 :life-cnt 20 :kind 2 :state 1
				 :beforetime 32 :pattern-number 5 :pattern-cnt 1 :repeat-flag 1)))
                   (push enemy (enemy-list enemy-manager))))
                 ((70)                             ; when id is 70
                   (let ((enemy (make-instance 'foe      ; large class boss1 enemy generate
                                  :id (aref map *enemy-map-pointer* j)
                                  :x (+ 160 (* j 32)) :y 0 :width 96 :height 96
                                  :life-cnt 300 :kind 3 :state 1
				  :beforetime 64 :pattern-number 6 :pattern-cnt 2 :repeat-flag 1)))
                   (push enemy (enemy-list enemy-manager))))
		 ((71)                             ; when id is 71
                   (let ((enemy (make-instance 'foe      ; large class boss2 enemy generate 
                                  :id (aref map *enemy-map-pointer* j)
                                  :x (+ 160 (* j 32)) :y 0 :width 96 :height 96
                                  :life-cnt 300 :kind 3 :state 1
				  :beforetime 64 :pattern-number 8 :pattern-cnt 3 :repeat-flag 1)))
                   (push enemy (enemy-list enemy-manager))))
		 ((72)                             ; when id is 72
                   (let ((enemy (make-instance 'foe      ; large class boss3 enemy generate
                                  :id (aref map *enemy-map-pointer* j)
                                  :x (+ 160 (* j 32)) :y 0 :width 96 :height 96
                                  :life-cnt 300 :kind 3 :state 1
				  :beforetime 64 :pattern-number 11 :pattern-cnt 3 :repeat-flag 1)))
                   (push enemy (enemy-list enemy-manager))))
		 ((17 18)                                ; when id is 17 or 18
                   (let ((item (make-instance 'foe       ; item generate
                                  :id (aref map *enemy-map-pointer* j)
                                  :x (+ 160 (* j 32)) :y 0 :width 32 :height 32 :dx 0 :dy 2 :kind 4 :state 1)))
                   (push item (item-list item-manager)))))))     ; store items into item-list
     (if (/= *enemy-map-pointer* 0)
       (decf *enemy-map-pointer*)                        ; attention ! *enemy-map-pointer* 0 keep!
       (setf *enemy-generate-flag* nil))))               ; *enemy-map-pointer* 64 -> 0 (end position)  

;; step7 <Judge Stage End>
;; -----------------------------------------------------------------------------------------------
(defgeneric Judge-stage-end (stage enemy-manager score shot-manager))
(defmethod Judge-stage-end (stage enemy-manager score  shot-manager)
  (when (and (eql *stage-end-flag* t)             ; large enemy dead
             (/= (n-ship score) 0))               ; and not n-ship 0
    (incf *interval*)
    (when (= *interval* 200)                      ; original 150     
      (setf *interval* 0
	    *stage-end-flag* nil
            *repeat* nil
            *map-pointer* 64 
            *enemy-map-pointer* 64                ; map and enemy-map set start position
            (shot-list shot-manager) nil
            (enemy-list enemy-manager) nil
	    (stage-flag stage) t)
      (when (= (stage-number stage) 3)
	(Stop-sound)                              ; BGM Stop!
	(setf (ending-loop stage) t)))))          ; GAME OVER flag  ON

;; step7 <Move Enemy>
;; -----------------------------------------------------------------------------------------------
(defgeneric Move-enemy (enemy-manager game-field))
(defmethod Move-enemy (enemy-manager game-field)
  (dolist (enemy (enemy-list enemy-manager))              
    (when (or (= (state enemy) 1)
              (= (state enemy) 2))
      (case (id enemy)
        ((7 8 9 14 15 16 80 81 82 83 84 85)       ; id 7 8 9(yellow enemy) or id 80 81 82(purple enemy)
          (let((row (mod (move-cnt enemy) 16)))   ; row from 0 to 15
            (case (id enemy)
              ((7 8 9 14 15 16)       ; id 7 8 9(yellow enemy)
               (setf (dx enemy) (aref *enemy-move-pattern1* row 0)
                     (dy enemy) (aref *enemy-move-pattern1* row 1)))
              ((80 81 82 83 84 85)    ; id 80 81 82(purple enemy)
               (setf (dx enemy) (aref *enemy-move-pattern3* row 0)
                     (dy enemy) (aref *enemy-move-pattern3* row 1))))))    
        ((50 52 51 53)                ; id 50 52(blue enemy)
          (let((row (mod (move-cnt enemy) 12)))   ; row from 0 to 11
            (setf (dx enemy) (aref *enemy-move-pattern2* row 0)
                  (dy enemy) (aref *enemy-move-pattern2* row 1)))) 
        ((76 78 77 79)                ; id 76 78(middle class gray and green enemy)                 
           (if (or (= (id enemy) 76)
                   (= (id enemy) 77))
             (setf (dx enemy) (first *enemy-move-pattern4*)
                   (dy enemy) (second *enemy-move-pattern4*))
             (setf (dx enemy) (first *enemy-move-pattern5*)
                   (dy enemy) (second *enemy-move-pattern5*))))
        ((70 71 73 74)                ; id 70 71(large class enemy)       
          (let((row (mod (move-cnt enemy) 32)))   ; row from 0 to  31
            (if (or (= (id enemy) 70)
                    (= (id enemy) 73))   
              (setf (dx enemy) (aref *enemy-move-pattern6* row 0)
                    (dy enemy) (aref *enemy-move-pattern6* row 1))
              (setf (dx enemy) (aref *enemy-move-pattern7* row 0)
                    (dy enemy) (aref *enemy-move-pattern7* row 1)))))
        ((72 75)                      ; id 72(large class enemy)
          (setf (dx enemy) (first *enemy-move-pattern8*)
                (dy enemy) (second *enemy-move-pattern8*))))
      (incf (x enemy) (dx enemy))
      (incf (y enemy) (dy enemy))
      (incf (move-cnt enemy) 1)
      (when (or (>= (y enemy) (height game-field))                           ; bottom of game field
                (>= (x enemy) (width game-field))                            ; right of game field   
                (<= (x enemy) (- (field-x game-field) (* 32 (kind enemy))))) ; left of game field
        (setf (state enemy) 0)))))     

;; step10 <Set Reset Id>
;; -----------------------------------------------------------------------------------------------
(defun Set-id (enemy)
  "Set enemy id"
  (case (id enemy)
    (7  (setf (id enemy) 14))
    (8  (setf (id enemy) 15))
    (9  (setf (id enemy) 16))
    (50 (setf (id enemy) 51))
    (52 (setf (id enemy) 53))
    (80 (setf (id enemy) 83))
    (81 (setf (id enemy) 84))
    (82 (setf (id enemy) 85))
    (76 (setf (id enemy) 77))
    (78 (setf (id enemy) 79))
    (70 (setf (id enemy) 73))
    (71 (setf (id enemy) 74))
    (72 (setf (id enemy) 75))))

(defun Reset-id (enemy)
  "Reset enemy id"
  (case (id enemy)
    (14 (setf (id enemy) 7))
    (15 (setf (id enemy) 8))
    (16 (setf (id enemy) 9))
    (51 (setf (id enemy) 50))
    (53 (setf (id enemy) 52))
    (83 (setf (id enemy) 80))
    (84 (setf (id enemy) 81))
    (85 (setf (id enemy) 82))
    (77 (setf (id enemy) 76))
    (79 (setf (id enemy) 78))
    (73 (setf (id enemy) 70))
    (74 (setf (id enemy) 71))
    (75 (setf (id enemy) 72))))

;; step7 <Change Id> + explode
;; -----------------------------------------------------------------------------------------------
(defun Change-id (enemy)
  (when (= (state enemy) 1)
    (Reset-id enemy) 
    (case (id enemy)
      ((7 8 9)
         (case (mod (floor (move-cnt enemy) 16) 4)            ; enemy id change
            (0 (setf (id enemy) 7))                           ; change pattern --> 0~~0, 1~~1, 2~~2 , 3~~3
            (1 (setf (id enemy) 8))                           ;                    id7 , id8 , id9  , id8
            (2 (setf (id enemy) 9))
            (3 (setf (id enemy) 8))))
      ((50 52)
         (case (mod (floor (move-cnt enemy) 16) 2)            ; enemy id change
           (0 (setf (id enemy) 50))                           ; change pattern --> 0~~0, 1~~1
           (1 (setf (id enemy) 52))))                         ;                    id50 , id52
      ((80 81 82)
         (case (mod (floor (move-cnt enemy) 16) 4)            ; enemy id change
           (0 (setf (id enemy) 80))                           ; change pattern --> 0~~0, 1~~1 , 2~~2 , 3~~3
           (1 (setf (id enemy) 81))                           ;                    id80 ,id81 , id82 , id81
           (2 (setf (id enemy) 82))
           (3 (setf (id enemy) 81)))))))

;; step10 <Change Damaged Id>
;; -----------------------------------------------------------------------------------------------
(defun Change-damaged-id (enemy)
  (when (= (state enemy) 2)
    (Set-id enemy)
    (case (id enemy)
      ((14 15 16)
         (case (mod (floor (move-cnt enemy) 16) 4)           ; enemy id change
            (0 (setf (id enemy) 14))                         ; change pattern --> 0~~0, 1~~1 , 2~~2 , 3~~3
            (1 (setf (id enemy) 15))                         ;                    id14 ,id15 , id16  ,id15
            (2 (setf (id enemy) 16))
            (3 (setf (id enemy) 15))))
      ((51 53)
         (case (mod (floor (move-cnt enemy) 16) 2)           ; enemy id change
           (0 (setf (id enemy) 51))                          ; change pattern --> 0~~0, 1~~1
           (1 (setf (id enemy) 53))))                        ;                    id51 , id53
      ((83 84 85)
         (case (mod (floor (move-cnt enemy) 16) 4)           ; enemy id change
           (0 (setf (id enemy) 83))                          ; change pattern --> 0~~0, 1~~1 , 2~~2 , 3~~3
           (1 (setf (id enemy) 84))                          ;                    id83 ,id84 , id85 , id84
           (2 (setf (id enemy) 85))
           (3 (setf (id enemy) 84)))))))

;; step7 <Remove Enemy>
;; -----------------------------------------------------------------------------------------------
(defgeneric Remove-enemy (enemy-manager))
(defmethod Remove-enemy (enemy-manager)
  (setf (enemy-list enemy-manager) 
	(delete-if #'(lambda (enemy) (= (state enemy) 0)) (enemy-list enemy-manager))))

;; step7 <Draw Enemy> + damage
;; -----------------------------------------------------------------------------------------------
(defgeneric Draw-enemy (enemy-manager))
(defmethod Draw-enemy (enemy-manager)
  (dolist (enemy (enemy-list enemy-manager))
    (Change-id enemy)
    (Change-damaged-id enemy)
    (when (or (= (state enemy) 1)
              (= (state enemy) 2))
      (Draw enemy))))

;; step8 <Move Balloon> + radian-degree
;; -----------------------------------------------------------------------------------------------
(declaim (inline degree-radian))
(defun degree-radian (degree)                       ; convert from radian to degree
  (/ (* degree pi) 180))                            ; degree -> radian

(declaim (inline radian-degree))
(defun radian-degree (radian)                       ; convert from radian to degree
  (/ (* radian 180) pi))                            ; radian -> degree

(defvar *angle* 0)

(defgeneric Move-balloon (balloon-manager ship))
(defmethod Move-balloon (balloon-manager ship)
  (when (or (= (state ship) 1)
            (= (state ship) 3))
    (when (> *angle* 360)
      (setf *angle* 0))
    (incf *angle* 4)
    (when (<= *angle* 360)
      (let ((i (balloon-cnt balloon-manager))) 
        (dolist (balloon (balloon-list balloon-manager))                   
          (if (= i 2)            ; 48 is distance from left balloon to right balloon
            (setf (x balloon) (+ (x ship) (* (cos (degree-radian *angle*)) 48))
                  (y balloon) (+ (y ship) (* (sin (degree-radian *angle*)) 48)))
            (setf (x balloon) (- (x ship) (* (cos (degree-radian *angle*)) 48))
                  (y balloon) (- (y ship) (* (sin (degree-radian *angle*)) 48))))
          (decf i 1))))))

;; Step8 <Generate Balloon>
;; -----------------------------------------------------------------------------------------------  
(defgeneric Generate-balloon (balloon-manager ship))
(defmethod Generate-balloon (balloon-manager ship)
  "balloon appear position set"
  (when (and (/= (balloon-cnt balloon-manager) 0)
             (< (length (balloon-list balloon-manager)) 2)) ; max 2 balloon   
    (dotimes (i (balloon-cnt balloon-manager))
      (let ((balloon (make-instance 'entity :id 10 :state 1)))
           (if (= i 0)
             (setf (x balloon) (- (x ship) 48)) 
           (setf (x balloon) (+ (x ship) 48)))          
           (setf (y balloon) (y ship))
           (push balloon (balloon-list balloon-manager))))))

;; step8 <Draw Balloon>
;; -----------------------------------------------------------------------------------------------
(defgeneric Draw-balloon (balloon-manager ship))
(defmethod Draw-balloon (balloon-manager ship)
  (when (and (or (= (state ship) 1)
                 (= (state ship) 3))
             (/= (balloon-cnt balloon-manager) 0))
    (dolist (balloon (balloon-list balloon-manager))
      (Draw balloon))))

;; Step8 <Move Item>
;; ----------------------------------------------------------------------------------------------- 
(defgeneric Move-item (item-manager game-field))
(defmethod Move-item (item-manager game-field)
  "item move"
  (dolist (item (item-list item-manager))               
    (when (= (state item) 1)                       ; option-item is alive
      (incf (y item) (dy item)))                   ; option-item 16 dot down 
    (when (> (y item) (height game-field))         ; out of screen 
      (setf (state item) 0))))

;; Step8 <Hit Item>
;; ----------------------------------------------------------------------------------------------- 
(defvar *itemget-flag* nil)

(defgeneric Hit-item-p (item-manager balloon-manager score ship))
(defmethod Hit-item-p (item-manager balloon-manager score ship)
  (when (/= (state ship) 2)                                                            
    (dolist (item (item-list item-manager))
      (when(and (> (+ (x ship) 32) (x item))
                (< (x ship) (+ (x item) 32))
                (> (+ (y ship) 32) (y item))
                (< (y ship) (+ (y item) 32)))
          (setf (state item) 0)                    ; item is disappered
          (when (eql *itemget-flag* nil)
            (setf *itemget-flag* t)
            (Play-sample *itemget*))  
          (when (= (id item) 17)
            (if (< (balloon-cnt balloon-manager) 2)
              (incf (balloon-cnt balloon-manager) 1)
              (incf (score score) 500)))
          (when (= (id item) 18)
            (incf (n-bomb score) 1))
          (setf *itemget-flag* nil)))))

;; Step8 <Remove Item>
;; -----------------------------------------------------------------------------------------------      
(defgeneric Remove-item (item-manager))
(defmethod Remove-item (item-manager)
  "item remove from list"
  (setf (item-list item-manager) 
	(delete-if #'(lambda (item) (= (state item) 0)) (item-list item-manager))))

;; step8 <Draw Item>
;; -----------------------------------------------------------------------------------------------
(defgeneric Draw-item (item-manager))
(defmethod Draw-item (item-manager)
  (dolist (item (item-list item-manager))
    (Draw item)))

;; Step9 <Move Enemy Shot>
;; -----------------------------------------------------------------------------------------------
(defgeneric Move-enemy-shot (enemy-manager game-field))
(defmethod Move-enemy-shot (enemy-manager game-field)
  "enemy shot move"
  (dolist (enemy-shot (enemy-shot-list enemy-manager))               
    (when (= (state enemy-shot) 1)           ; set enemy-shot state alive
        (incf (x enemy-shot) (dx enemy-shot))
        (incf (y enemy-shot) (dy enemy-shot))
      (when (or (< (x enemy-shot) (- (field-x game-field) 8))
                (> (x enemy-shot) (width game-field))
                (< (y enemy-shot) (- (field-y game-field) 8))
                (> (y enemy-shot) (height game-field)))
        (setf (state enemy-shot) 0)))))

;; Step13 <Charge Enemy Shot>
;; ---------------------------------------------------------------------------------------------
(defun Charge-enemy-shot (enemy enemy-manager)
  (dotimes (i (shotdata-number-battery (aref *shot-pattern-data* (pattern-number enemy))))
    (let ((enemy-shot (make-instance 'entity :id 4 :width 8 :height 8 :dx 0 :dy 0 :state 0)))
      (push enemy-shot (enemy-shot-list enemy-manager)))))

;; Step13 <Set Enemy center>
;; ---------------------------------------------------------------------------------------------
(defun Set-enemy-center (enemy enemy-shot)
  (case (kind enemy)
    ((1) ; small class enemy
      (setf (y enemy-shot) (+ (y enemy) 12))    ; center of small class enemy x
      (setf (x enemy-shot) (+ (x enemy) 12)))   ; center of small class enemy y 
    ((2) ; middle class enemy
      (setf (y enemy-shot) (+ (y enemy) 28))    ; center of middle class enemy x
      (setf (x enemy-shot) (+ (x enemy) 28)))   ; center of middle class enemy y 
    ((3) ; large class enemy
      (setf (y enemy-shot) (+ (y enemy) 44))    ; center of large class enemy x
      (setf (x enemy-shot) (+ (x enemy) 44))))) ; center of large class enemy y

;; Step13 <Set Repeat Flag OFF>
;; ---------------------------------------------------------------------------------------------
(defun Set-repeat-flag-OFF (enemy)
  (case (id enemy)  ; if enemy is blue or purple, repeat-flag OFF    
    ((80 81 82 83 84 85)                
      (setf (repeat-flag enemy) 0))       
    ((50 51 52 53)
      (setf (repeat-flag enemy) 0))))

;; Step13 <Set Enemy Variables>
;; ---------------------------------------------------------------------------------------------
(defun Set-enemy-variables (enemy)
  (when (= (count-flag enemy) 0)   ; --- if flag ON, set variables
    (setf (shot-counter enemy)                                  ; set shotcounter    (number of shot per 1 pattern)
	  (shotdata-number-battery-shot (aref *shot-pattern-data* (pattern-number enemy))))
    (setf (timing enemy) (beforetime enemy))                    ; set shot beforetime 
    (setf (pattern-cnt-store enemy) (pattern-cnt enemy))        ; each enemy has 0-2 patterns
    (setf (pattern-number-store enemy) (pattern-number enemy))  ; store patternnumber
    (setf (count-flag enemy) 1)))   ; --- flag OFF, no more set
  
;; Step13 <Set Enemy Shot angle>
;; ---------------------------------------------------------------------------------------------
(defvar *range-x*)
(defvar *range-y*)
(defvar *distance*)

(defun Set-enemy-shot-angle (shotangle ship enemy enemy-shot)
  (let ((angle (nth shotangle (shotdata-battery-angle (aref *shot-pattern-data* (pattern-number enemy)))))
	(rotation-angle (shotdata-direction-battery-angle (aref *shot-pattern-data* (pattern-number enemy))))
   	(speed (shotdata-shotspeed (aref *shot-pattern-data* (pattern-number enemy))))
        (ship-x  (+ (x ship) (/ (width ship) 2)))                  ; ship x position
   	(ship-y  (+ (y ship) (/ (height ship) 2)))                 ; ship y position
   	(ene-shot-x (+ (x enemy-shot) (/ (width enemy-shot) 2)))   ; enemy-shot x position
   	(ene-shot-y (+ (y enemy-shot) (/ (height enemy-shot) 2)))) ; enemy-shot y position
    (case (shotdata-battery-direction (aref *shot-pattern-data* (pattern-number enemy)))
      ((0)  ; beneath
        (when (= rotation-angle 0)  ; not rotation
	     (setf (dx enemy-shot) (* (cos (degree-radian angle)) speed)) ; dx from angle list
	     (setf (dy enemy-shot) (* (sin (degree-radian angle)) speed))); dy from angle list
	(when (/= rotation-angle 0) ; rotation
	     (if (= shotangle 0)
	       (setf (angle-store enemy) (+ (angle-store enemy) rotation-angle)))	
	     (setf (dx enemy-shot) (* (cos (degree-radian (+ angle (angle-store enemy)))) speed)) ; dx from angle list
	     (setf (dy enemy-shot) (* (sin (degree-radian (+ angle (angle-store enemy)))) speed)))); dy from angle list
      ((1)  ; direction of ship
        (when (/= ship-x ene-shot-x)                                ;<------------------ Bug Fix
	  (setf *range-x* (- ship-x ene-shot-x))                    ; 
          (setf *range-y* (- ship-y ene-shot-y)))                   ;  
        (setf *distance* (sqrt (+ (* *range-x* *range-x*) (* *range-y* *range-y*))))
        (setf (first-x enemy) (* (/ *range-x* *distance*) speed))    ; x distance from enemy to ship
        (setf (first-y enemy) (* (/ *range-y* *distance*) speed))    ; y distance form enemy to ship  
        (if (< (atan (/ (first-y enemy) (first-x enemy))) 0)         ; find angle in Arc tangent
          (setf (first-angle enemy) (radian-degree (+ (atan (/ (first-y enemy) (first-x enemy))) (/ pi 2))))
          (setf (first-angle enemy) (radian-degree (- (atan (/ (first-y enemy) (first-x enemy))) (/ pi 2)))))
        (when (= rotation-angle 0)  ; not rotation	  
	   (setf (dx enemy-shot) (* (cos (degree-radian (+ angle (first-angle enemy)))) speed))
	   (setf (dy enemy-shot) (* (sin (degree-radian (+ angle (first-angle enemy)))) speed)))
        (when (/= rotation-angle 0) ; rotation 
	     (if (= shotangle 0)
	       (setf (angle-store enemy) (+ (angle-store enemy) rotation-angle)))	   
	   (setf (dx enemy-shot) 
		 (* (cos (degree-radian (+ angle (first-angle enemy) (angle-store enemy)))) speed)) ; dx from angle list
	   (setf (dy enemy-shot)
		 (* (sin (degree-radian (+ angle (first-angle enemy) (angle-store enemy)))) speed))))))); dy from angle list

;; Step13 <Set Enemy Shot Timing>
;; ---------------------------------------------------------------------------------------------
(defun Set-enemy-shot-timing (ship enemy enemy-manager) 
  (when(and (= (move-cnt enemy) (timing enemy))            ; equal move-cnt and timing 
	    (= (repeat-flag enemy) 1))                     ; and shot repeat
    (let ((shotangle 0))                                   ; set enemy shot direction (start <- 0)
      (dolist (enemy-shot (enemy-shot-list enemy-manager))
        (when (= (state enemy-shot) 0)      
	  (Set-enemy-center enemy enemy-shot)                      ; set enemy center position
	  (Set-enemy-shot-angle shotangle ship enemy enemy-shot)   ; set enemy shot angle
	  (incf shotangle)                                         ; judge length of battery-angle-list
	  (if (= shotangle (length (shotdata-battery-angle (aref *shot-pattern-data* (pattern-number enemy)))))
	    (setf shotangle 0))                                    ; reset shotangle
	  (setf (state enemy-shot) 1))))                           ; shot state ON into battery-angle list
    (setf (shot-counter enemy) (decf (shot-counter enemy))) ; go to next shot timing
    (cond ((/= (shot-counter enemy) 0)            ; when shot counter not 0
	    (setf (timing enemy)                 ; set betweentime
	          (+ (timing enemy) (shotdata-betweentime (aref *shot-pattern-data* (pattern-number enemy))))))
          ((= (shot-counter enemy) 0)             ; when  shot counter 0
	    (setf (timing enemy)                 ; set aftertime
	          (+ (timing enemy) (shotdata-aftertime (aref *shot-pattern-data* (pattern-number enemy)))))
	    (setf (angle-store enemy) 0)    ; angle-store reset 0
	    (setf (pattern-cnt enemy) (decf (pattern-cnt enemy)))       ; go to next pattern  (ex: 1 -> 0)
	    (Set-repeat-flag-OFF enemy))))) ; blue and purple enemy repeat OFF

;; Step13 <Set Enemy Shot Pattern>
;; ---------------------------------------------------------------------------------------------
(defun Set-enemy-shot-pattern (enemy)
  (when (= (shot-counter enemy) 0)
    (if (= (pattern-cnt enemy) 0)	      ; reset shot pattern 
        (progn	  
          (setf (pattern-cnt enemy) (pattern-cnt-store enemy))        ; back to first pattern count
          (setf (pattern-number enemy) (pattern-number-store enemy))  ; back to first pattern number   
          (setf (shot-counter enemy)                ; back to first shotcounter
   	        (shotdata-number-battery-shot (aref *shot-pattern-data* (pattern-number enemy)))))
	(progn	    
          (setf (pattern-number enemy) (incf (pattern-number enemy))) ; pattern number + 1 (ex: 3 -> 4 etc)   
          (setf (shot-counter enemy)               ; set shotcounter (number of shot per 1 pattern)
	        (shotdata-number-battery-shot (aref *shot-pattern-data* (pattern-number enemy))))))))
    
;; Step9 <Set Enemy Shot>
;; ---------------------------------------------------------------------------------------------
(defgeneric Set-enemy-shot (enemy-manager ship game-field))
(defmethod Set-enemy-shot (enemy-manager ship game-field)
  "enemy shot appear position set and move" 
  (dolist (enemy (enemy-list enemy-manager))
    (when (and (or (= (state enemy) 1)                           ; enemy is alive or damaged
      	           (= (state enemy) 2))		   
	       (>= (x enemy) 0)                                  ; and into the game field
    	       (< (x enemy) (- (width game-field) (width enemy)))
	       (>= (y enemy) 0)
	       (< (y enemy) (- (height game-field) (width enemy))))
      (unless (and (> (+ (x ship) 16) (x enemy))                 ; if ship and enemy are not collision
	      	   (< (+ (x ship) 16) (+ (x enemy) (width enemy)))
		   (> (+ (y ship) 16) (y enemy))
		   (< (+ (y ship) 16) (+ (y enemy) (height enemy))))        
        (Charge-enemy-shot enemy enemy-manager)                  ; charge enemy shot
	(Set-enemy-variables enemy)  
        (Set-enemy-shot-timing ship enemy enemy-manager)
	(Set-enemy-shot-pattern enemy)))))                       ; set enemy shot timing
	     
;; Step9 <Remove Dead Enemy Shot>
;; -----------------------------------------------------------------------------------------------
(defgeneric Remove-dead-enemy-shot (enemy-manager))
(defmethod Remove-dead-enemy-shot (enemy-manager)
  "dead enemy shot remove from list"
  (setf (enemy-shot-list enemy-manager) 
	(delete-if #'(lambda (enemy-shot) (= (state enemy-shot) 0)) (enemy-shot-list enemy-manager))))

;; Step9 <DrawEnemy Shot>
;; -----------------------------------------------------------------------------------------------
(defgeneric Draw-enemy-shot (enemy-manager))
(defmethod Draw-enemy-shot (enemy-manager)
  (dolist (enemy-shot (enemy-shot-list enemy-manager))
    (when (= (state enemy-shot) 1)
      (Draw enemy-shot))))

;; Step10 <Enemy Hit P>
;; -----------------------------------------------------------------------------------------------
(defvar *oneup-flag* nil)

(defgeneric Enemy-hit-p (shot-manager enemy-manager))
(defmethod Enemy-hit-p (shot-manager enemy-manager)
  (dolist (shot (shot-list shot-manager))
    (when (= (state shot) 1)                       ; if shot is on                                  
      (dolist (enemy (enemy-list enemy-manager))                
        (when (and (or (= (state enemy) 1)         ; if enemy is alive
                       (= (state enemy) 2))        ; or enemy is damaged 
                   (> (+ (x shot) 4) (x enemy))
                   (< (x shot) (+ (x enemy) (width enemy)))
                   (> (+ (y shot) 16) (y enemy))
                   (< (y shot) (+ (y enemy) (height enemy))))
          (setf (state enemy) 2                    ; enemy is damaged
                (damage-cnt enemy) 0               ; damage counter on
                (state shot) 0)                    ; shot is off
          (when (> (life-cnt enemy) 0)
            (decf (life-cnt enemy) 1)))))))

;; Step10 <Score Up>
;; -----------------------------------------------------------------------------------------------
(defgeneric Score-up (score))
(defmethod Score-up (score)                          
  (when (> (score score) (highscore score))
    (setf (highscore score) (score score)))
  (when (>= (score score) (oneup score))
    (when (eql *oneup-flag* nil)
        (setf *oneup-flag* t)                      ; oneup sound on
        (Play-sample *oneup*))                     ; oneup sound    
    (incf (n-ship score) 1)                        ; plus one ship
    (setf (oneup score) (+ (oneup score) 100000)
          *oneup-flag* nil)))                      ; oneup sound off    

;; Step10 <Damage Counter>
;; -----------------------------------------------------------------------------------------------
(defvar *damage-flag* nil)

(defgeneric Damage-counter (enemy-manager score))
(defmethod Damage-counter (enemy-manager score)
  (dolist (enemy (enemy-list enemy-manager))
    (when (= (state enemy) 2)                     ; if enemy is damaged
      (incf (damage-cnt enemy) 1)                 ; damage-cnt is 3 times loop (from 0 to 3)
      (when (eql *damage-flag* nil)
        (setf *damage-flag* t)                    ; damage sound on
        (Play-sample *damage*))                   ; damage sound         
      (when (= (damage-cnt enemy) 3)        
        (decf (life-cnt enemy) 1)                 ; life-cnt is 3 times loop (form 3 to 0)
        (setf (damage-cnt enemy) 0                ; enemy's damage-cnt reset 0
              (state enemy) 1
              *damage-flag* nil))                 ; damage sound off
      (when (= (life-cnt enemy) 0)
        (case (kind enemy)
          ((1)    
            (incf (score score) 200))             ; small class enemy    200 point 
          ((2)
            (incf (score score) 10000))           ; middle class enemy 10000 point
          ((3)
            (incf (score score) 30000)))          ; large class enemy  30000 point
        (setf (state enemy) 3)))))                ; enemy explode

;; Step10 <Explode Enemy>
;; -----------------------------------------------------------------------------------------------
(defvar *explodes-flag* nil)
(defvar *large-enemy-explosion-flag* nil)

(defgeneric Explode-enemy (enemy-manager))
(defmethod Explode-enemy (enemy-manager)
  "enemy explosion while 16 times loop"
  (dolist (enemy (enemy-list enemy-manager))
    (when (and (= (state enemy) 3)
               (or (= (kind enemy) 1)
                   (= (kind enemy) 2)))    
            (incf (explode-cnt enemy) 1)
            (when (eql *explodes-flag* nil)
              (setf *explodes-flag* t)                  ; explode sound on
              (Play-sample *explodes*))                 ; explode sound              
            (when (= (explode-cnt enemy) 15)            ; enemy explode count 15	      
              (setf (state enemy) 0 
                    *explodes-flag* nil)))))

;; Step10 <Draw Enemy Explosion>
;; -----------------------------------------------------------------------------------------------
(defgeneric Draw-enemy-explosion (enemy-explosion enemy-manager))
(defmethod Draw-enemy-explosion (enemy-explosion enemy-manager)
  (dolist (enemy (enemy-list enemy-manager))
    (when (and (= (state enemy) 3)
               (or (= (kind enemy) 1)
                   (= (kind enemy) 2)))    
      (setf (x enemy-explosion) (x enemy)
            (y enemy-explosion) (y enemy))
        (cond ((and (>= (explode-cnt enemy) 0)
                    (<  (explode-cnt enemy) 5))
                (case (kind enemy)
                  ((1)      
                    (setf (id enemy-explosion) 11))    ; bomb id 11
                  ((2)
                    (setf (id enemy-explosion) 64))))  ; bomb id 64     
              ((and (>= (explode-cnt enemy) 5) 
                    (<  (explode-cnt enemy) 10))
	        (case (kind enemy)
		  ((1)
                    (setf (id enemy-explosion) 12))    ; bomb id 12 
		  ((2)
	            (setf (id enemy-explosion) 65))))  ; bomb id 65 
              ((and (>= (explode-cnt enemy) 10)
                    (<  (explode-cnt enemy) 15))
	        (case (kind enemy)
	          ((1)
                    (setf (id enemy-explosion) 13))    ; bomb id 13
		  ((2)
		    (setf (id enemy-explosion) 66))))) ; bomb id 66
      (Draw enemy-explosion))))   

;; step12 <Set Explosion>
;; -----------------------------------------------------------------------------------------------
(defvar *rnd1*)
(defvar *rnd2*)

(defgeneric Set-explosion (explosion-manager enemy-manager))
(defmethod Set-explosion (explosion-manager enemy-manager)
  "set explosion"
  (dolist (enemy (enemy-list enemy-manager))
    (when (and (= (state enemy) 3)
               (= (kind enemy) 3))
      (when (and (= (mod (bomb-cnt explosion-manager) 15) 0)  ; bomb-cnt is 150 -> 10 times
                 (/= (bomb-cnt explosion-manager) 0))     
        (dotimes (i (bomb-number explosion-manager))          ; 5 bomb
          (let ((bomb (make-instance 'entity :id 64 :width 64 :height 64 :state 1)))
            (setf *rnd1* (random 96)                          ;  x: from -32 to 96
                  *rnd2* (random 96))                         ;  y: from -32 to 96 
            (setf (x bomb) (- (+ (x enemy) *rnd1*) 32)
                  (y bomb) (- (+ (y enemy) *rnd2*) 32)) 
            (push bomb (bomb-list explosion-manager)))))                 
            (decf (bomb-cnt explosion-manager))                     ; decrement bomb-cnt 150 -> 0        
            (when (= (bomb-cnt explosion-manager) 0)                ; if bomb counter is 0 , reset 150
              (setf (bomb-cnt explosion-manager) 150
		    (state enemy) 0
                    *stage-end-flag* t)                             ; stage end!
              (Stop-sound)))))                                      ; BGM stop!

;; step12 <Explode Large Enemy>
;; -----------------------------------------------------------------------------------------------
(defvar *large-enemy-explodes-flag* nil)

(defgeneric Explode-large-enemy (explosion-manager))
(defmethod Explode-large-enemy (explosion-manager)
  "large enemy explosion while 15 times loop"
    (dolist (bomb (bomb-list explosion-manager))   
      (incf (explode-cnt bomb) 1)      
      (when (eql *large-enemy-explodes-flag* nil)
        (setf *large-enemy-explodes-flag* t)          ; bomb explode sound on
        (Play-sample *bomb*))                         ; bomb explode sound                           
      (when (= (explode-cnt bomb) 15)                 ; when bomb explode count is 15
        (setf (state bomb) 0                          ; set state of bomb  0
              *large-enemy-explodes-flag* nil)))) 

;; step12 <Remove Explode large enemy>
;; -----------------------------------------------------------------------------------------------
(defgeneric Remove-explode-large-enemy (explosion-manager))
(defmethod Remove-explode-large-enemy (explosion-manager)
  "explode bomb remove from list"
  (setf (bomb-list explosion-manager) 
	(delete-if #'(lambda (bomb) (= (state bomb) 0)) (bomb-list explosion-manager))))

;; step12 <Draw Explosion Large Enemy>
;; -----------------------------------------------------------------------------------------------
(defgeneric Draw-explosion-large-enemy (explosion-manager))
(defmethod Draw-explosion-large-enemy (explosion-manager)
  (dolist (bomb (bomb-list explosion-manager))
    (when (= (state bomb) 1)
      (cond ((and (>= (explode-cnt bomb) 0)
                  (<  (explode-cnt bomb) 5))
              (setf (id bomb) 64))
            ((and (>= (explode-cnt bomb) 5) 
                  (<  (explode-cnt bomb) 10))
              (setf (id bomb) 65))
            ((and (>= (explode-cnt bomb) 10)
                  (<  (explode-cnt bomb) 15))          
              (setf (id bomb) 66)))
      (Draw bomb))))                                     ; draw  bomb

;; step11 <Set Bomb Key>
;; -----------------------------------------------------------------------------------------------
(define-class bomb-manager ()
  (bomb-list bomb-cnt bomb-flag bomb-number) nil)
; bomb-list    list of bomb       
; bomb-cnt     60 times waiting   
; bomb-flag    on (exploding) / off(not exploding)
; bomb-number  max 10

(defgeneric Set-bomb-key (ship bomb-manager keystate score))
(defmethod Set-bomb-key (ship bomb-manager keystate score)
  "bomb key push"
  (when (and (/= (state ship) 2)
             (/= (n-bomb score) 0)
             (eql (bomb-flag bomb-manager) nil)
             (eql (lshift keystate) t)
	     (eql *stage-end-flag* nil))        ; not stage end
    (decf (n-bomb score) 1)                     ; decrement n-bomb    3 -> 0     
    (setf (bomb-flag bomb-manager) t            ; bomb-flag ON
          (lshift keystate) nil)))              ; reset lshift key
        
;; step11 <Set Bomb>
;; -----------------------------------------------------------------------------------------------
(defgeneric Set-bomb (bomb-manager enemy-manager))
(defmethod Set-bomb (bomb-manager enemy-manager)
  "set bomb"
  (when (eql (bomb-flag bomb-manager) t)          ; when bomb-flag is ON
    (when (and (= (mod (bomb-cnt bomb-manager) 15) 0)
               (> (bomb-cnt bomb-manager) 0))      
      (dotimes (i (bomb-number bomb-manager))
        (let ((bomb (make-instance 'entity :id 67 :width 64 :height 64 :state 1)))
	  (setf *rnd1* (random 256)
                *rnd2* (random 384))
          (setf (x bomb) (+ 160 *rnd1*)
                (y bomb) (+ 16  *rnd2*)) 
          (push bomb (bomb-list bomb-manager))))      
      (dolist (enemy (enemy-list enemy-manager))
        (when (or (= (state enemy) 1)               ; if enemy is alive
                  (= (state enemy) 2))              ; or enemy is damaged
          (setf (state enemy) 2
                (damage-cnt enemy) 0)
          (decf (life-cnt enemy) 1))))
    (decf (bomb-cnt bomb-manager) 1)              ; decrement bomb-cnt 60 -> 0
    (when (= (bomb-cnt bomb-manager) 0)           ; if bomb counter is 0 , reset 60
      (setf (bomb-cnt bomb-manager) 60            ; and set bomb flag nil 
            (bomb-flag bomb-manager) nil))))    
                                                      
;; step11 <Explode Bomb>
;; -----------------------------------------------------------------------------------------------
(defvar *bomb-explodes-flag* nil)

(defgeneric Explode-bomb (bomb-manager enemy-manager))
(defmethod Explode-bomb (bomb-manager enemy-manager)
  "bomb explosion while 15 times loop"
  (dolist (bomb (bomb-list bomb-manager))
     (incf (explode-cnt bomb) 1)      
      (when (eql *bomb-explodes-flag* nil)
        (setf *bomb-explodes-flag* t)             ; bomb explode sound on
        (Play-sample *bomb*))                     ; bomb explode sound                           
      (when (= (explode-cnt bomb) 15)             ; when bomb explode count is 15
        (setf (state bomb) 0                      ; set state of bomb  0
              *bomb-explodes-flag* nil)
        (dolist (enemy-shot (enemy-shot-list enemy-manager))
          (setf (state enemy-shot) 0)))))         ; set state of enemy-shot 0

;; step11 <Remove Explode Bomb>
;; -----------------------------------------------------------------------------------------------
(defgeneric Remove-explode-bomb (bomb-manager))
(defmethod Remove-explode-bomb (bomb-manager)
  "explode bomb remove from list"
  (setf (bomb-list bomb-manager) 
	(delete-if #'(lambda (bomb) (= (state bomb) 0)) (bomb-list bomb-manager))))

;; step11 <Draw Bomb>
;; -----------------------------------------------------------------------------------------------
(defgeneric Draw-bomb (bomb-manager))
(defmethod Draw-bomb (bomb-manager)
  (dolist (bomb (bomb-list bomb-manager))
    (when (= (state bomb) 1)
      (cond ((and (>= (explode-cnt bomb) 0)
                  (<  (explode-cnt bomb) 5))
              (setf (id bomb) 67))
            ((and (>= (explode-cnt bomb) 5) 
                  (<  (explode-cnt bomb) 10))
              (setf (id bomb) 68))
            ((and (>= (explode-cnt bomb) 10)
                  (<  (explode-cnt bomb) 15))          
              (setf (id bomb) 69)))
      (Draw bomb))))                                     ; draw  bomb

;; Step12 <Ship Hit P>
;; -----------------------------------------------------------------------------------------------
(defgeneric Ship-hit-p (ship enemy-manager balloon-manager score shot-manager))
(defmethod Ship-hit-p (ship enemy-manager balloon-manager score shot-manager)
  (when (= (state ship) 1)
    (let ((hit 0))
      (dolist (enemy (enemy-list enemy-manager))
        (when (and (or (= (state enemy) 1)              ; if enemy is alive
                       (= (state enemy) 2))             ; or enemy is damaged
                 (> (+ (x ship) 16) (x enemy))          ; 16 is center of ship
                 (< (+ (x ship) 16) (+ (x enemy) (width enemy)))    ; width enemy
                 (> (+ (y ship) 16) (y enemy))
                 (< (+ (y ship) 16) (+ (y enemy) (height enemy))))  ; height enemy
            (if (/= (kind enemy) 3)      ; when not large enemy
              (setf (state enemy) 0      ; small or middle enemy explosion
                     hit 1)
              (setf (state enemy) 3      ; large enemy is dameged
	             hit 1))))
      (dolist (enemy-shot (enemy-shot-list enemy-manager))
        (when (and (= (state enemy-shot) 1)            ; enemies shot and ship hit
                  (> (+ (x ship) 16) (x enemy-shot))
                  (< (+ (x ship) 16) (+ (x enemy-shot) (width enemy-shot)))
                  (> (+ (y ship) 16) (y enemy-shot))
                  (< (+ (y ship) 16) (+ (y enemy-shot) (height enemy-shot))))
          (setf (state enemy-shot) 0
                 hit 1)))
      (when (= hit 1)                                  ; hit on ship  
        (Play-sample *crushed*)                        ; ship crushed sound
        (setf (state ship) 2                           ; ship is explode
	      (shot-list shot-manager) nil
              (balloon-cnt balloon-manager) 0
	      (balloon-list balloon-manager) nil
              (n-bomb score) 3
              (explode-cnt ship) 0)))))                ; ship explode count 0

;; Step12 <Explode Counter>
;; ----------------------------------------------------------------------------------------------- 
(defgeneric Explode-counter (ship score game-field stage))
(defmethod Explode-counter (ship score game-field stage)     
  (when (= (state ship) 2)                    ; ship is explode
    (incf (explode-cnt ship) 1)      
    (when (= (explode-cnt ship) 100)
      (decf (n-ship score) 1)
      (when (> (n-ship score) 0)
        (setf (state ship) 3                  ; set ship revival
	      (revival-cnt ship) 0
	      (x ship) (- (* (field-x game-field) 2) (/ (width ship) 2))
	      (y ship) (- (height game-field) (* (height ship) 2)))))))    

;; Step12 <Revive Counter>
;; ----------------------------------------------------------------------------------------------- 
(defgeneric Revive-counter (ship enemy-manager))
(defmethod Revive-counter (ship enemy-manager)
  (when (= (state ship) 3)                            ; ship is revival
    (incf (revival-cnt ship) 1)
    (when (>= (revival-cnt ship) 200)                 ; revival counter is over 200
    (dolist (enemy (enemy-list enemy-manager))
      (unless (and (or (= (state enemy) 1)            ; if enemy is alive
                       (= (state enemy) 2))           ; or enemy is damaged
                 (> (+ (x ship) 16) (x enemy))        ; 16 is center of ship
                 (< (+ (x ship) 16) (+ (x enemy) (width enemy)))      ; width enemy
                 (> (+ (y ship) 16) (y enemy))
                 (< (+ (y ship) 16) (+ (y enemy) (height enemy))))))  ; height enemy
      (setf (state ship) 1))))

;; Step12 <Draw Ship Explosion>
;; ----------------------------------------------------------------------------------------------- 
(defgeneric Draw-ship-explosion (ship explosion))
(defmethod Draw-ship-explosion (ship explosion)
  (when (and (= (state ship) 2)
                (< (explode-cnt ship) 30))               
    (setf (x explosion) (- (x ship) 16)           ; x : center of explosion 
          (y explosion) (y ship))                 ; y : center of explosion
    (cond ((<= (explode-cnt ship) 10)
            (setf (id explosion) 47))
          ((and (> (explode-cnt ship) 10) 
                (<= (explode-cnt ship) 20))
            (setf (id explosion) 48))
         ((> (explode-cnt ship) 20)          
            (setf (id explosion) 49)))
     (Draw explosion)))                           ; draw ship explosion

;; Step12 <Draw Ship>
;; ----------------------------------------------------------------------------------------------- 
(defgeneric Draw-ship (ship))
(defmethod Draw-ship (ship)
  (when (or (= (state ship) 1)
            (and (= (state ship) 3)
       	         (> 5 (mod (revival-cnt ship) 10))))   ; ship flushes on and off
    (Draw ship)))                                      ; draw ship revival
  
;; step1 <Game Frame>
;; -----------------------------------------------------------------------------------------------
(defun Common-abogadro ()
  "main routine"
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio) ; use video and audio
    (sdl:window 640 480 :position 'center                ; size 640*480, position center
                      ; :position #(192 50)              ;               position x(192) y(50)
                        :title-caption "ABOGADRO"
                        :icon-caption  "ABOGADRO"  
                        :flags '(sdl:sdl-doublebuf sdl:sdl-sw-surface))

    ; <Initialize>
      (Initialize)                                       ; graphics initialize

    ; <Set Font>
      (Set-font)                                         ; set font

    ; <Audio>
      (Open-sound)                                       ; sound open

    ; <Set Charactor Object>
      (let((ship (make-instance         'entity :id 0 :x 304 :y 416 :width 32 :height 32 :dx 4 :dy 4 :state 1))
           (keystate (make-instance     'keystate))         
           (game-field (make-instance   'game-field :field-x 160 :field-y 16 :width 480 :height 464))
           (stage (make-instance        'stage :stage-number (or nil 0) :title-loop t :ending-loop nil)) 
           (character (make-instance    'object :id 19 :y 100))
           (pointer (make-instance      'object :x 208 :y 328))
           (score (make-instance        'score  :highscore 50000 :oneup 50000 :n-ship 3 :n-bomb 3))           
           (score-ship (make-instance   'object :id 5 :x 160 :y 48))
           (score-bomb (make-instance   'object :id 6 :x 160 :y 448))
           (shot-manager (make-instance 'shot-manager))
           (enemy-manager (make-instance 'enemy-manager))
           (balloon-manager (make-instance 'balloon-manager :balloon-cnt (or nil 0)))
           (item-manager (make-instance 'item-manager))
	   (enemy-explosion (make-instance 'object :id 11))
	   (bomb-manager (make-instance 'bomb-manager :bomb-cnt (or nil 60) :bomb-number (or nil 10)))
	   (explosion (make-instance 'object :id 47))
           (explosion-manager (make-instance 'explosion-manager :bomb-cnt (or nil 150) :bomb-number (or nil 5)))) 

      (sdl:with-events (:poll)
        (:quit-event ()
          (setf *screen-mode* 1
                *switch* nil)
          (Reset-variables stage enemy-manager balloon-manager score ship shot-manager)
	  (Stop-sound)
          (Close-sound)
          t)

      ; <Update Key State> 
        (:key-down-event (:key key)
          (if (sdl:key= key :SDL-KEY-ESCAPE)
              (sdl:push-quit-event)
	  (Update-keystate key t keystate)))
        (:key-up-event (:key key)
          (Update-keystate key nil keystate)
          (setf (id ship) 0))                                 ; set ship id 0 (normal form)  

        (:idle ()
        ;<Title Loop> 
	  (when (and (eql (title-loop stage) t)               ; GAME TITLE flag   ON
                     (eql (ending-loop stage) nil))           ; GAME OVER flag    OFF
            (Game-start-message pointer character stage keystate))

	;<Game Over Loop>  
	  (when (and (eql (title-loop stage) nil)             ; GAME TITLE flag   OFF
                     (eql (ending-loop stage) t))             ; GAME OVER flag    ON
	    (Game-over-message stage enemy-manager score balloon-manager ship shot-manager keystate))
                   
        ; <Game Loop> 
          (when (and (eql (title-loop stage) nil)             ; GAME TITLE flag   OFF
	             (eql (ending-loop stage) nil))           ; GAME OVER flag    OFF

          ; <Set Screen Mode> 
            (Set-screen-mode)
          ; <Clear Display>                  
            (sdl:clear-display sdl:*black*)
          ; <Show Message and sound start>
            (Stage-start-message stage keystate)
          ; <Draw Map>
            (Scroll-background *atlas*)          
          ; <Move Ship> 
	    (Move-ship ship keystate)
          ; <Fix Ship Position>
	    (Fix-ship-position ship game-field)

          ; <Enemy :Move Generate Draw Remove>
            (Move-enemy enemy-manager game-field)
	    (Remove-enemy enemy-manager)
            (Generate-enemy-item *enemymap* enemy-manager item-manager)	    
            (Draw-enemy enemy-manager)
                       
          ; <Item : Move Draw Remove Hit>
	    (Move-item item-manager game-field)	        
            (hit-item-p item-manager balloon-manager score ship)
	    (Remove-item item-manager)
            (Draw-item item-manager)
	
          ; <Balloon :Move Set Draw>
            (Move-balloon balloon-manager ship)
            (Generate-balloon balloon-manager ship)
            (Draw-balloon balloon-manager ship)

	  ; <Enemy-shot :Move Set Draw Remove>  
	    (Move-enemy-shot enemy-manager game-field)
	    (Remove-dead-enemy-shot enemy-manager)
	    (Set-enemy-shot enemy-manager ship game-field)
            (Draw-enemy-shot enemy-manager)               ; draw enemy shot

	  ; <Enemy-hit-p :Damage Draw Explode>
	    (Enemy-hit-p shot-manager enemy-manager)
            (Damage-counter enemy-manager score)
            (Explode-enemy enemy-manager)
	    (Draw-enemy-explosion enemy-explosion enemy-manager)
            (Score-up score)

	  ; <Large Enemy Explosion>  
	    (Set-explosion explosion-manager enemy-manager)
	    (Explode-large-enemy explosion-manager)
	    (Remove-explode-large-enemy explosion-manager)
	    (Draw-explosion-large-enemy explosion-manager)	    

	  ; <Bomb :Set Explode Draw Remove>  
	    (Set-bomb-key ship bomb-manager keystate score)
            (Set-bomb bomb-manager enemy-manager)
            (Explode-bomb bomb-manager enemy-manager)	    
	    (Remove-explode-bomb bomb-manager)
	    (Remove-dead-enemy-shot enemy-manager)
	    (Draw-bomb bomb-manager)
     
          ; <Shot :Move Set Draw Delete> 
            (Move-shot shot-manager)
	    (Remove-dead-shot shot-manager)
	    (Set-shot shot-manager ship keystate balloon-manager)            
            (Draw-shot shot-manager)            

	  ; <Ship :Hit Explode Revive Draw>  
	    (Ship-hit-p ship enemy-manager balloon-manager score shot-manager)
	    (Remove-dead-shot shot-manager)
	    (Explode-counter ship score game-field stage)
	    (Draw-ship-explosion ship explosion)
            (Revive-counter ship enemy-manager)
	    (Draw-ship ship)
	    
          ; <Upper and Lower Window Mask>
            (Scroll-mask)
          ; <Draw Score Panel>
            (Score-panel score score-ship score-bomb)
          ; <Set Map Pinter> 
            (Set-map-pointer)                                         ; set map draw point
	  ; <Judge Stage End>	    
            (Judge-stage-end stage enemy-manager score shot-manager)  ; judge GAME OVER
	  ; <N-ship Zero P>  
	    (N-ship-zero-p score stage)                               ; judge GAME OVER

            (sdl:update-display)))))))

;(Common-abogadro)
