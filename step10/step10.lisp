;;;; The Common-Abogadro
;;; step1  <Game Frame> <Sprite Sheets> <Define Package> <Macro> <Character Object> <Draw>
;;;        <Initialize> <Key State> <Game Field>
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
      
;; step1 <Sprite Sheets>
;; -----------------------------------------------------------------------------------------------
(load "C:\\work\\sprite-sheets.lisp")

;; step2 <Map>
;; -----------------------------------------------------------------------------------------------  
(load "C:\\work\\map-list.lisp")

;; step7 <Enemy Map List>
;; -----------------------------------------------------------------------------------------------
(load "C:\\work\\enemy-map-list.lisp")

;; step7 <Enemy Map List>
;; -----------------------------------------------------------------------------------------------
(load "C:\\work\\move-pattern.lisp")

;; step10 <Audio>
;; -----------------------------------------------------------------------------------------------  
(load "C:\\work\\audio-list.lisp")

;; step1 <Define Package> + audio
;; -----------------------------------------------------------------------------------------------
(defpackage :game
  (:use :common-lisp :lispbuilder-sdl :sprite-sheets :map-list :enemy-map-list :move-pattern :audio-list)
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

(define-class foe (entity)
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
  (stage-flag stage-number title-loop start) t)
 ; stage-flag        on-stage or not
 ; stage-number      map change
 ; title-loop        waiting for input-key
 ; start             game start

;; Step3 <Start Stage Message>
;; -----------------------------------------------------------------------------------------------
(defvar *atlas*)                                           ; map set
(defvar *enemymap*)                                        ; enemy map set

(defgeneric Stage-start-message (stage))
(defmethod Stage-start-message (stage)                     ; stage start message
  "Draw stage start message and set game parameters"
  (when (eql (stage-flag stage) t)
    (setf (stage-flag stage) nil)
    (incf (stage-number stage) 1)
    (case (stage-number stage)
      (1 (setf *atlas* *map1*
               *enemymap* *enemy-map1*))
      (2 (setf *atlas* *map2*
               *enemymap* *enemy-map2*))
      (t (setf *atlas* *map3* 
               *enemymap* *enemy-map3*)))
    (sdl:clear-display sdl:*black*)
    (sdl:draw-string-solid-* 
         (format nil "S T A G E  ~d" (stage-number stage)) 272 208 :color sdl:*white* :font *menu-font*)
    (sdl:update-display)
    (sleep 3)))

;; step 2 <Scroll>
;; -----------------------------------------------------------------------------------------------  
(defvar *scroll-cnt* 0)                                  ; scroll counter
(defvar *map-pointer* 64)                                ; map start line
(defvar *draw-position-y* 0)                             ; y-axis start position
(defvar *repeat* nil)                                    ; scroll repeat flag
(defvar *rotation* '(0 1 2 3 4 5 6 7))                   ; map rotation list

(defun Rotate-map-pointer ()
  "Move last element to front of the list"
  (let ((x *rotation*))                                  ; list rotation right
    (setf *rotation* (append (last x) (butlast x)))))    ; --> (8 0 1 2 3 4 5 6 7)

(defun Set-map-pointer ()
  "Set map pointer"
  (incf *scroll-cnt* 2)                                  ; 2 dot scroll
  (when (eql (mod *scroll-cnt* 64) 0)                    ; mapchip draw position
    (setf *draw-position-y* 0)
    (decf *map-pointer*)                                 ; else scroll-line -1
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

;; Step7 <Reset Variables>
;; -----------------------------------------------------------------------------------------------
(defvar *enemy-map-pointer* 64)

(defgeneric Reset-variables (stage enemy-manager balloon-manager score))
(defmethod Reset-variables (stage enemy-manager balloon-manager score)
  "reset variables" 
  (setf (title-loop stage) t
        (stage-flag stage) t
        *scroll-cnt* 0
        *draw-position-y* 0
        *map-pointer* 64
        *repeat* nil
        *enemy-map-pointer* 64
	(enemy-list enemy-manager) nil
        (stage-number stage) 0
        (balloon-cnt balloon-manager) 0
        (score score) 0
        (n-ship score) 3
        (n-bomb score) 3))

;; Step4 <Game Over Message> + score
;; -----------------------------------------------------------------------------------------------
(defgeneric Game-over-message (stage enemy-manager score balloon-manager))
(defmethod Game-over-message (stage enemy-manager score balloon-manager)
  "Draw game ending message"
  (sdl:clear-display sdl:*black*)
 ; message
  (sdl:draw-string-solid-* "君の活躍によりアボガドロ軍は撤退した。"
                                          178 64  :color sdl:*white* :font *menu-font*)
 ; (sdl:draw-string-solid-* "THE ABOGADRO FORCES WITHDREW BY YOUR SUCCESS"
 ;                                          150 64  :color sdl:*white* :font *menu-font*)
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
  (sdl:update-display)
  (sleep 10)
  (Reset-variables stage enemy-manager balloon-manager score))

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
      (setf (state shot) 0))))              ; shote is dead

;; step6 <Set Shot> + Balloon Shot
;; -----------------------------------------------------------------------------------------------
(defgeneric Set-shot (shot-manager ship keystate balloon-manager))
(defmethod Set-shot (shot-manager ship keystate balloon-manager)
  "set shot"
  (when (and (/= (state ship) 2)                          ; if ship is not explode
	     (eql (mod *scroll-cnt* 8) 0)                 ; 1 shot / 8 loop          
             (eql (z keystate) t))                        ; and set z key    
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
  (when (and (= (mod *scroll-cnt* 64) 0)
             (<= (length (enemy-list enemy-manager)) 10)); max 10 enemy
     (dotimes (j 10)
       (when (/= (aref map *enemy-map-pointer* j) -1)
         (case (aref map *enemy-map-pointer* j)
                 ((7 50 80)                              ; when id is 7 or 50 or 80
                   (let ((enemy (make-instance 'foe      ; small class enemy generate
                                 :id (aref map *enemy-map-pointer* j)
                                 :x (+ 160 (* j 32)) :y 0 :width 32 :height 32
                                 :life-cnt 3 :kind 1 :state 1)))
                   (push enemy (enemy-list enemy-manager))))
                 ((76 78)                                ; when id is 76 or 78
                   (let ((enemy (make-instance 'foe      ; middle class enemy generate
                                 :id (aref map *enemy-map-pointer* j)
                                 :x (+ 160 (* j 32)) :y 0 :width 64 :height 64
                                 :life-cnt 20 :kind 2 :state 1)))
                   (push enemy (enemy-list enemy-manager))))
                 ((70 71 72)                             ; when id is 70 or 71 or 72
                   (let ((enemy (make-instance 'foe      ; large class enemy generate
                                  :id (aref map *enemy-map-pointer* j)
                                  :x (+ 160 (* j 32)) :y 0 :width 96 :height 96
                                  :life-cnt 300 :kind 3 :state 1)))
                   (push enemy (enemy-list enemy-manager))))
		 ((17 18)                                ; when id is 17 or 18
                   (let ((enemy (make-instance 'foe      ; item generate
                                  :id (aref map *enemy-map-pointer* j)
                                  :x (+ 160 (* j 32)) :y 0 :width 32 :height 32 :dx 0 :dy 2 :kind 4 :state 1)))
                   (push enemy (item-list item-manager)))))))     ; store items into item-list
     (when (/= *enemy-map-pointer* 0)
       (decf *enemy-map-pointer*))))             ; *enemy-map-pointer* 64 -> 0 (end position)  

;; step7 <Judge Stage End>
;; -----------------------------------------------------------------------------------------------
(defgeneric Judge-stage-end (stage enemy-manager score balloon-manager))
(defmethod Judge-stage-end (stage enemy-manager score balloon-manager)
  (dolist (enemy (enemy-list enemy-manager))              
    (when (and (= (kind enemy) 3)                  ; large class enemy
               (= (state enemy) 0))                ; this enemy is dead
      (case (stage-number stage)
        ((1 2)                                     ; 1 or 2 stage                              
          (setf (stage-flag stage) t
                *repeat* nil
                *map-pointer* 64 
                *enemy-map-pointer* 64             ; map and enemy-map set start position
                (enemy-list enemy-manager) nil))
        ((3)                                       ; 3rd stage  
          (setf *repeat* nil
                *map-pointer* 64
                *enemy-map-pointer* 64
                (enemy-list enemy-manager) nil)
                (Game-over-message stage enemy-manager score balloon-manager))))))

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
        ((76 78 77 79)                ; id 76 78(middle class enemy)                 
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

;; step8 <Move Balloon>
;; -----------------------------------------------------------------------------------------------
(defun degree-radian (degree)                       ; convert from radian to degree
  (/ (* degree pi) 180))                            ; degree -> radian

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
  (when (and (or (/= (state ship) 1)
                     (state ship) 3) 
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

;; Step8 <Remove Item>
;; -----------------------------------------------------------------------------------------------      
(defgeneric Remove-item (item-manager))
(defmethod Remove-item (item-manager)
  "item remove from list"
  (setf (item-list item-manager) 
	(delete-if #'(lambda (item) (= (state item) 0)) (item-list item-manager))))

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
            (setf *itemget-flag* t))  
          (when (= (id item) 17)
            (if (< (balloon-cnt balloon-manager) 2)
              (incf (balloon-cnt balloon-manager) 1)
              (incf (score score) 500)))
          (when (= (id item) 18)
            (incf (n-bomb score) 1))
          (setf *itemget-flag* nil)))))

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

;; Step9 <Set Enemy Shot>
;; -----------------------------------------------------------------------------------------------
(defvar *range-x*)
(defvar *range-y*)
(defvar *distance*)

(defparameter *enemy-shot-max* 10)

(defgeneric Set-enemy-shot (enemy-manager ship game-field))
(defmethod Set-enemy-shot (enemy-manager ship game-field)
  "enemy shot appear position set and move"  
  (dolist (enemy (enemy-list enemy-manager)) 
    (when (and (= (state enemy) 1)
               (>= (x enemy) 0)
               (< (x enemy) (- (width game-field) (width enemy)))  ; 32 or 64 or 96
               (>= (y enemy) 0)
               (< (y enemy) (- (height game-field) (width enemy))) ; 32 or 64 or 96
               (= (mod (move-cnt enemy) 64) 0))
       (when (< (length (enemy-shot-list enemy-manager)) *enemy-shot-max*)                       
         (let ((enemy-shot (make-instance 'entity :id 4 :width 8 :height 8 :dx 0 :dy 6 :state 0)))
               (push enemy-shot (enemy-shot-list enemy-manager))))
       (dolist (enemy-shot (enemy-shot-list enemy-manager))
         (when (= (state enemy-shot) 0)
           (case (kind enemy)
             ((1)
               (setf (y enemy-shot) (+ (y enemy) 12))    ; center of small class enemy x
               (setf (x enemy-shot) (+ (x enemy) 12)))   ; center of small class enemy y 
             ((2)
               (setf (y enemy-shot) (+ (y enemy) 28))    ; center of middle class enemy x
               (setf (x enemy-shot) (+ (x enemy) 28)))   ; center of middle class enemy y 
             ((3)
	       (setf (y enemy-shot) (+ (y enemy) 44))    ; center of large class enemy x
               (setf (x enemy-shot) (+ (x enemy) 44))))  ; center of large class enemy y 
           (let ((ship-x  (+ (x ship) (/ (width ship) 2)))
                 (ship-y  (+ (y ship) (/ (height ship) 2)))
                 (ene-shot-x (+ (x enemy-shot) (/ (width enemy-shot) 2)))
                 (ene-shot-y (+ (y enemy-shot) (/ (height enemy-shot) 2))))
             (setf *range-x* (- ship-x ene-shot-x))
             (setf *range-y* (- ship-y ene-shot-y))
             (setf *distance* (sqrt (+ (* *range-x* *range-x*) (* *range-y* *range-y*))))
             (setf (dx enemy-shot) (* (/ *range-x* *distance*) 6))
             (setf (dy enemy-shot) (* (/ *range-y* *distance*) 6)))
           (setf (state enemy-shot) 1))))))

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
                   (< (x shot) (+ (x enemy) 32))
                   (> (+ (y shot) 16) (y enemy))
                   (< (y shot) (+ (y enemy) 32)))
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

(defgeneric Explode-enemy (enemy-manager))
(defmethod Explode-enemy (enemy-manager)
  "enemy explosion while 16 times loop"
  (dolist (enemy (enemy-list enemy-manager))
    (when (= (state enemy) 3)
      (incf (explode-cnt enemy) 1)
      (when (eql *explodes-flag* nil)
        (setf *explodes-flag* t)                  ; explode sound on
        (Play-sample *explodes*))                 ; explode sound              
      (when (= (explode-cnt enemy) 15)            ; enemy explode count 15
        (setf (state enemy) 0
              *explodes-flag* nil)))))            ; explode sound off

;; Step10 <Draw Enemy Explosion>
;; -----------------------------------------------------------------------------------------------
(defgeneric Draw-enemy-explosion (enemy-explosion enemy-manager))
(defmethod Draw-enemy-explosion (enemy-explosion enemy-manager)
  (dolist (enemy (enemy-list enemy-manager))
    (when (= (state enemy) 3)
      (setf (x enemy-explosion) (x enemy)
            (y enemy-explosion) (y enemy))
        (cond ((and (>= (explode-cnt enemy) 0)
                    (<  (explode-cnt enemy) 5))
                (case (kind enemy)
                  ((1)      
                    (setf (id enemy-explosion) 11))    ; bomb id 11
                  ((2 3)
                    (setf (id enemy-explosion) 64))))  ; bomb id 64     
              ((and (>= (explode-cnt enemy) 5) 
                    (<  (explode-cnt enemy) 10))
	        (case (kind enemy)
		  ((1)
                    (setf (id enemy-explosion) 12))    ; bomb id 12 
		  ((2 3)
	            (setf (id enemy-explosion) 65))))  ; bomb id 65 
              ((and (>= (explode-cnt enemy) 10)
                    (<  (explode-cnt enemy) 15))
	        (case (kind enemy)
	          ((1)
                    (setf (id enemy-explosion) 13))    ; bomb id 13
		  ((2 3)
		    (setf (id enemy-explosion) 66))))) ; bomb id 66
      (Draw enemy-explosion))))   

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
           (stage (make-instance        'stage :stage-number (or nil 0) :title-loop t)) 
           (character (make-instance    'object :id 19 :y 100))
           (pointer (make-instance      'object :x 208 :y 328))
           (score (make-instance        'score  :highscore 50000 :oneup 50000 :n-ship 3 :n-bomb 3))           
           (score-ship (make-instance   'object :id 5 :x 160 :y 48))
           (score-bomb (make-instance   'object :id 6 :x 160 :y 448))
           (shot-manager (make-instance 'shot-manager))
           (enemy-manager (make-instance 'enemy-manager))
           (balloon-manager (make-instance 'balloon-manager :balloon-cnt (or nil 0)))
           (item-manager (make-instance 'item-manager))
	   (enemy-explosion (make-instance 'object :id 11)))

      (sdl:with-events (:poll)
        (:quit-event ()
          (setf *screen-mode* 1
                *switch* nil)
          (Reset-variables stage enemy-manager balloon-manager score) 
          t)

      ; <Update Key State> 
        (:key-down-event (:key key)
          (if (sdl:key= key :SDL-KEY-ESCAPE)
              (sdl:push-quit-event)
	  (Update-keystate key t keystate)))
        (:key-up-event (:key key)
          (Update-keystate key nil keystate)
          (setf (id ship) 0))                            ; set ship id 0 (normal form)  

        (:idle ()
        ;<Title Loop> 
	  (when (eql (title-loop stage) t)               ; title loop
            (sdl:clear-display sdl:*black*)
            (Game-start-message pointer character stage keystate))                   

        ; <Game Loop> 
          (when (eql (title-loop stage) nil)             ; game loop

          ; <Set Screen Mode> 
            (Set-screen-mode)
          ; <Clear Display>                  
            (sdl:clear-display sdl:*black*)
          ; <Show Message>
            (Stage-start-message stage)
          ; <Draw Map>
            (Scroll-background *atlas*)          
          ; <Move Ship> 
	    (Move-ship ship keystate)
          ; <Fix Ship Position>
	    (Fix-ship-position ship game-field)
     
          ; <Shot :Move Set Draw Delete> 
            (Move-shot shot-manager)       
	    (Set-shot shot-manager ship keystate balloon-manager)
            (when (= (state ship) 1)
              (Draw ship))                                ; draw ship
            (Draw-shot shot-manager)
            (Remove-dead-shot shot-manager)

          ; <Enemy :Move Generate Draw Remove>
            (Move-enemy enemy-manager game-field)
            (Generate-enemy-item *enemymap* enemy-manager item-manager)
            (Draw-enemy enemy-manager)
            (Remove-enemy enemy-manager)

          ; <Balloon :Move Set Draw>
            (Move-balloon balloon-manager ship)
            (Generate-balloon balloon-manager ship)
            (Draw-balloon balloon-manager ship)

          ; <Item : Move Draw Remove Hit>
	    (Move-item item-manager game-field)
            (Draw-item item-manager)
	    (Remove-item item-manager)
            (hit-item-p item-manager balloon-manager score ship)

	  ; <Enemy-shot :Move Set Draw Remove>  
	    (Move-enemy-shot enemy-manager game-field)
	    (Set-enemy-shot enemy-manager ship game-field)
            (Draw-enemy-shot enemy-manager)               ; draw enemy shot
	    (Remove-dead-enemy-shot enemy-manager)

	  ; <Enemy-hit-p :Damage Draw Explode>
	    (Enemy-hit-p shot-manager enemy-manager)
            (Damage-counter enemy-manager score)
            (Explode-enemy enemy-manager)
	    (Draw-enemy-explosion enemy-explosion enemy-manager)
            (Score-up score)

          ; <Upper and Lower Window Mask>
            (Scroll-mask)
          ; <Draw Score Panel>
            (Score-panel score score-ship score-bomb)
          ; <Set Map Pinter> 
            (Set-map-pointer)                             ; set map draw point
          ; <Judge Stage End>
            (Judge-stage-end stage enemy-manager score balloon-manager) 

            (sdl:update-display)))))))

(common-abogadro)
