;;;; The Common-Abogadro
;;; step1  <Game Frame> <Sprite Sheets> <Define Package> <Macro> <Character Object> <Draw>
;;;        <Initialize> <Key State> <Game Field>
;;; step2  <Map> <Scroll> 
;;; step3  <Font> <Stage Class> <Start Stage Message>
;;; step4  <Game Start Message> <Game Over Message> <Set Screen mode>
      
;; step1 <Sprite Sheets>
;; -----------------------------------------------------------------------------------------------
(load "C:\\work\\sprite-sheets.lisp")

;; step2 <Map>
;; -----------------------------------------------------------------------------------------------  
(load "C:\\work\\map-list.lisp")

;; step1 <Define Package>
;; -----------------------------------------------------------------------------------------------
(defpackage :game
  (:use :common-lisp :lispbuilder-sdl :sprite-sheets :map-list)
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

(defgeneric Stage-start-message (stage))
(defmethod Stage-start-message (stage)                     ; stage start message
  "Draw stage start message and set game parameters"
  (when (eql (stage-flag stage) t)
    (setf (stage-flag stage) nil)
    (incf (stage-number stage) 1)
    (case (stage-number stage)
      (1 (setf *atlas* *map1*))
      (2 (setf *atlas* *map2*))
      (t (setf *atlas* *map3*)))                           ; *map3* loops forever
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

(defun Scroll-background (map)
  "draw background"
  (setf *draw-position-y* (+ -48 (mod *scroll-cnt* 64))) ; scroll start from y(-48) to y(16)
  (dotimes (i 8)                                         ; 8 row
    (dotimes (j 5)                                       ; 5 column
      (sdl:draw-surface-at-* *images* (+ 160 (* j 64)) (+ *draw-position-y* (* i 64))
          :cell (aref map (+ *map-pointer* i) j)))))

(defgeneric Set-map-edge (stage))
(defmethod Set-map-edge (stage)
  (incf *scroll-cnt*)
  (when (eql (mod *scroll-cnt* 64) 0)                    ; mapchip draw position
    (setf *draw-position-y* 0)
    (when (= *map-pointer* 0)                            ; when scroll-line is 0 (end line)
          (setf *map-pointer* 64)                        ; set scroll-line 64 (start line)
          (setf (stage-flag stage) t))                   ; change stage
    (decf *map-pointer*)))                               ; else scroll-line -1


(defun Scroll-mask ()
  (sdl:draw-box-* 160 0 320 16 :color sdl:*black*)       ; mask scroll upper side
  (sdl:draw-box-* 160 464 320 480 :color sdl:*black*))   ; mask scroll lower side

;; Step4 <Game Start Message>
;; -----------------------------------------------------------------------------------------------
(defvar *screen-mode* 1)        ; screen mode toggle switch  0:fullscreen 1:windowed
(defvar *switch* nil)           ; screen change flag

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
          (when (=  (y pointer) 360) 
            (if (= *screen-mode* 1)                               ; screen-mode toggle switch
              (setf *screen-mode* 0)                              ; 0:fullscreen  1:windowed
              (setf *screen-mode* 1)))
          (setf *switch* t)
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

;; Step4 <Game Over Message>
;; -----------------------------------------------------------------------------------------------
(defgeneric Game-over-message (stage))
(defmethod Game-over-message (stage)
  "Draw game ending message"
  (sdl:clear-display sdl:*black*)
 ; message
 ; (sdl:draw-string-solid-* "君の活躍によりアボガドロ軍は撤退した。"
 ;                                         178 64  :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* "THE ABOGADRO FORCES WITHDREW BY YOUR SUCCESS"
                                           150 64  :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* "C O N G R A T U L A T I O N S"
                                           208 96  :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* "Y O U R S C O R E 0000000 "                                    ; <--dummy
                                           224 160 :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* "H I G H S C O R E 0005000"                                     ; <--dummy
                                           224 192 :color sdl:*white* :font *menu-font*)
  (sdl:update-display)
  (sleep 10)
 ; reset variables 
  (setf (title-loop stage) t
        (stage-flag stage) t
        *map-pointer* 64 
        (stage-number stage) 0)) 

;; Step4 <Judge Map End>
;; -----------------------------------------------------------------------------------------------
(defgeneric Judge-map-end(stage))
(defmethod Judge-map-end (stage)
  (when (and (= (stage-number stage) 3)                  ; stage 3 and map-poiner is 0
             (= *map-pointer* 0))                        ; game over
    (Game-over-message stage)))

;; Step4 <Set Screen Mode>
;; -----------------------------------------------------------------------------------------------
(defun Set-screen-mode()
  (when (eql *switch* t) 
    (if (= *screen-mode* 0)                    ; fullscreen-mode on 
      (sdl:resize-window 640 480 :sw t :fullscreen t)
      (sdl:resize-window 640 480 :sw t :resizable t))       
    (setf *switch* nil)))                      ; twice executing prevent

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

    ; <Set Charactor Object>
      (let((ship (make-instance       'entity :id 0 :x 304 :y 416 :width 32 :height 32 :dx 4 :dy 4 :state 1))
           (keystate (make-instance   'keystate))         
           (game-field (make-instance 'game-field :field-x 160 :field-y 16 :width 480 :height 464))
           (stage (make-instance      'stage :stage-number (or nil 0) :title-loop t)) 
           (character (make-instance  'object :id 19 :y 100))
           (pointer (make-instance    'object :x 208 :y 328)))   

      (sdl:with-events (:poll)
        (:quit-event ()
          (setf *screen-mode* 1
                *switch* nil)
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
            (Scroll-mask)
          ; <Move Ship> 
	    (Move-ship ship keystate)
          ; <Fix Ship Position>
	    (Fix-ship-position ship game-field)       
          ; <Draw Images>
            (when (= (state ship) 1)
              (Draw ship))                                ; draw ship
          ; <Set Map Edge> 
            (Set-map-edge stage)                          ; set map draw point
          ; <Judge All Map Used> 
            (Judge-map-end stage)

            (sdl:update-display)))))))

(Common-abogadro)

