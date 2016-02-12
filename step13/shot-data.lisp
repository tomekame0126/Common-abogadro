(defpackage :shot-data
  (:use :common-lisp)
  (:export #:shotdata #:shotdata-battery-angle #:shotdata-direction-battery-angle
	   #:shotdata-number-battery #:shotdata-number-battery-shot
	   #:shotdata-battery-direction #:shotdata-aftertime #:shotdata-betweentime
	   #:shotdata-shotspeed #:*shot-pattern-data*))

(in-package :shot-data)

;; step13 <Enemy-shot-data>
;; -----------------------------------------------------------------------------------------------
(defstruct shotdata  battery-angle
	             direction-battery-angle      ; revolving or not revolving
		     number-battery     ; battery
		     number-battery-shot; shot
		     battery-direction  ; direction of ship
		     betweentime        ; time between 1 shot and 1 shot 
		     aftertime          ; waiting time for next shot pattern
		     shotspeed)         ; dot move
(defvar shotdata)
(setf shotdata (make-shotdata))

(defvar shotdata0)
(setf shotdata0 (make-shotdata
	              :battery-angle '(90)   ; 1way  ;-- 90 degree is center position --;
	              :direction-battery-angle 0                     ; not revolving
		      :number-battery 1     ; 1 battery
		      :number-battery-shot 1; 1 shot
		      :battery-direction 1  ; direction of ship
                      :betweentime 0        ; waiting time for next shot
		      :aftertime 64
		      :shotspeed 5))        ; 5 dot move
(defvar shotdata1)
(setf shotdata1 (make-shotdata
                      :battery-angle '(79 90 101)  ;3way
	              :direction-battery-angle 0                     ; not revolving
		      :number-battery 3     ; 3 battery
		      :number-battery-shot 1; 1 shot
		      :battery-direction 0  ; below
        	      :betweentime 0        ; waiting time for next shot
		      :aftertime 64
		      :shotspeed 4))        ; 4 dot move
(defvar shotdata2)
(setf shotdata2 (make-shotdata 
                      :battery-angle '(79 90 101)   ;3way
	              :direction-battery-angle 0                     ; not revolving
		      :number-battery 3     ; 3 battery
		      :number-battery-shot 1; 1 shot
		      :battery-direction 1  ; direction of ship
	 	      :betweentime 0        ; waiting time for next shot
		      :aftertime 64
		      :shotspeed 3))        ; 3 dot move
(defvar shotdata3)
(setf shotdata3  (make-shotdata
                      :battery-angle '(79 169 259 349) ; 4 way
	              :direction-battery-angle 6                    ; revolve
		      :number-battery 4     ; 4 battery
		      :number-battery-shot 5; 5 shot
		      :battery-direction 0  ; below
		      :betweentime 2        ; waiting time for next shot
		      :aftertime 32
		      :shotspeed 3))        ; 3 dot move
(defvar shotdata4)
(setf shotdata4 (make-shotdata
                      :battery-angle '(90)   ;1 way		      
	              :direction-battery-angle 0                     ; not revolving
		      :number-battery 1     ; 1 battery
		      :number-battery-shot 5; 5 shot
		      :battery-direction 1  ; direction of ship
		      :betweentime 2 ; waiting time for next shot
		      :aftertime 64
		      :shotspeed 4))        ; 4 dot move
(defvar shotdata5)
(setf shotdata5 (make-shotdata
                      :battery-angle '(48 62 76 90 104 118 132) ;7 way
	              :direction-battery-angle 0                     ; not revolving
		      :number-battery 7     ; 7 battery
		      :number-battery-shot 1; 1 shot
		      :battery-direction 1  ; direction of ship
		      :betweentime 0 ; waiting time for next shot
		      :aftertime 64
		      :shotspeed 3))        ; 3 dot move
(defvar shotdata6)
(setf shotdata6  (make-shotdata
                      :battery-angle '(69 73 77 82 98 103 107 111) ; 8way
	              :direction-battery-angle 0                     ; not revolving
		      :number-battery 8     ; 8 battery
		      :number-battery-shot 5; 5 shot
		      :battery-direction 1  ; direction of ship
		      :betweentime 4 ; waiting time for next shot
		      :aftertime 32
		      :shotspeed 8))        ; 8 dot move
(defvar shotdata7)
(setf shotdata7 (make-shotdata
                      :battery-angle '(79 90 101)  ;3way		      
	              :direction-battery-angle 0                     ; not revolving
		      :number-battery 3     ; 3 battery
		      :number-battery-shot 5; 5 shot
		      :battery-direction 1  ; direction of ship
		      :betweentime 4 ; waiting time for next shot
		      :aftertime 16
		      :shotspeed 6))        ; 6 dot move
(defvar shotdata8)
(setf shotdata8 (make-shotdata
		      :battery-angle '(11 23 34 45 56 68 79 90
				       101 113 124 135 146 158 169 180
				       191 203 214 225 236 248 259 270
				       281 293 304 315 326 338 349 360);32 way                          
	              :direction-battery-angle 0                       ; not revolving
		      :number-battery 32    ; 32 battery
		      :number-battery-shot 1; 1 shot
		      :battery-direction 0  ; below
		      :betweentime 0  ; waiting time for next shot
		      :aftertime 8          ; original 4 but don't work <---------
		      :shotspeed 4))        ; 4 dot move
(defvar shotdata9) 
(setf shotdata9 (make-shotdata
                      :battery-angle '(17 28 39 51 62 73 84 96 
                                       107 118 129 141 152 163 174 186
		       		       197 208 219 231 242 253 264 276
				       287 298 309 321 332 343 354 6);32way	      
	              :direction-battery-angle 0                       ; not revolving
		      :number-battery 32    ; 32 battery
		      :number-battery-shot 1; 1 shot
		      :battery-direction 0  ; below
		      :betweentime 0 ; waiting time for next shot
		      :aftertime 16
		      :shotspeed 4))        ; 4 dot move
(defvar shotdata10)
(setf shotdata10  (make-shotdata
                      :battery-angle '(90)  ;1 way		      
	              :direction-battery-angle 0                       ; not revolving
		      :number-battery 1      ; 1 battery
		      :number-battery-shot 10; 10 shot
		      :battery-direction 1   ; direction of ship
		      :betweentime 8         ; original 2 but don't work <----------
		      :aftertime 64
        	      :shotspeed 8))         ; 8 dot move
(defvar shotdata11)
(setf shotdata11 (make-shotdata
                      :battery-angle '(180)  ;1 way like screw	      
	              :direction-battery-angle 8                       ; revolve
		      :number-battery 1      ; 1 battery
		      :number-battery-shot 64; 64 shot
		      :battery-direction 1   ; direction of ship
		      :betweentime 1
		      :aftertime 16
		      :shotspeed 6))         ; 6 dot move
(defvar shotdata12)
(setf shotdata12 (make-shotdata
                      :battery-angle '(79 101 124)  ;3 way		      
	              :direction-battery-angle  -6                      ; revolve
		      :number-battery 3     ; 3 battery
		      :number-battery-shot 5; 5 shot
		      :battery-direction 1  ; direction of ship
		      :betweentime 2 ; waiting time for next shot
		      :aftertime 32
         	      :shotspeed 6))        ; 6 dot move
(defvar shotdata13)
(setf shotdata13 (make-shotdata
                      :battery-angle '(84 87 90 93 96)  ;5 way	      
	              :direction-battery-angle 0                         ; not revolving
		      :number-battery 5     ; 5 battery
		      :number-battery-shot 5; 5 shot
		      :battery-direction 1  ; direction of ship
		      :betweentime 4 ; waiting time for next shot
		      :aftertime 32
		      :shotspeed 8))        ; 8 dot move

(defparameter *shot-pattern-data* (make-array 14))

(setf (aref *shot-pattern-data* 0) shotdata0)
(setf (aref *shot-pattern-data* 1) shotdata1)
(setf (aref *shot-pattern-data* 2) shotdata2)
(setf (aref *shot-pattern-data* 3) shotdata3)
(setf (aref *shot-pattern-data* 4) shotdata4)
(setf (aref *shot-pattern-data* 5) shotdata5)
(setf (aref *shot-pattern-data* 6) shotdata6)
(setf (aref *shot-pattern-data* 7) shotdata7)
(setf (aref *shot-pattern-data* 8) shotdata8)
(setf (aref *shot-pattern-data* 9) shotdata9)
(setf (aref *shot-pattern-data* 10) shotdata10)
(setf (aref *shot-pattern-data* 11) shotdata11)
(setf (aref *shot-pattern-data* 12) shotdata12)
(setf (aref *shot-pattern-data* 13) shotdata13)

