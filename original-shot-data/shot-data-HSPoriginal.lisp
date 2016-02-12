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
		     aftertime          ; waiting time for next shot
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
                      :battery-angle '(78.75 90 101.25)  ;3way
	              :direction-battery-angle 0                     ; not revolving
		      :number-battery 3     ; 3 battery
		      :number-battery-shot 1; 1 shot
		      :battery-direction 0  ; below
        	      :betweentime 0        ; waiting time for next shot
		      :aftertime 64
		      :shotspeed 4))        ; 4 dot move
(defvar shotdata2)
(setf shotdata2 (make-shotdata 
                      :battery-angle '(78.75 90 101.25)   ;3way
	              :direction-battery-angle 0                     ; not revolving
		      :number-battery 3     ; 3 battery
		      :number-battery-shot 1; 1 shot
		      :battery-direction 1  ; direction of ship
	 	      :betweentime 0        ; waiting time for next shot
		      :aftertime 64
		      :shotspeed 3))        ; 3 dot move
(defvar shotdata3)
(setf shotdata3  (make-shotdata
                      :battery-angle '(78.75 168.75 258.75 348.75) ; 4 way
	              :direction-battery-angle 5.62                    ; revolve
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
                      :battery-angle '(47.82 61.88 75.94 90 104.06 118.12 132.18) ;7 way
	              :direction-battery-angle 0                     ; not revolving
		      :number-battery 7     ; 7 battery
		      :number-battery-shot 1; 1 shot
		      :battery-direction 1  ; direction of ship
		      :betweentime 0 ; waiting time for next shot
		      :aftertime 64
		      :shotspeed 3))        ; 3 dot move
(defvar shotdata6)
(setf shotdata6  (make-shotdata
                      :battery-angle '(68.91 73.13 77.35 81.56 98.43 102.65 106.87 111.09) ; 8way
	              :direction-battery-angle 0                     ; not revolving
		      :number-battery 8     ; 8 battery
		      :number-battery-shot 5; 5 shot
		      :battery-direction 1  ; direction of ship
		      :betweentime 4 ; waiting time for next shot
		      :aftertime 32
		      :shotspeed 8))        ; 8 dot move
(defvar shotdata7)
(setf shotdata7 (make-shotdata
                      :battery-angle '(78.75 90 101.25)  ;3way		      
	              :direction-battery-angle 0                     ; not revolving
		      :number-battery 3     ; 3 battery
		      :number-battery-shot 5; 5 shot
		      :battery-direction 1  ; direction of ship
		      :betweentime 2;4 ; waiting time for next shot
		      :aftertime 16
		      :shotspeed 6))        ; 6 dot move
(defvar shotdata8)
(setf shotdata8 (make-shotdata
		      :battery-angle '(11.25 22.5 33.75 45 56.25 67.5 78.75 90
				       101.25 112.5 123.75 135 146.25 157.5 168.75 180
				       191.25 202.5 213.75 225 236.25 247.5 258.75 270
				       281.25 292.5 303.75 315 326.25 337.5 348.75 360);32 way                          
	              :direction-battery-angle 0                     ; not revolving
		      :number-battery 32    ; 32 battery
		      :number-battery-shot 1; 1 shot
		      :battery-direction 0  ; below
		      :betweentime 0  ; waiting time for next shot
		      :aftertime 4
		      :shotspeed 4))        ; 4 dot move
(defvar shotdata9) 
(setf shotdata9 (make-shotdata
                      :battery-angle '(16.87 28.12 39.37 50.62 61.87 73.12 84.37 95.62 
                                       106.87 118.12 129.37 140.62 151.87 163.12 174.37 185.62
		       		       196.87 208.12 219.37 230.62 241.87 253.12 264.37 275.62
				       286.87 298.12 309.37 320.62 331.87 343.12 354.37 5.62);32way	      
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
	              :direction-battery-angle 0                         ; not revolving
		      :number-battery 1      ; 1 battery
		      :number-battery-shot 10; 10 shot
		      :battery-direction 1   ; direction of ship
		      :betweentime 2  ; waiting time for next shot
		      :aftertime 64
        	      :shotspeed 8))         ; 8 dot move
(defvar shotdata11)
(setf shotdata11 (make-shotdata
                      :battery-angle '(180)  ;1 way like screw	      
	              :direction-battery-angle 8.43                       ; revolve
		      :number-battery 1      ; 1 battery
		      :number-battery-shot 64; 64 shot
		      :battery-direction 1   ; direction of ship
		      :betweentime 1
		      :aftertime 16
		      :shotspeed 6))         ; 6 dot move
(defvar shotdata12)
(setf shotdata12 (make-shotdata
                      :battery-angle '(78.75 101.25 123.75)  ;3 way		      
	              :direction-battery-angle  -5.62                      ; revolve
		      :number-battery 3     ; 3 battery
		      :number-battery-shot 5; 5 shot
		      :battery-direction 1  ; direction of ship
		      :betweentime 2 ; waiting time for next shot
		      :aftertime 32
         	      :shotspeed 6))        ; 6 dot move
(defvar shotdata13)
(setf shotdata13 (make-shotdata
                      :battery-angle '(84.38 87.19 90 92.81 95.62)  ;5 way	      
	              :direction-battery-angle 0                          ; not revolving
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

