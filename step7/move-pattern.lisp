(defpackage :move-pattern
  (:use :common-lisp)
  (:export #:*enemy-move-pattern1* #:*enemy-move-pattern2* #:*enemy-move-pattern3*
           #:*enemy-move-pattern4* #:*enemy-move-pattern5* #:*enemy-move-pattern6*
           #:*enemy-move-pattern7* #:*enemy-move-pattern8*))

(in-package :move-pattern)

;; step7 <Enemy-move-pattern>
;; -----------------------------------------------------------------------------------------------  
(defparameter *enemy-move-pattern1* (make-array '(16 2) :initial-contents '((-8 4)  ; 0
				                                            (-6 4)
								            (-4 4)
								            (-2 4)
								            (0 4)
								            (2 4)
								            (4 4)
								            (6 4)
								            (8 4)
								            (6 4)
								            (4 4)   ; 10
								            (2 4)
								            (0 4)
								            (-2 4)
								            (-4 4)
								            (-6 4))))

(defparameter *enemy-move-pattern2* (make-array '(12 2) :initial-contents '((2 2)  ; 0
				                                            (2 4)
								            (2 6)
								            (2 8)
								            (2 8)
								            (2 6)
								            (2 4)
								            (2 2)
								            (2 -1)
								            (2 -2)
								            (2 -2)   ; 10
                                                                            (2 -1))))
								            
(defparameter *enemy-move-pattern3* (make-array '(16 2) :initial-contents '((1 3)  ; 0
				                                            (1 3)
								            (1 3)
								            (1 3)
								            (1 3)
								            (1 3)
								            (1 3)
								            (1 3)
								            (0 1)
								            (0 1)
								            (0 1)   ; 10
                                                                            (0 1)
                                                                            (0 1)
								            (0 1)
                                                                            (0 1)
                                                                            (0 1))))

(defparameter *enemy-move-pattern4* '(0 2))  ; 0

(defparameter *enemy-move-pattern5* '(1 2))  ; 0

(defparameter *enemy-move-pattern6* (make-array '(32 2) :initial-contents '((-2 1)   ; 0
                                                                            (-2 1)
									    (-2 1)
									    (-2 1)
									    (-2 1)
									    (-2 1)
									    (-2 1)
									    (-2 1)
									    (2 1)
									    (2 1)
									    (2 1)    ; 10
									    (2 1)
									    (2 1)
									    (2 1)
									    (2 1)
									    (2 1)
									    (2 -1)
									    (2 -1)
									    (2 -1)
									    (2 -1)
									    (2 -1)   ; 20
									    (2 -1)
									    (2 -1)
									    (2 -1)
									    (-2 -1)
									    (-2 -1)
									    (-2 -1)
									    (-2 -1)
									    (-2 -1)
									    (-2 -1)
									    (-2 -1)  ; 30
									    (-2 -1))))

(defparameter *enemy-move-pattern7* (make-array '(32 2) :initial-contents '((-2 -2)   ; 0
                                                                            (-2 -2)
									    (-2 -2)
									    (-2 -2)
									    (2 2)
									    (2 2)
									    (2 2)
									    (2 2)
									    (-2 2)
									    (-2 2)
									    (-2 2)    ; 10
									    (-2 2)
									    (2 -2)
									    (2 -2)
									    (2 -2)
									    (2 -2)
									    (2 2)
									    (2 2)
									    (2 2)
									    (2 2)
									    (-2 -2)   ; 20
									    (-2 -2)
									    (-2 -2)
									    (-2 -2)
									    (2 -2)
									    (2 -2)
									    (2 -2)
									    (2 -2)
									    (-2 2)
									    (-2 2)
									    (-2 2)  ; 30
									    (-2 2))))

(defparameter *enemy-move-pattern8* '(0 0))  ; 0

