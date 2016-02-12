(defpackage :sprite-sheets
  (:use :common-lisp)
  (:export #:*images*
           #:Set-imageid))

(in-package :sprite-sheets)

;; step2 <Sprite Sheets>
;; -----------------------------------------------------------------------------------------------
(defparameter *path-samplecg* "C:\\work\\graphics\\samplecg.bmp")  ; set path to samplecg.bmp
(defvar *images*)
(defvar *imagecells*)

(defun Set-imageid ()
  "load image data and set id"
  (setf *images* (sdl:load-image *path-samplecg* :color-key(sdl:color :r 0 :g 0 :b 0))) 
  (let* ((temp1cells (append (loop for x from 0 to 64 by 32      ;id 0 - 2
                       collect (list x 0 32 32))))
         (temp2cells (loop for y from 0 to 16 by 16              ;id 3 - 6
                       append (loop for x from 96 to 112 by 16
                         collect (list x y 16 16))))      
         (temp3cells (append (loop for x from 128 to 224 by 32   ;id 7 -10
                       collect (list x 0 32 32))))
         (temp4cells (append (loop for x from 0 to 224 by 32     ;id 11-18
                       collect (list x 32 32 32))))
         (temp5cells (append (loop for x from 0 to 224 by 32     ;id 19-26
                       collect (list x 64 32 64))))
         (temp6cells (loop for y from 128 to 384 by 64           ;id 27-46
                       append (loop for x from 0 to 192 by 64
                         collect (list x y 64 64))))
         (temp7cells (append (loop for x from 0 to 128 by 64     ;id 47-49
                       collect (list x 448 64 32))))
         (temp8cells (append (loop for x from 0 to 96 by 32      ;id 50-53
                       collect (list x 480 32 32 ))))
         (temp9cells (loop for y from 0 to 192 by 64             ;id 54-69
                       append (loop for x from 256 to 448 by 64
                         collect (list x y 64 64))))
         (temp10cells (loop for y from 256 to 384 by 96          ;id 70-75
                        append (loop for x from 256 to 448 by 96
                          collect (list x y 96 96))))
         (temp11cells (append (loop for x from 192 to 384 by 64  ;id 76-79 
                        collect (list x 448 64 64))))
         (temp12cells (loop for y from 448 to 480 by 32          ;id 80-85
                        append (loop for x from 448 to 512 by 32
                          collect (list x y 32 32)))))
         (setf *imagecells* (append temp1cells temp2cells temp3cells temp4cells
                                    temp5cells temp6cells temp7cells temp8cells
                                    temp9cells temp10cells temp11cells temp12cells)))
  (setf (sdl:cells *images*) *imagecells*))
