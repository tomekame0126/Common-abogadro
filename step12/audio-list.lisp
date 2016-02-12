(defpackage :audio-list
  (:use :common-lisp)
  (:export #:Open-sound
           #:Stop-sound
           #:Close-sound
           #:Play-music
	   #:Play-music-once
           #:Play-sample
           #:*explodes*
           #:*explodem*
           #:*damage*
           #:*crushed*
           #:*itemget*
           #:*oneup*
           #:*bomb*
           #:*shot*
           #:*samplebgm1*
           #:*samplebgm2*
           #:*samplebgm3*
           #:*bossbgm*
           #:*endbgm*))

(in-package :audio-list)

;; step2 <Audio>
;; -----------------------------------------------------------------------------------------------
(defparameter *path-explodes-sound*   "C:\\work\\sound\\explodes.wav")
(defparameter *path-explodem-sound*   "C:\\work\\sound\\explodem.wav")
(defparameter *path-damage-sound*     "C:\\work\\sound\\damage.wav")
(defparameter *path-crushed-sound*    "C:\\work\\sound\\crushed.wav")
(defparameter *path-itemget-sound*    "C:\\work\\sound\\itemget.wav")
(defparameter *path-oneup-sound*      "C:\\work\\sound\\oneup.wav")
(defparameter *path-bomb-sound*       "C:\\work\\sound\\bomb.wav")
(defparameter *path-shot-sound*       "C:\\work\\sound\\shot.wav")

(defparameter *path-samplebgm1*       "C:\\work\\sound\\samplebgm1.ogg")
(defparameter *path-samplebgm2*       "C:\\work\\sound\\samplebgm2.ogg")
(defparameter *path-samplebgm3*       "C:\\work\\sound\\samplebgm3.ogg")
(defparameter *path-bossbgm*          "C:\\work\\sound\\bossbgm.ogg")
(defparameter *path-endbgm*           "C:\\work\\sound\\endbgm.ogg")

(defvar *explodes*)                   ; explodes sound
(defvar *explodem*)                   ; explodem sound
(defvar *damage*)                     ; damage sound
(defvar *crushed*)                    ; crushed sound
(defvar *itemget*)                    ; itemget sound
(defvar *oneup*)                      ; oneup sound
(defvar *bomb*)                       ; bomb sound
(defvar *shot*)                       ; shot sound
(defvar *samplebgm1*)                 ; BGM1  
(defvar *samplebgm2*)                 ; BGM2
(defvar *samplebgm3*)                 ; BGM3
(defvar *bossbgm*)                    ; boss BGM 
(defvar *endbgm*)                     ; ending BGM

(defun Open-sound ()
  "load sound data and set"
  (sdl-mixer:open-audio :chunksize 1024 :channels 2)
  (sdl-mixer:allocate-channels 16)
  (setf *explodes*   (sdl-mixer:load-sample *path-explodes-sound*)
        *explodem*   (sdl-mixer:load-sample *path-explodem-sound*)
        *damage*     (sdl-mixer:load-sample *path-damage-sound*)
        *crushed*    (sdl-mixer:load-sample *path-crushed-sound*)
        *itemget*    (sdl-mixer:load-sample *path-itemget-sound*)
        *oneup*      (sdl-mixer:load-sample *path-oneup-sound*)
        *bomb*       (sdl-mixer:load-sample *path-bomb-sound*)
        *shot*       (sdl-mixer:load-sample *path-shot-sound*)
        *samplebgm1*  (sdl-mixer:load-music  *path-samplebgm1*)
        *samplebgm2*  (sdl-mixer:load-music  *path-samplebgm2*)
        *samplebgm3*  (sdl-mixer:load-music  *path-samplebgm3*)
        *bossbgm*     (sdl-mixer:load-music  *path-bossbgm*)
	*endbgm*      (sdl-mixer:load-music  *path-endbgm*)))
 
(defun Stop-sound ()
  "sound stop"
  (when (sdl-mixer:music-playing-p)
        (sdl-mixer:halt-music))         ; BGM stop
  (when (sdl-mixer:sample-playing-p nil)
        (sdl-mixer:halt-sample)))       ; Shot,Bomb,etc soud stop  

(defun Close-sound ()
  "close sound file" 
 (sdl-mixer:free *explodes*)
 (sdl-mixer:free *explodem*)
 (sdl-mixer:free *damage*)
 (sdl-mixer:free *crushed*)
 (sdl-mixer:free *itemget*)
 (sdl-mixer:free *oneup*)
 (sdl-mixer:free *bomb*)
 (sdl-mixer:free *shot*)
 (sdl-mixer:free *samplebgm1*)
 (sdl-mixer:free *samplebgm2*)
 (sdl-mixer:free *samplebgm3*)
 (sdl-mixer:free *bossbgm*)
 (sdl-mixer:free *endbgm*)
 (sdl-mixer:close-audio))

(defun Play-music-once (music)
  "play music once"
  (sdl-mixer:play-music music))  ; BGM play once

(defun Play-music (music)
  "play music loop"
  (sdl-mixer:play-music music :loop t :position 0))  ; BGM play loop

(defun Play-sample (sample)
  "play sample"
  (sdl-mixer:play-sample sample))                     ; shot sound etc    
