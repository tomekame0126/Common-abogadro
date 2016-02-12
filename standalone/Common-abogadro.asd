;;;; common-abogadro.asd
(asdf:defsystem :common-abogadro
  :version "0.1"
  :description "A remake of abogadro made with HSP"
  :author "tomekame0126"
  :license "MIT"
  :depends-on (:lispbuilder-sdl
               :lispbuilder-sdl-ttf
               :lispbuilder-sdl-mixer)
  :serial t
  :components ((:file "sprite-sheets")
	       (:file "map-list")
	       (:file "enemy-map-list")
	       (:file "audio-list")
	       (:file "move-pattern")
	       (:file "shot-data")
	       (:file "package")
	       (:file "common-abogadro")))
