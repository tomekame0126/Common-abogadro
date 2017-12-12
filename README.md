# Common-abogadro
[Lisp Game Programming 2][5]
 
This program is remade with [CommonLisp] [2] + [Lispbuilder-sdl] [3].

Original was made with [HSP][1]. 

## Environment ##

> *Windows 10*

> *Emacs 24.2*

> *Sbcl 1.3.1*

> *Slime 2.15*

> *Lispbuilder-sdl + ttf + mixer*

## How to use ##

> *From step1~13 , enemy-test or standalone holder, move target program to c:\work.*

> *Start the slime from Emacs, and then run the program(step1 ~ 13, enemy-test, Common-abogadro).*

> *Note) Do not forget the path settings to c:\work , etc. !*


## Caution ##

> There is a Japanese message in the program [step4 later], but can not display the Japanese message in lispbuilder-sdl-ttf.
> Therefore, using lispbuilder-sdl-ttf modified as follows.

> Holder

>lispbuilder-sdl-ttf -> sdl-ttf

> Change

> 1.string-blended.lisp: render-text-blended -> render-utf8-blended

> 2.string-shaded.lisp: render-text-shaded -> render-utf8-shaded

> 3.string-solid.lisp: render-text-solid -> render-utf8-solid


## Images ##
![abogadro][6]

## Acknowledgments ##

> *[Takato toki][4] : the original author.*

[1]: http://mclass13.web.fc2.com/hsplecture/index.htm
[2]: http://www.sbcl.org/
[3]: https://github.com/lispbuilder/lispbuilder
[4]: http://mclass13.web.fc2.com/index.htm
[5]: http://tomekame0126.hatenablog.com/entry/2015/05/24/182132
[6]: https://github.com/tomekame0126/Common-abogadro/blob/master/Common-abogadro.png

by Google Translate
