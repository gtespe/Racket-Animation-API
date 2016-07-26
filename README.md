# Racket-Animation-API

A simple animation api for racket,

  To create an animation, use the 'make-animation' macro
  
  the make-animation macro takes a list of any of the following commands:

      (define-struct addcmd (gobj))
      (define-struct removecmd (name))
      (define-struct slidecmd (name xvel yvel))
      (define-struct slideuntilcmd (name1 xvel yvel name2))
      (define-struct jumpcmd (name pos))
      (define-struct jumprandcmd (name))
      (define-struct jumpranduntilcmd (name1 name2))


   You will have to include some graphic objects which are created with the following struct:

      (define-struct gobj (name shape pos))

  A few example animations are included within the .rkt file, simply run and execute any of the following commands:

    (run-animation anim1)
    (run-animation anim2)
    (run-animation anim3)
    (run-animation anim4)
    (run-animation anim5)
    

