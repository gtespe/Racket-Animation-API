;;By Grant Espe

(require "world-cs1102.rkt")

;;a graphic object is (make-gobj string shape posn)
;; the name field is to be used as an identifier

(define-struct gobj (name shape pos))

;; a shape is either
;; - a circle (make-circ string number)
;; - or a rectangle (make-rect string number number)

(define-struct circ (color rad))
(define-struct rect (color w h))

;; a cmd is either
;; - (make-addcmd gobj)
;; - (make-removecmd string)
;; - (make-slidecmd string number number)
;; - (make-slideuntilcmd string number number string)
;; - (make-jumpcmd string posn)
;; - (make-jumprandcmd string number number)
;; - (make-jumpranduntilcmd string string)

(define-struct addcmd (gobj))
(define-struct removecmd (name))
(define-struct slidecmd (name xvel yvel))
(define-struct slideuntilcmd (name1 xvel yvel name2))
(define-struct jumpcmd (name pos))
(define-struct jumprandcmd (name))
(define-struct jumpranduntilcmd (name1 name2))

;; an animation is (make-animation list[cmd])
;; the interpreter will add the graphic objects to a global variable, the canvas,
;;; using the set! command in order to access them later with other commands
(define-struct animation (cmdlist))

(define globalcanvas empty)

(define anim1 (make-animation
               (list (make-addcmd (make-gobj "rcirc" (make-circ "red" 5) (make-posn 10 20)))
                     (make-addcmd (make-gobj "brect" (make-rect "blue" 10 50) (make-posn 85 40)))
                     (make-slideuntilcmd "rcirc" 3 1 "brect")
                     (make-removecmd "brect")
                     (make-slideuntilcmd "rcirc" -3 1 'left)
                     )))

(define anim2 (make-animation
               (list (make-addcmd (make-gobj "pcirc" (make-circ "purple" 10) (make-posn 80 50)))
                     (make-jumpranduntilcmd "pcirc" 'top))))

(define anim3 (make-animation
               (list
                (make-addcmd (make-gobj "ocirc" (make-circ "orange" 7) (make-posn 30 20)))
                (make-addcmd (make-gobj "grect" (make-rect "green" 80 10) (make-posn 50 70)))
                (make-slideuntilcmd "ocirc" 0 2 "grect")
                (make-addcmd (make-gobj "rrect" (make-rect "red" 10 50) (make-posn 70 40)))
                (make-slideuntilcmd "ocirc" 2 0 "rrect")
                (make-jumprandcmd "ocirc"))))


(define anim4 (make-animation
                (list
                 (make-addcmd (make-gobj "bob" (make-circ "red" 10) (make-posn 15 15)))
                 (make-slidecmd "bob" 2 1)
                 (make-slidecmd "bob" 2 2)
                 (make-slidecmd "bob" 2 3)
                 (make-slidecmd "bob" 2 4)
                 (make-slidecmd "bob" 2 5)
                 (make-slidecmd "bob" 2 6)
                 (make-slidecmd "bob" 2 7)
                 (make-slidecmd "bob" 2 8)
                 (make-slidecmd "bob" 2 9)
                 (make-slidecmd "bob" 2 10)
                 (make-jumpranduntilcmd "bob" 'any)
                 (make-removecmd "bob"))))

(define anim5 (make-animation
                (list
                 (make-addcmd (make-gobj "bob" (make-circ "red" 10) (make-posn 15 15)))
                 (make-jumpranduntilcmd "bob" 'any)
                 (make-removecmd "bob")
                 (make-addcmd (make-gobj "john" (make-circ "yellow" 10) (make-posn 15 15)))
                 (make-jumpranduntilcmd "john" 'any)
                 (make-removecmd "john")
                 (make-addcmd (make-gobj "jame" (make-circ "blue" 10) (make-posn 15 15)))
                 (make-jumpranduntilcmd "jame" 'any)
                 (make-removecmd "jame")
                 (make-addcmd (make-gobj "tom" (make-circ "green" 10) (make-posn 15 15)))
                 (make-jumpranduntilcmd "tom" 'any)
                 (make-removecmd "tom"))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Interpreter;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;global variable used to store the list of slides
(define renders empty)

;;run-animation: animation -> void
;;displays an animation

(define (run-animation aanim)
  (set! globalcanvas empty)
  (set! renders empty)
  (run-cmdlist (animation-cmdlist aanim))
  (set! renders (reverse renders))
  (run-renders renders))

;;run-cmdlist list[cmd] -> void
;; runs a list of cmds, returns void

(define (run-cmdlist cmds)
  (cond [(empty? cmds) (printf "End render")]
        [(cons? cmds) (run-cmd (first cmds)) (run-cmdlist (rest cmds))]))

;;run-cmd: cmd -> void
;;takes and runs a cmd, returns void]

(define (run-cmd a-cmd)
  (cond [(addcmd? a-cmd) (set! globalcanvas (cons (addcmd-gobj a-cmd) globalcanvas))
                         (add-render (gen-scene globalcanvas))]
        [(removecmd? a-cmd)
         (set! globalcanvas (filter (lambda (a-gobj) (not (string=? (removecmd-name a-cmd) (gobj-name a-gobj)))) globalcanvas))
         (add-render (gen-scene globalcanvas))]
        [(slidecmd? a-cmd)
         (set! globalcanvas (map (lambda (a-gobj)
                                   (cond [(string=? (slidecmd-name a-cmd) (gobj-name a-gobj))
                                          (make-gobj (gobj-name a-gobj)
                                                     (gobj-shape a-gobj)
                                                     (make-posn (+ (posn-x (gobj-pos a-gobj)) (slidecmd-xvel a-cmd))
                                                                (+ (posn-y (gobj-pos a-gobj)) (slidecmd-yvel a-cmd))))]
                                         [else a-gobj]))
                                 globalcanvas))
         (add-render (gen-scene globalcanvas))]
        
        [(slideuntilcmd? a-cmd) (run-slideuntilcmd a-cmd)]
        
        [(jumprandcmd? a-cmd)
         (set! globalcanvas (map (lambda (a-gobj)
                                   (cond [(string=? (jumprandcmd-name a-cmd) (gobj-name a-gobj))
                                          (cond [(circ? (gobj-shape a-gobj))
                                                     (make-gobj (gobj-name a-gobj)
                                                     (gobj-shape a-gobj)
                                                     (make-posn (+ (/ (circ-rad (gobj-shape a-gobj)) 1.5)
                                                                   (* (- WIDTH (circ-rad (gobj-shape a-gobj))) (random)))
                                                                (+ (/ (circ-rad (gobj-shape a-gobj)) 1.5)
                                                                   (* (- HEIGHT (circ-rad (gobj-shape a-gobj))) (random)))))]
                                                [(rect? (gobj-shape a-gobj))
                                                     (make-gobj (gobj-name a-gobj)
                                                     (gobj-shape a-gobj)
                                                     (make-posn (+ (/ (rect-w (gobj-shape a-gobj)) 3)
                                                                   (* (- WIDTH (/ (rect-w (gobj-shape a-gobj)) 2)) (random))
                                                                (+ (/ (rect-h (gobj-shape a-gobj)) 3)
                                                                   (* (- HEIGHT (/ (rect-h (gobj-shape a-gobj)) 2)) (random))))))])]
                                         [else a-gobj]))
                                 globalcanvas))
         (add-render (gen-scene globalcanvas))]
        [(jumpranduntilcmd? a-cmd) (run-jumpranduntilcmd a-cmd)]))

;;run-slideuntilcmd: cmd -> void
;; a helper method that runs the slideuntilcmd cmd type
(define (run-slideuntilcmd a-cmd)
  (cond [(symbol? (slideuntilcmd-name2 a-cmd))
                (cond [(not (overlaps? (first (filter (lambda (a-gobj) (string=? (slideuntilcmd-name1 a-cmd) (gobj-name a-gobj))) globalcanvas))
                                       (slideuntilcmd-name2 a-cmd)))
                       (run-cmd (make-slidecmd (slideuntilcmd-name1 a-cmd) (slideuntilcmd-xvel a-cmd) (slideuntilcmd-yvel a-cmd)))
                       (add-render (gen-scene globalcanvas))
                       (run-cmd a-cmd)])]
               [(not (overlaps? (first (filter (lambda (a-gobj) (string=? (slideuntilcmd-name1 a-cmd) (gobj-name a-gobj))) globalcanvas))
                                (first (filter (lambda (a-gobj) (string=? (slideuntilcmd-name2 a-cmd) (gobj-name a-gobj))) globalcanvas))))
                (run-cmd (make-slidecmd (slideuntilcmd-name1 a-cmd) (slideuntilcmd-xvel a-cmd) (slideuntilcmd-yvel a-cmd)))
                (add-render (gen-scene globalcanvas))
                (run-cmd a-cmd)]))

;;run-jumpranduntilcmd: cmd -> void
;; runs the jumpranduntilcmd until its gobj reaches its target, updating the canvas each time
(define (run-jumpranduntilcmd a-cmd)
  (cond [(symbol? (jumpranduntilcmd-name2 a-cmd))
         (cond [(not (overlaps? (first (filter (lambda (a-gobj) (string=? (jumpranduntilcmd-name1 a-cmd) (gobj-name a-gobj))) globalcanvas))
                                (jumpranduntilcmd-name2 a-cmd)))
                (run-cmd (make-jumprandcmd (jumpranduntilcmd-name1 a-cmd)))
                (add-render (gen-scene globalcanvas))
                (run-cmd a-cmd)])]
        [(not (overlaps? (first (filter (lambda (a-gobj) (string=? jumpranduntilcmd-name1 a-cmd) (gobj-name a-gobj))) globalcanvas)
                         (first (filter (lambda (a-gobj) (string=? (jumpranduntilcmd-name2 a-cmd) (gobj-name a-gobj))) globalcanvas))))
         (run-cmd (make-jumprandcmd (jumpranduntilcmd-name a-cmd)))
         (add-render (gen-scene globalcanvas))
         (run-cmd a-cmd)]))
;;overlaps?: gobj gobj -> boolean
;; determines whether or not two objects overlap or touch
(define (overlaps? gobj1 target)
  (cond
    [(and (circ? (gobj-shape gobj1)) (symbol? target))
     (overlaps? (make-gobj (gobj-name gobj1)
                           (make-rect (circ-color (gobj-shape gobj1))
                                      (* 2 (circ-rad (gobj-shape gobj1)))
                                      (* 2 (circ-rad (gobj-shape gobj1))))
                           (make-posn (posn-x (gobj-pos gobj1))
                                      (posn-y (gobj-pos gobj1)))) target)]
    
    [(and (rect? (gobj-shape gobj1)) (symbol? target))
     (let [(r1 (+ (/ (rect-w (gobj-shape gobj1)) 2) (posn-x (gobj-pos gobj1))))
           (l1 (- (posn-x (gobj-pos gobj1)) (/ (rect-w (gobj-shape gobj1)) 2)))
           (b1 (+ (/ (rect-h (gobj-shape gobj1)) 2) (posn-y (gobj-pos gobj1))))
           (t1 (- (posn-y (gobj-pos gobj1)) (/ (rect-h (gobj-shape gobj1)) 2)))]
       (cond [(symbol=? 'left target) (<= l1 1)]
             [(symbol=? 'right target)(>= r1 (- WIDTH 1))]
             [(symbol=? 'top target)(<= t1 1)]
             [(symbol=? 'bottom target)(>= b1 (- HEIGHT 1))]
             [else (or (>= r1 (- WIDTH 1)) (<= l1 1) (<= t1 1) (>= b1 (- HEIGHT 1)))]))]
    
    [(circ? (gobj-shape gobj1)) (overlaps? (make-gobj (gobj-name gobj1)
                                                      (make-rect (circ-color (gobj-shape gobj1))
                                                                 (* 2 (circ-rad (gobj-shape gobj1)))
                                                                 (* 2 (circ-rad (gobj-shape gobj1))))
                                                      (make-posn (posn-x (gobj-pos gobj1))
                                                                 (posn-y (gobj-pos gobj1)))) target)]
    [(circ? (gobj-shape target)) (overlaps? gobj1 (make-gobj (gobj-name target)
                                                            (make-rect (circ-color (gobj-shape target))
                                                                       (* 2 (circ-rad (gobj-shape target)))
                                                                       (* 2 (circ-rad (gobj-shape target))))
                                                            (make-posn (posn-x (gobj-pos target))
                                                                       (posn-y (gobj-pos target)))) gobj1)]
    [(and (rect? (gobj-shape gobj1)) (rect? (gobj-shape target)))
     (let [(r1 (+ (/ (rect-w (gobj-shape gobj1)) 2) (posn-x (gobj-pos gobj1))))
           (l1 (- (posn-x (gobj-pos gobj1)) (/ (rect-w (gobj-shape gobj1)) 2)))
           (b1 (+ (/ (rect-h (gobj-shape gobj1)) 2) (posn-y (gobj-pos gobj1))))
           (t1 (- (posn-y (gobj-pos gobj1)) (/ (rect-h (gobj-shape gobj1)) 2)))
           (r2 (+ (/ (rect-w (gobj-shape target)) 2) (posn-x (gobj-pos target))))
           (l2 (- (posn-x (gobj-pos target)) (/ (rect-w (gobj-shape target)) 2)))
           (b2 (+ (/ (rect-h (gobj-shape target)) 2) (posn-y (gobj-pos target))))
           (t2 (- (posn-y (gobj-pos target)) (/ (rect-h (gobj-shape target)) 2)))]
       (or (and (>= r1 l2) (<= r1 r2) (<= b1 t2) (>= b1 b2))
           (and (>= r1 l2) (<= r1 r2) (<= t1 b2) (>= t1 t2))
           (and (>= l1 l2) (<= l1 r2) (>= b1 t2) (<= b1 b2))
           (and (>= l1 l2) (<= l1 r2) (>= t1 t2) (<= t1 b2))))]))





(define WIDTH 120)
(define HEIGHT 90)

;;gen-scene list[gobj]->scene
;; interprets a list of gobjs, turning it into a scene
(define (gen-scene canv)
  (cond [(empty? canv) (empty-scene WIDTH HEIGHT)]
        [(cons? canv) (cond [(circ? (gobj-shape (first canv)))
                             (place-image (circle
                                           (circ-rad (gobj-shape (first canv)))
                                           "solid" (circ-color (gobj-shape (first canv))))
                                          (posn-x (gobj-pos (first canv)))
                                          (posn-y (gobj-pos (first canv)))
                                          (gen-scene (rest canv)))]
                            [(rect? (gobj-shape (first canv)))
                             (place-image (rectangle
                                           (rect-w (gobj-shape (first canv)))
                                           (rect-h (gobj-shape (first canv)))
                                           "solid" (rect-color (gobj-shape (first canv))))
                                          (posn-x (gobj-pos (first canv)))
                                          (posn-y (gobj-pos (first canv)))
                                          (gen-scene (rest canv)))])]))

;;add-render: scene -> void
;; adds a scene to the global variable renders
(define (add-render a-scene)
  (set! renders (cons a-scene renders)))

;;run-renders: list[scene] -> void
;; recursively displays a list of scenes
(define (run-renders a-los)
  (begin
    (sleep/yield 0.05)
    (cond [(empty? a-los) (printf "~nEnd Display")]
          [(cons? a-los) (update-frame (first a-los)) (run-renders (rest a-los))])))



(big-bang WIDTH HEIGHT (/ 1 28) true)



