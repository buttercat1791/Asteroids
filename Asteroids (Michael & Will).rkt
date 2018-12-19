#lang racket/gui

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GLOBAL CONSTANT DEFINITIONS ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define CANVAS (freeze (scale .27 (make-object image-snip% "Images/Canvas.jpeg" 'jpeg)))) ;.35
(define CANVAS-WIDTH (image-width CANVAS))
(define CANVAS-HEIGHT (image-height CANVAS))
(define SHIP-IMAGE (freeze (scale (/ CANVAS-WIDTH 14940) (bitmap "Images\\FastestInTheFleet.png"))))
(define AST1-IMAGE (freeze (scale (/ CANVAS-WIDTH 37350) (bitmap "Images\\Ast1.png"))))
(define AST2-IMAGE (freeze (scale (/ CANVAS-WIDTH 27666.67) (bitmap "Images\\Ast2.png"))))
(define AST3-IMAGE (freeze (scale (/ CANVAS-WIDTH 10521) (bitmap "Images\\Ast3.png"))))
(define AST4-IMAGE (freeze (scale (/ CANVAS-WIDTH 11149) (bitmap "Images\\Ast4.png"))))
(define AST5-IMAGE (freeze (scale (/ CANVAS-WIDTH 12450) (bitmap "Images\\Ast5.png"))))
(define EXPLOSION (scale (/ CANVAS-WIDTH 4980) (bitmap "Images\\Boom.png")))
(define FLASH (scale (/ CANVAS-WIDTH 1867.5) (bitmap "Images\\FLASH.png")))
(define loi (list AST1-IMAGE AST2-IMAGE AST3-IMAGE AST4-IMAGE AST5-IMAGE))

;Ship constants
(define SHIP-MASS (/ CANVAS-WIDTH 14.94))
(define START-HEALTH SHIP-MASS)
(define M (/ CANVAS-WIDTH 996)) ;Magnitude for acceleration vector
(define BOUNCE-NUM 400) ;Determines how "bouncy" the ship is when hit by an asteroid, smaller number = more bouncy
(define DAMAGE-MITIGATION 35) ;Larger number makes asteroid impacts hurt less
(define ROTATION 15) ;How many degrees you rotate when hitting left or right

;Laser constants
(define L-SPEED (/ CANVAS-WIDTH 59.76))  ;Laser velocity
(define L-LENGTH (/ CANVAS-WIDTH 99.6)) ;Length of laser line
(define L-POWER (/ CANVAS-WIDTH 19.92))  ;Damage laser inflicts on a hit

;Asteroid constants
(define AST-MASS1 (/ CANVAS-WIDTH 14.94))
(define AST-MASS2 (/ CANVAS-WIDTH 7.74))
(define AST-MASS3 (/ CANVAS-WIDTH 4.94))
(define AST-MASS4 (/ CANVAS-WIDTH 3.735))
(define AST-MASS5 (/ CANVAS-WIDTH 2.988))

;HUD constants
(define RAD-SCALE 1.5)




;;;;;;;;;;;;;;;;;;;;;;;;;
; STRUCTURE DEFINITIONS ;
;;;;;;;;;;;;;;;;;;;;;;;;;

;ship is a ship structure
;loast is a list of asteroid structures
;lolas is a list of laser structures
(define-struct WS (ship
                   loast
                   lolas))


;x and y are the location of the ship
;magnitude and angle are velocity
;health (0-100) is health of the ship, health = 0 = dead
(define-struct ship (x
                     y
                     angle
                     dx
                     dy 
                     health
                     mass
                     flashtime))


;x and y are the location of the asteroid
;magnitude and angle are velocity
;health (0-100), health = 0 = asteroid destroyed
;mass is mass, used in collisions
(define-struct ast (x
                    y
                    mag
                    angle
                    health
                    mass
                    rspeed
                    rotation))

;x and y are front-point of laser
;angle is from ship at time of firing, used for updating x and y
;power is...*not* over 9000
(define-struct laser (x
                      y
                      angle
                      power))



(define TEST-SHIP (make-ship
                   500
                   500
                   90
                   0
                   0
                   100
                   150
                   0))


(define TEST-LASER (make-laser 100 100 90 10))
(define TEST-AST   (make-ast 109 100 10 75 100 100 10 10))
(define TEST-AST2  (make-ast 500 500 10 80 100 250 10 10))
(define TEST-AST3  (make-ast 101 101 10 90 100 50 10 10))
(define TEST-AST4  (make-ast 103 103 10 170 100 150 10 10))
(define TEST-WS (make-WS TEST-SHIP
                           (list TEST-AST TEST-AST2)
                           (list TEST-LASER)))




;;;;;;;;;;;;;;;;;;
; SHIP FUNCTIONS ;
;;;;;;;;;;;;;;;;;;

;Ship -> Ship
;Adds new velocity vector to dx and dy
(define (change-vel ship)
  (local [(define ddx (* M (cos (degrees->radians (ship-angle ship)))))
          (define ddy (* M (sin (degrees->radians (ship-angle ship)))))]
    (make-ship (ship-x ship)
               (ship-y ship)
               (ship-angle ship)
               (+ (ship-dx ship) ddx)
               (- (ship-dy ship) ddy)
               (ship-health ship)
               (ship-mass ship)
               (ship-flashtime ship))))
;(check-equal? (ship-dy (change-vel TEST-SHIP)) -1.5)
;(check-equal? (ship-dx (change-vel TEST-SHIP)) 0)
;The second check-equal is pretty much 0, but apparently
;9.184850993605148e-17 isn't close enough for Racket, so
;we can't give this little guy a check-equal
;But we're definitely not bitter at all

;Ship -> Ship
;Updates ship's location
(define (update-ship ship)
  (local [(define new-x (+ (ship-x ship)
                           (ship-dx ship)))
          (define new-y (+ (ship-y ship)
                           (ship-dy ship)))]
    (make-ship
     (if (> new-x CANVAS-WIDTH)
         (- new-x CANVAS-WIDTH)
         (if (< new-x 0)
             (+ new-x CANVAS-WIDTH)
             new-x))
     (if (> new-y CANVAS-HEIGHT)
         (- new-y CANVAS-HEIGHT)
         (if (< new-y 0)
             (+ new-y CANVAS-HEIGHT)
             new-y))
     (ship-angle ship)
     (ship-dx ship)
     (ship-dy ship)
     (ship-health ship)
     (ship-mass ship)
     (if (> (ship-flashtime ship) 0)
         (- (ship-flashtime ship) 1)
         (ship-flashtime ship))))) ;Here we can see a herd of wild parentheses in their natural habitat.
                                   ;They stand close together to hide their number and confuse programmers.
(check-equal? (ship-x (update-ship TEST-SHIP)) (ship-x TEST-SHIP))
(check-equal? (ship-y (update-ship TEST-SHIP)) (ship-y TEST-SHIP))


;Ship, Number -> Ship
;Changes angle of ship
(define (change-angle ship n)
  (local [(define new-angle (+ n (ship-angle ship)))]
    (make-ship (ship-x ship)
               (ship-y ship)
               (cond
                 [(> new-angle 360) (- new-angle 360)]
                 [(< new-angle 0) (+ new-angle 360)]
                 [else new-angle]) ;Keep the angle within the range 0-360
               (ship-dx ship)
               (ship-dy ship)
               (ship-health ship)
               (ship-mass ship)
               (ship-flashtime ship))))
;Test Ship and check-expects for change-angle function
(define angle-test-ship (make-ship 0 0 180 0 0 100 50 0))
(check-eq? (ship-angle (change-angle angle-test-ship 225))
           (ship-angle (make-ship 0 0 45 0 0 100 50 0))
           "")
(check-eq? (ship-angle (change-angle angle-test-ship -225))
           (ship-angle (make-ship 0 0 315 0 0 100 50 0))
           "")
;That feeling when you realize you already have unit tests for a function,
;and don't have to try to figure out what was going through your sleep-deprived brain
;when you wrote the function so you can make unit tests for it.

;Number, Number, Number, Number, Number -> Boolean
;Returns true if distance between (x,y) and (h,k) is less than radius
(define (under-distance? x y h k radius)
  (<= (+ (sqr (- x h)) (sqr (- y k))) (sqr radius)))
(check-eq? (under-distance? 100 100 200 200 300) #t)
(check-eq? (under-distance? 100 100 600 600 300) #f)


;List of Asteroids, Ship -> Ship
;Checks ship for asteroid collisions
;If no collisions = nothing happens to ship
;If ship is hit = new random angle, velocity changed, health lowered
(define (collide-ship loast ship)
  (foldl (λ (ast ship)
           (if (under-distance? (ship-x ship) (ship-y ship) (ast-x ast) (ast-y ast) (+ (/ (ast-mass ast) 10)
                                                                                       (+ (/ (image-width SHIP-IMAGE) 2) (/ (image-width SHIP-IMAGE) 16))))
               (make-ship (ship-x ship)
                          (ship-y ship)
                          (random 361)
                          (/ (* (ship-mass ship) (+ (ship-dx ship) (* (ast-mag ast) (cos (degrees->radians (ast-angle ast)))))) BOUNCE-NUM)
                          (/ (* (ship-mass ship) (+ (ship-dy ship) (* (ast-mag ast) (sin (degrees->radians (ast-angle ast)))))) BOUNCE-NUM)
                          (- (ship-health ship) (/ (ast-mass ast) DAMAGE-MITIGATION))
                          (ship-mass ship)
                          (ship-flashtime ship))
               ship))
           ship loast))
(check-equal? (ship-dx (collide-ship (list TEST-AST2) TEST-SHIP))  (/ (* 150 (* 10 (cos (degrees->radians 80))))  BOUNCE-NUM))
(check-equal? (ship-dy (collide-ship (list TEST-AST2) TEST-SHIP)) (/ (* 150 (* 10 (sin (degrees->radians 80)))) BOUNCE-NUM))
(check-equal? (ship-dy (collide-ship (list TEST-AST4) TEST-SHIP)) 0)




;;;;;;;;;;;;;;;;;;;
; LASER FUNCTIONS ;
;;;;;;;;;;;;;;;;;;;

;Laser, Image -> Image
;Draws lasers
(define (place-laser laser img)
  (add-line img
            (laser-x laser)
            (laser-y laser)
            (- (laser-x laser) (* L-LENGTH (cos (degrees->radians (laser-angle laser)))))
            (+ (laser-y laser) (* L-LENGTH (sin (degrees->radians (laser-angle laser)))))
            'red))




;List of Lasers -> List of Lasers
;Moves lasers
(define (update-lasers lolas)
  (filter (λ (laser) (and (<= (laser-x laser) CANVAS-WIDTH) ;checks to see if lasers are on screen, keeps them if they are
                          (>= (laser-x laser) 0)
                          (<= (laser-y laser) CANVAS-HEIGHT)
                          (>= (laser-y laser) 0)))
          (map (λ (laser) (make-laser
                           (+ (laser-x laser) (* (cos (degrees->radians (laser-angle laser))) L-SPEED)) ;x-coordinates
                           (- (laser-y laser) (* (sin (degrees->radians (laser-angle laser))) L-SPEED)) ; y-coordinates  
                           (laser-angle laser)
                           (laser-power laser))) lolas)))
(check-equal? (update-lasers (list (make-laser 5000 5000 90 10))) empty)
(check-equal? (laser-x (first (update-lasers (list TEST-LASER)))) 100.0)
;(check-equal? (laser-y (first (update-lasers (list TEST-LASER)))) 75.0)


;Laser, List of Asteroids -> Number
;Returns 1+ if laser hits an asteroid, 0 if not
(define (laser-hit? laser loast)
  (apply + (map (λ (ast) (if (under-distance? (laser-x laser) (laser-y laser) (ast-x ast) (ast-y ast) (/ (ast-mass ast) 10))
                             1 0))
         loast)))
(check-equal? (laser-hit? TEST-LASER (list TEST-AST)) 1)
(check-equal? (laser-hit? TEST-LASER (list TEST-AST2)) 0)
(check-equal? (laser-hit? TEST-LASER (list TEST-AST TEST-AST2 TEST-AST3 TEST-AST4)) 3)

;List of Lasers, List of Asteroids -> List of Lasers
;Removes lasers after they hit asteroids
(define (remove-lasers lolas loast)
  (filter (λ (laser) (< (laser-hit? laser loast) 1)) lolas))
(check-equal? (remove-lasers (list TEST-LASER) (list TEST-AST3)) empty)
(check-equal? (remove-lasers (list TEST-LASER) (list TEST-AST TEST-AST2)) empty)
(check-equal? (remove-lasers (list TEST-LASER) (list TEST-AST2)) (list TEST-LASER))




;;;;;;;;;;;;;;;;;;;;;;
; ASTEROID FUNCTIONS ;
;;;;;;;;;;;;;;;;;;;;;;

;List of Asteroids, Ship -> List of Asteroids
;Damages asteroids if they hit the ship
(define (collide-ast loast ship)
  (map (λ (ast) (if (under-distance? (ast-x ast) (ast-y ast) (ship-x ship) (ship-y ship) (+ (/ (ast-mass ast) 10)
                                                                                            (+ (/ (image-width SHIP-IMAGE) 2) (/ (image-width SHIP-IMAGE) 16))))
                    (make-ast (ast-x ast) (ast-y ast) (ast-mag ast) (- (ast-angle ast) 180)
                              (- (ast-health ast) 101) (ast-mass ast) (ast-rspeed ast) (ast-rotation ast))
                    ast)) loast))
(check-equal? (ast-health (first (collide-ast (list TEST-AST2) TEST-SHIP))) -1)
(check-equal? (ast-health (first (collide-ast (list TEST-AST3) TEST-SHIP))) 100)

;Asteroid, Number -> Asteroid
;Subtracts n from asteroid's health, used in laser-asteroid group
(define (hit-ast ast n)
  (make-ast (ast-x ast)
            (ast-y ast)
            (ast-mag ast)
            (ast-angle ast)
            (- (ast-health ast) n)
            (ast-mass ast)
            (ast-rspeed ast)
            (ast-rotation ast)))
(check-equal? (ast-health (hit-ast TEST-AST L-POWER)) (- (ast-health TEST-AST) L-POWER))
(check-equal? (ast-health (hit-ast TEST-AST2 L-POWER)) (- (ast-health TEST-AST2) L-POWER))

;List of Lasers, Asteroid -> Asteroid
;Checks to see what lasers hit a particular asteroid
(define (check-hits lolas ast)
  (hit-ast ast (apply +
         (map (λ (laser)
         (if (in-hitbox? ast (laser-x laser) (laser-y laser))
             (laser-power laser)
             0))
       lolas))))

;List of Lasers, List of Asteroids -> List of Asteroids
;Checks laser hits on list of asteroids
(define (check-laser-hits lolas loast)
  (map (λ (ast) (check-hits lolas ast)) loast))


;Ast, Image -> Image
;Places asteroids on canvas
(define (ast-render ast img)
  (place-image (rotate (ast-rotation ast) (cond [(= (ast-mass ast) (/ CANVAS-WIDTH 14.94)) AST1-IMAGE]
                                                [(= (ast-mass ast) (/ CANVAS-WIDTH 7.74)) AST2-IMAGE]
                                                [(= (ast-mass ast) (/ CANVAS-WIDTH 4.94)) AST3-IMAGE]
                                                [(= (ast-mass ast) (/ CANVAS-WIDTH 3.735)) AST4-IMAGE]
                                                [(= (ast-mass ast) (/ CANVAS-WIDTH 2.988)) AST5-IMAGE]
                                                [else (circle 15 'solid 'green)]))
               (ast-x ast)
               (ast-y ast)
               img))

; Ast -> Ast
; Accounts for screen wrap when processing asteroid motion
(define (wrap-ast ast)
  (make-ast (cond [(> (ast-x ast) CANVAS-WIDTH)
                   (+ 0 (- (ast-x ast) CANVAS-WIDTH))]
                  [(< (ast-x ast) 0)
                   (+ (ast-x ast) CANVAS-WIDTH)]
                  [else (ast-x ast)])
            (cond [(> (ast-y ast) CANVAS-HEIGHT)
                   (+ 0 (- (ast-y ast) CANVAS-HEIGHT))]
                  [(< (ast-y ast) 0)
                   (+ (ast-y ast) CANVAS-HEIGHT)]
                  [else (ast-y ast)])
            (ast-mag ast)
            (ast-angle ast)
            (ast-health ast)
            (ast-mass ast)
            (ast-rspeed ast)
            (ast-rotation ast)))
; Michael wrote these monstrous check-expects, too
(check-equal? (ast-x (wrap-ast (make-ast (+ CANVAS-WIDTH 1)
                                         (+ CANVAS-HEIGHT 1)
                                         0
                                         0
                                         100
                                         100
                                         1
                                         0))) 1)
(check-equal? (ast-y (wrap-ast (make-ast (+ CANVAS-WIDTH 1)
                                         (+ CANVAS-HEIGHT 1)
                                         0
                                         0
                                         100
                                         100
                                         1
                                         0))) 1)
                                  
            

; Ast -> Ast
; Uses asteroid velocity to change the asteroid's position on the screen
(define (move-ast ast)
  (local [(define dx (* (ast-mag ast)
                        (cos (degrees->radians (ast-angle ast)))))
          (define dy (* (ast-mag ast)
                        (sin (degrees->radians (ast-angle ast)))))
          (define new-angle (+ (ast-rspeed ast) (ast-rotation ast)))]
    (wrap-ast (make-ast (+ (ast-x ast) dx)
                        (+ (ast-y ast) dy)
                        (ast-mag ast)
                        (ast-angle ast)
                        (ast-health ast)
                        (ast-mass ast)
                        (ast-rspeed ast)
                        (cond
                          [(> new-angle 360) (- new-angle 360)]
                          [(< new-angle 0) (+ new-angle 360)]
                          [else new-angle])))))
(check-equal? (ast-x (move-ast TEST-AST3)) 101.0)
(check-equal? (ast-y (move-ast TEST-AST3)) 111.0)
(check-equal? (ast-x (move-ast TEST-AST)) (+ 109 (* 10 (cos (degrees->radians 75)))))
(check-equal? (ast-y (move-ast TEST-AST)) (+ 100 (* 10 (sin (degrees->radians 75)))))

; List of Asts -> List of Asts
; Updates the positions of all the asteroids in loast
(define (update-asts loast)
  (map move-ast loast))

; Ast, Number Number -> Boolean
; Asteroid hitbox testing
; Hitboxes are circular (how convenient...)
; Hitbox size is dependent on asteroid mass
(define (in-hitbox? ast x y)
  (local [(define radius (/ (ast-mass ast) 10))
          (define h (ast-x ast))
          (define k (ast-y ast))]
    (<=
     (+ (sqr (- x h)) (sqr (- y k)))
     (sqr radius))))
(check-equal? (in-hitbox? (make-ast 500 500 10 0 100 500 10 10) 480 480) #t)
(check-equal? (in-hitbox? (make-ast 500 500 10 0 100 250 10 10) 600 600) #f)

; List of Asteroids -> List of Asteroids
; Splits asteroids into smaller asteroids when health drops below certain thresholds 
; Despawns asteroids when health drops below 100
; Michael wrote this monster too, slight editing from Will
; Will had nothing to do with it, anything else is just propaganda
(define (kill-asteroids loast)
  (foldl (λ (ast new-loast) (append
                             (local [(define rot (ast-rotation ast))]
                               (cond [(< (ast-health ast)  (/ CANVAS-WIDTH 14.94)) empty]
                                     [(and (< (ast-health ast) (/ CANVAS-WIDTH 7.74))
                                           (> (ast-health ast) (/ CANVAS-WIDTH 14.94)))
                                      (build-list 1 (λ (x) (make-ast (ast-x ast)
                                                                     (ast-y ast)
                                                                     (+ 1 (random (ast-mag ast)))
                                                                     (random 360)
                                                                     (/ CANVAS-WIDTH 14.94)
                                                                     (/ CANVAS-WIDTH 14.94)
                                                                     (+ 1 (random 3))
                                                                     rot)))]
                                     [(and (< (ast-health ast) (/ CANVAS-WIDTH 4.94))
                                           (> (ast-health ast) (/ CANVAS-WIDTH 7.74)))
                                      (build-list 2 (λ (x) (make-ast (ast-x ast)
                                                                     (ast-y ast)
                                                                     (+ 1 (random (ast-mag ast)))
                                                                     (random 360)
                                                                     (/ CANVAS-WIDTH 7.74)
                                                                     (/ CANVAS-WIDTH 7.74)
                                                                     (+ 1 (random 3))
                                                                     rot)))]
                                     [(and (< (ast-health ast) (/ CANVAS-WIDTH 3.735))
                                           (> (ast-health ast) (/ CANVAS-WIDTH 4.94)))
                                      (build-list 3 (λ (x) (make-ast (ast-x ast)
                                                                     (ast-y ast)
                                                                     (+ 1 (random (ast-mag ast)))
                                                                     (random 360)
                                                                     (/ CANVAS-WIDTH 4.94)
                                                                     (/ CANVAS-WIDTH 4.94)
                                                                     (+ 1 (random 3))
                                                                     rot)))]
                                     [else (list ast)]))
                             new-loast))
         empty
         loast))
;(check-equal? (kill-asteroids (list (make-ast 500 500 5 90 67 100 10 10))) empty)
(check-equal? (ast-health (check-hits (list TEST-LASER) TEST-AST2)) (ast-health TEST-AST2))



;;;;;;;;;;;;;;;;;;;;;;;;;
; WORLDSTATE GENERATION ;
;;;;;;;;;;;;;;;;;;;;;;;;;

; Number -> Function that Makes Asts
;Used in difficulty settings
(define (populate-ast max-speed)
  (λ (num) (local [(define class (+ 1 (random 5)))
                   (define mass (cond [(= class 1) AST-MASS1]
                                      [(= class 2) AST-MASS2]
                                      [(= class 3) AST-MASS3]
                                      [(= class 4) AST-MASS4]
                                      [(= class 5) AST-MASS5]))]
             (make-ast (+ 1 (random CANVAS-WIDTH))
                       (+ 1 (random CANVAS-HEIGHT))
                       (+ 1 (random max-speed))
                       (+ 1 (random 360))
                       mass 
                       mass
                       (+ 1 (random 3))
                       (random 361)))))

; Nothing -> WorldState
;Easy difficulty
(define (make-easy)
  (local [(define max-speed 6)]
    (make-WS (make-ship (/ CANVAS-WIDTH 2)
                        (/ CANVAS-HEIGHT 2)
                        90
                        0
                        0
                        100
                        150
                        0)
             (build-list (+ 10 (random 10)) (populate-ast max-speed))
             empty)))

; Nothing -> WorldState
;Normal difficulty
(define (make-normal)
  (local [(define max-speed 8)]
    (make-WS (make-ship (/ CANVAS-WIDTH 2)
                        (/ CANVAS-HEIGHT 2)
                        90
                        0
                        0
                        100
                        150
                        0)
             (build-list (+ 20 (random 10)) (populate-ast max-speed))
             empty)))

; Nothing -> WorldState
;Hard difficulty
(define (make-hard)
  (local [(define max-speed 16)]
    (make-WS (make-ship (/ CANVAS-WIDTH 2)
                        (/ CANVAS-HEIGHT 2)
                        90
                        0
                        0
                        100
                        150
                        0)
             (build-list (+ 30 (random 10)) (populate-ast max-speed))
             empty)))

; Nothing -> WorldState
;It is 3,720 to 1, after all...
(define (make-never)
  (local [(define max-speed 26)]
    (make-WS (make-ship (/ CANVAS-WIDTH 2)
                        (/ CANVAS-HEIGHT 2)
                        90
                        0
                        0
                        100
                        150
                        0)
             (build-list (+ 50 (random 10)) (populate-ast max-speed))
             empty)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RENDER HELPER FUNCTIONS ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Ship -> Image
;Health bar image used in HUD
;MICHAEL WROTE THIS MONSTER
(define (healthbar ship)
  (local [(define H (/ CANVAS-HEIGHT 5.5))
          (define STATUS-BAR (rectangle (* H .8 3) (* H .8) 'solid 'black))
          (define HEALTH-BAR (rectangle (* H .8 .4) (* H .65) 'solid 'red))
          (define INC (/ (image-width STATUS-BAR) 5))
          (define HEALTH (ship-health ship))]
    (cond [(> HEALTH 80) (underlay/offset
                          (underlay/offset
                           (underlay/offset
                            (underlay/offset
                             (underlay/offset STATUS-BAR (* 2 INC) 0 HEALTH-BAR)
                             INC 0 HEALTH-BAR)
                            0 0 HEALTH-BAR)
                           (* -1 INC) 0 HEALTH-BAR)
                          (* -2 INC) 0 HEALTH-BAR)]
          [(and (> HEALTH 60)
                (<= HEALTH 80)) (underlay/offset
                                 (underlay/offset
                                  (underlay/offset
                                   (underlay/offset STATUS-BAR (* 2 INC) 0 HEALTH-BAR)
                                   INC 0 HEALTH-BAR)
                                  0 0 HEALTH-BAR)
                                 (* -1 INC) 0 HEALTH-BAR)]
          [(and (> HEALTH 40)
                (<= HEALTH 60)) (underlay/offset
                                 (underlay/offset
                                  (underlay/offset STATUS-BAR (* 2 INC) 0 HEALTH-BAR)
                                  INC 0 HEALTH-BAR)
                                 0 0 HEALTH-BAR)]
          [(and (> HEALTH 20)
                (<= HEALTH 40)) (underlay/offset
                                 (underlay/offset STATUS-BAR (* 2 INC) 0 HEALTH-BAR)
                                 INC 0 HEALTH-BAR)]
          [(and (> HEALTH 0)
                (<= HEALTH 20)) (underlay/offset STATUS-BAR (* 2 INC) 0 HEALTH-BAR)]
          [else STATUS-BAR])))

;Ast -> Image
;Puts asteroids on radar image
(define (ast-ghost-render ast img)
  (place-image (circle (/ (ast-mass ast) 10) 'solid 'red)
               (* RAD-SCALE (ast-x ast))
               (* RAD-SCALE (ast-y ast))
               img))

;WorldState -> Image
;Used in HUD which is used in render
;returns "radar" rectangle
(define (radar ws)
  (local [(define W CANVAS-WIDTH)
          (define H (/ CANVAS-HEIGHT 5.5))]
(foldl ast-ghost-render
    (place-image
                 (circle (+ (/ (image-width SHIP-IMAGE) 2) (/ (image-width SHIP-IMAGE) 16)) 'solid 'green)
                 (* RAD-SCALE (ship-x (WS-ship ws)))
                 (* RAD-SCALE (ship-y (WS-ship ws)))
                 (rectangle (* RAD-SCALE CANVAS-WIDTH) (* RAD-SCALE CANVAS-HEIGHT) 'solid 'black))
    (WS-loast ws))))

;Ship -> Image
;Returns red triangle inside circle showing which way the ship is pointed
(define (angle ship)
  (local [(define cir (circle (/ (/ CANVAS-HEIGHT 5.5) 2.35) 'solid 'black))
          (define indicator (overlay/offset (isosceles-triangle 40 45 'solid 'SlateGray) 0 3.5 (circle 25 'outline 'red)))]
    (overlay
     (rotate (- (ship-angle ship) 90) indicator)
     cir)))

;WorldState -> Image
;Used in render, produces HUD at bottom of screen
(define (HUD ws)
  (local [(define W CANVAS-WIDTH)
          (define H (/ CANVAS-HEIGHT 5.5))
          (define HUD-base (rectangle W H 'solid 'LightSteelBlue))
          (define space (/ W 10))]
    (place-image (healthbar (WS-ship ws)) ;Ship health bar
                 (* space 8)
                 (/ H 2)
                 (place-image (scale .1 (radar ws)) ;Ship-Asteroid radar
                              (* space 4.7)
                              (/ H 2)
                              (place-image (angle (WS-ship ws))  ;Angle picture
                                           (* space 1.5)
                                           (/ H 2)
                                           HUD-base)))))
          
    






;;;;;;;;;;;;;;;;;;;;;;
; BIG BANG FUNCTIONS ;
;;;;;;;;;;;;;;;;;;;;;;


;WorldState -> Image
(define (render ws)
  (above (foldl ast-render 
         (foldl (λ (laser img) (place-laser laser img))
                (place-image
                 (if (> (ship-flashtime (WS-ship ws)) 0)
                     FLASH
                     (rotate (- (ship-angle (WS-ship ws)) 90) SHIP-IMAGE))
                 (ship-x (WS-ship ws))
                 (ship-y (WS-ship ws))
                 CANVAS)
                (WS-lolas ws))
         (WS-loast ws))
         (HUD ws)))



;WorldState, Event -> WorldState
;All the key presses
;left and right rotate the ship by 15 degrees
;up boosts the velocity of the ship
;down teleports the ship to a random spot on screen
;space fires a laser
(define (key-handler ws event)
  (cond [(equal? event "up") (make-WS (change-vel (WS-ship ws))
                                      (WS-loast ws)
                                      (WS-lolas ws))]
        [(equal? event "left") (make-WS (change-angle (WS-ship ws) ROTATION)
                                        (WS-loast ws)
                                        (WS-lolas ws))]
        [(equal? event "right") (make-WS (change-angle (WS-ship ws) (- ROTATION))
                                         (WS-loast ws)
                                         (WS-lolas ws))]
        [(equal? event " ") (make-WS (WS-ship ws)
                                     (WS-loast ws)
                                     (append (list (make-laser (+ (ship-x (WS-ship ws)) ;starts laser-x in right place
                                                                          (* L-LENGTH (cos (degrees->radians (ship-angle (WS-ship ws))))))
                                                                       (- (ship-y (WS-ship ws)) ;starts laser-y in right place
                                                                          (* L-LENGTH (sin (degrees->radians (ship-angle (WS-ship ws))))))
                                                                       (ship-angle (WS-ship ws))
                                                                       L-POWER))
                                             (WS-lolas ws)))]
        [(equal? event "down") (local [(define x-str (number->string (round (- CANVAS-WIDTH (/ CANVAS-WIDTH 1.494)))))
                                       (define y-str (number->string (round (- CANVAS-HEIGHT (/ CANVAS-WIDTH 3.735)))))]
                                         (make-WS
                                (make-ship (+ (/ CANVAS-WIDTH 2.988)
                                              (random
                                               (string->number (substring x-str 0 (- (string-length x-str) 2))))) ;keeps ship from appearing too close 
                                           (+ (/ CANVAS-WIDTH 7.74)
                                              (random
                                               (string->number (substring y-str 0 (- (string-length y-str) 2))))) ;to the sides of the screen 
                                           (ship-angle (WS-ship ws))
                                           (ship-dx (WS-ship ws))
                                           (ship-dy (WS-ship ws))
                                           (ship-health (WS-ship ws))
                                           (ship-mass (WS-ship ws))
                                           6.5)
                                (WS-loast ws)
                                (WS-lolas ws)))]
        [else ws]))
;(check-equal? (ship-dx (WS-ship (key-handler TEST-WS "up"))) 0)
;Racket hates me and won't count this as 0 even though it pretty much is
;(check-equal? (ship-dy (WS-ship (key-handler TEST-WS "up"))) -1.5)
(check-equal? (ship-angle (WS-ship (key-handler TEST-WS "left"))) 105)
(check-equal? (ship-angle (WS-ship (key-handler TEST-WS "right"))) 75)
(check-equal? (laser-x (first (WS-lolas (key-handler TEST-WS " ")))) 500.0)
;(check-equal? (laser-y (first (WS-lolas (key-handler TEST-WS " ")))) 485.0)
(check-equal? (key-handler TEST-WS "w") TEST-WS)


;WorldState -> WorldState
;One tock to rule them all,
;One tock to update them,
;One tock to tick them all
;and in the darkness return them
(define (tock ws)
  (local [(define updated-lasers (update-lasers (WS-lolas ws)))
          (define updated-asteroids (check-laser-hits updated-lasers (update-asts (WS-loast ws))))
          (define updated-ship (collide-ship updated-asteroids (update-ship (WS-ship ws))))]
  (make-WS
   updated-ship
   (kill-asteroids (collide-ast updated-asteroids updated-ship))
   (remove-lasers updated-lasers updated-asteroids))))

;WorldState -> Boolean
;Kills game when health falls below 0 or all the asteroids are gone
(define (last-world? ws)
  (or (<= (ship-health (WS-ship ws)) 0)
      (empty? (WS-loast ws))))

;WorldState -> Image
;Final image varies upon end circumstances
(define (final-screen ws)
  (local [(define LOSE-TEXT (above (text/font "''The possibility of 𝑠𝑢𝑐𝑐𝑒𝑠𝑠𝑓𝑢𝑙𝑙𝑦 navigating an"
                                              40 'Gold "Times New roman" 'roman 'normal 'normal #f)
                                   (text/font " asteroid field is approximately 3,720 to 1. . .''"
                                              40 'Gold "Times New roman" 'roman 'normal 'normal #f)))
          (define WIN-TEXT (text/font (quote "''Never tell me the odds.''")
                                      60 'Gold "Times New roman" 'roman 'normal 'normal #f))]
    (above (place-image
     (if (empty? (WS-loast ws)) WIN-TEXT
     LOSE-TEXT)
     (/ CANVAS-WIDTH 2)
     (/ CANVAS-HEIGHT 2)
     (foldl ast-render 
            (foldl (λ (laser img) (place-laser laser img))
                   (place-image
                    (rotate (- (ship-angle (WS-ship ws)) 90) (if (empty? (WS-loast ws)) SHIP-IMAGE
                                                                 EXPLOSION))
                    (ship-x (WS-ship ws))
                    (ship-y (WS-ship ws))
                    CANVAS)
                   (WS-lolas ws))
            (WS-loast ws)))
           (HUD ws))))






; String -> World;
; Main accepts an argument to specify difficulty
; (easy/medium/hard), and uses that parameter to
; determine which algorithm to use when randomizing
; the start condition
(define (main mode)
  (big-bang
   (cond [(string=? mode "easy") (make-easy)]
         [(string=? mode "normal") (make-normal)]
         [(string=? mode "hard") (make-hard)]
         [(string=? mode "never tell me the odds") (make-never)])
   (to-draw render)
   (on-key key-handler)
   (on-tick tock .02)
   (stop-when last-world? final-screen)
   (name "Anoat Asteroid Belt")))

  












