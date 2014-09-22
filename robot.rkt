;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Problem 1

(require rackunit)
(require "extras.rkt")
(require 2htdp/image)

(provide
 initial-robot
  robot-left 
  robot-right
  robot-forward
  robot-north? 
  robot-south? 
  robot-east? 
  robot-west?) 

(define-struct robot(x y radius facedirection))

;; A robot is a
;;(make-robot Real Real NonNegInt String)
;; WHERE: radius is equal to 15
;; INTERP:
;;        x is the x co-ordinate of the center of the robot
;;        y is the y co-ordinate of the center of the robot
;;        radius is the radius of the robot which is a constant at 15
;;        facedirection is the direction in which the face of the robot is
;;        Not all Strings is a facedirection but all facedirections is a String
;;        facedirection can be one of the four directions i.e east,west,north
;;        or south

;; Template: Compound Data
;; robot-fn : robot => ??
;; (define (robot-fn rob)
;;  (...
;;     (robot-x rob)
;;     (robot-y rob)
;;     (robot-radius rob)
;;     (robot-facedirection))


;; Defining Constant : Defining the radius of the robot as constant as it 
;;                    is fixed at size 15.

(define radius 15)

;; Defining Function initial-robot
;; initial-robot : Real Real-> Robot
;; GIVEN: a set of (x,y) coordinates
;; RETURNS: a robot with its center at those coordinates, facing north(up)
;; STRATEGY: Functional Composition
;; EXAMPLE:
;;         (initial-robot 10.5 10.5) => Creates a Robot of fixed radius 15 with 
;;         its centre at co-ordinates (10.5,10.5) with the face of the Robot in 
;;         North Direction

(define (initial-robot x y )
  (
   make-robot 
   x
   y
   radius
   "north" ))
 
;; helper function
;; rotate-robot: robot String -> robot
;; GIVEN: A robot along with its facedirection 
;; RETURNS: A robot with center at same co-ordinates 
;;          but with the direction of the face changed to either left or right
;; STRATEGY: Structural Decomposition on robot-facedirection 
;; EXAMPLE:
;;         (rotate-robot (make-robot 50 20 radius "north"))=> Rotates the given
;;             robot to either the left or right of north i.e west or east  
;;         (rotate-robot (make-robot 50 20 radius "south"))=> Rotates the given
;;             robot to either the left or right of south i.e east or west
;;         (rotate-robot (make-robot 50 20 radius "east"))=> Rotates the given
;;             robot to either the left or right of east i.e north or south
;;         (rotate-robot (make-robot 50 20 radius "west"))=> Rotates the given
;;             robot to either the left or right of west i.e south or north


(define (rotate-robot rob newfacedirection)
  (make-robot
   (robot-x rob)
   (robot-y rob)
   radius
   newfacedirection))

;; robot-left:  Robot -> Robot
;; robot-right: Robot -> Robot
;; GIVEN: a robot
;; RETURNS: a robot like the original, but turned 90 degrees either left
;; or right
;; STRATEGY: Structural Decomposition on robot-facedirection
;; EXAMPLES for robot-left:
;;          ( robot-left (make-robot 10 20 radius "north"))
;;               => A robot with the centre at co-ordinates (10,20) but with
;;                  facedirection towards west i.e. to the left of north
;;          ( robot-left (make-robot 10 20 radius "west"))
;;               => A robot with the centre at co-ordinates (10,20) but with
;;                  facedirection towards south i.e. to the left of west
;;          ( robot-left (make-robot 10 20 radius "south")) 
;;               => A robot with the centre at co-ordinates (10,20) but with
;;                  facedirection towards east i.e. to the left of south
;;          ( robot-left (make-robot 10 20 radius "east")) 
;;               => A robot with the centre at co-ordinates (10,20) but with
;;                  facedirection towards north i.e. to the left of east

;; Idea is to transfer the robot and the new direction of face to the 
;; rotate-robot function which makes a new robot


(define (robot-left rob)
  (cond
    [( string=? (robot-facedirection rob) "north") 
     (rotate-robot rob "west")]
    [( string=? (robot-facedirection rob) "west") 
     (rotate-robot rob "south")]
    [( string=? (robot-facedirection rob) "south") 
     (rotate-robot rob "east")]
    [( string=? (robot-facedirection rob) "east") 
     (rotate-robot rob "north")]
    [else 
     "Direction name of the Robot's face has not been provided correctly."]))

;; robot-right
;; EXAMPLES for robot-right:
;;          ( robot-right (make-robot 10 20 radius "north"))
;;               => A robot with the centre at co-ordinates (10,20) but with
;;                  facedirection towards east i.e. to the right of north
;;          ( robot-right (make-robot 10 20 radius "east"))
;;               => A robot with the centre at co-ordinates (10,20) but with
;;                  facedirection towards south i.e. to the right of east
;;          ( robot-right (make-robot 10 20 radius "south")) 
;;               => A robot with the centre at co-ordinates (10,20) but with
;;                  facedirection towards west i.e. to the right of south
;;          ( robot-right (make-robot 10 20 radius "west")) 
;;               => A robot with the centre at co-ordinates (10,20) but with
;;                  facedirection towards east i.e. to the right of west

(define (robot-right rob)
  (cond
    [( string=? (robot-facedirection rob) "north") 
     (rotate-robot rob "east")]
    [( string=? (robot-facedirection rob) "east") 
     (rotate-robot rob "south")]
    [( string=? (robot-facedirection rob) "south") 
     (rotate-robot rob "west")]
    [( string=? (robot-facedirection rob) "west") 
     (rotate-robot rob "north")]
    [else 
     "Direction name of the Robot's face has not been provided correctly."]))


;; helper function to add the distance to the existing co-ordinates
;; add-distance : Real PosInt -> Real
;; GIVEN : One co-ordinate of robot (either x or y) and a distance
;; RETURNS : the new co-ordinate of robot (either x or y)
;; STRATEGY: Functional Composition
;; EXAMPLE:
;;         (add-distance (make-robot 20.6 23.5 radius "west") 8)
;;           => Returns a robot with its co-ordinates at(28.6,31.5) facing west 

(define (add-distance robot-coordinate distance)
  (+ robot-coordinate distance))


;; helper function
;; robot-new-position : robot Real Real -> robot
;; GIVEN: A robot and new co-ordinates in the forward direction
;; RETURNS: Same robot but moved to the new co-ordinates
;; STRATEGY: Functional Composition
;; EXAMPLE:
;;          (robot-new-position (make-robot 20 30 radius "north") 72.7 54.6) => 
;;            => Returns a robot with its co-ordinates at (72.7,54.6) facing north 

(define (robot-new-position rob x y)
  (
   make-robot
   x
   y
   radius
   (robot-facedirection rob)))

;; helper functions
;; movement-robot-inside-room : robot PosInt -> robot
;; movement-robot-outside-or-edge-room : robot PosInt -> robot
;; GIVEN : a robot and a distance
;; RETURNS : a robot at the new co-ordinates by adding the distance
;; STRATEGY : Structural Decomposition on robot-x and robot-y


;;Called when the initial position of the robot is inside the room
(define (movement-robot-inside-room rob distance)   
  (cond                                             
    [( and (< (add-distance (robot-x rob) distance)
              185)  ;; if both the new coordinates
                    ;; are inside the room
           (< (add-distance (robot-y rob) distance) 
              385)) 
     ( robot-new-position  ;; Calling the function to move the robot
       rob 
       (add-distance (robot-x rob) distance) 
       (add-distance (robot-y rob) distance))]
    
    [( and (>= (add-distance (robot-x rob) distance) 
               185) ;; If only new x-coordinate is
                    ;; going over the edge   
           (< (add-distance (robot-y rob) distance) 
              385)) 
     ( robot-new-position  ;; Calling the function to move the robot
       rob
       185 
       (add-distance (robot-y rob) distance))]
    
    [( and (< (add-distance (robot-x rob) distance) 
              185)   ;; If only new y-coordinate is
                     ;; going over the edge
           (>= (add-distance (robot-y rob) distance) 
               385))
     ( robot-new-position   ;; Calling the function to move the robot 
       rob
       (+ (robot-x rob) distance) 
       385)]
    
    [( and (>= (add-distance (robot-x rob) distance) 
               185) ;; If both new co-ordinates are
                    ;; going over the edge
           (>= (add-distance (robot-y rob) distance) 
               385))
     ( robot-new-position  ;; Calling the function to move the robot
       rob
       185 
       385)]))

;; EXAMPLES:
;; (movement-robot-inside-room (make-robot 160 340 radius "west") 10)   
;;    => Moves the robot to new co-ordinates of (170,350) 
;; (movement-robot-inside-room (make-robot 120 240 radius "west") 80)   
;;    => Moves the robot to new co-ordinates of (185,320) 
;; (movement-robot-inside-room (make-robot 110 340 radius "west") 60 )  
;;    => Moves the robot to new co-ordinates of (170,385) 
;; (movement-robot-inside-room (make-robot 160 340 radius "west") 100)  
;;    => Moves the robot to new co-ordinates of (185,385) 


;; called when the initial position of the robot is either inside the room
;; or is partially inside and partially outside
(define (movement-robot-outside-or-edge-room rob distance)
  (cond
    [( and (<= (add-distance (robot-x rob) distance)  ;; if both new coordinates 
              185)                                   ;; are inside the room
           (<= (add-distance (robot-y rob) distance) 
              385)) 
     ( robot-new-position  ;; Calling the function to move the robot
       rob
       (add-distance (robot-x rob) distance) 
       (add-distance (robot-y rob) distance))]
    
    
    [( and (and 
            (<= (robot-x rob) -15)                   ;; If only new x-coordinate
            (>= (add-distance (robot-x rob) distance);; is going over the edge 
                185))  
           (or 
            (>= (robot-y rob) 415) 
            (<= (add-distance (robot-y rob) distance) 
               385))) 
     ( robot-new-position ;; Calling the function to move the robot
       rob
       185 
       (add-distance (robot-y rob) distance))]
    
    [( and (or 
            (>= (robot-x rob) 185) 
            (<= (add-distance (robot-x rob) distance) 185))   
           (and                                  
            (>= (robot-y rob) -15)                 ;; If only new y-coordinate
            (>=                                    ;; is going over the edge 
             (add-distance (robot-y rob) distance)
             385)))     
     
     ( robot-new-position ;; Calling the function to move the robot
       rob
       (+ (robot-x rob) distance) 
       385)]
    
    [( and (and                                   ;; If both new co-ordinates 
            (<= (robot-x rob) -15)                ;; are going over the edge 
            (>= (add-distance (robot-x rob) distance) 185))
           (and                                  
            (>= (robot-y rob) -15)                 
            (>=(add-distance (robot-y rob) distance) 385)))      
     ( robot-new-position ;; Calling the function to move the robot
       rob
       185  
       385)]
    [else 
     ( robot-new-position 
       rob
       185  
       385)]))


;; EXAMPLES:
;; (movement-robot-outside-or-edge-room (make-robot -20 -30 radius "west") 50)
;;    => Moves the robot to new co-ordinates of (30,20) 
;; (movement-robot-outside-or-edge-room (make-robot 120 200 radius "west") 100)
;;    => Moves the robot to new co-ordinates of (170,350) 
;; (movement-robot-outside-or-edge-room (make-robot 110 340 radius "west") 60)
;;    => Moves the robot to new co-ordinates of (170,350) 
;; (movement-robot-outside-or-edge-room (make-robot 160 340 radius "west") 100)
;;    => Moves the robot to new co-ordinates of (170,350) 
;; (movement-robot-outside-or-edge-room (make-robot -20 -300 radius "west") 220)
;;    => Moves the robot to new co-ordinates of (170,350) 
;; (movement-robot-outside-or-edge-room (make-robot -30 -50 radius "west") 500)
;;    => Moves the robot to new co-ordinates of (170,350) 
;; (movement-robot-outside-or-edge-room (make-robot 150 500 radius "west") 100)
;;    => Moves the robot to new co-ordinates of (170,350) 


; helper function
;; robot-initial-position: robot -> String
;; GIVEN: A robot
;; RETURNS: The relative position of the robot with respect to the room
;; STRATEGY: Structural Decomposition on robot-x and robot-y

(define (robot-initial-position rob)
  (cond
    [( and
       (and (> (robot-x rob) 15) (< (robot-x rob) 185))
       (and (> (robot-y rob) 15) (< (robot-y rob) 385)))
     "Inside Room"]
    [(and 
      (or (< (robot-x rob) -15) (> (robot-x rob) 215))
      (or (< (robot-y rob) -15) (> (robot-y rob) 415)))
      "Outside Room"]
    [else "On-Edge"]))


;; EXAMPLES:
;;(robot-initial-position (make-robot 2.5 3.5 radius "north")) 
;;    => On-Edge 
;;(robot-initial-position (make-robot -20.0 5.0 radius "north"))
;;    => On-Edge
;;(robot-initial-position (make-robot 100.0 50.9 radius "north"))
;;    => Inside Roon
;;(robot-initial-position (make-robot 250.5 503.6 radius "north")) 
;;    => Outside Room
;;(robot-initial-position (make-robot -20.0 -43.5 radius "north")) 
;;    => Outside Room


;; robot-forward : Robot PosInt -> Robot
;; GIVEN: a robot and a distance
;; RETURNS: a robot like the given one, but moved forward by the
;; specified number of pixels distance.  If moving forward the 
;; specified number of pixels distance would cause the robot 
;; to move from being entirely inside the canvas room to being
;; even partially outside the canvas room, then the robot should
;; stop at the wall.
;; STRATEGY: Cases on robot-initial-position

(define (robot-forward rob distance)
  (cond
    [(string=? (robot-initial-position rob) "Inside Room")
     (movement-robot-inside-room rob distance)]
    [(or(string=? (robot-initial-position rob) "Outside Room")
        (string=? (robot-initial-position rob) "On-Edge"))
     (movement-robot-outside-or-edge-room rob distance)]))  

;; robot-north?: Robot -> Boolean
;; robot-south?: Robot -> Boolean
;; robot-east? : Robot -> Boolean
;; robot-west? : Robot -> Boolean
;; GIVEN: a robot
;; RETURNS: whether the robot is facing in the specified direction.
;; STRATEGY: Structural Decomposition on robot-facedirection
(define (robot-north? rob)
  (
   if (string=? (robot-facedirection rob) "north") "True"
      "False"))

(define (robot-south? rob)
  (
   if (string=? (robot-facedirection rob) "south") "True"
      "False"))

(define (robot-east? rob)
  (
   if (string=? (robot-facedirection rob) "east") "True"
      "False"))

(define (robot-west? rob)
  (
   if (string=? (robot-facedirection rob) "west") "True"
      "False"))

;; EXAMPLES:
;;(robot-north?(make-robot 70 60 radius "north")) => True
;;(robot-south?(make-robot 70 60 radius "south")) => True
;;(robot-east?(make-robot 70 60 radius "east"))   => True
;;(robot-west?(make-robot 70 60 radius "west"))   => True
;;(robot-north?(make-robot 70 60 radius "south")) => False
;;(robot-south?(make-robot 70 60 radius "west"))  => False
;;(robot-east?(make-robot 70 60 radius "north"))  => False
;;(robot-west?(make-robot 70 60 radius "east"))   => False


;(initial-robot 25 12)
;(robot-left (make-robot 50 20 radius "north"))
;(robot-left (make-robot 30 30 radius "west"))
;(robot-left (make-robot 50 70 radius "south")) 
;(robot-left (make-robot 0 0 radius "east")) 
;(robot-left (make-robot 50 60 radius "Arbitrary")) 
;(robot-right (make-robot 50 20 radius "north"))
;(robot-right (make-robot 30 30 radius "west"))
;(robot-right (make-robot 50 70 radius "south")) 
;(robot-right (make-robot 0 0 radius "east")) 
;(robot-right (make-robot 50 60 radius "Arbitrary"))
;(movement-robot-inside-room (make-robot 160 340 radius "west") 25)
;(movement-robot-inside-room (make-robot 120 340 radius "west") 20)
;(movement-robot-inside-room (make-robot 110 340 radius "west") 60)
;(movement-robot-inside-room (make-robot 160 340 radius "west") 100)
;(movement-robot-outside-or-edge-room (make-robot 160 340 radius "west") 20)
;(movement-robot-outside-or-edge-room (make-robot 120 200 radius "west") 100)
;(movement-robot-outside-or-edge-room (make-robot 110 340 radius "west") 60)
;(movement-robot-outside-or-edge-room (make-robot 160 340 radius "west") 100)
;(movement-robot-outside-or-edge-room (make-robot -20 -300 radius "west") 220)
;(movement-robot-outside-or-edge-room (make-robot -30 -50 radius "west") 500)
;(movement-robot-outside-or-edge-room (make-robot 150 500 radius "west") 100)
;(movement-robot-outside-or-edge-room (make-robot -20 -10 radius "west") 400)
;(robot-initial-position (make-robot 2 3 radius "north")) 
;(robot-initial-position (make-robot -20 5 radius "north"))
;(robot-initial-position (make-robot 100 50 radius "north"))
;(robot-initial-position (make-robot 250 503 radius "north")) 
;(robot-initial-position (make-robot -20 -43 radius "north")) 
;(robot-forward (make-robot 160 340 radius "west") 20)
;(robot-forward (make-robot 250 450 radius "west") 40)
;(robot-forward (make-robot 2 3 radius "west") 40)
;(robot-north?(make-robot 70 60 radius "north"))
;(robot-south?(make-robot 70 60 radius "south"))
;(robot-east?(make-robot 70 60 radius "east"))
;(robot-west?(make-robot 70 60 radius "west"))
;(robot-north?(make-robot 70 60 radius "south"))
;(robot-south?(make-robot 70 60 radius "west"))
;(robot-east?(make-robot 70 60 radius "north"))
;(robot-west?(make-robot 70 60 radius "east"))

