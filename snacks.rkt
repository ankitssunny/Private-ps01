;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snacks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(provide
 initial-machine
 machine-chocolates
 machine-carrots
 machine-bank
 machine-next-state
 )



;;Defining the constant price of chocolate and carrot in cents
(define price-chocolate 175)
(define price-carrots 70)


;; DATA DEFINITIONS

(define-struct machine
  (
   chocolate-quantity 
   carrots-quantity
   amount-in-receiver
   change-back
   amount-in-bank))

;; A machine is a 
;; (make-machine NonNegInt NonNegInt PosReal PosReal )
;; INTERPRETATION:
;;                chocolate-quantity: number of chocolates 
;;                present in the machine 
;;                carrots-quantity:  number of carrots present in the machine 
;;                amount-in-receiver: money inserted into the receiver of the
;;                                    snacks machine by customer in cents 
;;                change-back: amount of money returned back to customer after
;;                             a successfull order
;;                amount-in-bank: money present in the container of the machine
;;                                in cents

;;TEMPLATE:
;; machine-fn: machine ->??

;;(define (machine-fn machine1)
;;  (...
;;   (make-machine machine1)
;;   (machine-chocolate-quantity machine1)
;;   (machine-carrots-quantity machine1)
;;   (machine-amount-in-receiver machine1)
;;   (machine-change-back machine1)
;;   (machine-amount-in-bank machine1)
;;   ))


;; DATA DEFINITIONS
;; A CustomerInput is one of
;; -- a positive Number PosInt interp: insert the specified number of cents
;; -- "chocolate"       interp: request a chocolate bar
;; -- "carrots"         interp: request a package of carrot sticks
;; -- "release"         interp: return all the coins that the customer has put in

;; Template:
;; CustomerInput-fn: CustomerInput->??
;;(define (CustomerInput-fn cust-input)
;;  (cond
;;    [(not(string? cust-input))
;;     ...]
;;    [(string=? cust-input "chocolate")
;;     ...]
;;    [(string=? cust-input "carrots")
;;     ...]
;;    [(string=? cust-input "release")
;;     ...]
;;    [else ...]))

;; Defining Constants

(define chocolate "chocolate")
(define carrots "carrots")
(define release "release")

;; Defining initial-machine
;; initial-machine : NonNegInt NonNegInt-> Machine
;; GIVEN: the number of chocolate bars and the number of packages of
;; carrot sticks
;; RETURNS: a machine loaded with the given number of chocolate bars and
;; carrot sticks, with an empty bank.
;; EXAMPLE:
;;         (initial-machine 20 30) -> Returns a machine 
;;                                   (make-machine 20 30 0 0 0) with 20 
;;                                   chocolates and 30 carrots



(define (initial-machine number-of-chocolates number-of-carrots)
  (
   make-machine
   number-of-chocolates 
   number-of-carrots
   0   ;;Since it is initial state so there would be no customer inserted money 
   0   ;;Since initial state there is no money that has to be returned 
   0))
 
;; Defining machine-after-choice
;; machine-after-choice : NonNegInt NonNegInt PosInt PosInt PosInt -> Machine
;; GIVEN: Attributes of a machine
;; RETURNS: A machine with the given attributes
;; EXAMPLE: 
;;          (machine-after-choice 12 5 200 25 34) -> Returns a machine 
;;                                           (make-machine 12 5 200 25 34)

(define (machine-after-choice 
         chocolates-qnty 
         carrots-qnty 
         amount-received
         change
         bank)
  (
   make-machine
   chocolates-qnty 
   carrots-qnty 
   amount-received
   change
   bank))


         
;; machine-chocolates : Machine -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of chocolate bars left in the machine
;; EXAMPLE:
;;         (machine-chocolates(make-machine 20 15 200 0 500)) -> Returns 20 i.e.
;;                  number of chocolates in the machine.      

(define (machine-chocolates machine1)
  (
   machine-chocolate-quantity machine1))


;; machine-carrots : Machine -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of packages of carrot sticks left in the machine
;; EXAMPLE:
;;         (machine-carrots(make-machine 20 15 200 0 500)) -> Returns 15 i.e.
;;                  number of carrots in the machine.
;; STRATEGY: Structural Decomposition

(define (machine-carrots machine1)
  (
   machine-carrots-quantity machine1))


;; machine-bank : Machine -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the amount of money in the machine's bank, in cents
;; EXAMPLE:
;;         (machine-bank(make-machine 20 15 200 0 500)) -> Returns 500 i.e.
;;                  amount in the bank of the machine.
;; STRATEGY: Structural Decomposition

(define (machine-bank machine1)
  (
   machine-amount-in-bank machine1))

;; machine-amount : Machine -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the amount of money in the machine's receiver, in cents
;; EXAMPLE:
;;         (machine-amount(make-machine 20 15 200 0 500)) -> Returns 200 i.e.
;;                  amount in the bank of the machine.
;; STRATEGY: Structural Decomposition

(define (machine-amount machine1)
  (
   machine-amount-in-receiver machine1))


;; machine-next-state : Machine CustomerInput -> Machine
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's
;;         input
;; STRATEGY: CASES on CustomerInput
;; EXAMPLES:
;;      (machine-next-state (make-machine 12 5 0 0 34 ) "200")
;;         => Returns (make-machine 12 5 "200" 0 34) 
;;      (machine-next-state (make-machine 12 5 200 0 34 ) chocolate)
;;         => Returns (make-machine 11 5 200 25 209)
;;      (machine-next-state (make-machine 12 5 200 0 34 ) carrots)
;;         => Returns (make-machine 12 4 200 130 104)
;;      (machine-next-state (make-machine 12 5 200 0 34 ) release)
;;         => Returns (make-machine 12 5 0 200 34)

(define (machine-next-state machine1 cust-input)
  (
   cond
    [(not(string? cust-input))
     (
      machine-after-choice
      (machine-chocolates machine1)
      (machine-carrots machine1)
      cust-input
      (machine-change-back machine1)
      (machine-bank machine1))]
    

;; For the machine to dispense chocolates
;; all three conditions below should be true
;; 1. customer input is chocolote
;; 2. chocolate quantity should be greater than 0 in the machine
;; 3. amount inserted by customer should be greater than or equal to price 
;;    of chocolate
    [(and 
      (string=? cust-input chocolate)
      (> (machine-chocolates machine1) 0 )
      (>= (machine-amount machine1) price-chocolate))
     (
      machine-after-choice
      ;;decreasing qnty of chocolate by 1
      (- (machine-chocolates machine1) 1) 
      (machine-carrots machine1)
      (machine-amount-in-receiver machine1)
      ;;returning change
      (- (machine-amount-in-receiver machine1) price-chocolate)
      ;;adding amount to machine's bank
      ( + (machine-bank machine1) price-chocolate))]
    

;; For the machine to dispense carrots
;; all three conditions below should be true
;; 1. customer input is carrots
;; 2. carrots quantity should be greater than 0 in the machine
;; 3. amount inserted by customer should be greater than or equal to price
;;    of carrots
    
    [(and 
      (string=? cust-input carrots)
      (> (machine-carrots machine1) 0 )
      (>= (machine-amount machine1) price-carrots))
     (
      machine-after-choice
      (machine-chocolates machine1) 
      ;;decreasing qnty of carrots by 1
      (- (machine-carrots machine1) 1)
      (machine-amount-in-receiver machine1)
      ;;returning change
      (- (machine-amount-in-receiver machine1) price-carrots)
      ;;adding amount to machine's bank
      ( + (machine-bank machine1) price-carrots))]
    
          
;; The entire amount inserted by the customer is returned      
    [(string=? cust-input release)
     (
      machine-after-choice
      (machine-chocolates machine1) 
      (machine-carrots machine1)
      ;; Amount in receiver will become zero since we 
      ;; are returning the entire amount
      0
      ;; change-back to be given would be equal to the 
      ;; amount in the receiver of the machine
      (machine-amount-in-receiver machine1)
      (machine-bank machine1))]
    
    [else "Machine Waiting for Customer Input."]))

;;(initial-machine 20 30)
;;(machine-next-state (make-machine 12 5 0 0 34 ) 200)
;;(machine-next-state (make-machine 12 5 200 0 34 ) chocolate)
;;(machine-next-state (make-machine 12 5 200 0 34 ) carrots)
;;(machine-next-state (make-machine 12 5 200 0 34 ) release)


(machine-next-state 
               (machine-next-state (initial-machine 3 4) 12)
            "chocolate")
