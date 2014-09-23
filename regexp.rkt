;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname regexp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Problem 4

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(provide
 initial-state
 next-state
 accepting-state?
 error-state? )

(define state-ab "state-ab")
(define state-cd "state-cd")
(define state-e "state-e")
(define state-error "state-error")

;; DATA DEFINITIONS
;; A machine-state is one of
;; --"initial"    interp: machine is in the initial state
;; --"state-ab"        
;; interp: machine state is such that it can accept any number of a's or b's
;;         and will change to next state on encountering  c or d or else go to
;;         error state on any other input 
;; --"state-cd"                  
;; interp: machine state is such that it can accept any number of c's or d's
;;         and will change to next state on encountering  e or else go to 
;;         error state on any other input
;; --"state-e"          interp: machine state in which any input will make the 
;;                        machine go to the error state
;; --"state-error"      interp: machine state in which it stays in error state 
;;                        on any input

;; Template:
;; machine-state-fn: machine-state1 -> ??

;;(define (machine-state-fn machine-state1)
;;  (cond
;;    [(string=? machine-state1 state-ab)
;;     ...]
;;    [(string=? machine-state1 state-cd)
;;     ...]
;;    [(string=? machine-state1 state-e)
;;     ...]
;;    [(string=? machine-state1 state-error)
;;     ...]))


;; DATA DEFINITIONS
;; A KeyEvent is one of
;; --"a"    interp: keyevent is a
;; --"b"    interp: keyevent is b
;; --"c"    interp: keyevent is c
;; --"d"    interp: keyevent is d
;; --"e"    interp: keyevent is e

;; Template:
;; KeyEvent-fn: KeyEvent -> ??

;;(define (Keyevent-fn ke)
;;  (cond
;;    [(> (string-length ke) 1)
;;     ...]
;;    [(or (string=? ke "a") (string=? ke "b"))
;;     ...]
;;    [(or (string=? ke "c") (string=? ke "d"))
;;     ...]
;;    [(string=? ke "e")
;;     ...]))


;; initial-state : Number -> State
;; GIVEN: a number
;; RETURNS: a representation of the initial state
;;          of your machine.  The given number is ignored.
;; EXAMPLE: (initial-state 5)=> Returns the initial state of
;;                          the machine i.e. state-ab
(define (initial-state num)
  state-ab)


;; helper function
;; change-state : State -> State
;; GIVEN: a state
;; RETURNS: a representation of the new state of your machine.
;; EXAMPLE: (change-state state-ab)=> Returns the new state of
;;                          the machine i.e. state-cd
;; STRATEGY: CASES on machine-state

(define (change-state state1)
  (cond
    [(string=? state1 state-ab)
     state-cd]
    [(string=? state1 state-cd)
     state-e]))
    

;; next-state : State KeyEvent -> State
;; GIVEN: a state of the machine and a key event.
;; RETURNS: the state that should follow the given key event.  A key
;; event that is to be discarded should leave the state unchanged.
;; EXAMPLES:
;;         (next-state state-ab ab)=> Returns the same state of
;;                          the machine i.e. state-ab
;;         (next-state state-ab a)=> Returns the new state of
;;                          the machine i.e. state-ab
;;         (next-state state-ab c)=> Returns the new state of
;;                          the machine i.e. state-cd
;;         (next-state state-ab e)=> Returns the new state of
;;                          the machine i.e. state-error
;;         (next-state state-cd cd)=> Returns the same state of
;;                          the machine i.e. state-cd
;;         (next-state state-cd c)=> Returns the new state of
;;                          the machine i.e. state-cd
;;         (next-state state-cd e)=> Returns the new state of
;;                          the machine i.e. state-e
;;         (next-state state-cd g)=> Returns the new state of
;;                          the machine i.e. state-error
;;         (next-state state-e a)=> Returns the new state of
;;                          the machine i.e. state-error
;;         (next-state state-e r)=> Returns the new state of
;;                          the machine i.e. state-error
;;         (next-state state-error e)=> Returns the same state of
;;                          the machine i.e. state-error
;; STRATEGY: CASES on machine-state
 
(define (next-state state1 ke)
  (cond
    [(> (string-length ke) 1)
     state1]
   
    ;;State-ab reaction to keyevent a or b
    
    [(and
      (or 
       (string=? ke "a") 
       (string=? ke "b"))
      (string=? state1 state-ab))
     state1]
    
    ;;State-ab reaction to keyevent c or d
    [(and
      (or 
       (string=? ke "c") 
       (string=? ke "d"))
      (string=? state1 state-ab))
      (
      change-state state1)]
    
    ;;State-ab reaction to keyevent which is not either a,b,c or d
    
    [(and
      (and 
       (not(string=? ke "a"))
       (not(string=? ke "b"))
       (not(string=? ke "c"))
       (not(string=? ke "d"))) 
      (string=? state1 state-ab))
      state-error]
    
   
    ;;State-cd reaction to keyevent c or d
    
    [(and
      (or 
       (string=? ke "c") 
       (string=? ke "d"))
      (string=? state1 state-cd))
     state1]
   
    
    ;; State-cd reaction to keyevent e
    
    [(and
      (string=? ke "e") 
      (string=? state1 state-cd))
     (
      change-state state1)]
    
    ;;State-cd reaction to keyevent which is not either c,d or e
    
    
    [(and
      (and 
       (not(string=? ke "c"))
       (not(string=? ke "d"))
       (not(string=? ke "e"))) 
      (string=? state1 state-cd))
      state-error]
    
    
    ;;State-e reaction to any input
    [
     (string=? state1 state-e)
      state-error]
    
    ;;State-error reaction to any input
    [
     (string=? state1 state-error)
      state-error]))


;; accepting-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the given state is a final (accepting) state
;; EXAMPLE:
;;        (accepting-state? state-ab)-> Returns true
;;        (accepting-state? state-error)-> Returns false
;; STRATEGY: CASES on Machine State

(define (accepting-state? state1)
  (
   if(
      or 
      (string=? state1 state-ab)
      (string=? state1 state-cd)
      (string=? state1 state-e)
      )
     "true" "false"))



;; error-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the string seen so far does not match the specified
;; regular expression and cannot possibly be extended to do so.
;;        (error-state? state-ab)-> Returns false
;;        (error-state? state-error)-> Returns true
;; STRATEGY: CASES on Machine State

(define (error-state? state1)
  (
   if(
      string=? state1
      state-error
      )
     "true" "false"))



;;(initial-state 5)
;;(change-state state-ab)
;;(change-state state-cd)
;;(next-state state-ab "atere")
;;(next-state state-ab "a")
;;(next-state state-ab "b")
;;(next-state state-ab "c")
;;(next-state state-ab "d")
;;(next-state state-ab "e")
;;(next-state state-cd "atere")
;;(next-state state-cd "c")
;;(next-state state-cd "d")
;;(next-state state-cd "e")
;;(next-state state-cd "g")
;;(next-state state-e "k")
;;(next-state state-error "a")
;;(accepting-state? state-ab)
;;(accepting-state? state-error)
;;(error-state? state-error)
;;(error-state? state-ab)
