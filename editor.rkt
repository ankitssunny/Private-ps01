;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Problem 2

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(provide
 make-editor
 editor-pre
 editor-post
 edit)

(define-struct editor (pre post))
;; An Editor is a
;; (make-editor String String)
;; INTERPRETATION:
;;                pre is the string before the cursor
;;                post is the string after the cursor

;; Template:
;; editor-fn: editor -> ??
;;(define (editor-fn ed)
;;  (...
;;   (make-editor ed)
;;   (editor-pre ed)
;;   (editor-post ed)
;;   )


;; DATA DEFINITION 
;; A KeyEvent is one of
;; -- "\b"
;; -- single character string except "\t" and "\u007F" and "\b"
;; -- "left"
;; -- "right"
;; INTERPRETATION:
;;                right is selected when right arrow has been pressed
;;                left is selected when left arrow has been pressed
;;                tab (\t) and delete (\u00F7) are ignored
;;                backspace (\b) removes the last character in the pre string
;;                single character except as mentioned above are added to the
;;                pre string of the editor

;; Template:
;; keyevent-fn: keyevent->??
;;(define (KeyEvent-fn ke)
;;  (cond
;;    [(string=? ke "\b")
;;     ...]
;;    [(= (string-length ke) 1)
;;     ...]
;;    [(string=? ke "left")
;;     ...]
;;    [(string=? ke "right")
;;     ...]
;;    [else ...]))
 
(define left-arrow "left")
(define right-arrow "right")
(define backspace "\b")
(define tab "\t")
(define delete "\u007F")

;; Function edit
;; edit: editor keyevent -> editor
;; GIVEN: An Editor and a keyevent
;; RETURNS: An Editor after making changes depending on the chosed keyevent
;; STRATEGY: Cases on keyevent

;; EXAMPLES:
;;         (edit (make-editor "Ankit" "is here") "\b")
;;           => Returns (make-editor "Anki" "is here")
;;         (edit (make-editor "Ankit" "is here") "\bob")
;;           => Returns "Invalid Input."
;;         (edit (make-editor "Ankit" "is here") "s")
;;           => Returns(make-editor "Ankits" "is here")
;;         (edit (make-editor "" "is here") "s")
;;           => Returns(make-editor "s" "is here")
;;         (edit (make-editor "Ankit" "is here") "left")
;;           => Returns (make-editor "Anki" "tis here")
;;         (edit (make-editor "Ankit" "is here") "right")
;;           => Returns (make-editor "Ankiti" "s here")
;;         (edit (make-editor "" "is here") "\b")
;;           => Returns "Prestring is empty hence cannot delete any character."
;;         (edit (make-editor "" "is here") "left")
;;           => Returns "Cursor is at the left-most position. Cannot go left anymore"
;;         (edit (make-editor "Ankit" "") "right")
;;           => Returns "Cursor is at the right-most position. Cannot go right anymore"
;;         (edit (make-editor "Ankit" "is here") "\t")
;;           => Returns "Tab and backspace characters are ignored."
;;         (edit (make-editor "Ankit" "is here") "\u007F")
;;           => Returns "Tab and backspace characters are ignored."
;;         (edit (make-editor "Ankit" "is here") "sharma")
;;           => "Invalid Input."


(define (edit ed ke)
  (cond
    [(string=? ke backspace)
     (delete-last-from-pre ed)]
    
    [(or
      (string=? ke tab)
      (string=? ke delete))
     "Tab and backspace characters are ignored."]
    
    [(= (string-length ke) 1)
     (add-to-pre ed ke )]
    
    [(string=? ke left-arrow)
     (move-cursor-left ed)]
    
    [(string=? ke right-arrow)
     (move-cursor-right ed)]
    
    [else "Invalid Input."]))


;; helper function to update the pre and the post strings of the editor
;; update-editor: editor String String-> editor
;; GIVEN: An editor and two Strings
;; RETURNS: An updated editor with the two strings as new part of the editor
;; STRATEGY: Functional Composition
;; EXAMPLE:
;;        (update-editor(make-editor "Ankit" "Sharma") "AnkitS" "harma" )
;;           => Returns editor as (make-editor "AnkitS" "harma" )

(define (update-editor ed pre-string post-string)
  (
   make-editor
   pre-string
   post-string))


;;helper function
;; delete-last-from-pre: editor -> editor
;; GIVEN: The string present before the cursor
;; RETURNS : A new editor with the last character removed 
;;           from the string if  any string is present
;; STRATEGY: Structural Decomposition on editor-pre length
;; EXAMPLE:
;;        (delete-last-from-pre(make-editor "Ankit" "Sharma"))
;;           => Returns editor as (make-editor "Anki" "Sharma" )
;;        (delete-last-from-pre(make-editor "" "Sharma"))
;;           => Returns "Prestring is empty hence cannot delete any character."

(define (delete-last-from-pre ed )
  (cond
    [(> (string-length(editor-pre ed)) 0)
     (update-editor 
      ed 
      (substring 
       (editor-pre ed) 
       0 
       (- (string-length(editor-pre ed)) 1)) 
      (editor-post ed) )]
    [else "Prestring is empty hence cannot delete any character."]))

;; helper function to add a single character to pre string
;; add-to-pre: editor ke -> editor
;; GIVEN: An editor and a single character keyevent
;; RETURNS : A new editor with the last character removed 
;;           from the string if  any string is present
;; STRATEGY: Structural Decomposition on editor
;; EXAMPLES:
;;        (add-to-pre(make-editor "Ankit" "Sharma") "Y")
;;           => Returns editor as (make-editor "AnkitY" "Sharma" )

(define (add-to-pre ed ke)
  (  update-editor 
      ed 
      (string-append  
       (editor-pre ed) ke) 
      (editor-post ed)))

;; helper function to move cursor to the left
;; move-cursor-left: editor -> editor
;; GIVEN: An editor
;; RETURNS : A new editor with the last character removed 
;;           from the prestring  and added to the beginning of the poststring
;; STRATEGY: Structural Decomposition on editor-pre string
;; EXAMPLES:
;;        (move-cursor-left(make-editor "Ankit" "Sharma"))
;;           => Returns editor as (make-editor "Anki" "tSharma" )


(define (move-cursor-left ed)
  (cond
    [(> (string-length (editor-pre ed)) 0)
     (update-editor
      ed 
      (substring 
       (editor-pre ed)
       0
       (- (string-length(editor-pre ed)) 1))
      (string-append 
       (string-ith (editor-pre ed) 
                   (- (string-length (editor-pre ed)) 1))
       (editor-post ed)))]
    
    [else "Cursor is at the left-most position. Cannot go left anymore"]))


;; helper function to move cursor to the right of the string
;; move-cursor-right: editor -> editor
;; GIVEN: An editor
;; RETURNS : A new editor with the first character removed 
;;           from the poststring  and added to end of the prestring
;; STRATEGY: Structural Decomposition on editor-post string
;; EXAMPLES:
;;        (move-cursor-right(make-editor "Ankit" "Sharma"))
;;           => Returns editor as (make-editor "AnkitS" "harma" )

(define (move-cursor-right ed)
  (cond
    [(> (string-length (editor-post ed)) 0)
     (update-editor
      ed 
      (string-append 
       (editor-pre ed)
       (string-ith 
        (editor-post ed) 
        0))
      (substring 
       (editor-post ed) 
       1 
       (string-length (editor-post ed))))]

    [else "Cursor is at the right-most position. Cannot go right anymore"]))


;;(edit (make-editor "Ankit" "is here") "\b")
;;(edit (make-editor "Ankit" "is here") "\bob")
;;(edit (make-editor "Ankit" "is here") "s")
;;(edit (make-editor "" "is here") "s")
;;(edit (make-editor "Ankit" "is here") "left")
;;(edit (make-editor "Ankit" "is here") "right")
;;(edit (make-editor "" "is here") "\b")
;;(edit (make-editor "" "is here") "left")
;;(edit (make-editor "Ankit" "") "right")
;;(edit (make-editor "Ankit" "is here") "\t")
;;(edit (make-editor "Ankit" "is here") "\u007F")
;;(edit (make-editor "Ankit" "is here") "sharma")
