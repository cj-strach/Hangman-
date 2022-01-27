;;
(define source-name "glossary.txt")

;; Side effect:
;; Strig-> IO([String])
;; Passed the path, open the file containig glossary
(define (read-words-from filename)
  (let* ((port (open-input-file filename))
         (res (read-word-list port '())))
    (close-input-port port)
    res))

;; Side effect
;; Fd -> [String] -> IO ([String])
;; Passed port and acumulator, return the all the words as strings
(define (read-word-list port acc)
  (let ((stuff (read port)))
    (if (eof-object? stuff)
        acc
        (read-word-list port
                        (cons (symbol->string stuff) acc)))))

(define list-of-words(read-words-from source-name))


;; STATE OF THE GAME
(define random-number 
  (random (length list-of-words))) ;; this will get a random number from how many words there are
(define word-to-guess
  (string->list(list-ref list-of-words random-number))) ;; this gets a random word from the list of words
(define partial-sol
  (map (λ (x) #\*) word-to-guess)) ;;hides letters of word to guess with *

(define hits 0)
(define plays 0)
(define failures 0)
(define total-failures 6)
(define total-hits (length word-to-guess))
(define glossary (map string->list list-of-words))





;; 
;; IO(String)
(define (game-status)
  (begin
    (format "~a H:~a/~a F:~a/~a ~a ~a"
            (list->string partial-sol)
            hits  total-hits
            failures  total-failures
            plays
            (if (and
                 (< hits total-hits)
                 (< failures total-failures))
                ""
                (string-append "GAME-OVER(" (list->string word-to-guess) ")")))))

          

;;;
;;  PURELY FUNCTIONAL
;;


(define (occurrences word char) ;;defines function 'occurrences' and takes two inputs
  (cond
    [(null? word)0]
    [(equal? (car word) char) (+ 1 (occurrences (cdr word) char))] ;;if the character given is present when iterated through the list then occurrences goes up by one
    [else (occurrences (cdr word) char)]
    ))


(define (indices word char)
  (let loop ((i 0)
             (r '())
             (word word))
    (cond
      [(null? word)
       (reverse r)] ;;if not found reverse list so there is still an output
      [(equal? (car word) char) ;;if character is found at start of list 
       (loop (+ i 1) (cons i r) (cdr word))] ;;continue
      [else
       (loop (+ i 1) r (cdr word))]  ;;if not found then continue
      )))
  
(define (replace-indices word idx new)
  (if (or (null? word)(null? idx))
      ;'doesn't need to do anything
      word
      (let ril ((lt word) ;;end of list
                (ltt (rest word)) ;;end of the end
                (rp (first word)) ;;current replacement
                (la '()) ;;accumulator for reversed list
                (it idx) ;;end of the indices
                (ia '())) ;;accumulator for decremeneted indices
        (cond
          ((null? it)
           (if (null? ltt)
               (reverse (cons rp la))
               (ril ltt (rest ltt) (first ltt) (cons rp la)
                    ia '())))
          ((zero? (first it))
           ;;loop with replacement dropping the zero index
           (ril lt ltt new la
                (rest it) ia))
          (else
           ;;loop with exisiting replacement
           (ril lt ltt rp la
                (rest it) (cons (- (first it) 1) ia)))))))


(define (noOfHits hidden)
  (cond ((null? hidden) 0)   ;;if list empty return 0           
        ((not (equal? (car hidden) #\*))   ;;if doesn't equal * increase count by 1    
         (+ 1 (noOfHits (cdr hidden)))) 
        (else                            
         (noOfHits (cdr hidden))))) ;;continue


;; Side effects
;; IO(String)
(define (restart)
  (begin
    ;; some statements
    ;;my word-to-guess already pulls a random word on restart so no need to use set!
    (set! plays 0)
    (set! hits 0)
    (set! total-hits (length word-to-guess))
    (set! failures 0)
    (set! total-failures 6)
    ;; last statement
    (game-status)))


;; Char -> IO(String)
(define (guess char)
  (cond
    [(and (< failures 6) (< hits (length word-to-guess))) ;Checks for game end
     (set! plays (+ 1 plays)) ;Adds +1 to plays for every guess
     (set! partial-sol (replace-indices partial-sol (indices word-to-guess char) char)) ;Updates partial-sol
     (set! hits (noOfHits partial-sol)) ;Increases hits using noOfHits
     (if (= (occurrences word-to-guess char) 0) ;Checks if letter is in word
         (set! failures (+ 1 failures))
         (void))(game-status)])) ;Shows game status


;; IO(String)
(define (solve word)
  (for-each guess (string->list word)) ;Iteration over every letter using guess
  (game-status))


;;
;; EXTRA -F3
;;;;;;;;;;;;;;;;;;;;;;;;;;
   
;; p: all-words as list of list of char
(define (words-containing all-words char ) null)


;; p: all-words as list of list of char
;;  : chars as a list of char
(define (words-containing-ext all-words chars) null)

;; IO([String])
;; this is very hard.
(define (sieve chars) (void))

