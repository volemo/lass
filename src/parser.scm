(import scheme
	(chicken io)
	comparse
	(chicken irregex)
	srfi-14
	(chicken pretty-print)
	srfi-1)

(define (clean-omitted lst)
  (cond [(null? lst) '()]
	[(list? lst) (map clean-omitted
			  (remove (lambda (x) (eq? x 'omitted)) lst))]
	[#t lst]))

(define (token token parser)
  (bind parser
	(lambda (x)
	  (if (list? x)
	      (result (cons token x))
	      (result (list token x))))))

(define (consume parser)
  (bind parser
	(lambda (x)
	  (result 'omitted))))

(define ws
  (consume (one-or-more (in (char-set-delete
			     char-set:whitespace
			     #\newline)))))

(define ows
  (consume (maybe ws)))

(define filename
  (let ([all-but-prohibited
	 (char-set-delete
	  char-set:graphic
	  #\< #\> #\: #\; #\, #\? #\\ #\" #\* #\| #\/)])
    (as-string (one-or-more (in all-but-prohibited)))))

(define comment
  (let ([all-but-newline
	 (char-set-delete char-set:full
			  #\newline)])
    (consume
     (zero-or-more (in all-but-newline)))))
		  

(define string-literal
  (let ([all-but-prohibited
	 (char-set-delete char-set:full
			  #\" #\\ #\newline)]
	[escape-chars
	 '(#\a #\b #\e #\f #\n #\r #\t #\v #\\ #\' #\")])
    (token 'string-literal
	   (as-string
	    (enclosed-by (is #\")
			 (zero-or-more
			  (any-of (in all-but-prohibited)
				  (sequence (is #\\)
					    (in escape-chars))
				  (sequence (is #\\)
					    (in char-set:digit))))
			 (is #\"))))))

(define width
  (as-string (one-or-more (in char-set:digit))))

(define number
  (as-string (one-or-more (in char-set:hex-digit))))

(define name
  (let ([initial-chars (char-set-adjoin char-set:letter #\_)]
	[latter-chars (char-set-adjoin char-set:letter+digit
				       #\_ #\- #\! #\? #\. #\# #\/)]) 
    (token 'name
	   (as-string (sequence (one-or-more (in initial-chars))
				(zero-or-more (in latter-chars)))))))

(define (make-number sign width base digits)
  (let ([s->n
	 (lambda (digits #!optional (base #\d))
	   (let* ([base (if base base #\d)]
		  [base (if (char=? base #\h) #\x base)])
	     (string->number (string-append "#"
					    (string base)
					    digits))))])
    (make-rectangular (if sign
			  (- (s->n digits base))
			  (s->n digits base))
		      (if width
			  (s->n width)
			  0))))
	 
(define literal
  (token 'literal
	 (bind
	  (sequence (maybe (is #\-))
		    (maybe (sequence (maybe width)
				     (consume (is #\'))
				     (in '(#\b #\o #\d #\h)))
			   '(#f #f))
		    number)
	  (lambda (x)
	    ; (literal #\- ("4" #\h) "F"))
	    (let* ([x (clean-omitted x)]
		   [sign (car x)]
		   [width (caadr x)]
		   [base (cadadr x)]
		   [digits (caddr x)]
		   [number (make-number sign width base digits)])
	      (result number))))))

(define instruction
  (token 'instruction
	 (bind
	  (sequence (one-or-more
		     (bind
		      (sequence (any-of name
					literal
					string-literal)
				ows)
		      (lambda (x)
			(result (car x))))))
	  (lambda (x)
	    (result (car x))))))

(define label
  (token 'label
	 (sequence (any-of name
			   literal)
		   (consume (is #\:)))))

(define block
  (token 'block
	 (bind
	  (sequence (consume (is #\{))
		    ows
		    (consume (maybe (is #\newline)))
		    ows
		    (zero-or-more (any-of label
					  instruction))
		    ows
		    (consume (maybe (is #\newline)))
		    ows
		    (consume (is #\})))
	  (lambda (x)
	    (result (car (clean-omitted x)))))))

(define argument
  (token 'argument
	 (sequence (maybe (sequence width
				    (is #\')))
		   name)))

(define argument-list
  (token 'argument-list
	 (bind (sequence
		 argument
		 ows
		 (zero-or-more (sequence (consume (is #\,))
					 ows
					 argument)))
	       (lambda (x)
		 (result (cons (car x)
			       (map (lambda (y)
				      (caddr y))
				    (caddr x))))))))
	 
(define macro
  (token 'macro
	 (sequence (consume (is #\())
		   (maybe argument-list)
		   (consume (is #\)))
		   ows
		   block)))

(define definition
  (token 'definition
  (sequence name
	    ows
	    (consume (is #\=))
	    ows
	    (any-of name
		    literal
		    macro))))

(define import
  (token 'import
	 (preceded-by (sequence (is #\>) ows)
		      filename)))

(define expression
  (any-of import label definition instruction))

(define statement
  (bind
   (sequence (maybe expression)
	     ows
	     (maybe (sequence (consume (is #\;)) ows comment))
	     (consume (is #\newline)))
   (lambda (x)
     (result (car x)))))

(define code
  (sequence (zero-or-more statement) end-of-input))

(define (lass-parse text)
  (car (clean-omitted (parse code (->parser-input text)))))
 

(pp (lass-parse (read-string #f (open-input-file "example.lass"))))

(exit)

#|
(
  (
    (
      (import "nao-16.lass")
      ()
    )
    (
      (label (name "start"))
      #f
      #f
    )
  )
  #t
)
|#
