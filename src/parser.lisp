(ql:quickload :smug)

(defun .zero-or-more (parser)
  (smug:.plus
   (smug:.let* ((x parser)
                (xs (.zero-or-more parser)))
     (smug:.identity (cons x xs)))
   (smug:.identity ())))


(defun .one-or-more (parser)
  (smug:.let* ((x parser)
               (y (.zero-or-more parser)))
    (smug:.identity (cons x y))))


(defun .ws ()
  (smug:.bind
   (smug:.first
    (smug:.map 'string
	       (smug:.is #'member
			 '(#\Space #\Tab))))
   (lambda (whitespace)
     (smug:.identity nil))))


(defconstant +chars-forbidden-in-filename+
  '(#\< #\> #\: #\; #\, #\? #\" #\* #\/))

(defun .filename ()
  (smug:.bind
   (smug:.first
    (smug:.map 'string
	       (smug:.is-not #'member
			     +chars-forbidden-in-filename+)))
   (lambda (text)
     (smug:.identity (list :FILENAME text)))))


(defun .comment ()
  (smug:.bind
   (smug:.first
    (smug:.progn (smug:.string-equal ";")
		 (smug:.map 'string
			    (smug:.is-not #'char= #\Newline))))
   (lambda (text)
     (smug:.identity nil))))


(defun .string ()
  (smug:.let* ((_ (smug:.char= #\"))
	       (str
		(smug:.first
		 (smug:.map 'list
			    (smug:.or
			     (smug:.string-equal "\\\"")
			     (smug:.is-not #'char= #\")))))
	       (_ (smug:.char= #\")))
    (smug:.identity
     (list :STRING (apply #'concatenate
			  'string
			  (mapcar #'string str))))))


(defun .width ()
  (smug:.bind
   (smug:.first
    (smug:.map 'string
	       (smug:.is #'digit-char-p)))
   (lambda (text)
     (smug:.identity (read-from-string text)))))


(defconstant +binary-number-chars+ '(#\0 #\1 #\_))

(defun .binary-number ()
  (smug:.let* ((width (smug:.optional (.width)))
	       (_ (smug:.string-equal "'b"))
	       (number-as-string
		(smug:.first
		 (smug:.map
		  'string
		  (smug:.is #'member
			    +binary-number-chars+)))))
    (let* ((number-without-underscores
	     (remove-if (lambda (c)
			  (char= c #\_))
			number-as-string))
	   (number-with-prefix
	     (concatenate 'string
			  "#b"
			  number-without-underscores))
	   (number
	     (read-from-string number-with-prefix)))
      (smug:.identity (list :NUMBER width number)))))


(defconstant +decimal-number-chars+
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\_))

(defun .decimal-number ()
  (smug:.let* ((width (smug:.optional (.width)))
	       (_ (smug:.string-equal "'d"))
	       (number-as-string
		(smug:.first
		 (smug:.map
		  'string
		  (smug:.is #'member
			    +decimal-number-chars+)))))
    (let* ((number-without-underscores
	     (remove-if (lambda (c)
			  (char= c #\_))
			number-as-string))
	   (number
	     (read-from-string number-without-underscores)))
      (smug:.identity (list :NUMBER width number)))))


(defconstant +hexadecimal-number-chars+
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\_
    #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f))

(defun .hexadecimal-number ()
  (smug:.let* ((width (smug:.optional (.width)))
	       (_ (smug:.string-equal "'h"))
	       (number-as-string
		(smug:.first
		 (smug:.map
		  'string
		  (smug:.is #'member
			    +hexadecimal-number-chars+)))))
    (let* ((number-without-underscores
	     (remove-if (lambda (c)
			  (char= c #\_))
			number-as-string))
	   (number-with-prefix
	     (concatenate 'string
			  "#x"
			  number-without-underscores))
	   (number
	     (read-from-string number-with-prefix)))
      (smug:.identity (list :NUMBER width number)))))


(defconstant +simple-number-chars+
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\_ #\-))

(defun .simple-number ()
  (smug:.bind
   (smug:.first
    (smug:.map
     'string
     (smug:.is #'member
	       +simple-number-chars+)))
   (lambda (number-as-string)
     (let* ((number-without-underscores
	      (remove-if (lambda (c)
			   (char= c #\_))
			 number-as-string))
	    (number
	      (read-from-string number-without-underscores)))
       (smug:.identity (list :NUMBER nil number))))))


(defun .number ()
  (smug:.or (.hexadecimal-number)
	    (.decimal-number)
	    (.binary-number)
	    (.simple-number)))


(defconstant +additional-name-chars+
  (coerce "@$+_\-*&^%|" 'list))
(defun .name ()
  (smug:.let* ((first
		(smug:.or
		 (smug:.is #'member +additional-name-chars+)
		 (smug:.is #'cl:lower-case-p)
		 (smug:.is #'cl:upper-case-p)))
	       (rest
		(smug:.optional
		 (smug:.first
		  (smug:.map
		   'string
		   (smug:.or
		    (smug:.is #'member +additional-name-chars+)
		    (smug:.is #'cl:lower-case-p)
		    (smug:.is #'cl:upper-case-p)
		    (smug:.is #'cl:digit-char-p)))))))
    (let ((name (concatenate 'string (string first) rest)))
      (smug:.identity (list :NAME name)))))


(defun .arithmetic ()
  (smug:.let* ((width (smug:.optional
		       (smug:.prog1 (.width)
				    (smug:.string-equal "'"))))
	       (_ (smug:.string-equal "["))
	       (name (.name))
	       (body
		(smug:.optional
		 (smug:.first
		  (smug:.map
		   'list
		   (smug:.progn
		    (.ws)
		    (smug:.or
		     (.arithmetic)
		     (.name)
		     (.number)))))))
	       (_ (smug:.string-equal "]")))
    (let* ((beginning (list :ARITHMETIC width name))
	   (lst (concatenate 'list beginning body)))
      (smug:.identity lst))))

(defun .instruction ()
  (smug:.let* ((first
		(smug:.or (.arithmetic)
			  (.number)
			  (.name)
			  (.string)))
	       (rest
		(smug:.optional
		 (smug:.first
		  (smug:.map
		   'list
		   (smug:.progn
		    (.ws)
		    (smug:.or (.arithmetic)
			      (.number)
			      (.name)
			      (.string))))))))
    (smug:.identity (cons :INSTRUCTION (cons first rest)))))


(defun .block ()
  (smug:.let* ((_ (smug:.string-equal "{"))
	       (_ (smug:.optional (.ws)))
	       (_ (smug:.optional (smug:.char= #\Newline)))
	       (code (.code))
	       (_ (smug:.optional (smug:.char= #\Newline)))
	       (_ (smug:.optional (.ws)))
	       (_ (smug:.string-equal "}")))
    (smug:.identity (cons :BLOCK code))))


(defun .argument ()
  (smug:.let* ((width (smug:.optional
		       (smug:.prog1
			(.width)
			(smug:.string-equal "'"))))
	       (name (.name)))
    (smug:.identity (list :ARGUMENT width name))))


(defun .argument-list ()
  (smug:.let* ((_ (smug:.string-equal "("))
	       (_ (smug:.optional (.ws)))
	       (first (smug:.optional (.argument)))
	       (rest (smug:.first
		      (smug:.map
		       'list
		       (smug:.progn
			(.ws)
			(.argument)))))
	       (_ (smug:.optional (.ws)))
	       (_ (smug:.string-equal ")")))
    (smug:.identity (cons :ARGUMENT-LIST
			  (cons first rest)))))

(defun .macro ()
  (smug:.let* ((argument-list (.argument-list))
	       (_ (smug:.optional (.ws)))
	       (body (.block)))
    (smug:.identity (list :MACRO argument-list body))))


(defun .definition ()
  (smug:.let* ((name (.name))
	       (_ (smug:.optional (.ws)))
	       (_ (smug:.string-equal "="))
	       (_ (smug:.optional (.ws)))
	       (body (smug:.or
		      (.macro)
		      (.arithmetic)
		      (.number)
		      (.name))))
    (smug:.identity (list :DEFINITION name body))))


(defun .label ()
  (smug:.let* ((label (smug:.or (.name)
				(.number)))
	       (_ (smug:.optional (.ws)))
	       (_ (smug:.string-equal ":")))
    (smug:.identity (list :LABEL label))))


(defun .import ()
  (smug:.let* ((_ (smug:.string-equal ">"))
	       (_ (smug:.optional (.ws)))
	       (filename (.filename)))
    (smug:.identity (list :IMPORT filename))))


(defun .statement ()
  (smug:.or (.import)
	    (.label)
	    (.definition)
	    (.instruction)))

(defun .code ()
  (smug:.first
   (smug:.map
    'list
    (smug:.let* ((_ (smug:.optional (.ws)))
		 (statement (smug:.optional (.statement)))
		 (_ (smug:.optional (.ws)))
		 (_ (smug:.optional (.comment)))
		 (_ (smug:.char= #\Newline)))
      (smug:.identity (list :STATEMENT statement))))))
