;Procedure: symbol-length
(define symbol-length
	(lambda (inSym)
		(if (symbol? inSym)
			(string-length (symbol->string inSym))
			0
		)
	)
)

;Procedure: sequence?
(define sequence?
	(lambda (inSeq)
		(if (list? inSeq)
			(if (null? inSeq)
				#t
				(if (and (symbol? (car inSeq))  (eq? (symbol-length (car inSeq)) 1))
					(sequence? (cdr inSeq))
					#f
				)
			)
			#f
		)
	)
)

;Procedure: same-sequence?
(define same-sequence?
	(lambda (inSeq1 inSeq2)
		(if (and (sequence? inSeq1) (sequence? inSeq2))
			(if (or (null? inSeq1) (null? inSeq2))
				(if (and (null? inSeq1) (null? inSeq2)) 
					#t 
					#f
				)
				(if(eq? (car inSeq1) (car inSeq2))
					(same-sequence? (cdr inSeq1) (cdr inSeq2)) 
					#f
				)
			)
			(error "ERROR305: first and/or second input is not sequence.")
		)
	)
)

;Procedure: reverse-sequence
(define reverse-sequence
	(lambda (inSeq)
		(if (null? inSeq)
			()
			(if (sequence? inSeq)
				(append (reverse-sequence (cdr inSeq)) (list (car inSeq)))
				(error "ERROR305: input is not sequence.")
			)
		)
	)
)

;Procedure: palindrome?
(define palindrome?
	(lambda (inSeq)
		(define revSeq (reverse-sequence inSeq))
		(if (sequence? inSeq)
			(if (same-sequence? inSeq revSeq) 
				#t 
				#f 
			)
			(error "ERROR305: input is not sequence.")
		)
	)
)

;Procedure: member?
(define member?
	(lambda (inSym inSeq)
		(if (and (symbol? inSym) (sequence? inSeq))
			(if (null? inSeq)
				#f
				(if (equal? inSym (car inSeq))
					#t
					(member? inSym (cdr inSeq))
				)
			)
			(error "ERROR305: first input is not a symbol and/or second input is not sequence.")
		)
	)
)

;Procedure: remove-member
(define remove-member
	(lambda (inSym inSeq)
		(if (and (symbol? inSym) (sequence? inSeq))
			(if (member? inSym inSeq)
				(if(equal? inSym (car inSeq))
					(cdr inSeq)
					(cons (car inSeq) (remove-member inSym (cdr inSeq)))
				)
				(error "ERROR305: symbol is not a member of sequence.")
			)
			(error "ERROR305: first input is not a symbol and/or second input is not sequence.")
		)
	)
)

;Procedure: anagram?
(define anagram?
	(lambda (inSeq1 inSeq2)
		(if (and (sequence? inSeq1) (sequence? inSeq2))
			(cond 
				((and (null? inSeq1) (null? inSeq2))  #t)
				((or (null? inSeq1) (null? inSeq2))   #f)
				;((member? (car inSeq1) inSeq2) (anagram? (cdr inSeq1) (remove-member (car inSeq1) inSeq2)))
				;(else #f)
				(else 
					(if (member? (car inSeq1) inSeq2)
						(anagram? (cdr inSeq1) (remove-member (car inSeq1) inSeq2))
						#f
					)
				)
			)
			(error "ERROR305: first and/or second input is not sequence.")
		)
	)
)

;Procedure: anapoli?
(define anapoli?
	(lambda (inSeq1 inSeq2)
		(if (and (sequence? inSeq1) (sequence? inSeq2))
			(if (and (palindrome? inSeq2) (anagram? inSeq1 inSeq2)) 
				#t 
				#f
			)	
			(error "ERROR305: first and/or second input is not sequence.")
		)
	)
)
