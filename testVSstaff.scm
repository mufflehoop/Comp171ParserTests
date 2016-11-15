(load "parser.so")
(load "compiler.scm")

(define <my-sexpr> <sexpr-2>)
(define <staff-sexpr> <sexpr>)

(define testVSstaff
	(lambda (input test-name)
		(let ((my-res (test-string <my-sexpr> input))
		      (staff-res (test-string <staff-sexpr> input)))
			(display test-name)
			(display ": ")
			(display staff-res)
			(cond ((equal? my-res staff-res)
				(newline) #t)
				(else (display ": Failed with input string: ") 
				      (display input)
					(display ", expected: ")					
					(display staff-res)
					(display ", actual:")
					(display my-res)
					(newline)))
			)))


(define test-fail
	(lambda (parser input test-name)
		(display test-name)
		(let ((res (test-string parser input)))
			(cond ((equal? '(failed with report:) res) (newline) #t)
				(else (display "Failed with input string: ") (display input) (newline))
			)
			)))				

(define testsResults
	(lambda (tests)
		(if (andmap (lambda (exp) (equal? exp #t)) tests)
			(display "SUCCESS!")
			(display "FAILED!"))
			(newline)
))

(define tests
	(list
	  (begin
	  ; Boolean
	  (testVSstaff "#t" "Boolean True value")
	  (testVSstaff "#f" "Boolean False value")	  
	  
	  ;<Number>	
	  (testVSstaff "0" "<Number> Test1")
	  (testVSstaff "9" "<Number> Test2")
	  (testVSstaff "123" "<Number> Test3")
	  (testVSstaff "0987" "<Number> Test4")
	  (testVSstaff "-5" "<Number> Test5")
	  (testVSstaff "+8" "<Number> Test6")
	  (testVSstaff "   -456   " "<Number> Test7")
	  (testVSstaff "657" "<Number> Test8")
	  (testVSstaff "0/4" "<Number> Test1")
	  (testVSstaff "8/28" "<Number> Test2")
	  (testVSstaff "-2/4" "<Number> Test3")
	  (testVSstaff "+2/4" "<Number> Test4")
	  (testVSstaff "-2 / 4" "<Number> Test5")
	  
	  ;<Char>	
	  (testVSstaff "#\\a" "<Char> Test1")
	  (testVSstaff "#\\9" "<Char> Test2")
	  (testVSstaff "#\\space" "<Char> Test3")
	  (testVSstaff "#\\lambda" "<Char> Test4")
	  (testVSstaff "#\\newline" "<Char> Test5")	  
	  (testVSstaff "#\\nul" "<Char> Test6")
	  (testVSstaff "#\\page" "<Char> Test7")	  
	  (testVSstaff "#\\return" "<Char> Test8")	  
	  (testVSstaff "#\\tab" "<Char> Test9")	  
	  (testVSstaff "#\\x41" "<Char> Test10")
	  (testVSstaff "#\\x23" "<Char> Test11")	  
	  (testVSstaff "#\\x20" "<Char> Test12")
	  
	  (testVSstaff "(a b      #t c . (d e #f \"str1\"   )    )" "Improper List1")
	  (testVSstaff "(#t abc . a)" "Improper List2")											
	  (testVSstaff "(abc#t . a)" "Improper List3")		
	  (testVSstaff "(#t #f #\\a abc . (a b c)  )" "Improper List4")	  
	  
)))

(testsResults tests)

