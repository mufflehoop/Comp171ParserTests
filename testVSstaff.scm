(load "~/compilation/parser.so")
(load "~/compilation/compiler.scm")

(define <my-sexpr> <sexpr-2>)
(define <staff-sexpr> <sexpr>)

(define testVSstaff
	(lambda (input)
		(let ((my-res (test-string <my-sexpr> input))
		      (staff-res (test-string <staff-sexpr> input)))
			(display input)
			(display ": ")			
			(cond ((equal? my-res staff-res)
				(display "\033[1;32mSuccess!\033[0m") (newline) #t)
				(else (display ": \033[1;31mFailed!\033[0m ") 
					(display ", expected: ")					
					(display staff-res)
					(display ", actual:")
					(display my-res)
					(newline)
					#f))
			)))
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "=============")
	(newline)
	(cond ((andmap (lambda (exp) (equal? (testVSstaff exp) #t)) lst)		
		(display "\033[1;32mSUCCESS!\033[0m\n") #t)
		(else (display "\033[1;31mFAILED!\033[0m\n") #f))
		(newline)
))

(define runAllTests
  (lambda (lst)
    (cond ((andmap (lambda (test) (display (car test)) (equal? (runTests (car test) (cadr test)) #t)) lst)
      (display "\033[1;32m !!!!!! ALL TESTS SUCCEED !!!!! \033[0m\n") #t)
      (else (display "\033[1;31m ****** SOME TESTS FAILED ***** \033[0m\n") #f))
))

(define booleanTests (list "#t" "#f"))
	 
(define numberTests
	(list "0" "9" "123" "0987" "-5" "+8" "   -456   "
	      "657" "0/4" "8/28" "-2/4" "+2/4" "-2 / 4"
	 ))
	  
(define charTests
	(list	"#\\a" "#\\9" "#\\space" "#\\lambda"
		"#\\newline" "#\\nul" "#\\page"
		"#\\return" "#\\tab" "#\\x41" "#\\x23" "#\\x20"
	  ))
	  
(define stringTests
	(list
	  "\"\\\\\"" "\"\\t\"" "\"\\\"\"" "\"\\f\""	  	  
	  "\"\\n\"" "\"\\r\"" "\"\\x09af;\"" "\"\\x41;\""
	  "\" 4 1;\"" "\"Akuna Matata\""
	  ))

(define symbolTests
	(list 
	"0123456789" "abcdeABCDE" "!$^*-_=+<>?/" 
	"0123456789abcdeABCDE!$^*-_=+<>?/" 
	"Hellomynameisasaf"
	  ))
	  
	  
(define properListTests
	(list
	  "(a)" "(\"abc\")" "(a #t #f -2/4 -14 0 \"\\t\" #\\lambda \"\\x41;\")"
	  "(#\\return)" "( (a b c) #t #f)" "(#\\a (a b .c ) -54/32)" "((a b.c))"
	  ))	  
	  
(define improperListTests
	(list
	    "(a b      #t c . (d e #f \"str1\"   )    )"
	    "(#t abc . a)"							
	    "(abc#t . a)"
	    "(#t #f #\\a abc . (a b c)  )"
	    "(#\\a #\\b (\"a1234\" . 123) . #t)"
	  ))
	  
(define vectorTests
  (list 
    "#()" "#(#\\lambda \"abc\" -56/38)" 
  ))
  
(define quotedTests
  (list 
    "'123" "'#t" "'#\\lambda" "'\"123a\"" "'a123" 
  ))
  
(define quasiquotedTests
  (list 
    "`123" "`#t" "`#\\lambda" "`\"123a\"" "`a123" 
  ))
  
(define unquotedTests
  (list 
    ",123" ",#t" ",#\\lambda" ",\"123a\"" ",a123" 
  ))
  
(define unquoteAndSplicedTests
  (list 
    ",@123" ",@#t" ",@#\\lambda" ",@\"123a\"" ",@a123" 
  ))
	  
(define infixExpTests
	(list	  
	    "##1   +    2"
	    "##-123+45"
	    "##1+2+3"
	    "##1+2+30+40"
	    "#%1+2-50+60-70"
	    "##1+2"
	    "##1+2+3+4+150"
	    "##1+2   *    5+4"
	    "##1*5*4*6"  
	    "##5*4+3*7*9*154"
	    "##  5   /   2 - 4 + 3 *  -7/16  * 9 * 154  "
))


(runTests "Boolean" booleanTests)
(runTests "Number" numberTests)
(runTests "Char" charTests)
(runTests "String" stringTests)
(runTests "Symbol" symbolTests)
(runTests "Vector" vectorTests)
(runTests "Quasiquoted" quasiquotedTests)
(runTests "Quoted" quotedTests)
(runTests "UnquoteAndSpliced" unquoteAndSplicedTests)
(runTests "Unquoted" unquotedTests)
(runTests "Proper List" properListTests)
(runTests "Improper List" improperListTests)
(runTests "Infix Exp" infixExpTests)
