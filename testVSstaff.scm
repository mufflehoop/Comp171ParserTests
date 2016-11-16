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
	(let ((results (map testVSstaff lst)))
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32mSUCCESS!\033[0m\n") (newline) #t)
		(else (display "\033[1;31mFAILED!\033[0m\n") (newline) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m !!!!! ALL TESTS SUCCEEDED !!!!\033[0m\n"))
		(else (display "\033[1;31m ##### SOME TESTS FAILED #####\033[0m\n")))
		(newline))
))		

(define booleanTests (list "#t" "#f" "   #t" "#f   " "  #t    "))
	 
(define numberTests
	(list "0" "9" "123" "0987" "-5" "+8" "   -456   "
	      "657" "0/4" "8/28" "-2/4" "+2/4" "-2 / 4"
	 ))
	  
(define charTests
	(list	"#\\a" "#\\9" "#\\space" "#\\lambda"
		"#\\newline" "#\\nul" "#\\page"
		"#\\return" "#\\tab" "#\\x41" "#\\x23" "#\\x20" "  #\\xab   "
	  ))
	  
(define stringTests
	(list
	  "\"\\\\\"" "\"\\t\"" "\"\\\"\"" "\"\\f\""	  	  
	  "\"\\n\"" "\"\\r\"" "\"\\x09af;\"" "\"\\x41;\""
	  "\" 4 1;\"" "    \"  Akuna Matata  \"    "
	  ))

(define symbolTests
	(list 
	"0123456789" "abcdeABCDE" "!$^*-_=+<>?/" 
	"0123456789abcdeABCDE!$^*-_=+<>?/" 
	"    Hellomynameisasaf   "
	  ))
	  
	  
(define properListTests
	(list
	  "(a)" "(\"abc\")" "(a #t #f -2/4 -14 0 \"\\t\" #\\lambda \"\\x41;\")"
	  "(#\\return)" "( (a b c) #t #f)" "(#\\a (a b .c ) -54/32)" "  ((a b.c))   "
	  ))	  
	  
(define improperListTests
	(list
	    "(a b      #t c . (d e #f \"str1\"   )    )"
	    "(#t abc . a)"							
	    "(abc#t . a)"
	    "(#t #f #\\a abc . (a b c)  )"
	    "    (#\\a #\\b (\"a1234\" . 123) . #t)   "
	  ))
	  
(define vectorTests
  (list 
    "#()" "#(#\\lambda \"abc\" -56/38)" " #(1 2 3)   " " #(#\\a #\\b #t -015/54)  "
  ))
  
(define quotedTests
  (list 
    "'123" "'#t" "'#\\lambda" "'\"123a\"" "  'a123   " 
  ))
  
(define quasiquotedTests
  (list 
    "`123" "`#t" "`#\\lambda" "`\"123a\"" "   `a123  " 
  ))
  
(define unquotedTests
  (list 
    ",123" ",#t" ",#\\lambda" ",\"123a\"" "   ,a123  " 
  ))
  
(define unquoteAndSplicedTests
  (list 
    ",@123" ",@#t" ",@#\\lambda" ",@\"123a\"" "  ,@a123   " 
  ))
  
(define infixArrayGetTests
	(list	  
	    "##1[2]"
	    "##-123[+45/54]"
	    "##1+[2+3]"
	    "##1+2[+30+40]"
	    "#%1+2[-50+60-70]"
	    "##1+2 [3+4+150]"
	    "##1+2   [*    5+4]"
	    "##1*5*[4*6]"  
	    "##5*4[+3*7*9*154]"
	    "  #%  5   /   2 [- 4 + a *  -7/16  * 9 * 154 ] "
	    " ## 123a[ + bc321 ** 3  /  6]" 
))  
	  
(define infixExpTests
	(list	  
	    "##(1-2)"
	    "#% (1+2)*3 "
	    "##1   +    2"
	    "## (3+1)^(5--60/5)"
	    "##-123+45"
	    "##1+2+3"
	    "##1+2+30+40"
	    "#%1+2-50+60-70"
	    "##1+2"
	    "##1+2+3+4+150"
	    "##1+2   *    5+4"
	    "##1*5*4*6"  
	    "##((5*4)+(3*7)*9*154)"
	    " #% (1+2)*3 "
	    "  #%  5   /   (2 - 4) + a *  -7/16  * 9 * 154  "
	    " ## 123a + bc321 ** 3  /  6" 
	    "##(b ^ 2 - 4 * a * c)"
))

(define MayerTests
  (list
  "(let* ((d ##sqrt(b ^ 2 - 4 * a * c))
(x1 ##((-b + d) / (2 * a)))
(x2 ##((-b - d) / (2 * a))))
`((x1 ,x1) (x2 ,x2)))"
))

(runAllTests
  (list
      (cons "Boolean" booleanTests)
      (cons "Number" numberTests)
      (cons "Char" charTests)
      (cons "String" stringTests)
      (cons "Symbol" symbolTests)
      (cons "Vector" vectorTests)
      (cons "Quasiquoted" quasiquotedTests)
      (cons "Quoted" quotedTests)
      (cons "UnquoteAndSpliced" unquoteAndSplicedTests)
      (cons "Unquoted" unquotedTests)
      (cons "Proper List" properListTests)
      (cons "Improper List" improperListTests)
      ;(cons "InfixArrayGet" infixArrayGetTests)
      (cons "InfixExp" infixExpTests)  
      ;(cons "MayerTests" MayerTests)    
))

