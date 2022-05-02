;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; EMACS LISP QUICKSTART GUIDE ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; INFO DOCUMENTATION
;; Emacs Lisp Intro --- simple introduction to elisp programming
;; Elisp --- The Emacs Lisp Reference Manual



;;;; EVALUATING LISP EXPRESSIONS IN EMACS...
;; ... see Emacs manual (31.9 Evaluating Emacs Lisp Expressions)
;;
;; M-:             ; (eval-expression) ; Input a single expression in the minibuffer.
;; C-x C-e         ; (eval-last-sexp) ; Eval the expression before point - print value in the echo area.
;; C-M-x           ; (eval-defun) ; Eval the defun containing or after point - print value in the echo area.
;; M-x eval-region ; Eval all the Lisp expressions in the region.
;; M-x eval-buffer ; Eval all the Lisp expressions in the buffer.
;; M-x load-file   ; Eval whole file (if no file specified, current file is evaluated).



;; LISP MODES
;; M-x lisp-mode : For editing non-emacs lisp
;; M-x emacs-lisp-mode :
;; M-x scheme-mode :
;; M-x lisp-interaction-mode : This is basically the same as emacs-lisp-mode, only C-j is bound to (eval-print-last-sexp) ---- This is the default mode of the *scratch* buffer.
;; M-x ielm ; elisp REPL (Interactively Evaluate Lisp Mode)

;; OTHER USEFUL EMACS PACKAGES

;; nameless : automatically hides your namespace prefix



;;;; JUMPING TO ELISP SOURCE CODE DEFINITIONS
;;
;; ... need to have uncompiled emacs lisp files...
;; $ sudo apt-get install emacs25-el
;;
;; ... can now jump to source code definition for elisp functions...
;;
;; M-. ; (xref-find-definitions)
;; (xref-find-definitions-other-window)
;; (xref-find-definitions-other-frame)



;;;; MORE BEGINNER TIPS ON EMACSWIKI
;; https://www.emacswiki.org/emacs/ElispCookbook



(pushnew '(gradle (""))
         compilation-error-regexp-alist-alist)



;;;;
(defun ...)     ; define function
(defvar ...)    ; define global constant
(defmacro ...)  ; define macro
(defadvice ...) ; define advice (things to be done any time a certain function is run



;;;; DOCUMENTATION AND HELP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Place cursor on function name, then do describe-function...
; C-h f



;;;;;;;;;;;;;;;;;;;;;;;;; NAMING CONVENTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;

;; NAME BEGINNING WITH AN UNDERSCORE
;; e.g. _window _buffer
;; ... The underscore is a valid symbol name. The convention for the byte compiler is that an unused argument name should start with an underscore in order to avoid the "Unused lexical variable" warning during compilation.

;; PACKAGE/LIBRARY NAMESPACE
;; externally visible function or variable...
;; ... package-prefix-function-name
;; internal function or variable...
;; ... package-prefix--function-name

;; PACKAGE/LIBRARY NAMESPACE (ALTERNATIVE STYLE)
;; package-prefix/function-name



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; QUOTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; BACKTICK/QUASIQUOTE

;; normal quoting
'(1 2 (+ 3 4)) ; => (1 2 (+ 3 4))

;; quasiquote - comma to 'unquote' i.e. evaluate that expression
`(1 2 ,(+ 3 4)) ; => (1 2 7)

;; @ at symbol unpacks a list into quasiquoted expression
(setf abc '("a" "b" "c"))
`(1 2 3 ,abc)  ; => (1 2 3 ("a" "b" "c"))
`(1 2 3 ,@abc) ; => (1 2 3 "a" "b" "c")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRINTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "hello!")                 ; print a message in the echo area
(message "hello %s!" name)         ; message can also take format arguments
(print "hello!")                   ; print in the echo area - also return a value
(insert "hello!")                  ; insert text into buffer
(insert (format "hello %s!" name)) ; insert formatted text into buffer




;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATA TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; USEFUL FUNCTIONS
(type-of 45) ; returns the type of the argument
(typep 45 'integer) ; predicate - true if first arg is the stated type


;; ATOM
;; SYMBOL
;; LIST
;; CONS CELL
;; STRING
;; VECTOR
;; ASSOCIATION LIST
;; PROPERTY LIST
;; HASH TABLE
;; FUNCTION
;; MACRO



;;;; HASH TABLE ;;;;
;; ... can use any lisp objects as keys and values
;; ... although you may need to set the TEST differently... see below (strings...)
(make-hash-table) ; makes a new hash table using eql to test key-equality
(setq ht (make-hash-table)) ; assign new hash table to a variable
(puthash 'a "flipping" ht)
(puthash 32 "dingle" ht)
(gethash 'a ht)
(gethash 'b ht "DEFAULT VAL")
;; have to use 'equal if we want to use strings for keys
(setq ht2 (make-hash-table :test 'equal))
(puthash "a" "berlingo" ht2)
(gethash "a" ht2)
;; other functions
(hash-table-count ht2) ; number of key-value pairs
(remhash "a" ht2) ; remove key and value pair
(clrhash ht2) ; remove all entries
(hash-table-keys) ; get list of keys



;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; LOCAL VARIABLES

;; LET and LET*
; define variables within a local scope...
; (let VARLIST BODY...)

(let ((dingo 2)
      (dongo 12))
  (* dingo dongo)) ; returns 24 (product of dingo & dongo)

; let* allows definitions to reference earlier variables in the same assignment...
(let* ((dingo 3)
       (dongo (+ dingo 1)))
  (+ dingo dongo)) ; returns 7 (sum of dingo & dongo)



;;;; GLOBAL VARIABLES

(defvar things '(telephone shirt plug) "Optional docstring.")

;; NOTE: annoyingly defvar doesn't override if it already has a value
;; - if you are working on developing a program, you may want to init
;; it as nil and then set it, so that the value will be updated when
;; you re-load the file...
(defvar carnivores nil "A list of meat eating animals.")
(setq carnivores '(owl stoat spider))



;;;; BUFFER-LOCAL VARIABLES




;; SET and SETQ
; Define and set value of variables with global scope...

(set 'carnivores '(lion tiger leopard))
(setq carnivores '(lion tiger leopard)) ; with setq, the first arg is quoted automatically.

; setq can make multiple assignments (NOT set)
(setq trees '(pine fir oak maple)
      herbivores '(gazelle antelope zebra))

; Using setq to count (set would work as well)
(setq counter 0)             ; initializer
(setq counter (+ counter 1)) ; incrementer



;;;;;;;;;;;;;;;;;;;;;;;;;;; RETURN VALUES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; last value in an expression is returned...
(setq name "jim") ; returns "jim"
(+ 8 1 2) ; returns 11
(let* ((dingo 3)
       (dongo (+ dingo 1)))
  (+ dingo dongo)) ; returns 7 (sum of dingo & dongo)



;;;;;;;;;;;;;;;;;;;;;;;; FUNCTION DEFINITION ;;;;;;;;;;;;;;;;;;;;;;;;;

; - Macro: defun name args [doc] [declare] [interactive] body...

(defun insert-plug () (insert "PLUG"))

(insert-plug) ; do "C-x C-e" here to see effect of function...



;;;; OPTIONAL ARGUMENTS
;; &optional
(defun bsmsg-optional (&optional text)
  "Prints 'text' if it exists."
  (if text
      (message text)
    (message "NO MESSAGE")))

;;;; KEYWORD ARGUMENTS/DEFAULT AURGUMENTS
;; Keyword arguments are not supported in elisp, however you can import a macro...
;; NOTE: using defun*, NOT defun (defun* is an alias for cl-defun)
(require 'cl)
(defun* dink (a &key (b 21) (c 500))
  (format "A=%d B=%d C=%d" a b c))

;; use keyword args like this...
(dink 45 :c 10040)




;;;;;;;;;;;;;;;;;;;;;;;;; GETTING USER INPUT ;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((your-name (read-from-minibuffer "enter your name: ")))
  (message "hello %s" your-name))

;; other functions for getting input...
;; read-buffer
;; read-string
(read-string "enter your name: ")
;; completing-read

;;; for prefix argument:
;; current-prefix-arg
;; prefix-numeric-value



;;;;;;;;;;;;;;;;;;;;;;; INTERACTIVE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE: for full list of code letters, 'C-h f interactive'

(defun silly-test ()
  "'interactive' means that you will be able to call the function via M-x in emacs."
  (interactive)
  (insert "Inserted by the SILLY-TEST function!"))

(defun silly-args-test (str)
  "'interactive' takes a string argument. Now the user will be prompted for input in minibuffer and that will be mapped on to the function args.
 Use the code 's' at beginning of string to specify string input."
  (interactive "sEnter a string: ")
  (insert "THE SILLY FUNCTION STRING IS'" str "'"))

(defun silly-multi-args-test (str num)
  "For multiple args, separate them by a newline character."
  (interactive "sEnter a string: \nnEnter a number: ")
  (insert "YOU ENTERED THE STRING '" str "' AND THE NUMBER " (number-to-string num)))



;;;; USING INTERACTIVE WITH NON-STRING ARGUMENT

;; you can also give interactive an sexp which results in a list of the actual arguments

;; my defun with standard string argument...
;; ... but I want to insert the current value in the minibuffer prompt
(defun bsc/set-global-text-size (size)
  (interactive "nText size: ")
  (set-face-attribute 'default nil :height size))

;; just a test to see how the first element of the list is applied to the first
;; argument of the defun
(defun bsc/set-global-text-size (size)
  (interactive
   (list 150))
  (set-face-attribute 'default nil :height size))

;; here's the fully working thing
(defun bsc/set-global-text-size (size)
  (interactive
   (list (string-to-number
          (read-from-minibuffer
           (format "Current global text size is %d --- set new size: "
                   (face-attribute 'default :height))))))
  (set-face-attribute 'default nil :height size))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATA TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(type-of 12)   ; integer
(type-of 12.0) ; float

;;; SYMBOLS
blah     ; symbol (a named lisp object)
:blah    ; keyword symbol (they evaluate to themselves & are used as constants (cannot be rebound))
(intern "blah") ; gets the symbol whose name is the string "blah"

;;; NUMBERS
124      ; number
-325.34  ; number

;;; CHARACTERS
?a       ; character literal
?/;      ; character literal (semicolon needs to be escaped, else it's a comment)

;;;; BOOLEAN LITERALS
()  ; FALSE (empty list)
nil ; FALSE ('nil' is equivalent to the empty list)
t   ; anything else is TRUE (by convention the symbol 't' is used)

;;; SEQUENCE FUNCTIONS
(length '[0 1 2 3 4])    ; length of a sequence
(elt '[9 8 7 6 5 4 3] 3) ; 3rd element in sequence
;;; SEQUENCES/LISTS
(list 1 2 3)  ; a list (each element will be evaluated)
'(1 2 3)      ; same
;;; SEQUENCES/ARRAYS (fixed length sequence)
;;; ... array functions

;;; ... string
"dongle"       ; string (an array of characters)

;;; STRING WITH TEXT PROPERTIES
;;; special read and print syntax
#("dingle" 0 3 (face bold))
;; ... characters 0 to 3 have bold property...
#("foo bar" 0 3 (face bold) 3 4 nil 4 7 (face italic))
;; ... 0 to 3 is bold, 3 to 4 has no text properties (nil), 4 to 7 is italic...

(insert (propertize "flip" 'font-lock-face '(:foreground "red"))) ; this one works!

;;; SET TEXT PROPERTIES IN REGION OF CURRENT BUFFER
;;; NOTE: doesn't work if font-lock mode is active

;; set text to bold between character 10 to 15
(add-text-properties 10 15 (list 'face 'bold))



;;; ... vector
(vector 1 2 3)  ; a vector
'[1 2 3]        ; same
(make-vector 8 'z) ; vector of length 8, filled with specified value, 'z
(vconcat '[1 2 3] "frig" '('B 'U 'M)) ; joins sequences and returns a new vector...
;;... NOTE: characters of the string evaluate to their ascii numbers here.
(append '[1 2 3] nil) ; handy way to convert vector into list
;;; ... bool vector (all values are evaluated to T/F)
(bool-vector 1 0 '() nil)

;; access elements with elt (short for 'element'?)
(defvar animals ["cat" "dog" "rabbit" "hamster"])
(elt animals 2) ; returns "rabbit" -- value at index 2
(aset animals 2 "frog") ; set value of element at index 2



;;;; TYPE CONVERSION

(number-to-string 312.4)

; (string-to-number string &optional base)
(string-to-number "-6210")
(string-to-number "100" 8) ; base 8 (returns 64)




;;;; DEFINING NEW DATA TYPES





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LISTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(list 1 2 3 4)   ; returns a list containing the rest of the items
'(1 2 3 4)       ; shorthand for the above
(car '(1 2 3 4)) ; returns FIRST item in list
(cdr '(1 2 3 4)) ; returns REST of list (minus first item)

;;;; PREDICATES ON LISTS
(atom '(1 2 3))        ; true if item is an atom
(consp '(1 2 3))       ; true if item is a cons cell
(listp '(1 2 3))       ; true if item is a list (NOTE: empty list ('()' or 'nil') is a list but NOT a cons cell)
(nlistp '(1 2 3))      ; true if item NOT a list
(not (listp '(1 2 3))) ; same as above
(null '(1 2 3))        ; true if item is 'nil'. NOTE: This function is identical to not, but as a matter of clarity we use null when object is considered a list and not when it is considered a truth value.

;;;; MORE LIST FUNCTIONS
(first '(1 2 3)) ; alias for car
(rest '(1 2 3)) ; alias for cdr
(car-safe '(1 2 3)) ; as car, but if arg is not a cons cell, returns nil instead of triggering an error
(cdr-safe '(1 2 3))
(nth 3 '('a 'b 'c 'd 'e)) ; returns nth item in list
(nthcdr 3 '(1 2 3 4 5 6)) ; skip first n links and return what follows
(last '(1 2 3 4 5)) ; last item in list
(last '(1 2 3 4 5) 2) ; optional numeric arg counts back from end
(length '(1 2 3))
(safe-length '(1 2 3)) ; like length, but no risk of errors or infinite loop (e.g. case of circular list)
(reverse '(1 2 3))
(remq 3 '(1 2 3 4)) ; returns copy of list with all instances eq to given object removed

;; add and remove elements from front of list
(setq my-list '(1 2 3 4 5))
(push 405 my-list) ; add element to front of list
(pop my-list)      ; remove item from front of list and return it

;; adding element to list (only if it isn't already there)
(add-to-list 'my-list 32) ; add to front of list
(add-to-list 'my-list -7 't) ; add to end, if APPEND is non-nil
;; (add-to-list LIST ELEMENT APPEND COMPARE-FN)
;; ... SEE ALSO: add-to-ordered-list


;; adding element to list using CONS
;; ... add to front of list
(setf my-list (cons 1001 my-list))
;; ...add to end of list
(setf (cdr (last my-list)) (cons 907 nil))
;; could make this into a function...
(defun bsc-add-to-end (lst item)
  "Adds ITEM to end of list LST.
Returns the new length of list LST."
  (setf (cdr (last lst)) (cons item nil))
  (length my-list))


;;;; ANATOMY OF LISTS
; lists are made of chains of cons cells
; cons cell is an ordered pair (CAR & CDR)
(cons 1 2)
; CDR of each cons cell refers to the following cons cell
(cons 1 (cons 2 nil)) ; <-- this is a properly formed list of 2 items
(cons 1 (cons 2 (cons 3 (cons 4 nil))))
'(1 2 3 4) ; same as above


; ETYMOLOGY OF CAR & CDR

; Lisp was originally implemented on the IBM 704 computer, in the late 1950s. The 704 hardware had special support for splitting a 36-bit machine word into four parts, an "address part" and "decrement part" of 15 bits each and a "prefix part" and "tag part" of three bits each.

; Precursors to Lisp included functions:
;    car (short for "Contents of the Address part of Register number"),
;    cdr ("Contents of the Decrement part of Register number"),
;    cpr ("Contents of the Prefix part of Register number"), and
;    ctr ("Contents of the Tag part of Register number"),

; each of which took a machine address as an argument, loaded the corresponding word from memory, and extracted the appropriate bits.

; A machine word could be reassembled by cons, which took four arguments (a,d,p,t).

;; The prefix and tag parts were dropped in the early stages of Lisp's design, leaving CAR, CDR, and a two-argument CONS.



;;;;;;;;;;;;;;;;;;;;;;;;;;; SORTING LISTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;; HANDY STRING FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;

(length "hello")
(make-string 30 ?#) ; character literal starts with '?'
(concat "doo" "dee" "doo") ; concatenate strings

(upcase "dingle DANGLE")
(downcase "dingle DANGLE")
(capitalize "dingle DANGLE")

;; gets substring from start index to end index
(substring "boogaloo" 3 6)

;; NOTE: the second arg is actually a regular expression
(split-string "bing bong flip" " ")

;; concatenate strings and insert string in between
(mapconcat 'identity '("the" "cat" "sat" "on" "the" "mat") "--->")
(mapconcat 'upcase '("the" "cat" "sat" "on" "the" "mat") "--->")

;; insert a space in between each character
(upcase ; convert to uppercase
 (mapconcat 'identity ; identity returns the thing itself
            (mapcar '(lambda (char) (format "%c" char)) ; convert each char in seq to string
                    (append "aesthetic")) ; make string into list of characters
            "---")) ; string to insert in between



;;;;;;;;;;;;;;;;;;;;;;;;; FORMATTING STRINGS ;;;;;;;;;;;;;;;;;;;;;;;;;
; FUNCTION: format string &rest objects

(format "The value of fill-column is %d." fill-column)
(format "The name of this buffer is %s." (buffer-name))
(format "The buffer object prints as %s." (current-buffer))
(format "The buffer object prints as %S." (current-buffer)) ; quoted string representation
(format "Non-string objects can be converted to string e.g. list --> %s" '(1 2 3))
(format "The octal value of %d is %o, and the hex value is %x." 18 18 18)


;;;; FORMAT SPECIFICATIONS

; %s --- string - without quoting (i.e. using princ)
; %S --- string - with quoting (i.e. using prin1)

; %c --- character

; %o --- number - octal integer
; %d --- number - decimal integer
; %x --- number - hexadecimal integer (lowercase)
; %X --- number - hexadecimal integer (UPPERCASE)
; %e --- number - exponential notation (scientific notation)
; %f --- number - floating point decimal
; %g --- number - floating point decimal OR exponential notation, whichever is shorter

; %% --- insert `%'. Does NOT use a value. EXAMPLE: (format "%% %d" 30) returns "% 30".


;;;; PADDING
; Optional numeric prefix defines minimum width...

(format "%6d is padded on the left" 123)
(format "%06d is padded on the left with zeros" 123) ; this only works for left-padding
(format "%-6d is padded on the right" 123)

(format "The word `%7s' actually has %d letters in it."
        "foo" (length "foo")) ; left padding
(format "The word `%7s' actually has %d letters in it."
        "specification" (length "specification")) ; string representation is never truncated
(format "The word `%-7s' actually has %d letters in it."
        "foo" (length "foo")) ; right padding



;;;;;;;;;;;;;;;;;;;;;;;;; COMPARING EQUALITY ;;;;;;;;;;;;;;;;;;;;;;;;;

;; null
;; ... tests whether something is NIL

;; eq
;; ... tests whether args are identical symbols
;; ... 'a and 'a are equal
;; ... 1 and 1 are equal (symbol for integers should be identical)
;; ... 1.0 and 1.0 are NOT equal (floats probably not same symbol)
;; ... like comparing pointers in C of Java

;; eql
;; ... like eq, but handles numbers of same type
;; ... (eql 1.0 1.0) => t
;; ... (eql 1 1.0) => nil --- to compare numbers of different types use =

;; equal
;; ... true if parts of an object are the same
;; ... good for comparing lists, strings or other composite objects
(equal '(1 3 5) '(1 3 5))
(equal "hello" "hello")

;; equalp
(equalp "hello" "Hello")

;; =
;; ... only for numbers and can take multiple arguments
;; ... tests whether args are numerically equal, even if different type of number
;; ... ERROR, if argument is not number
;; ... 1 and 1.0 are equal
(= 1 1 1.0 (- 6 5))

;; string-equal
;; ... compare strings (regardless of text properties)
;; ... case-sensitive
;; (string-equal "bill" "bill") => t
;; (string-equal "bill" "Bill") => nil

;; equal-including-properties
;; ... like equal, but also requires that strings must have the same text properties



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BUFFERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; operate on buffer without showing it

(set-buffer "name-of-buffer or buffer object")

(with-current-buffer "name-of-buffer or buffer object" &rest body)

;; EXAMPLE

(with-current-buffer (find-file-noselect "blah.txt")
  (goto-line 5)
  (insert "Bannana Man!"))



;;;;;;;;;;;;;;;;;;;;;;;; LOOPS AND ITERATION ;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; DOTIMES

(dotimes (count 10) (insert "#")) ; ignoring the count variable here

(dotimes (n 10)
  (insert "\nCOUNT = " (number-to-string n)))

;;; dotimes - optional [result] param...
;;; ... in this example, I just pass result param to dotimes so that it gets returned unmodified...
;;; ... meanwhile, text is inserted...
(let ((result "This is the result!"))
  (dotimes (n 3 result)
    (insert (format "\nCOUNT = %d" n))))

;;; using [result] to add up sum of numbers...
;;; ... to make triangular numbers
(let ((result 0)
      (num-iterations 4))
  (dotimes (n (+ num-iterations 1) result)
    (progn
      (setq result (+ result n))
      (insert (format "\nCOUNT = %d --- TOTAL = %d" n result)))))





;(dotimes (n 10 (+ 2))
;  (insert (format "\nCOUNT = %d" n)))




;;;; DOLIST

(dolist (n '(32 45 12))
  (print (format "CURRENT NUM = %d" n)))



;;;; WHILE

; iterate through items of a list
(let ((animals '(giraffe lion zebra antelope)))
  (while animals ; TRUE while list is not empty
    (print (format "CURRENT ANIMAL = %s" (car animals))) ; print first item on list
    (setq animals (cdr animals)))) ; discard first item

; incrementing counter (SEE dotimes (above))
(let ((count -5))
  (while (< count 5)
    (print (format "count = %d" count))
    (setq count (+ count 1))))

(defun triangular-number (num-rows)
  "Calculate number of spots needed to build an equilateral triangle with any number of rows of spots."
  (let ((count 1)
        (total 0))
    (while (<= count num-rows)
      (setq total (+ total count))
      (setq count (+ count 1)))
    total)) ; 'total' is the last expression to be evaluated, therefore it's value is returned

;;;; USING THROW... CATCH TO BREAK OUT OF A LOOP EARLY
;; ... when exception is thrown, loop is broken and the specified value is returned
(let ((nums '(12 34 14 156 72)))
  (catch 'not-even
    (while nums
      (if (not (eq (% (car nums) 2) 0))
          (throw 'not-even "LOOP BROKEN: non-even value in list!"))
      (setq nums (cdr nums)))
  t))


(catch 'wrong-number
  (let ((n 32))
    (if (eq n (+ 2 30))
        (throw 'wrong-number "I don't like that number!"))))

(catch 'wrong-number
  (let ((n 33))
    (if (eq n (+ 2 30))
        (throw 'wrong-number "I don't like that number!"))))




;;;;;;;;;;;;;;;;;;;;;;;;;; COMBINING SEXPS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; progn evaluates each sexp in order and returns the result of the last one

(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (insert "hello!")
  (other-window 1)) ; switch back to this window

;; or use (let VAR-LIST BODY)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RECURSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; head or tail recursion...

(setq animals '(giraffe lion zebra antelope lion tiger))

; print elements of a list
(defun print-elements-recursively (list)
  (print (car list))
  (setq list (cdr list))
  (if list
      (print-elements-recursively list)))

; using recursion instead of a counter
(defun triangular-number-recursively (num-rows)
  (if (= 1 num-rows) ; do-again test
      1              ; then-part
    (+ num-rows      ; else-part
       (triangular-number-recursively ; recursive call
        (- num-rows 1)))))            ; next-step expression

; nice neat version using cond
(defun triangle-using-cond (number)
  (cond ((<= number 0) 0)
        ((= number 1) 1)
        ((> number 1)
         (+ number (triangle-using-cond (- number 1))))))



;;;;;;;;;;;;;;;;;;;;;; FUNCTIONAL FUNCTIONS... ;;;;;;;;;;;;;;;;;;;;;;;

;; APPLY and FUNCALL - use these to indirectly call functions

;; APPLY: applies a list as the argument list for executing a function

;; FUNCALL: calls a named function


;; EXAMPLE 1:

;; CONCAT joins strings together
(concat "bill" "ben" "weed")

;; This won't work because CONCAT expects strings, not a list...
(setq names-list '("Maria" "Pablo" "Jose" "Carlo"))
(concat names-list)

;; This DOES work!
;; NAMES-LIST is applied as the argument list...
;; ... whereas before the list was seen as the first argument in the list
(apply 'concat names-list)


;; EXAMPLE 2:

;; FUNCALL calls a function in the same format that you would normally use it.
(funcall 'concat "Stegosaurus" "Triceratops" "Parasaurolophus")

;; can be used to call a functions when given as argument to another function
(let ((some-func 'concat))
  (funcall some-func "Volvo" "Saab" "Maserati" "Lotus"))



;;;;;;;;;;;;;;;;;;;;;;; HIGHER ORDER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;

;; MAPCAR
;; (mapcar FUNCTION SEQUENCE)
;; ... applies FUNCTION to each item in SEQUENCE and returns transformed sequence
;; ... called mapcar because it repeatedly gets car from the list and applies the function to it
(mapcar '1+ '(2 4 6)) ; returns (3 5 7)

;; like mapcar, but for side-effects only...
;; ... returns the original list (doesn't accumulate results)
(mapc '1+ '(1 2 3)) ; completely pointless!
;; ... real-world usage...
(mapc #'disable-theme custom-enabled-themes)

;; REDUCE
;; (reduce FUNC SEQ)
;; ... FUNC is a two-argument function
;; ... reduce SEQ to a single value by repeatedly applying FUNC to first two elements
(reduce '(lambda (a b) (+ a b)) '(1 2 3)) ; returns 6



;;;;;;;;;;;;;;;;;;;;;;;;;;; RANDOM NUMBERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function: random &optional limit
(random)
(random 500) ; random integer between 0 and 499
(random t) ; seed pseudo-random number with current time
(random "doink!") ; seed with string (will get same number each time)

;; random item from a list
(let* ((items '(a b c d e)))
         (nth (random (length items)) items))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEBUGGING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lisp debugger

;; edebug - source-level debugger

;; The ordinary Lisp debugger provides the ability to suspend evaluation of a
;; form. While evaluation is suspended (a state that is commonly known as a
;; break), you may examine the run time stack, examine the values of local or
;; global variables, or change those values. Since a break is a recursive edit,
;; all the usual editing facilities of Emacs are available; you can even run
;; programs that will enter the debugger recursively. See Recursive Editing.

;;;; BUGGY TEST PROGRAM
(defun triangle-bugged (number)
  "Return sum of numbers 1 through NUMBER inclusive."
  (let ((total 0))
    (while (> number 0)
      (setq total (+ total number))
      (setq number (1= number)))      ; Error here.
    total))

(triangle-bugged 3)



;;;; DEBUG-ON-ERROR
(setq debug-on-error t)
(setq debug-on-error nil)

;;;; DEBUG-ON-ENTRY
; M-x debug-on-entry ... [you will be prompted to enter function name]
; M-x cancel-debug-on-entry
; ... debugger will start as soon as function is entered ---> press 'd' to advance one step...

;;;; DEBUG-ON-QUIT
; enter debugger on keyboard-quit (Ctrl+g) - maybe useful for infinite loops...
(setq debug-on-quit t)
(setq debug-on-quit nil)



;;;; EDEBUG BASICS
;; Nic Ferrier debugging tutorial video

;; ... try it on the function below...

;; STEP 1: instrument a function by positioning cursor inside it, then typing:
;; C-u ESC C-x

;; STEP 2: run the function

;; STEP 3: step through using SPACE key...
;; ... cursor and arrow at side of window show what part is being evaluated...
;; ... result of each expression is shown in minibuffer (if there is any output)

;; OTHER CONTROLS:
;; space - step
;; q     - quit
;; g     - evaluate to end
;; h - HERE! i.e. eval up to this point, then stop
;; b - set breakpoint at cursor --> press g to go to next breakpoint, press G to skip right to end
;; i - jump into function at point (& step through)

(defun bsc-split-string-convert-paragraph-breaks (text)
  "Splits TEXT on whitespace whilst converting paragraph breaks to [BREAK].

Paragraph break is interpreted as being two or more consecutive
newlines, optionally with whitespace in between them. Any single
newline characters along with any other whitespace characters are
discarded."
  (let ((pb "[BREAK]"))
    (seq-filter
     (lambda (x) (not (string-empty-p x)))
     (split-string
      (replace-regexp-in-string "\n[ *\n]+" (concat " " pb " ") text)
      "[ \n\f\t\r\v]+"))))

(bsc-split-string-convert-paragraph-breaks
 " \nbannana man\n\n \n \nflipping burgers\nbehind the bike shed \n \nso what!\n ")



;;;; DEBUGGING MACROS:

;; macroexpand

;; macrostep



;;;;;;;;;;;;;;;;;;;;;;;;;;;; UNIT TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ERT (Emacs Regression Testing)



;; BASIC USAGE: Put tests in source code files - test cases will be defined when
;; file is loaded

(ert-deftest addition-test ()
  (should (= (+ 1 2) 4))
  (should (= (+ 3 -2) 1)))



;; each ert-test may contain multiple should statements...

;; ...

;; M-x ert ; run all tests

;;;; in ert buffer: (these commands operate on test under cursor)
;; D ; ert-delete-test
;; l ; show list of 'should' forms executed in test
;; L ; show more (long expressions may be clipped)
;; T ; show time taken by test
;; h ; docstring for test
;; R ; re-run test

;; M-x bsc-ert-list-tests ; show list of all currently loaded tests


;;;; TROUBLESHOOTING ;;;;

;; Failing tests ABORT instead of fail, so the rest of the tests don't get run...
;; ... maybe some other package is interfering with ert - try starting emacs with -Q,
;; ... also, disable once package at a time in .emacs...




;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROFILING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-x profiler-start

;; M-x profiler-report ; generate report buffer

;; M-x profiler-stop



;;;; in profiler report buffer:
;; j = jump to function definition
;; d = view function documetation





;;;; TO CHECK TO SPEED OF INDIVIDUAL LISP FUNCTIONS ;;;;

;; benchmark-run

;; benchmark-run-compiled





;;;;;;;;;;;;;;;;;;;;;;;;;;; TEST PROGRAMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun times-tables-test (factor1)
  "Function: times-tables-test factor

Prompts user to answer a times tables question..."
  (interactive "nWhich table shall we test? ")
  (let* ((factor2 (random 13))
	 (result (* factor1 factor2))
	 (question (format "%d x %d = " factor1 factor2))
	 (answer (string-to-number (read-from-minibuffer question))))
    (print (format "... actual answer is %d" result))
    (print (if (= answer result) "CORRECT" "FAIL!"))))







;;;;;;;;;;;;;;;;;;;;;; EMACS MOVEMENT FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;

;;;; GETTING CURSOR POSITION ;;;;
(point) ; returns point in current buffer, as an integer
(current-column) ; integer



;;;; MOVING AROUND

(move-to-column INTEGER) ; go to column in currrent line

;; preserve {point, mark, current buffer} so that the cursor doesn't end up moved after function runs...
(save-excursion
  ;; lisp code here involving moving cursor, mark, changing buffer.
)




;;;; INTERACTIVE FUNCTIONS THAT WORK ON REGIONS ;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: Use the 'r' option to automatically pass the start & end points of the region in to the function...
;; ... 'interactive' function with 'r' should name two input args - the funtion does no I/O, but automatically passes the start and end points to the input args...
;; ... otherwise, can use region-beginning and region-end...

;; NOTE: See use-region-p below, to choose between region and the whole buffer...

(defun region-contents (rStart rEnd)
  "Prints region contents of region selected, and the start and end positions of that region"
  (interactive "r") ; region mode
  (setq rStr (buffer-substring rStart rEnd)) ; get region text
  (message "The string of the selected region is %s.  It starts at  %d and ends at %d"  rStr  rStart rEnd))

;; ITERATE THROUGH WORDS IN REGION
(defun reverse-words-region-message (rStart rEnd)
  "Reverses order of words and prints the result in the message area"
  (interactive "r")
  (setq myStr (buffer-substring rStart rEnd))
  (setq myWords (split-string myStr)) ; split string in to a list of words
  (setq reversed (reverse myWords)) ; reverse a list
  (message (mapconcat 'identity reversed " "))) ; convert list into string

(defun reverse-words-region (rStart rEnd)
  "Reverses order of words in place"
  (interactive "r")
  (setq myStr (buffer-substring rStart rEnd))
  (delete-region rStart rEnd) ; delete region after capturing text from buffer
  (setq myWords (split-string myStr))
  (setq reversed (reverse myWords))
  (insert (mapconcat 'identity reversed " "))) ; use insert instead of message

;; ITERATE THROUGH LETTERS IN REGION

(defun region-seperator (rStart rEnd)
  "Separates each character in region with a hyphen"
  (interactive "r")
  (setq rStr (buffer-substring rStart rEnd))
  (delete-region rStart rEnd)
  (insert (mapconcat 'string rStr "-")))

;; ENCLOSE A REGION

(defun myMark-elisp-region (rStart rEnd)
  "Mark region as Elisp source code for org mode export."
  (interactive "r")
  (save-excursion ; cursor etc will not be moved after function has run
    (goto-char rEnd) (insert "\n#+END_SRC\n") ; insert at end first, so that start index is still correct
    (goto-char rStart) (insert "#+BEGIN_SRC emacs-lisp -n\n")))


;; FIND AND REPLACE IN A REGION

(defun strip-smart-quotes (rStart rEnd)
  "Replace smart quotes with plain quotes in text"
  (interactive "r")
  (save-restriction ; so that the effect of narrow-to-region won't persist after the function ends
    (narrow-to-region rStart rEnd) ; restricts editing to only this region
    (goto-char (point-min)) ; go to start of file
    ; incrementally search for double-smart-quotes and replace each match with plain quotes
    (while (re-search-forward "[“”]" nil t) (replace-match "\"" nil t))
    (goto-char (point-min))
    (while (re-search-forward "[‘’]" nil t) (replace-match "'" nil t))))


;; CHOOSE BETWEEN OPERATING ON A REGION OR THE WHOLE BUFFER

(defun region-or-buffer ()
  "Strip smart quotes from region, or whole buffer if region not set"
  (interactive)
  (save-excursion
    (save-restriction
      ; if a region is active, narrow-to-region...
      (when (use-region-p) (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (while (re-search-forward "[“”]" nil t) (replace-match "\"" nil t))
      (goto-char (point-min))
      (while (re-search-forward "[‘’]" nil t) (replace-match "'" nil t)))))





;;;; REGULAR EXPRESSIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns the index of the first match
(string-match "f" "bran flakes flavour") ; ==> 5
;; can include start point
(string-match "f" "bran flakes flavour" 6) ; ==> 12

;; search and replace regex in a string
(replace-regexp-in-string "fo+" "baz" "da foo is foobared")

;; replacing newlines
(replace-regexp-in-string "\n" "--->" "da foo
 is
 foobared")

;; search and replace regex in REGION of a buffer
(save-restriction
  (narrow-to-region (mark) (point))
  (while (re-search-forward "^[ *]*")
    ;; NOTE: the latest match can be got with match-string
    (replace-match " * ")))

;;;; REGEXP RULES

"." ; any single character except newline

"*" ; postfix operator - match the preceeding element repetitively any number of times
"+" ; like *, but must match at least once
"?" ; like * and + but can match once or not at all
"*?" "+?" "??" ; non-greedy versions of the above
"\{N\}" ; postfix operator specifiying exactly N repeated matches
"\{N,M\}" ; postfix operator specifiying between N and M repeated matches

"[abc]" ; character set: a, b, & c
"[a-z]" ; character set: a to z
"[a-zA-Z0-9]" ; character set: all upper and lower case letters and numbers
"[^abc]" ; character set: all characters EXCEPT a, b, & c
"[^a-zA-Z0-9]" ; character set: all characters EXCEPT ascii letters and digits

"^" ; beginning of line
"$" ; end of line
"\b" ; beginning or end of a word

"\w" ; any word-constituent character (see SYNTAX TABLES)
"\W" ; any non-word-constituent character (see SYNTAX TABLES)

;; character classes (elisp only)
"[:space:]" ; any whitespace character (dependent on SYNTAX TABLE)

;; CAPTURE GROUPS
;; use \( and \) to capture groups
;; \1 refers to the string matched by the first group
;; \2 etc
;; ... can use these group references when doing replacement
(replace-regexp-in-string "\\(b\\)[a-z]\\(b\\)" "\\1BLAH\\2" "bheb bob barnard babadooley")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; search and replace
(while (search-forward "hello")
  (replace-match "bonjour"))

;; same, but doesn't throw an error when it reaches end of buffer
(while (search-forward "hello" nil t)
  (replace-match "bonjour"))

;;
(defun boldify-names ()
  "Switched to buffer named *test* and makes bold any name
immediately following 'Bonjour'."
    (switch-to-buffer-other-window "*test*")
    (goto-char (point-min))
    (while (re-search-forward "Bonjour \\(.+\\)!" nil t)
      (add-text-properties (match-beginning 1)
                           (match-end 1)
                           (list 'face 'bold)))
    (other-window 1))



;;;; RANDOM FUN THINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-background-color "yellow") ; change background colour

;; mapconcat
(mapconcat 'string "SEPARATE" "-") ; interpose dashes in between each letter

(string-to-list "hello") ; output a list of ascii character codes



;;;; GLOSSARY/DEFINITIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

alist ; association list ; Maps keys to values like a dictionary in python.  It is a list of cons cells called associations: the CAR of each cons cell is the key, and the CDR is the associated value.

plist ; property list



;;;;;;;;;;;;;;;;;;; OBJECT ORIENTED ELISP - EIEIO ;;;;;;;;;;;;;;;;;;;;
;;; EIEIO is Enhanced Implementation of Emacs Interpreted Objects...
;;; ... it is an object oriented layer for elisp based on...
;;; ... based on CLOS (Common Lisp Object System)


;;;; DEFINING CLASSES

;;; minimum class definition...
(defclass class0 () ())
(defclass class0.1 () () "Same, but with optional docstring.")


;;; a bit more...
(defclass class1 () ; no superclass
  ;; slots
  ((name :initarg :name ; symbol to be used for keyword args in constructor
         :initform "default-name"
         :type string ; only accept string type
         :documentation "Optional docstring.")
   (slot2 :documentation
          "This slot can hold any type of lisp object, and is
          UNBOUND by default")))


;;; with inheritance...
(defclass class2 (class1) ; inherit from superclass class1
  ((slot2 :initarg :anything ; initarg can be different from slot name
          :initform 320
          :documentation "Overrides slot2 from superclass")
   (slot3 :initarg :num
          :initform 19.1
          :type float))
  "Docstring for the class")



;;; with a method...
(defclass class3 ()
  ((name :initarg :name
         :initform "DEFAULT NAME"
         :type string)
   (num1 :initarg :num1
         :initform 3
         :type integer)
   (num2 :initarg :num2
         :initform 12
         :type integer)))

;;; ... define a method for the class...
(defmethod multiply-nums
  ((obj class3) ; class
   arg1 arg2)   ; optional additional arguments
  "Takes a class3 instance - multiplies it's two 'num' slots, then adds on arg1 and arg2."
  (+ arg1
     arg2
     (* (oref obj num1) (oref obj num2))))

;;; ... instantiate the class with default values...
(setq c3inst (class3))
;;; ... call method...
(multiply-nums c3inst 4 -1) ; gives 39 ---> (3 * 12) + 4 - 1


;;; class with initialization method...
(defclass class4 ()
  ((a :initarg :size
      :initform 10
      :type integer
      :documentation "The size of the internal array."))
  "Class encapsulates an array of size specified in constructor.")





(initialize-instance c1 '((:internal-array [1 2 3 4 5])))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; very simple macro, works like the ++ operator to increment value of
;; a variable by 1
(defmacro incr (var)
  (list 'setq var (list '1+ var)))
;; ... when called with `(incr x)`, the expansion would be...
;; ... (`setq x (1+ x))

;; ... to see expanded form of a macro, use macroexpand...
(macroexpand '(incr x))
;; ... if macro contains references to other macros, use
;; macroexpand-all to expand all macros in form...


;;;; FURTHER STUDY...

https://mullikine.github.io/posts/macro-tutorial/







;;;;;;;;;;;;;;;;;;;;;;;; WRITING EMACS MODES ;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;;;;; SYMBOL TABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; from http://ergoemacs.org/emacs/elisp_syntax_table.html

;; Emacs has a concept of Syntax Table. The basic idea is, each character (every Unicode character), is categorized into a class. Classes are: letters, punctuations, brackets, char for programing language identifiers, comment character, string delimiters, etc.

;; Syntax table is heavily used in emacs. For example,

;; - Most cursor movement commands rely on syntax table. For example, when you Alt+x forward-word 【Alt+f】, emacs will move cursor until it reaches a character that's not in the “word” class.
;;    Syntax coloring of strings and comments rely on syntax table. 〔►see Elisp: How to Color Comment in Major Mode〕
;;    Programing language comment/uncomment command also relies on syntax table.
;;    Lisp mode's parenthesis navigation also depends on syntax table. 〔►see Emacs: Navigate Lisp Code as Tree〕

;; Each buffer has its own version of syntax table. Typically, when a major mode is activated, it changes the current buffer's syntax table.

(describe-syntax) ; displays syntax table for current buffer





;;;;;;;;;;;;;;; EXTENDING/RE-USING OTHER PEOPLES CODE ;;;;;;;;;;;;;;;;

;; simplest (pointless) example using ivy-read
(defun bsc-ivy-test ()
  (interactive)
  (ivy-read "Choose one from the list:"
            '("red" "green" "blue" "orange" "purple" "brown")
            :action (lambda (x) (message "You chose %s!" (upcase x)))))



;; this is my custom theme-switching function in it's original version, and then
;; in much simpler version re-written to utilise ivy to select from the list of
;; themes

;; variables etc required for function...

(defvar *bsc-themes-by-darkness* '(alect-light
                                   dracula
                                   paganini
                                   toxi
                                   cyberpunk)
  "list of themes ordered from light to dark")

(defadvice load-theme (before theme-dont-propogate activate)
  "disable other theme before loading new one"
  (mapc #'disable-theme custom-enabled-themes))


;; OLD VERSION
(defun bsc-choose-theme (num)
  "Switches themes from a list of pre-defined themes organised by darkness of colouring.
Choose from 1 to 5 --> 1 is lightest, 5 is darkest."
  (interactive "nChoose theme by darkness (1-5): ")
  (if (and (> num 0)
           (<= num (length *bsc-themes-by-darkness*)))
      (let ((new-theme (nth (- num 1) *bsc-themes-by-darkness*)))
        (load-theme new-theme)
        (message (format "Theme chosen: %s" new-theme)))
    (message (format "number out of range: %d" num))))

;; NEW VERSION (see documentation for ivy-read...)
(defun bsc-choose-theme ()
  "Switches theme, prompting user to choose from a pre-determined list."
  (interactive)
  (ivy-read "Switch to theme (arranged light to dark):"
            *bsc-themes-by-darkness*
            ;; INTERN gets the SYMBOL whose name is X
            ;; ... otherwise we're just passing a string
            :action (lambda (x) (load-theme (intern x)))))



;;;;;;;;;;;;;;;;;;;;;;; CUSTOM BBDB EXTENSIONS ;;;;;;;;;;;;;;;;;;;;;;;

;;; returns a list of all records where the name contains the string "chambers"
(bbdb-search (bbdb-records) :all-names "chambers")

;;; gets the same list and inserts it into a new buffer
(progn
  (switch-to-buffer-other-window "*results-list*")
  (insert (format "%s" (bbdb-search (bbdb-records) :all-names "chambers"))))

;;; same, but inserts each record in the list in a line of it's own
(progn
  (switch-to-buffer-other-window "*results-list*")
  (let ((counter 1)
        (records (bbdb-search (bbdb-records) :all-names "chambers")))
    (while records
      (insert (format "%s: %s\n\n" counter (car records)))
      (setq counter (+ 1 counter))
      (setq records (cdr records)))))

;;; same, but prints only the name
(progn
  (switch-to-buffer-other-window "*results-list*")
  (let ((counter 1)
        (records (bbdb-search (bbdb-records) :all-names "chambers")))
    (while records
      (insert (format "%s: %s\n\n" counter (bbdb-record-name (car records))))
      (setq counter (+ 1 counter))
      (setq records (cdr records)))))

;;; same, but prints NAME & EMAIL
(progn
  (switch-to-buffer-other-window "*results-list*")
  (let ((counter 1)
        (records (bbdb-search (bbdb-records) :all-names "chambers")))
    (while records
      (insert (format "%s: %s --> %s\n\n"
                      counter
                      (bbdb-record-name (car records))
                      (bbdb-record-field (car records) 'mail)))
      (setq counter (+ 1 counter))
      (setq records (cdr records)))))

;;; only shows records with DOB field - prints NAME & DOB
(progn
  (switch-to-buffer-other-window "*results-list*")
  (erase-buffer) ; erase any existing content in results buffer
  (let ((counter 1)
        ;; (records (bbdb-search (bbdb-records) :xfield '('dob . "^[0-9]+"))))
        ;; (records (bbdb-search (bbdb-records) :xfield (cons 'url ".+"))))
        (records (bbdb-search (bbdb-records) :xfield (cons 'dob ".+"))))
        ;; (records (bbdb-records)))
    (while records
      (insert (format "%s: %s --> DOB: %s\n\n"
                      counter
                      (bbdb-record-name (car records))
                      (bbdb-record-field (car records) 'mail)
                      ;; (bbdb-record-field (car records) 'url)))
                      (bbdb-record-field (car records) 'dob)))
                      ;; (bbdb-record-xfields (car records))))
      (setq counter (+ 1 counter))
      (setq records (cdr records)))))

;;; same, but sorts list by DOB (month, then day, ignoring the year)
(progn
  (switch-to-buffer-other-window "*results-list*")
  (erase-buffer) ; erase any existing content in results buffer
  (let ((counter 1)
        (records
         (seq-sort
          ;; the sorting function should return non-nil if A comes before B
          ;; This function takes two bbdb records and compares the DOB field
          (lambda (a b)
            ;; get the two DOB field values
            (let* ((return-val nil)
                   (dob-a (bbdb-record-field a 'dob))
                   (dob-b (bbdb-record-field b 'dob))
                   ;; extract the month and day from DOB
                   (month-a (string-to-number (substring dob-a 3 5)))
                   (month-b (string-to-number (substring dob-b 3 5)))
                   (day-a (string-to-number (substring dob-a 0 2)))
                   (day-b (string-to-number (substring dob-b 0 2))))
              ;; compare month
              (if (< month-a month-b)
                  (setq return-val 't))
              ;; if month is equal, then compare day
              (if (and (= month-a month-b) (< day-a day-b))
                  (setq return-val 't))
              return-val))

         (bbdb-search (bbdb-records) :xfield (cons 'dob ".+")))))
    (while records
      (insert (format "%s: %s --> DOB: %s\n\n"
                      counter
                      (bbdb-record-name (car records))
                      ;; (bbdb-record-field (car records) 'mail)
                      (bbdb-record-field (car records) 'dob)))
      (setq counter (+ 1 counter))
      (setq records (cdr records)))))

;;; same, but also finds the position of the current date relative to the birthdays
(progn
  (switch-to-buffer-other-window "*results-list*")
  (erase-buffer) ; erase any existing content in results buffer
  (let ((counter 1)
        (current-position 0)
        (current-month 0)
        (current-day 0)

        ;; Get sorted list of all records which have a DOB field
        (records
         (seq-sort
          ;; The sorting function should return non-nil if A comes before B.
          ;; This function takes two bbdb records and compares the DOB field.
          (lambda (a b)
            ;; get the two DOB field values
            (let* ((return-val nil)
                   (dob-a (bbdb-record-field a 'dob))
                   (dob-b (bbdb-record-field b 'dob))
                   ;; extract the month and day from DOB
                   (month-a (string-to-number (substring dob-a 3 5)))
                   (month-b (string-to-number (substring dob-b 3 5)))
                   (day-a (string-to-number (substring dob-a 0 2)))
                   (day-b (string-to-number (substring dob-b 0 2))))
              ;; compare month
              (if (< month-a month-b)
                  (setq return-val 't))
              ;; if month is equal, then compare day
              (if (and (= month-a month-b) (< day-a day-b))
                  (setq return-val 't))
              return-val))
          (bbdb-search (bbdb-records) :xfield (cons 'dob ".+")))))

    ;; get current date
    (let* ((time-str (current-time-string))
           (month-str (downcase (substring time-str 4 7))))
      ;; set day
      (setq current-day (string-to-number (substring time-str 8 10)))
      ;; set month
      (if (equal month-str "jan") (setq current-month 1))
      (if (equal month-str "feb") (setq current-month 2))
      (if (equal month-str "mar") (setq current-month 3))
      (if (equal month-str "apr") (setq current-month 4))
      (if (equal month-str "may") (setq current-month 5))
      (if (equal month-str "jun") (setq current-month 6))
      (if (equal month-str "jul") (setq current-month 7))
      (if (equal month-str "aug") (setq current-month 8))
      (if (equal month-str "sep") (setq current-month 9))
      (if (equal month-str "oct") (setq current-month 10))
      (if (equal month-str "nov") (setq current-month 11))
      (if (equal month-str "dec") (setq current-month 12)))

    ;; find soonest birthday relative to today's date...
    ;; ... rotate records list until position is found

    (while records
      (let* ((current (car records))
             (name (bbdb-record-name current))
             (dob (bbdb-record-field current 'dob))
             (month (string-to-number (substring dob 3 5)))
             (day (string-to-number (substring dob 0 2))))

        (insert (format "%s: %s --> DOB: %s\n\n"
                        counter
                        name
                        dob))

        (if (< month current-month) (setq current-position (+ 1 current-position)))
        (if (and (= month current-month) (< day current-day))
            (setq current-position (+ 1 current-position))))

      (setq counter (+ 1 counter))
      (setq records (cdr records)))

    (insert (format "CURRENT MONTH: %s\n" current-month))
    (insert (format "CURRENT DAY: %s\n" current-day))
    (insert (format "CURRENT POSITION: %s\n" current-position))))

;;; same, but displays sorted list starting from soonest birthday relative to the current date
(progn
  (switch-to-buffer-other-window "*results-list*")
  (erase-buffer) ; erase any existing content in results buffer
  (let ((counter 1)
        (current-position 0)
        (current-month 0)
        (current-day 0)

        ;; Get sorted list of all records which have a DOB field
        (records
         (seq-sort
          ;; The sorting function should return non-nil if A comes before B.
          ;; This function takes two bbdb records and compares the DOB field.
          (lambda (a b)
            ;; get the two DOB field values
            (let* ((return-val nil)
                   (dob-a (bbdb-record-field a 'dob))
                   (dob-b (bbdb-record-field b 'dob))
                   ;; extract the month and day from DOB
                   (month-a (string-to-number (substring dob-a 3 5)))
                   (month-b (string-to-number (substring dob-b 3 5)))
                   (day-a (string-to-number (substring dob-a 0 2)))
                   (day-b (string-to-number (substring dob-b 0 2))))
              ;; compare month
              (if (< month-a month-b)
                  (setq return-val 't))
              ;; if month is equal, then compare day
              (if (and (= month-a month-b) (< day-a day-b))
                  (setq return-val 't))
              return-val))
          (bbdb-search (bbdb-records) :xfield (cons 'dob ".+")))))

    ;; get current date
    (let* ((time-str (current-time-string))
           (month-str (downcase (substring time-str 4 7))))
      ;; set day
      (setq current-day (string-to-number (substring time-str 8 10)))
      ;; set month
      (if (equal month-str "jan") (setq current-month 1))
      (if (equal month-str "feb") (setq current-month 2))
      (if (equal month-str "mar") (setq current-month 3))
      (if (equal month-str "apr") (setq current-month 4))
      (if (equal month-str "may") (setq current-month 5))
      (if (equal month-str "jun") (setq current-month 6))
      (if (equal month-str "jul") (setq current-month 7))
      (if (equal month-str "aug") (setq current-month 8))
      (if (equal month-str "sep") (setq current-month 9))
      (if (equal month-str "oct") (setq current-month 10))
      (if (equal month-str "nov") (setq current-month 11))
      (if (equal month-str "dec") (setq current-month 12)))

    ;; find soonest birthday relative to today's date...
    (dolist (current records)
      (let* ((name (bbdb-record-name current))
             (dob (bbdb-record-field current 'dob))
             (month (string-to-number (substring dob 3 5)))
             (day (string-to-number (substring dob 0 2))))

        (if (< month current-month) (setq current-position (+ 1 current-position)))
        (if (and (= month current-month) (< day current-day))
            (setq current-position (+ 1 current-position)))))

    ;; rotate list by the required number of steps
    (dotimes (_ current-position)
             (let ((first-record (car records)))
               ;; remove from start
               (setq records (cdr records))
               ;; add to end
               (setf (cdr (last records)) (cons first-record nil))))

    ;; insert opening message
    (insert (format "CURRENT TIME: %s\n\n" (current-time-string)))
    (insert (format "UPCOMING BIRTHDAYS:\n\n"))

    ;; insert formatted list in buffer
    ;; ... iterate list destructively
    (while records
      (let* ((current (car records))
             (name (bbdb-record-name current))
             (dob (bbdb-record-field current 'dob))
             (month (string-to-number (substring dob 3 5)))
             (day (string-to-number (substring dob 0 2))))

        (insert (format "%s: %s --> DOB: %s\n\n"
                        counter
                        name
                        dob))

        (setq counter (+ 1 counter))
        (setq records (cdr records))))

    (insert (format "CURRENT MONTH: %s\n" current-month))
    (insert (format "CURRENT DAY: %s\n" current-day))
    (insert (format "CURRENT POSITION: %s\n" current-position))))



;;; more compact/nicer version

(defun bsc-bbdb--sort-by-dob-ignore-year (a b)
  "Sorting-function for use with SEQ-SORT. Compares two BBDB
  records by 'dob' (date of birth) field. Compares by month and
  day, ignoring the year of birth.

Returns non-nil if A comes before B."
  ;; get the two DOB field values
  (let* ((return-val nil)
         (dob-a (bbdb-record-field a 'dob))
         (dob-b (bbdb-record-field b 'dob))
         ;; extract the month and day from DOB
         (month-a (string-to-number (substring dob-a 3 5)))
         (month-b (string-to-number (substring dob-b 3 5)))
         (day-a (string-to-number (substring dob-a 0 2)))
         (day-b (string-to-number (substring dob-b 0 2))))
    ;; compare month
    (if (< month-a month-b)
        (setq return-val 't))
    ;; if month is equal, then compare day
    (if (and (= month-a month-b) (< day-a day-b))
        (setq return-val 't))
    return-val))

(defun bsc-bbdb--month-str-to-number (month-str)
  (let ((str (downcase month-str))
        (return-val 0))
      (if (equal str "jan") (setq return-val 1))
      (if (equal str "feb") (setq return-val 2))
      (if (equal str "mar") (setq return-val 3))
      (if (equal str "apr") (setq return-val 4))
      (if (equal str "may") (setq return-val 5))
      (if (equal str "jun") (setq return-val 6))
      (if (equal str "jul") (setq return-val 7))
      (if (equal str "aug") (setq return-val 8))
      (if (equal str "sep") (setq return-val 9))
      (if (equal str "oct") (setq return-val 10))
      (if (equal str "nov") (setq return-val 11))
      (if (equal str "dec") (setq return-val 12))
      return-val))

(defun bsc-bbdb-list-birthdays ()
  "Examines the BBDB database and lists all birthdays in order of
  the soonest from the current date. Any record which doesn't
  have a 'dob' field is ignored."
  (interactive)
  ;; get sorted list of records and current date
  (let* ((records (seq-sort
                   'bsc-bbdb--sort-by-dob-ignore-year
                   (bbdb-search (bbdb-records) :xfield (cons 'dob ".+"))))
         (time-str (current-time-string))
         (current-month (bsc-bbdb--month-str-to-number (substring time-str 4 7)))
         (current-day (string-to-number (substring time-str 8 10)))
         (current-position 0))

    ;; find soonest birthday relative to today's date and count the number of steps
    (dolist (current records)
      (let* ((name (bbdb-record-name current))
             (dob (bbdb-record-field current 'dob))
             (month (string-to-number (substring dob 3 5)))
             (day (string-to-number (substring dob 0 2))))
        ;; step on current position if date is earlier than current date
        (if (or (< month current-month)
                (and (= month current-month) (< day current-day)))
            (setq current-position (+ 1 current-position)))))

    ;; rotate list by the required number of steps
    (dotimes (_ current-position)
             (let ((first-record (car records)))
               ;; remove from start
               (setq records (cdr records))
               ;; add to end
               (setf (cdr (last records)) (cons first-record nil))))

    ;; display results in new buffer
    (switch-to-buffer-other-window "*upcoming-birthdays*")
    (erase-buffer) ; erase any existing content in results buffer

    ;; headling message
    (insert (format "UPCOMING BIRTHDAYS (%d found in bbdb database)\n\n"
                    (length records)))
    (insert (format "Current time: %s\n\n" (current-time-string)))

    ;; insert formatted list in buffer, iterating list destructively
    (let ((counter 1)
          (new-year-mark (- (length records) current-position))
          (beginning (point)))
      (while records
        (let* ((current (car records))
               (name (bbdb-record-name current))
               (dob (bbdb-record-field current 'dob))
               (month (string-to-number (substring dob 3 5)))
               (day (string-to-number (substring dob 0 2))))

          (insert (format "%s: %s DOB: %s\n"
                          counter
                          name
                          dob))

          (if (= counter new-year-mark)
              (insert (format "NEW YEAR\n")))

          (setq counter (+ 1 counter))
          (setq records (cdr records))))

      ;; align date of birth in column layout
      ;; NOTE: parenthesised sub-expression denotes the whitespace to be replaced
      (align-regexp beginning (point) "\\(\\s-*\\)DOB:"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TIME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(current-time-string)

(format-time-string "%H:%M")

(decode-time)

;; do something dependent on time of day
(let ((hour (string-to-number (format-time-string "%H"))))
  (cond 
   ((< hour 12) "BEFORE MIDDAY")
   (t "AFTER MIDDAY")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defun* curtain-fabric-calculator (width height &key (fabric-width 150) (width-ratio 1.5) (hem-size 3) (extra-top 30))
  "Calculate the length of fabric in cm needed to make curtains."
  (interactive)
  (let* ((adjusted-width (+ hem-size hem-size (* width width-ratio)))
         (adjusted-height (+ height hem-size extra-top))
        (num-strips (ceiling (/ adjusted-width fabric-width)))
        (total (* num-strips adjusted-height)))
    (message "%dcm fabric required for curtains %dcm wide & %dcm high --- BASED ON: fabric-width=%d, width-ratio=%f, hem-size=%d, extra-top=%d, adjusted-width=%d adjusted-height=%d)"
             total width height fabric-width width-ratio hem-size extra-top adjusted-width adjusted-height)))
