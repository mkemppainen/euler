(defun square (luku)
  (* luku luku))
(defun tri (luku)
  (* luku (/ (+ luku 1) 2)))

(defvar *luku*)

(defun kysy (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun kysy-luku ()
  (setf *luku* (kysy "anna luku:")j))

;;;;;;;;;;;;;;;

(defparameter *km-mile* 0.621381192)

(defun mile-to-km (km)
  (* km *km-mile*))

(defun average3 (luku1 luku2 luku3)
  (let* ((sum (+ luku1 luku2 luku3))
	 (result (/ sum 3)))
    result))

(defun cubesum (a b)
  (let* ((sum (+ a b)))
    (* sum sum sum)))

(defun pseudo-primes (x)
  (+ (* x x) (- x) 41))
	 
(defun swap (luvut)
  (cons (second luvut)
	(cons (first luvut)
	      (rest (rest luvut)))))

(defun rand (luvut)
  (nth (random (length luvut)) luvut))

(defun dup (luvut)
  (cons (first luvut) luvut))

(defun last-elt (lista)
  (nth (- (length lista) 1) lista))

(defun piglatin (word)
  (concatenate 'string (subseq word 1) (subseq word 0 1) "ay"))

(defun midverse (sana)
  (concatenate 'string (subseq sana 0 1)
	       (reverse (subseq sana 1 (- (length sana) 1)))
	       (subseq sana (- (length sana) 1))))

(defun rotate (sana maara)
  (concatenate 'string (subseq sana maara)
	       (subseq sana 0  maara)))

(defun  count-list (numerot)
  (if (null numerot) 0
      (+ 1 (count-list (rest numerot)))))

(defun reverse-list (sanat)
  (if (null sanat) nil
      (cons (reverse (first sanat)) (reverse-list (rest sanat)))))

(defun evenp-list (numerot)
  (if (null numerot) nil
      (cons (evenp (first numerot)) (evenp-list (rest numerot)))))

(defun max-list (numerot)
  (if (null (rest numerot)) (first numerot) 
      (max (first numerot) (max-list (rest numerot)))))

(defun dupli-list (elements)
  (if (null elements) nil
      (cons (first elements) (cons (first elements) (dupli-list (rest elements))))))

(defun compress (elements)
  (if (null elements) nil
      (if (equal (first elements) (second elements))
	  (compress (rest elements))
	  (cons (first elements)
		(compress (rest elements))))))

(defun interleave (list1 list2)
  (if (null list1) nil
      (cons (first list1) (cons (first list2) (interleave (rest list1) (rest list2))))))


(defun n-ekan-tulo (n luku)
  "Kertoo n kappaletta listan alusta keskenään"
  (if (eql n 0) 1
      (if (null (first luku)) 1
	  (* (first luku) (n-ekan-tulo (- n 1) (rest luku))))))

(defun multiply-n (n luku)
  "Multiplies n first numbers of a number string."
  (loop repeat n
     for i upto n
     for x = (digit-char-p (char luku 0)) then (* x (digit-char-p (char luku i)))
     finally (return x)))

(defun multiply-do (n luku)
  "Multiplies n first numbers of a number string."
  (let ((x 1))
    (dotimes (i (min n (length luku)))
      (setq x (* x (digit-char-p (char luku i)))))
    x))

(defun max-multiple (n luku)
  "Returns largest product of sequence of numbers of wanted length"
  (if (> n (length luku)) 0
      (max
       (multiply-n n (subseq luku 0 n))
       (max-multiple n (subseq luku 1)))))

(defun triplet (n sum)
  (declare (fixnum n sum))
  (let ((x))
    (dotimes (c n)
      (dotimes (b c)
	(dotimes (a b)
	  (progn
	    (when (and (test-triplet a b c) (test-sum-equals a b c sum))
	      (setf x (list a b c))
	      (return-from triplet (list a b c))))))
      (print c))
    x))

(defun test-triplet (a b c)
  (declare (fixnum a b c))
  (= (expt c 2) (+ (expt b 2) (expt a 2))))

(defun test-sum-equals (a b c sum)
  (declare (fixnum a b c sum))
  (= sum (+ a b c)))

(defun f (x y)
  "esimerkki homma"
   (declare (type fixnum x y))
   (let ((z (+ x y)))
     (declare (type fixnum z))
     (setf z (* 2 z))
     z))

(defun list-primes-defunct (n)
  (let ((primes (list 2)) (prime-count 1) (prime-canditate 3))
    (do ((a 3 (1+ a)))
	((>= a n) primes)
      (setf primes (cons a primes)))
    primes))

(defun list-primes (n)
  "returns array of primes smaller than n and their count"
  (declare (fixnum n))
  (when (< n 3) (return-from list-primes nil))
  (let ((primes (make-array 1000 :fill-pointer 0 :adjustable t :element-type 'integer)) (prime-count 1))
    (vector-push 2 primes)
    (do ((a 3 (1+ a)))
	((>= a n) primes)
      (when (test-prime a primes) (progn (vector-push-extend a primes) (incf prime-count))))
    primes))

(defun test-prime (canditate previous-primes)
  "tests whether a number is a prime by useing previous primes provided"
  (declare (integer canditate))
  (loop for a being the elements of previous-primes do
       (when (= (mod canditate a) 0) (return-from test-prime nil))
       (when (> a (sqrt canditate)) (return-from test-prime t))))

(defun test-prime-list (canditate previous-primes)
  "tests whether a number is a prime by using list of previous primes"
  (declare (integer canditate))
  (dolist (a previous-primes)
    (when (= (mod canditate a) 0) (progn (print (list a canditate)) (return-from test-prime-list nil)))
    (when (> a (sqrt canditate)) (progn (print (list a canditate)) (return-from test-prime-list t)))))

(defparameter bignum-string 
"37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690")

(defun sum-string (number-string)
  "sums numbers in a string split by any non digit character"
  (loop for x across number-string
     with sum = 0
     with string = (make-array 0
			       :element-type 'character
			       :fill-pointer 0
			       :adjustable t)
     if (digit-char-p x) do (vector-push-extend x string)
     else unless (= 0 (length string)) do (incf sum (parse-integer string))
     and do (setf (fill-pointer string) 0)
     finally (return (+ sum (parse-integer string)))))

(defun max-triangle-path (triangle)
  (let ((bottom (first triangle)) (above (second triangle)))
    (loop
	 for i = 0 then (1+ i)
	 for a = (nth i bottom)
	 for b = (nth (1+ i) bottom)
	 until (null b)
	 do (incf (nth i above) (max a b))
	 do (print b)
	 finally (when (rest triangle) (max-triangle-path (rest triangle)))
	 finally (return (last triangle)))))

(defun match-number-pattern (number)
  (declare (integer number))
  (let ((s (write-to-string number)))
    (dotimes (i 10)
      (unless (string= (elt s (1+ (floor i 2))) (write-to-string (- 9 i)))
	(return-from match-number-pattern nil))))
  number)

(defun euler-206()
  "Attemp at euler 206"
  (declare (optimize (speed 3) (safety 0)))
  (loop
     for i from 1010100000 upto 1389026600
     for p = (expt i 2)
     thereis (match-number-pattern p)))

;;;;;;;;;;;;;;;;  Euler 11  ;;;;;;;;;;;;;;;;;;;;;;

(defun split-number-string (number-string)
  "splits numbers from string into list, order reversed"
  (unless (vectorp number-string) (return-from split-number-string nil))
  (loop for x across number-string
     with list = ()
     with string = (make-array 0
			       :element-type 'character
			       :fill-pointer 0
			       :adjustable t)
     if (digit-char-p x) do (vector-push-extend x string)
     else unless (= 0 (length string)) do (push (parse-integer string) list)
     and do (setf (fill-pointer string) 0)
     finally (when (< 0 (length string)) (push (parse-integer string) list))
     finally (return list)))

(defun read-numbers (file)
  "Reads numbers separated by any non digit. Lines into their own lists."
  (with-open-file (stream file)
    (loop
       with numbers = ()
       for line = (read-line stream nil)
       while line do (push (reverse (split-number-string line)) numbers)
       finally (return-from read-numbers (reverse numbers))
       )))

(defun max-product-list (list n)
  "Finds largest product of n adjacent numbers"
  (when (null list) (return-from max-product-list))
  (let ((high 0) (cand 1))
    (dotimes (x (1+ (- (length list) n)))
      (dotimes (y n)
	(setf cand (* cand (nth (+ x y) list))))
      (setf high (max cand high))
      (setf cand 1))
    high))

(defun max-product-horizontal (2d-list n)
  "Finds max product of 2-dimensional list, product of n consecutive
   numbers in individual lists."
  (loop
     for list in 2d-list
     maximize (max-product-list list n)))

(defun max-product-vertical (2d-list n)
  "Find max product of a row of 2-dimensional list."
  (loop
     with l = (apply #'mapcar #'list 2d-list)
     for list in l
     maximize (max-product-list list n)))

(defun transpose-diagonally (2d-list)
  "Transpose diagonally upper triangle of 2-dimensional list"
  (loop
     with result = ()

     with down = 0
     for i from 0 upto (1- (length 2d-list))
     for up = i
     do (when (> i (1- (length 2d-list)))
	  (setf down (- (length 2d-list) 1))
	  (setf up (- i (length 2d-list) 1)))
     do (loop
	   with row = ()
	   for x from up downto down
	   for y from down upto up
	   do (push (nth x (nth y 2d-list)) row)
	   finally (push row result))
     do (setf down 0)
     finally (return result)))

(defun transpose-diagonally (2d-list)
  "Transpose 2-dimensional list diagonally i.e. 'rotate' it 45 degrees
  clockwise."
  (loop
     with result = ()

     with down = 0
     for i from 0 upto (- (* 2 (length 2d-list)) 2)
     for up = i
     do(when (> i (1- (length 2d-list)))
	 (incf down)
	 (setf up (1- (length 2d-list))))
       (loop
	  with row = ()
	  for x from up downto down
	  for y from down upto up
	  do (push (nth x (nth y 2d-list)) row)
	  finally (push row result))
     finally (return result)))

(defun max-product-diagonal-right (2d-list n)
  (max-product-horizontal (transpose-diagonally 2d-list) n))

(defun max-product-diagonal-left (2d-list n)
  (max-product-horizontal (transpose-diagonally (map 'list 'reverse 2d-list)) n))

(defun largest-product-in-grid (2d-list n)
  (max
   (max-product-horizontal 2d-list n)
   (max-product-vertical 2d-list n)
   (max-product-diagonal-right 2d-list n)
   (max-product-diagonal-left 2d-list n)))

(defun euler-11 ()
  (largest-product-in-grid (read-numbers "number-grid.txt") 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun permutations (list)
  (let ((result ())) ;slfjsdlkfj
    (if (= (length list) 1) (list)
	(progn
	  (dolist (x list) 
	    (push (permutations (rest list)) result)
	    (push (map 'list '* )))))))

;;;;;;;;;;;;;;;;;;;;Euler 15;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lattice-paths (h w)
  "Lattice paths i.e. no. of routes through a grid."
  (declare (fixnum h w) (optimize (speed 3) (safety 0)))
  (the fixnum
       (cond ((or (= 0 h) (= 0 w)) 1)
	     ((or (= 1 h) (= 1 w)) (+ h w))
	     ((= h w) (* 2 (lattice-paths h (1- w))))
	     (t (+ (lattice-paths h (1- w)) (lattice-paths (1- h) w))))))

;;;;;;;;;;;;;;;;;;;;Euler 14;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun collatz-length (pn)
  (loop
     with chain = 1
     with n = pn
     when (= n 1) do (return-from collatz-length chain)
     if (evenp n) do (setf n(/ n 2))
     else do (setf n (1+ (* 3 n)))
     do (incf chain))
  0)

(defun collatz (max-num)
  (declare (inline collatz-length))
  (loop 
     with start = 1
     with max-start = 1
     while (< start max-num)
     for cl = (collatz-length start)
     for max-chain = 0 then (max cl max-chain)
     when (= max-chain cl)
     do (setf max-start start)
     do (incf start)
     finally (return max-start)))

;;;;;;;;;;;;;;;;;;;;;;Euler 16;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun euler-16 ()
  "Count sum of digits of number 2^1000"
  (let ((s (write-to-string (expt 2 1000))))
    (loop
       for x across s
       summing (digit-char-p x))))
;;;;;;;;;;;;;;;;;;;;;;Euler 17;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun num-in (number &rest numbers) ;Much redundancy, many saved parentheses
  "Return first number if it is in the rest numbers, else nil."
  (find number numbers))

(defun letters-1s (number)
  "Return how many letters in a number from 0 to 9 (0 returns 0)"
  (cond
    ((num-in number 0) 0)
    ((num-in number 1 2 6) 3)
    ((num-in number 4 5 9) 4)
    ((num-in number 3 7 8) 5)))

(defun letters-0-19 (number)
  "Tell how many letters is in a number from 0 to 19 (0 returns 0)"
  (cond
    ((num-in number 0) 0)
    ((num-in number 1 2 6) 3)
    ((num-in number 4 5 9) 4)
    ((num-in number 3 7 8) 5)
    ((num-in number 10) 3)
    ((num-in number 11 12) 6)
    ((num-in number 15 16) 7)
    ((num-in number 13 14 18 19) 8)
    ((num-in number 17) 9)))

(defun letters-10s (number)
  "Return how many letters is in full tens,number being from 0 to 9 (0
   returns 0)"
  (cond
    ((num-in number 0) 0)
    ((num-in number 1) 3)
    ((num-in number 2 3 8 9) 6)
    ((num-in number 4 5 6) 5)
    ((num-in number 7) 7)))

(defun letters-upto-99 (num)
  (let
      ((1s (mod num 10))
       (10s (truncate num 10)))
    (if (< num 20) (letters-0-19 num)
	(+ (letters-1s 1s) (letters-10s 10s)))))

(defun letters-num (n)
  "Return how many letters a number from 1 to 999 has written as words"
  (let ((result 0))
    (let ((100s (mod (truncate n 100) 10))
	  (10s (mod (truncate n 10) 10))
	  (1s (mod n 10)))
      (when (< 0 100s) (incf result 7)                  ;"hundred" = 7 extra letters 
	    (when (or (< 0 10s) (< 0 1s)) (incf result 3))) ;"and" = 3 extra letters
      (incf result (letters-upto-99 100s))
      (incf result (if (< n 20) (letters-0-19 n)
		       (letters-upto-99 (mod n 100)))))
        result))


(defun euler-17 ()
  "Number of letters in numbers from 1 to 1000."
  (let ((result 0))
    (dotimes (n 1000)
      (incf result (letters-num n)))
    (incf result 11) ;for the "one thousand"
    result))

;;; all below for testing
(defun count-letters (s)
  (loop
     for x across s
     counting (alpha-char-p x)))

(defmacro test-letters (string num)
  "Test letters-sum function, with number and number as words"
  `(when (/= (letters-num ,num) (count-letters ,string))
     (format t "Function returned ~a, should have been ~a" (letters-num ,num) (count-letters ,string))
     t))

(defun test-letters-sum ()
  "Test for letters-num function. Return nil if all tests pass, else T."
  (test-letters "forty-four" 44)
  (test-letters "twenty-five" 25)
  (test-letters "seven" 7)
  (test-letters "one hundred and seventy-six" 176)
  (test-letters "five hundred and forty " 540)
  (test-letters "sixty-six" 66)
  (test-letters "nine hundred and ninety nine" 999)
  (test-letters "one hundred and fifty-five" 155)
  (test-letters "thirty" 30)
  (test-letters "five-hundred" 500)
  (test-letters "twenty-seven" 27))

;one   3            ten      3    twenty 6 
;two   3	    eleven   6	  thirty 6 
;three 5	    twelve   6	  forty  5
;four  4	    thirteen 8	  fifty  5 
;five  4	    fourteen 8	  sixty  5 
;six   3	    fifteen  7	  seventy7 
;seven 5	    sixteen  7	  eighty 6 
;eight 5	    seventeen9	  ninety 6 
;nine  4	    eighteen 8
;                   nineteen 8
;hundred 7

;;; VERSION 2.0 ;;;

(defun euler-17-advanced-edition ()
  "Calculate sum of letters in written out numbers 1 to 1000" ;sbcl
  (let ((result 0))                                           ;compatible
   (loop for n from 1 to 1000 do
	(let ((100s (mod (truncate n 100) 10))
	      (10s (mod (truncate n 10) 10))
	      (1s (mod n 10)))
	  (incf result
		(loop for x across (format nil "~r" n)  ; number as words
		   counting (alpha-char-p x)))          ; number of letters in string
	  (when (and (< 0 100s) (or (< 0 10s) (< 0 1s))
		     (incf result 3)))))                ; for the missing "and"
   result))

;;;;;;;;;;;;;;;;;;;Euler 12;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun count-divisors (n)
  "Count all divisors"
  (+ 2                             ;for divisors 1 and number itself
     (loop for i from 2 to (/ n 2)
	count (= 0 (mod n i)))))

(defun triangular-p (n)
  (let ((mid (floor (sqrt (* 2 n)))))
    (when (= (* mid (1+ mid)) (* 2 n)) t))
  )

(defun triangle-divisors (max) 
  "First number which has more than 500 divisors."
  (loop
     while (< n max)
     with divisors = 2 ;for divisors 1 and the number itself
     for n from 1 to max
     for sum = (/ (* n (+ n 1)) 2)
;     for sum = n then (+ sum n)
     do
       (loop
	  for m from 2 upto (/ sum 2)
	  when (= (mod sum m) 0) do (incf divisors))
     if (< 500 divisors) do (return-from triangle-divisors sum)
     do (print (list sum divisors))
       (setf divisors 2)
       ))

; (n**2 + n) / 2 = x
; n**2 + n = 2x
; n(n + 1) = 2x
; o

(defun triangle-divisors2 (max) 
  "First number which has more than 500 divisors."
  (loop
     while (< n max)
     with divisors = 2 ;for divisors 1 and the number itself
     with n = 16
;     for sum = (/ (* n (+ n 1)) 2)
     for sum = n then (+ sum n)
     do
       (loop
	  for m from 2 upto (/ sum 2)
	  when (= (mod sum m) 0) do (incf divisors))
     if (< 500 divisors) do (return-from triangle-divisors sum)
     do (print (list sum divisors))
     do (setf n (* 2 n))
       (setf divisors 2)
       ))
;;;;;;;;;;;;;;;;;;;;;;;;; Euler 20 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun factorial (n)
  (if (< n 2) 1
      (* n (factorial (1- n)))))

(defun euler-20 ()
  "Sum of factorial"
  (loop for x across (write-to-string (factorial 100))
     summing (digit-char-p x)))
;;;;;;;;;;;;;;;;;;;;;;;;; Euler 19 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter days-months
  '(((1 3 5 7 8 10 12) 31) ((4 6 9 11) 30) ((2) 28))
    "Number of days in months")

(defun get-days (month)
  "Return number of days in month (number)"
  (dolist (x days-months)
    (when (find month (first x)) (return (second x)))))

(defun leap-year (year)
  "Return T if the year is a leap year, otherwise nil."
  (when (or (and (= (mod year 4) 0) (/= (mod year 100) 0))
	    (= (mod year 400) 0))
    t))

(defun leap-year-count (year)
  "Return how many leap years there is from 1900 to year"
  (loop for y from 1900 to year
       counting (leap-year y)))

(defun days-in-year (year month day)
  "Return how many days have passed of the given date of the given year."
  (loop for m from 1 to (1- month) summing (get-days m) into sum
       finally (incf sum day)
       finally (when (and (leap-year year)
			  (or (> month 2)
			      (and (= month 2) (= day 29))))
		 (incf sum))
       finally (return sum)))

(defun days-from-1900 (year month day)
  "Return number of days from 1900/1/1"
  (let ((days 0))
    (incf days (+ (* 365 (- year 1900)) (leap-year-count (1- year))))
    (incf days (days-in-year year month day))
    days
    )
  )

(defun sunday-p (year month day)
  (when (= 0 (mod (days-from-1900 year month day) 7)) t))

(defun euler-19 ()
  (loop for year from 1901 to 2000
     sum (loop for month from 1 to 12
	   count (sunday-p year month 1))))

;;;;;;;;;;;;;;;;;;;;;;;;; Euler 21 ;;;;;;;;;;;;;;;;;;;;;;;;;

