;;; Hand is a list of card strings consisting of letter of the suit S,
;;; C, D or H and the value of card, number 2-9,T, J, Q, K or A.
;;; Example: ("AS" "KS" "QS" "JS" "TS") is a hand with cards
;;; from 10 to ace, all spades.
(in-package :poker.compare-hands)

(defun space-split (string)
  "Splits string separated by single space"
  (loop for start = 0 then (1+ finish)
        for finish = (position #\Space string :start start)
        collecting (subseq string start finish)
        until (null finish)))

(defmacro hand-pattern (pattern hand)
  "If pattern mathes the hand, return card values in order of the number
  of cards of that value and then in order of card value. Else return nil."
  `(when (equal ,pattern (list-values (card-value-counts ,hand)))
    (list-keys (card-value-counts ,hand))))

(defmacro hand (name hand-string)
  "Defines global variable to hold a hand"
  `(defparameter ,name (space-split ,hand-string)))

(defun suit (card)
  "Returns cards suit: D, H, S or C"
  (elt card (1- (length card))))

(defun card-value (card)
  "Returns card's numerical value"
  (let ((value (subseq card 0 1)))
    (cond ((string-equal value "T") 10)
	  ((string-equal value "J") 11)
	  ((string-equal value "Q") 12)
	  ((string-equal value "K") 13)
	  ((string-equal value "A") 14)
	  (t (parse-integer value)))))
       
(defun card-values (hand)
  "Returns sorted list of hands card values"
  (loop for c in hand
     with values = ()
     do (push (card-value c) values)
     finally (return (sort values #'<))))


(defun cdr< (a b)
  "Return true if cdr of A is smaller than cdr of B. If A and B equal
   return true if car of A is bigger than car of B. Usable only on 
   alist cells containing numbers."
  (if (< (cdr a) (cdr b)) t
      (when (and (= (cdr a) (cdr b)) (< (car a) (car b))) t)))

(defun list-values (alist)
  "Lists contents of alist in descending order"
  (reverse (map 'list #'cdr (sort alist 'cdr<))))

(defun list-keys (alist)
  "Lists keys of alist in descending order of their content's
   numerical values"
  (reverse (map 'list #'car (sort alist 'cdr<))))

(defun card-value-counts (hand)
  "Return numbers of same card values in an alist.
   Example: ((5 . 3) (2 . 2)) means full house of 5s and 2s,
   ((12 . 2) (8 . 2) (3 . 1)) means two pairs "
  (loop
     with counts = ()
     with values = (card-values hand)
     for c in values
     if (assoc c counts) do (incf (cdr (assoc c counts)))
     else do (setf counts (acons c 1 counts))
     finally (return counts)))

(defun flush-p (hand)
  "If all card are same suit, return ordered list of card values, else nil."
  (loop with suit = (suit (first hand))
     for c in hand
     always (string-equal (suit c) suit)
     finally (return (reverse (card-values hand)))))

(defun straight-p (hand)
  "Return highest card value in a list if straight, else nil"
  (loop for i upto (- (length hand) 2)
     with values = (card-values hand)
     for a = (nth i values)
     for b = (nth (1+ i) values)
     always (= b (1+ a))
     finally (return (list b))))

(defun fours-p (hand)
  "If hand is four of a kind, return value of fours, else return nil."
  (let ((cvalues (card-value-counts hand)))
    (when (= (cdr (first cvalues)) 4)
      (return-from fours-p (list (car (first cvalues)))))
    (when (= (cdr (second cvalues)) 4)
      (return-from fours-p (list (car (second cvalues)))))))

(defun threes-p (hand)
  "If hand is three of a kind, return value of threes, else return nil."
  (hand-pattern '(3 1 1) hand))

(defun full-house-p (hand)
  "If hand is full-house (threes and a pair) return card value of threes
   and pair, else return nil."
  (hand-pattern '(3 2) hand))

(defun two-pairs-p (hand)
  (hand-pattern '(2 2 1) hand))

(defun pair-p (hand)
  "If hand is one pair, return values of pairs in descending order and
  value of the other card, else return nil"
  (hand-pattern '(2 1 1 1) hand))

(defun straight-flush-p (hand)
  " If straight flush: return highest card value, else return nil." 
  (and (flush-p hand) (straight-p hand)))

(defun royal-flush-p (hand)
  "If hand is royal flush, return 1 (for consistency). Else return
  nil."
  (when (equal '(14) (straight-flush-p hand)) (list 1)))

(defun make-test-hands ()
  "Makes a list of all different hand"
  (list
  (hand *hi-hand* "JD 4D 3H 6S 9C")
  (hand *royal-hand* "AS KS JS QS TS")
  (hand *straight-flush-hand* "4D 2D 3D 5D 6D")
  (hand *straight-hand* "4S 3D 2C 5D 6H")
  (hand *flush-hand* "3D 2D QD KD 8D")
  (hand *four-hand* "4D 4H 4C 4S 2D")
  (hand *full-hand* "5C 5D 5H 2S 2S")
  (hand *three-hand* "8D 8H 8C 4C 5D")
  (hand *two-pairs-hand* "QS QC 8C 8H 3S")
  (hand *pair-hand* "4H 4D 5D 8D 9C")))

(defun rank (hand-p hand rank-number)
  "Tests hand with hand-p and if it returns non-nil value, return
   rank-number appended with output of hand-p function. Else return nil."
  (let ((value (funcall hand-p hand)))
  (when value (cons rank-number value))))

(defun high-hand-p (hand)
  "Return card values in order highest first"
  (hand-pattern '(1 1 1 1 1) hand))

(defparameter hand-order '(royal-flush-p straight-flush-p fours-p
  full-house-p flush-p straight-p threes-p two-pairs-p pair-p
  high-hand-p))

(defun rank-hand (hand)
 "Assings a rank for a hand as a list of values: First is rank of the
   hand from 0 to 9, 9 being royal flush and 0 high card. Rest of list
   is return value of hand test predicate.  Example return value: (7 8
   3) would be a hand of 3 eights and 2 threes, a full h."
    (loop
       for n from (1- (length hand-order)) downto 0
       thereis (rank (nth n (reverse hand-order)) hand n)))

(defvar hand-names
  (list
   "high-card"
   "pair"
   "two pairs"
   "threes"
   "straight"
   "flush"
   "full house"
   "fours"
   "straight flush"
   "royal flush"))

(defun name-card (card)
  "Return proper name of a card. Card is a string consisting a value 2-9, T, J, Q, K or A,
   and a suit S, H, D or C. Example: \"TS\" returns \"ten of spades\"."
  (let ((suit (elt card 1)) (value (elt card 0)))
    (if (digit-char-p value) (setf value (format nil "~r" (digit-char-p value)))
	(setf value 
	      (cdr (assoc value '((#\T . "ten") (#\J . "jack") (#\Q . "queen") (#\K . "king") (#\A . "ace"))))))
    (setf suit (cdr (assoc suit '((#\C . "clubs") (#\H . "hearts") (#\D . "diamonds") (#\S . "spades")))))
    (format nil "~a of ~a" value suit)
    
    
    
    )
  
  )

(defun name-hand (hand)
  "Return name of the poker hand."
  (nth (first (rank-hand hand)) hand-names)
  )

(defun compare-hands (hand1 hand2)
  "Return -1 if hand1 is greater, 1 if hand2 is greater, 0 otherwise.
   A hand is a list of 5 card strings, each consisting of a letter of
   suit and the cards value. Suits: H, D, C, S. Values: 1-9, T, J, Q, K.
   Example '(\"AS\" \"TS\" \"2H\" \"3H\" \"4H\") is a hand of ace of
   spades, ten of spades, 2, 3 and 4 of hearts."
  (flet ((bigger (one two) (cond ((> one two) -1) ((> two one) 1) (t 0))))
    (loop
       with a
       for i upto 5
       with  rank1 = (rank-hand hand1)
       with  rank2 = (rank-hand hand2)
       for r1 = (nth i rank1)
       for r2 = (nth i rank2)
       while (and r1 r2)
       do (setf a (bigger (nth i (rank-hand hand1))
			  (nth i (rank-hand hand2))))
       when (/= a 0) do (return-from compare-hands a)
       finally (return a))))

