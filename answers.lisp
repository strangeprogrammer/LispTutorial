#|
	Exercises taken from:
		https://www.youtube.com/watch?v=rGPjTSRbYyI&list=PL5D0328138F71BC67&index=6
		https://www.youtube.com/watch?v=MbrdaQC4yb4&list=PL5D0328138F71BC67&index=7
	
	Note:	You will probably find that my implementation of problem ';5'
		is noticably different from the videos; this is because it uses
		an accumulator argument to help optimize tail-recursive calls,
		as well as performs an initial check for list emptiness, and
		returns 'NIL' if the list given is 'NIL'
	
	Specification:
	
	; 1
	(m-subset '(a b) '(k m b l a))	= T
	(m-subset '(k) '(m a))		= NIL
	(m-subset '(m a) '(a k n m))	= T
	
	; 2
	(m-delete 'a '(a d c))		= '(d c)
	(m-delete 'a '((a h) d a))	= '((h) d)
	(m-delete 'a '(b c))		= '(b c)
	
	; 3
	(m-insert 'h 'a '(l m n a))	= '(l m n h a)
	(m-insert 'd 'a '(a n (a (a))))	= '(d a n (d a (d a)))
	(m-insert 'k 'b '(b (b b) b))	= '(k b (k b k b) k b)
	
	; 4
	(s-count '(((a) f) (k l) (m)))	= '(2 2 1)
	(s-count '((((k))) (m n (d))))	= '(1 3)
	(s-count '((a) (k)))		= '(1 1)
	
	; 5
	(s-maximum '(5 4 3))	= 5
	(s-maximum '(8 3 9 1))	= 9
|#

; 1
(defun m-subset (l1 l2)
	(if (null l1)
		T
		(if (member (car l1) l2)
			(m-subset (cdr l1) l2))))

; 2
(defun m-delete (x l)
	(cond
		((null l)
			NIL)
		((eq x (car l))
			(m-delete x (cdr l)))
		((listp (car l))
			(cons (m-delete x (car l)) (m-delete x (cdr l))))
		(T
			(cons (car l) (m-delete x (cdr l))))))

; 3
(defun m-insert (a b l)
	(cond
		((null l)
			NIL)
		((listp (car l))
			(cons (m-insert a b (car l)) (m-insert a b (cdr l))))
		((eq b (car l))
			(cons a (cons b (m-insert a b (cdr l)))))
		(T
			(cons (car l) (m-insert a b (cdr l))))))

; 4
(defun m-count (l)
	(cond
		((null l)
			0)
		((listp (car l))
			(+ (m-count (car l)) (m-count (cdr l))))
		(T
			(+ 1 (m-count (cdr l))))))

(defun s-count (l)
	(if (not (null l))
		(cons (m-count (car l)) (s-count (cdr l)))))

; 5
(defun m-maximum (a l)
	(if (null l)
		a
		(m-maximum (max a (car l)) (cdr l))))

(defun s-maximum (l)
	(if (not (null l))
		(let ((a (car l)))
		(m-maximum a (cdr l)))))
