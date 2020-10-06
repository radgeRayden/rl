(print ('reverse '(1 2 3)))
(print "hello world")
(print (empty? '()))

(fn print3 (a b c)
    (print a)
    (print b)
    (print c))

(print3 "12" "23" "34")

(fn ten () 10)
(print (ten))

(print
    (if false
        "yes"
        (if true
            "!"
            "!!")))

(fn ff (f) (f 1 2 3))
(ff print3)
(print print3)

((fn recurse (n)
    (print n)
    (if (< n 10)
        (recurse (+ n 1))
        n)) 0)

(print
    (let ((a 10)
          (b "banana")
          (c 21))
        (print b)
        (print (+ a c))
        (- a c)))

(print (+ 1 2))
