(print ('reverse '(1 2 3)))
(print "hello world")

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

(print (+ 1 2))
