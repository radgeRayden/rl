(print ('reverse '(1 2 3)))
(print "hello world")

(fn print3 (a b c)
    (print a)
    (print b)
    (print c))

(print3 "12" "23" "34")

(fn ff (f) (f 1 2 3))
(ff print3)
(ff 3)
