(print ('reverse '(1 2 3)))
(print "hello world")

(fn map (f lst)
    (if (empty? lst)
        '())
    (else
        (cons (f (car lst)) (map (cdr lst) f))))

(map (fn (x) (+ x 10)) '(1 2 3))
