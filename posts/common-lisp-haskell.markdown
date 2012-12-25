---
date: 2011-10-31
title: Common Lisp/Haskell syntactic comparison
description: Common Lisp/Haskell syntactic comparison
author: Chris Done
tags: haskell, common lisp
---

This is a little reminder/documentation for myself to explain that,
despite having nice regular s-expression syntax, Common Lisp actually
has a lot of syntactic concepts. I add comparison to Haskell because
Haskell is known (perhaps superfically) for having a lot of syntax
compared to other languages.

    CL: ; comment here
    HS: -- comment here

    CL: (defun x () …)
    HS: x = …

    CL: (COND (((= x 1) r1) ((= x 2) r2)) (t r3))
    HS: case x of 1 -> r1; 2 -> r2; _ -> r3

    CL: 'quotation
    HS: 'value
        ''type

    CL: '(quotation)
        `(quasiquotation)
    HS: [|quotation|]

    CL: `(quasiquotation with ,splice)
    HS: [| quotation with $(splice) |]

    CL: `(list splice with ,@splice)
    HS: [| list splice with $(splice) |]

    CL: #(reader macros)
        #(… …) ; vector
        #p" … " ; path
    HS: [| reader macros |]
        [v| … … |]
        [p| … |]

    CL: :keyword arguments
    CL: #: no idea what this is called

    CL: (multiple-value-bind (x y) some-values (print x))
    HS: (let (x,y) = someValues in print x)

    CL: (list 1 2 3)
       '(1 2 3)
    HS: [1,2,3]

    CL: (cons 1 (cons 2 (cons 3 nil)))
        (cons 1 (cons 2 (cons 3 '())))
    HS: 1 : 2 : 3 : []

    CL: (funcall foo '(arg1 arg2))
    HS: foo arg1 arg2

    CL: #'function
    HS: function

    CL: (let ((f 1)) …)
        (let*((g 0) (f g)) …)
        (flet((g (n) (cons n …)) (f () (g 1))) …)
    HS: let f = g 1; g n = n : g (n+1) in f

    CL: *global*

    CL: (lambda (a b) …)
    HS: \a b -> …

    CL: (tagbody (go …))

    CL: (defun x (f &optional args) "Docs here." …)

    CL: (declare (single-float x) (optimize (safety 3)))
    HS: x :: Float

    CL: (do () (…) (…))

    CL: (values a b c)
    HS: (a,b,c)

    CL: (loop … ? ¿ ‽)

    CL: #\space
        #\
    HS: ' '

    CL: (+) => 0
    HS: (+) => (+)

    CL: (+ 1) => 1
    HS: (+ 1) => (+ 1)

    CL: (+ 1 2) => 3
    HS: (1 + 2) => 3

    CL: (sort (list '(9 A) '(3 B) '(4 C)) #'< :key #'first)
    HS: sortBy (comparing fst) [(9,'A'),(3,'B'),(4,'C')]

    CL:  #1=(programmable . #1#) programming language
    HS: fix (\x -> "programmable " ++ x) ++ " programming language"

    CL: (defclass x (a b) ())
    HS: class X a b

    CL: (if foo-bar then-x else-y)
    HS: if fooBar then x else y

    HS: False, Nothing, []
    CL: nil
