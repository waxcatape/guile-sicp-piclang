# guile-sicp-piclang
An implementation of Henderson-Escher picture language from [Structure and Interpretation of Computer Programs Section 2.2.4](http://sarabander.github.io/sicp/html/2_002e2.xhtml#g_t2_002e2_002e4). WIP

This implementation makes it possible to run code examples from the book's Picture Language section without having to implement much of the language beforehand. Much of the code is courtesy of [ProducerMatt](https://github.com/ProducerMatt/SICP-solutions) and all of it is work in progress. In particular, a guix package recipe is to be added.

For functions that the book expects you to implement, such as `make-vect`, we provide working answers postfixed like `make-vect-answer`.

At the moment, the code gets you far enough to be able print the square limit of a wave painter in repl. Invoke `(paint-lines (square-limit wave-answer 4))` to test.

To use the module:
- get the `guile-picture-language` module in your environment. If you have Guix, you can run `guix shell guile guile-picture-language` to make it available in your shell.
- download `sicp-piclang.scm`, and load it into your repl with `(load "sicp-pictlang.scm")`
- invoke `(use-modules (sicp-piclang))` to make the functions available

Here's a detail you're likely to run into when iteratively building your answers. When re-defining a function, existing bindings to that function *don't* get changed. An example:

``` scheme
(define make-vect make-vect-answer)
(define (foo x)
    (make-vect (+ 1 x)
               (- 1 x)))

(display (foo 5))
=> (6 4)

;; redefining make-vect
(define (make-vect x y)
    (error "halt and catch fire"))

(display (foo 5))
=> (6 4)
;; though we redefined make-vect, it's still using the original function definition in foo

;; redefine foo
(define (foo x)
    (make-vect (+ 1 x)
               (- 1 x)))

(display (foo 5))
=> halts with error
```
