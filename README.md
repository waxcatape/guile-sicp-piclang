# guile-sicp-piclang
An implementation of Henderson-Escher picture language from Structure and Interpretation of Computer Programs for Guile Scheme. WIP

This implementation makes it possible to run code examples from the book's Picture Language section without having to implement much of the language beforehand.
Much of the code is courtesy of [ProducerMatt](https://github.com/ProducerMatt/SICP-solutions) and all of it is work in progress. In particular, a guix package recipe is to be added.
At the moment, the code gets you far enough to be able print the square limit of a wave painter in repl. Invoke (paint-lines (square-limit wave 4)) to test.
