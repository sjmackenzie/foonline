# foonline
#### a web-based Lifoo playground 

### support
This project is running on a shoestring budget; please consider [helping out](https://www.paypal.me/c4life), every contribution counts.

### welcome
Welcome to Foonline, a web-based [Lifoo](https://github.com/codr4life/lifoo) playground. If you can imagine that [HyperCard](https://en.wikipedia.org/wiki/HyperCard) and Forth had a baby together, that might give you an idea of where this train is going. Foonline attempts to fill a perceived void in the space of playful, creative and powerful tools for creating software. There is not much in the way of examples for the environment yet, but all examples from the [Lifoo](https://github.com/codr4life/lifoo) documentation apply.

![Example 1](https://github.com/codr4life/foonline/blob/master/example1.png)

### basics
Foonline provides direct real-time access to it's own user interface in the form of a virtual DOM interfaced from [Lifoo](https://github.com/codr4life/lifoo). The way this works is by running [Lifoo](https://github.com/codr4life/lifoo) inside of a Common Lisp server that publishes exactly the same virtual DOM to the browser and [Lifoo](https://github.com/codr4life/lifoo). ```doc``` contains the whole document, ```in``` and ```out``` deal with the REPL text areas; and ```canvas``` the right hand frame. Any method that [Lifoo](https://github.com/codr4life/lifoo) [supports](virtual DOM) for it's virtual DOM may be used to modify the document. Each page load starts a new session.
