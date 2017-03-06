# foonline
#### a web-based Lifoo playground 

### support
This project is running on a shoestring budget; please consider [helping out](https://www.paypal.me/c4life), every contribution counts.

### welcome
Welcome to Foonline, a web-based [Lifoo](https://github.com/codr4life/lifoo) playground. If you can imagine that [HyperCard](https://en.wikipedia.org/wiki/HyperCard), Common Lisp and Forth had a baby together, that might give you an idea of where this train is going. Foonline attempts to fill a perceived void in the space of playful yet powerful tools for creating software.

### basics
Foonline provides direct real-time access to it's own user interface in the form of a virtual DOM interfaced from [Lifoo](https://github.com/codr4life/lifoo). The way this works is by running [Lifoo](https://github.com/codr4life/lifoo) inside of a Common Lisp server that publishes the same virtual DOM to the browser and [Lifoo](https://github.com/codr4life/lifoo). Any method that [Lifoo](https://github.com/codr4life/lifoo) supports may be used to modify the document, the spell for emptying the console (```console empty```) may also be used to empty any other UI element. Each page load starts a new session.

![Title Example](https://github.com/codr4life/foonline/blob/master/example_title.png)

![Controls Example](https://github.com/codr4life/foonline/blob/master/example_controls.png)

![Events Example](https://github.com/codr4life/foonline/blob/master/example_events.png)

### future directions
I'm still figuring out the constraints of the initial design. Having programmatic control over the DOM potentially allows implementing multi-view applications in combination with the [global environment](https://github.com/codr4life/lifoo#environment).

### ps
You are perfect, immortal spirit; whole and innocent.<br/>
All is forgiven and released.

peace, out<br/>
