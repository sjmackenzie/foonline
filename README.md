# foonline
#### a web-based Lifoo IDE

### support
This project is running on a shoestring budget; please consider [helping out](https://www.paypal.me/c4life), every contribution counts.

### welcome
Welcome to Foonline, a web-based [Lifoo](https://github.com/codr4life/lifoo) IDE. If you can imagine that [HyperCard](https://en.wikipedia.org/wiki/HyperCard), Common Lisp and Forth had a baby together, that might give you an idea of where this train is going. Foonline attempts to fill a perceived void in the space of powerful, higher level tools for creating custom software.

### setup
Besides source, a Linux executable is [provided](https://github.com/codr4life/foonline/blob/master/foonline.tar.gz); running ```running ./foonline``` in the same directory as the included ```wwww``` catalog starts the server.

```
~/Andreas/Dev/Lisp/foonline > ./foonline 
Please specify server http-port: 8080

Foonline is waiting for your call,
press Enter to stop server and exit
```

### basics
Foonline provides direct real-time access to it's own user interface in the form of a [virtual DOM](https://github.com/codr4life/vicsydev/blob/master/wrap_up_virtual_dom.md) interfaced from [Lifoo](https://github.com/codr4life/lifoo). The way this works is by running [Lifoo](https://github.com/codr4life/lifoo) inside of a Common Lisp server that publishes the same DOM to the browser and [Lifoo](https://github.com/codr4life/lifoo). Any method that [Lifoo](https://github.com/codr4life/lifoo) supports may be used to modify the document, the spell for emptying the console (```console empty```) may also be used to empty any other UI element. Each page load starts a new session, and each new session inherits the combined history from all that came before.

![Title Example](https://github.com/codr4life/foonline/blob/master/example_title.png)

![Controls Example](https://github.com/codr4life/foonline/blob/master/example_controls.png)

![Events Example](https://github.com/codr4life/foonline/blob/master/example_events.png)

```
document title 
empty "example" text

canvas 
1 h "foo" text drop 
2 h "bar" text drop
3 h "baz" text drop
br

canvas input
:value attr "abc" set
drop drop br

canvas a
:href attr "http://www.github.com" set drop
"GitHub" text
drop br

canvas 
10 (1 list "Button ~a" fmt 
    swap button rotr text drop) times
      
canvas "table" tag
:width style "100%" set drop
:margin-top style "1em" set drop
"tr" tag
"th" tag "foo" text 
:width style "50%" set drop drop
"th" tag "bar" text drop 
drop
to-html

canvas button 
"click me!" text 
:font-size style "125%" set drop
(document "alert('clicked!');" update)@ onclick
```

### word list
Included below is an example of, and the source code for the word list; accessible by evaluating ```nil word-list```, passing ```t``` type-checks the list against the current stack.

![Words Example](https://github.com/codr4life/foonline/blob/master/example_words.png)

```
words :words swap set drop
canvas empty
1 h "word list" text drop
table
:width style "100%" set drop
:margin-top style "0.5em" set drop
tr
th "name" text
:width style "50%" set drop drop
th "args" text drop 
th "macro?" text drop
drop
:words
(:w swap set drop tr 
 td :w id str down swap drop text drop
 td :w args 1 list str down swap drop text drop
 td :w macro? "no" "yes" if swap drop text drop 
 drop)@ each
$ (:words :w) let
drop drop
```

### future directions
Having programmatic control over the DOM allows implementing multi-view applications in combination with the [global environment](https://github.com/codr4life/lifoo#environment), the next step is to explore that angle and figure out the constraints. I'm also working on a database layer for basic persistence and indexing of data.

### ps
You are perfect, immortal spirit; whole and innocent.<br/>
All is forgiven and released.

peace, out<br/>
