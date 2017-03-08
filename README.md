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

![Basics Example](https://github.com/codr4life/foonline/blob/master/example_basics.png)

```
document title 
empty "example" text

canvas 
1 h "foo" text drop 
2 h "bar" text drop
3 h "baz" text drop

canvas a
:href attr "http://www.github.com" set drop
"GitHub" text
drop br
```

### callbacks
Callbacks may be registered for DOM events, changes are automatically piggy-backed on top of requests and responses.

![Events Example](https://github.com/codr4life/foonline/blob/master/example_events.png)

```
canvas
label "name: " text drop
input :name swap set
button 
  "greet" text 
  (:name value 1 list "alert('hello ~a!');" fmt
   document swap update drop)@ onclick
drop
$ (:name) let
to-html
```

### word list
Included below is a screen shot of, and the source code for the word list; accessible by evaluating ```nil word-list```, passing ```t``` type-checks words against current stack contents.

![Word List Example](https://github.com/codr4life/foonline/blob/master/example_word_list.png)

```
words :words swap set drop
canvas empty
1 h "word list" text drop
table
:width style "100%" set drop
:margin-top style "0.5em" set drop
tr
  th
    span "macro" text :font-weight style "bold" set drop drop
    "/word" text
    :width style "50%" set drop drop
  th "arguments" text drop 
drop
:words
(:w swap set drop 
 tr
   :w macro? swap drop (:font-weight style "bold" set drop) when
   td :w id str down swap drop text drop
   td :w
     macro? (args "()" (args 1 list str down) if) "" if
     swap drop text drop
   td :w protocol 1 list str down swap drop text drop
 drop)@ each
$ (:words :w) let
drop drop
```

### ps
You are perfect, immortal spirit; whole and innocent.<br/>
All is forgiven and released.

peace, out<br/>
