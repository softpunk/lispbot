# lispbot
cute, common lisp based irc bot. Does very little, but shouldn't break anything.

## triggers
The default trigger is comma, you can also use lineline commands delimited with two commas, or simply address the message 
at lispbot directly. when in a query, you don't need to use any commas. Lisp syntax is optional, `,cute github` is the
same as `,(cute github)`, which is the same as `,(cute "github")`. See `lispify` for details on how this works.

Quick reference for triggers:
* `,foo bar baz`
* `lispbot: foo bar baz`
* `someone-else: hey, check this out ,, foo bar baz`

## dependencies and installation

Lispbot requires quicklisp to run. It's been tested on sbcl, but should work on other CL implementations.
to install quicklisp, head over to [quicklisp's website](https://www.quicklisp.org/beta/) and follow the instructions there.
Make sure you run `(ql:add-to-init-file)`, otherwise quicklisp won't be avaliable when you start your interpreter.

To use lispbot, clone the repo into `~/quicklisp/local-projects`, and run `(ql:quickload 'lispbot)`.

Use `(start-irc server &optional nick)` once lispbot is loaded to connect to a server. To make this automatic,
just append that line to the end of `lispbot.cl`, and it will connect on load.