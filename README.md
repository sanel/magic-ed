# Magic (ed)

*Magic (ed)* is a tiny editing facility for Common Lisp, where you can
directly load, edit, manipulate and evaluate file or file content from
[REPL](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop_).

I find myself often running REPL for various things but lazy enough to
engage Emacs/SLIME when I need some small editing work: I'm a UNIX guy
and I often start vi/vim/emacsclient for writing small throw-away
programs.

This package can be also starting point for people who are not
accustomed to Emacs or SLIME and would like to continue using their
editor with Common Lisp.

## Usage

You simply start it with:

```lisp
(magic-ed:magic-ed "/tmp/test.lisp")
```

and it will call function assigned to *EDITOR* environment
variable. When you are done with editing, save it and quit; *magic-ed*
will load that file and evaluate it. If save failed, it will do
nothing.

In background, *magic-ed* will use *(ed)* standard function but will
try to obey implementation specific features. For example, on SBCL if
*sb-ext:\*ed-functions** was set, *magic-ed* will not use environment
value.

If you do not want file to be evaluated, you use **:eval** parameter
like:

```lisp
(magic-ed:magic-ed "/tmp/test.lisp" :eval nil)
```

and file will be only saved. Also, if you would like file content to
be returned as escaped string, use **:output** parameter (accepts only
*:file* and *:string*):

```lisp
(setf content (magic-ed:magic-ed "/tmp/test.lisp" :output :string))
```

## Installation

First download the code (official release or clone it from repository)
and run:

```lisp
(asdf:load-system :magic-ed)
```

or if you have [Quicklisp](http://www.quicklisp.org):


```lisp
(ql:quickload :magic-ed)
```

Then setup *EDITOR* environment variable to point to your favorite
editor and try to be productive :)

This code is tested on SBCL, ECL and Clozure CL.

## License

Copyright (c) 2013 Sanel Zukan. You can use the code whatever you
like.
