# Magic (ed)

*Magic (ed)* is a tiny editing facility for Common Lisp, where you can
directly load, edit, manipulate and evaluate file or file content from
[REPL](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop_).

I find myself often running REPL for various things but lazy enough to
engage Emacs/SLIME when I need a small editing work: I'm a UNIX guy
and I get used to start vi/vim/emacsclient for writing small
throw-away programs.

This package also can be starting point for people who are not
accustomed to Emacs or SLIME and would like to continue using their
editor with Common Lisp.

## Usage

You start it with:

```lisp
(magic-ed:magic-ed "/tmp/test.lisp")
```

and it will start editor assigned to *EDITOR* environment
variable. When you are done with editing, save it and quit: *magic-ed*
will load that file and evaluate it. If file saving somehow failed, nothing
will be evaluated.

In background, *magic-ed* will try to use *(ed)* standard function and
will try to obey implementation specific features. For example, on
SBCL if *sb-ext:\*ed-functions** was set, *magic-ed* will not use
environment value.

If you do not want editing file to be evaluated, you use **:eval** parameter
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

First setup *EDITOR* environment variable to point to your favorite
editor and download magic-ed code (official release or clone it from
this repository) and run:

```lisp
(asdf:load-system :magic-ed)
```

or if you have [Quicklisp](http://www.quicklisp.org):


```lisp
(ql:quickload :magic-ed)
```

The code was tested on SBCL, ECL, Clozure CL and CMUCL.

## License

Copyright (c) 2013-2015 Sanel Zukan. You can use this code whatever you
like.
