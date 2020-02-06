# Magic (ed)

*Magic (ed)* is a tiny editing facility for Common Lisp, where you can
directly load, edit, manipulate and evaluate file or file content from
[REPL](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop_).

I find myself often running the REPL for various things but lazy enough to
engage Emacs/SLIME when I need just a small edit: I'm a UNIX guy
and I got used to start vi/vim/emacsclient for writing small
throw-away programs.

This package also can be a starting point for people who are not
accustomed to Emacs or SLIME and would like to continue using their
default terminal/console editor with Common Lisp.

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

If you do not want edited file to be evaluated, you use **:eval** parameter
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
editor (on *nix, often nano or vi, distribution dependent config and change) and 
download magic-ed code (official release or clone it from this repository) and run:

```lisp
(asdf:load-system :magic-ed)
```
### Install with Ultralisp Via Quicklisp 

Magic-ed is [available](https://ultralisp.org/projects/sanel/magic-ed) on Ultralisp. 
Install [Quicklisp](http://www.quicklisp.org), install [Ultralisp](https://ultralisp.org/).
Then use the standard Quicklisp installation method. 

### Manual download and installation with Quicklisp
If you have [Quicklisp](http://www.quicklisp.org) only:

Download or git clone magic-ed to Your quicklisp directory manually  
( usually ~/quicklisp/local-projects/ )

For example, execute in terminal:

```bash
cd ~/quicklisp/local-projects/

git clone https://github.com/sanel/magic-ed 
```

Then, launch Your Common Lisp of choice and in the REPL: 

```lisp
(ql:quickload :magic-ed)
```

### Automatic loading on REPL start with Quicklisp

Update Your Common Lisp configuration, adding the following line 
to the same file responcible for loading quicklisp on start.

In SBCL: edit Your ~/.sbclrc and add to the end, after ql lines:

```lisp
(ql:quickload :magic-ed)
```

## Compatibility 
The magic-ed code was tested on SBCL, ECL, Clozure CL and CMUCL.
## License

Copyright (c) 2013-2019 Sanel Zukan. You can use this code whatever you
like.
