refacterl
=========

A set of tools on top of erlang.el to help with developing Erlang software with Emacs

Installation
============

Right now manual installation is required:

```shell
git clone git@github.com:erlang-emacs/refacterl.git
```

```lisp
(add-to-list 'load-path "/path/to/refacterl")
(require 'refacterl)
```

Available commands
==================

* erlang--binaries-to-defines
* erlang--cycle-string-like
* erlang--split-exports
* erlang--export-fun-at-point

