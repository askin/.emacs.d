#!/bin/sh

emacs --daemon
emacsclient --eval '(progn
       (tool-bar-mode -1)
       (scroll-bar-mode -1)
       (set-scroll-bar-mode nil)
       (menu-bar-mode 0))'   
