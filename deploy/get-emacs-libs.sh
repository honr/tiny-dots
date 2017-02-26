#!/bin/bash

cd $HOME
mkdir -p Lake/elisp
mkdir -p $HOME/.emacs.d/site-lisp/lake
cd Lake/elisp

git clone http://mumble.net/~campbell/git/paredit.git paredit
ln -s $HOME/Lake/elisp/paredit $HOME/.emacs.d/site-lisp/lake

git clone https://github.com/dominikh/go-mode.el go-mode
ln -s $HOME/Lake/elisp/go-mode $HOME/.emacs.d/site-lisp/lake

git clone https://github.com/haskell/haskell-mode
ln -s $HOME/Lake/elisp/haskell-mode $HOME/.emacs.d/site-lisp/lake

git clone https://github.com/clojure-emacs/clojure-mode
ln -s $HOME/Lake/elisp/clojure-mode $HOME/.emacs.d/site-lisp/lake

git clone https://github.com/browse-kill-ring/browse-kill-ring
ln -s $HOME/Lake/elisp/browse-kill-ring $HOME/.emacs.d/site-lisp/lake
