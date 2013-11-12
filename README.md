# README

## Name

dayone.el

## Description

dayone.el is a simple emacs extension for placing the new post from the Emacs to [Day One](http://dayoneapp.com). It can post the region when the concerned region is selected and M-x dayone-add-note. When the region is not selected and M-x dayone-add-note is executed, the all contents in the buffer are posted. 
As for dayone-add-note(), it may be convenient if you assign it to an appropriate key or add alias.
For the Day One, the data is managed by either the iCloud or the Dropbox.  This emacs extension is supported for storing data of Dropbox only.

## Setting

```
(require 'dayone)
(setq dayone-dir (concat (getenv "HOME") "/Dropbox/APP/Day One/Journal.dayone/entries/"))
(setq dayone-timezone "America/Sao_Paulo")
```

## Author

* mori-dev

## License

GPLv3

## .emacs.d/init.el

```emacs-lisp
(require 'dayone)
(setq dayone-dir (concat (getenv "HOME") "/Dropbox/APP/Day One/Journal.dayone/entries/"))
;(setq dayone-timezone "America/Sao_Paulo")
```
