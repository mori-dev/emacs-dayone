# README

Japanese translation of README file available [here](./README.ja.md).

## Name

dayone.el

## Description

dayone.el is a simple emacs extension for placing the new post from the Emacs to [Day One](http://dayoneapp.com). It can post the region when the concerned region is selected and M-x dayone-add-note. When the region is not selected and M-x dayone-add-note is executed, the all contents in the buffer are posted. M-x dayone-add-note-with-tag shall be posted by attaching the tag. When multiple tags are attached, use the space key for separating each tag.
MAs for dayone-add-note and dayone-add-note-with-tag, it may be convenient if you assign it to an appropriate key or add alias.
For the Day One, the data is managed by either the iCloud or the Dropbox.  This emacs extension is supported for storing data of Dropbox only.

## Install

```
(package-install 'dayone)
```

## Setting

```
(setq dayone-dir (concat (getenv "HOME") "/Dropbox/APP/Day One/Journal.dayone/entries/"))
(setq dayone-timezone "America/Sao_Paulo")
```

## Author

* mori-dev


## Donate

* [mori-dev's Amazon Wish List](http://www.amazon.co.jp/registry/wishlist/3U1LOHTPSDG9V)

## License

GPLv3
