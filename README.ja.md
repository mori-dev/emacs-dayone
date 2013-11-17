# README

## 名前

dayone.el

## 概要

 dayone.el は Emacs から [Day One](http://dayoneapp.com) に新規投稿するためのシンプルな emacs 拡張です。リージョンを選択して M-x dayone-add-note すれば、そのリージョンで Day One のノートを新規作成します。リージョンを選択せずに M-x dayone-add-note した場合は、バッファ全体でノートを新規作成します。M-x dayone-add-note-with-tag はタグを付けて投稿します。複数のタグを付与する場合は、スペースで区切って入力して下さい。dayone-add-note や dayone-add-note-with-tag は適当なキーに割り当てるかエイリアスを付けると便利かもしれません。
Day One では、データを iCloud か Dropbox かのどちらかで管理します。この emacs 拡張は、Dropbox へのデータの格納にのみ対応しています。

## インストール

```
(package-install 'dayone)
```

## 設定

```
;; Day One のデータを配置している directory への path を格納します。必ずあなたの設定ファイルで設定して下さい。
(setq dayone-dir (concat (getenv "HOME") "/Dropbox/APP/Day One/Journal.dayone/entries/"))
;; Day One のノートの XML で指定するタイムゾーンの値を格納します。
(setq dayone-timezone "America/Sao_Paulo")
```

## 作者

* mori-dev


## 寄付

* [mori-dev の Amazon ウィッシュリスト](http://www.amazon.co.jp/registry/wishlist/3U1LOHTPSDG9V)

## ライセンス

GPLv3
