# lastpass - Emacs interface to LastPass

*Author:* Chunyang Xu <mail@xuchunyang.me><br>
*Version:* 0.1<br>

## About

This package use `lpass` (the LastPass command line tool) to let you access
your LastPass within Emacs.

## Requirements

    (and (version<= "24.3" emacs-version) (executable-find "lpass") (require 'csv)

- Emacs 24.3 or higher
- `lpass` (The LastPass command line tool)
- `csv.el` (can be installed with `package.el` from Melpa)

## Usage

- <kbd>M-x list-lastpass</kbd>
- <kbd>M-x helm-lastpass</kbd>
- <kbd>M-x lastpass-copy-password</kbd>

## Ideas

- TODO Not only read, but also add/delete/update

## Links

lpass:    https://github.com/lastpass/lastpass-cli
lpass(1): https://github.com/lastpass/lastpass-cli/blob/master/lpass.1.txt


---
Converted from `lastpass.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
