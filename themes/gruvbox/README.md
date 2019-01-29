![Gruvbox Head](images/gruvbox-head.png "Gruvbox theme header")

Gruvbox is a retro groove color scheme for Emacs. It is a port of the Vim
version originally by [Pavel Pertsev](https://github.com/morhetz) found
[here](https://github.com/morhetz/gruvbox).

## Theme versions

#### Gruvbox Dark Medium (aka. Gruvbox _default_)

Theme name `gruvbox-dark-medium`

![gruvbox-dark-medium aka. gruvbox](images/gruvbox-dark-medium-screenshot.png)

#### Gruvbox Dark Soft

Theme name `gruvbox-dark-soft`

![gruvbox-dark-soft](images/gruvbox-dark-soft-screenshot.png)

#### Gruvbox Dark Hard

Theme name `gruvbox-dark-hard`

![gruvbox-dark-hard](images/gruvbox-dark-hard-screenshot.png)

#### Gruvbox Light Medium

Theme name `gruvbox-light-medium`

![gruvbox-light-medium](images/gruvbox-light-medium-screenshot.png)

#### Gruvbox Light Soft

Theme name `gruvbox-light-soft`

![gruvbox-light-soft](images/gruvbox-light-soft-screenshot.png)

#### Gruvbox Light Hard

Theme name `gruvbox-light-hard`

![gruvbox-light-hard](images/gruvbox-light-hard-screenshot.png)

## Palette

Note the dark and light tones are transposed in the light versions.

![Gruvbox Palette Dark](images/darktones-palette.png "Gruvbox theme palette")

![Gruvbox Palette Mid](images/midtones-palette.png "Gruvbox theme palette")

![Gruvbox Palette Light](images/lighttones-palette.png "Gruvbox theme palette")

![Gruvbox Palette Bright](images/bright-colors-palette.png "Gruvbox theme palette")

![Gruvbox Palette Neutral](images/neutral-colors-palette.png "Gruvbox theme palette")

![Gruvbox Palette Faded](images/faded-colors-palette.png "Gruvbox theme palette")

![Gruvbox Palette Dark Notes](images/darknotes-palette.png "Gruvbox theme palette")

![Gruvbox Palette Solo Notes](images/solonotes-palette.png "Gruvbox theme palette")

![Gruvbox Palette Rainbow Delimiters](images/rainbow-delimiters-palette.png "Gruvbox theme palette")

## Mode Support

This theme contains custom support for the following features and plugins:

- [Ag.el](https://github.com/Wilfred/ag.el)
- Ace jump
- Ace window
- [Anzu](https://github.com/syohex/emacs-anzu#customization)
- Comint (and the like)
- Company
- Diffs
- Dired+
- Elfeed
- [ElScreen](https://github.com/knu/elscreen)
- Eshell
- Helm
- Highlight Indent
- Ivy
- [Js2-mode](https://github.com/mooz/js2-mode)
- LaTeX
- [Linum-relative](https://github.com/emacsmirror/linum-relative)
- Magit
- Markdown
- Message
- Org
- Popup
- [RainbowDelimiters](http://www.emacswiki.org/emacs/RainbowDelimiters)
- Shell script
- Smart modeline
- [Smartparens](https://github.com/Fuco1/smartparens)
- Term
- Tool Tips
- Undo Tree
- Whitespace-mode
_ Widget Faces

## Installation and usage

The recommended way to install the Gruvbox theme is with MELPA.

### MELPA

If you're an Emacs 24 user or you have a recent version of `package.el` you can
install the Gruvbox theme from the [MELPA
repository](http://melpa.milkbox.net/#/gruvbox-theme). The version of
`gruvbox-theme` there will always be up-to-date.

### No `package.el`

The following instructions are for in the case where you don't have access to
`package.el` for some reason.

1. Download `gruvbox-theme.el`, and put it in `~/.emacs.d/themes`. For example:
   ```shell
   curl https://raw.github.com/Greduan/emacs-theme-gruvbox/master/gruvbox-theme.el > ~/.emacs.d/themes/gruvbox-theme.el
   ```

1. Tell Emacs where to find themes in your `init.el` or `.emacs` file:
   ```lisp
   (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
   ```

1. Enable the theme:
   ```
   M-x load-theme RET gruvbox
   ```
   Or add the following to your `init.el` or `.emacs` file to load the theme at startup:
   ```
   (load-theme 'gruvbox t)
   ```


## Issues

See https://github.com/greduan/emacs-theme-gruvbox/issues

If you want the theme to support something please open a new issue and we'll
try our best to make it happen. :smile:

## License

```
The MIT License (MIT)

Copyright (c) 2013 Lee Machin
              2013-2016 Greduan
              2015-2017 Jason Milkins

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```
