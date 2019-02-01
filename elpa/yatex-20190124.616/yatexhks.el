;;; -*- Emacs-Lisp -*-
;;; Hooks for YaTeX

;;; 野鳥に関連する記述(たとえばアドイン関数)は yatexhks.el という名前の
;;; ファイルに入れてください。起動時に自動的にロードします。

;;; All the private definitions for YaTeX can be stuffed into the file
;;; named `yatexhks.el'.  The file `yatexhks.el' will be automatically
;;; loaded at the end of loading `yatex.el'.

;Private definitions begin from here.

;;97/1/27
(define-key YaTeX-user-extensional-map "v" 'YaTeX-section-overview)
;;initial version
(let ((map YaTeX-user-extensional-map))
  (define-key map "0"
    (function (lambda () (interactive)
		(YaTeX-make-section nil nil nil "part"))))
  (define-key map "1"
    (function (lambda () (interactive)
		(YaTeX-make-section nil nil nil "chapter"))))
  (define-key map "2"
    (function (lambda () (interactive)
		(YaTeX-make-section nil nil nil "section"))))
  (define-key map "3"
    (function (lambda () (interactive)
		(YaTeX-make-section nil nil nil "subsection"))))
  (define-key map "4"
    (function (lambda () (interactive)
		(YaTeX-make-section nil nil nil "subsubsection"))))
  (define-key map "5"
    (function (lambda () (interactive)
		(YaTeX-make-section nil nil nil "paragraph"))))
  (define-key map "6"
    (function (lambda () (interactive)
		(YaTeX-make-section nil nil nil "subparagraph"))))
  (define-key map "r"
    (function (lambda () (interactive)
		(YaTeX-make-section nil nil nil "ref"))))
  (define-key map "i"
    (function (lambda () (interactive)
		(YaTeX-make-singlecmd "item"))))
  (define-key map "\C-b"
    (function (lambda () (interactive)
		(YaTeX-make-singlecmd "leftarrow"))))
  (define-key map "l"
    (function (lambda () (interactive)
		(YaTeX-make-section nil nil nil "label"))))
  (define-key map "f"
    (function (lambda () (interactive)
		(YaTeX-make-section nil nil nil "frac"))))
  (define-key map "S"
    (function (lambda () (interactive)
		(YaTeX-make-section nil nil nil "setlength"))))
  (define-key map "b"
    (function (lambda () (interactive)
		(YaTeX-make-fontsize nil "bf"))))
  (define-key map "I" 'YaTeX-browse-info))

(defun YaTeX-browse-info ()
 "Browse YaTeX's info"
 (interactive)
 (require 'info)
 (Info-goto-node (if YaTeX-japan "(yatexj)Top" "(yatexe)Top")))


;
;;; End of yatexhks.el
(provide 'yatexhks)
