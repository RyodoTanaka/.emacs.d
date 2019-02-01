;;; yatex-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yahtml" "yahtml.el" (0 0 0 0))
;;; Generated autoloads from yahtml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yahtml" '("yahtml")))

;;;***

;;;### (autoloads nil "yatex" "yatex.el" (0 0 0 0))
;;; Generated autoloads from yatex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatex" '("yatex-mode" "YaTeX-" "tex-" "tmp-" "user-" "section-table" "singlecmd-table" "LaTeX2e-fontstyle-alist" "fontsize-table" "env-table" "NTT-jTeX" "latex-" "dvi" "makeindex-command" "bibtex-command")))

;;;***

;;;### (autoloads nil "yatex19" "yatex19.el" (0 0 0 0))
;;; Generated autoloads from yatex19.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatex19" '("YaTeX-" "hilit-patterns-alist")))

;;;***

;;;### (autoloads nil "yatex23" "yatex23.el" (0 0 0 0))
;;; Generated autoloads from yatex23.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatex23" '("YaTeX-")))

;;;***

;;;### (autoloads nil "yatexadd" "yatexadd.el" (0 0 0 0))
;;; Generated autoloads from yatexadd.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexadd" '("YaTeX")))

;;;***

;;;### (autoloads nil "yatexenv" "yatexenv.el" (0 0 0 0))
;;; Generated autoloads from yatexenv.el

(autoload 'YaTeX-what-column "yatexenv" "\
Show which kind of column the current position is belonging to.

\(fn)" t nil)

(autoload 'YaTeX-intelligent-newline "yatexenv" "\
Insert newline and environment-specific entry.
`\\item'	for some itemizing environment,
`\\> \\> \\'	for tabbing environemnt,
`& & \\ hline'	for tabular environment.

\(fn ARG)" t nil)

(autoload 'YaTeX-indent-line-equation "yatexenv" "\
Indent a line in equation family.

\(fn)" nil nil)

(autoload 'YaTeX-goto-corresponding-leftright "yatexenv" "\
Go to corresponding left or ight.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexenv" '("YaTeX-")))

;;;***

;;;### (autoloads nil "yatexflt" "yatexflt.el" (0 0 0 0))
;;; Generated autoloads from yatexflt.el

(autoload 'YaTeX-filter-goto-source "yatexflt" "\
Go to corresponding text source of the graphic file

\(fn FILE OTHER-WIN)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexflt" '("YaTeX-")))

;;;***

;;;### (autoloads nil "yatexgen" "yatexgen.el" (0 0 0 0))
;;; Generated autoloads from yatexgen.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexgen" '("YaTeX-")))

;;;***

;;;### (autoloads nil "yatexhie" "yatexhie.el" (0 0 0 0))
;;; Generated autoloads from yatexhie.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexhie" '("YaTeX-")))

;;;***

;;;### (autoloads nil "yatexhks" "yatexhks.el" (0 0 0 0))
;;; Generated autoloads from yatexhks.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexhks" '("YaTeX-browse-info")))

;;;***

;;;### (autoloads nil "yatexhlp" "yatexhlp.el" (0 0 0 0))
;;; Generated autoloads from yatexhlp.el

(autoload 'YaTeX-apropos "yatexhlp" "\


\(fn KEY)" t nil)

(autoload 'YaTeX-help "yatexhlp" "\
Show help buffer of LaTeX/TeX commands or macros.
Optional argument MACRO, if supplied, is directly selected to keyword.
Non-nil for optional second argument REF-ONLY inhibits call enrich-help
for non-interactive use.

\(fn &optional MACRO REF-ONLY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexhlp" '("YaTeX-")))

;;;***

;;;### (autoloads nil "yatexinf" "yatexinf.el" (0 0 0 0))
;;; Generated autoloads from yatexinf.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexinf" '("yatexinfo-")))

;;;***

;;;### (autoloads nil "yatexlib" "yatexlib.el" (0 0 0 0))
;;; Generated autoloads from yatexlib.el

(autoload 'YaTeX-kanji-ptex-mnemonic "yatexlib" "\
Return the kanji-mnemonic of pTeX from current buffer's coding-system.

\(fn)" nil nil)

(autoload 'YaTeX-define-key "yatexlib" "\
Define key on YaTeX-prefix-map.

\(fn KEY BINDING &optional MAP)" nil nil)

(autoload 'YaTeX-local-table-symbol "yatexlib" "\
Return the lisp symbol which keeps local completion table of SYMBOL.

\(fn SYMBOL)" nil nil)

(autoload 'YaTeX-sync-local-table "yatexlib" "\
Synchronize local variable SYMBOL.
Copy its corresponding directory dependent completion table to SYMBOL.

\(fn SYMBOL)" nil nil)

(autoload 'YaTeX-read-user-completion-table "yatexlib" "\
Append user completion table of LaTeX macros

\(fn &optional FORCETOREAD)" t nil)

(autoload 'YaTeX-reload-dictionary "yatexlib" "\
Reload local dictionary.
Use this function after editing ./.yatexrc.

\(fn)" t nil)

(autoload 'YaTeX-lookup-table "yatexlib" "\
Lookup WORD in completion table whose type is TYPE.
This function refers the symbol tmp-TYPE-table, user-TYPE-table, TYPE-table.
Typically, TYPE is one of 'env, 'section, 'fontsize, 'singlecmd.

\(fn WORD TYPE)" nil nil)

(autoload 'YaTeX-update-table "yatexlib" "\
Update completion table if the car of VALLIST is not in current tables.
Second argument DEFAULT-TABLE is the quoted symbol of default completion
table, third argument USER-TABLE is user table which will be saved in
YaTeX-user-completion-table, fourth argument LOCAL-TABLE should have the
completion which is valid during current Emacs's session.  If you
want to make LOCAL-TABLE valid longer span (but restrict in this directory)
create the file in current directory which has the same name with
YaTeX-user-completion-table.

\(fn VALLIST DEFAULT-TABLE USER-TABLE LOCAL-TABLE)" nil nil)

(autoload 'YaTeX-cplread-with-learning "yatexlib" "\
Completing read with learning.
Do a completing read with prompt PROM.  Completion table is what
DEFAULT-TABLE, USER-TABLE, LOCAL table are appended in reverse order.
Note that these tables are passed by the symbol.
Optional arguments PRED, REQMATH and INIT are passed to completing-read
as its arguments PREDICATE, REQUIRE-MATCH and INITIAL-INPUT respectively.
If optional 8th argument HSYM, history symbol, is passed, use it as
history list variable.

\(fn PROM DEFAULT-TABLE USER-TABLE LOCAL-TABLE &optional PRED REQMATCH INIT HSYM)" nil nil)

(autoload 'YaTeX-update-dictionary "yatexlib" "\


\(fn FILE SYMBOL &optional TYPE)" nil nil)

(autoload 'YaTeX-define-begend-key-normal "yatexlib" "\
Define short cut YaTeX-make-begin-end key.

\(fn KEY ENV &optional MAP)" nil nil)

(autoload 'YaTeX-define-begend-region-key "yatexlib" "\
Define short cut YaTeX-make-begin-end-region key.

\(fn KEY ENV &optional MAP)" nil nil)

(autoload 'YaTeX-define-begend-key "yatexlib" "\
Define short cut key for begin type completion.
Define both strokes for normal and region mode.
To customize YaTeX, user should use this function.

\(fn KEY ENV &optional MAP)" nil nil)

(autoload 'YaTeX-search-active-forward "yatexlib" "\
Search STRING which is not commented out by CMNTRX.
Optional arguments after BOUND, ERR, CNT are passed literally to search-forward
or search-backward.
Optional sixth argument FUNC changes search-function.

\(fn STRING CMNTRX &optional BOUND ERR CNT FUNC)" nil nil)

(autoload 'YaTeX-switch-to-buffer "yatexlib" "\
Switch to buffer if buffer exists, find file if not.
Optional second arg SETBUF t make use set-buffer instead of switch-to-buffer.

\(fn FILE &optional SETBUF)" t nil)

(autoload 'YaTeX-switch-to-buffer-other-window "yatexlib" "\
Switch to buffer if buffer exists, find file if not.

\(fn FILE)" t nil)

(autoload 'YaTeX-replace-format "yatexlib" "\
In STRING, replace first appearance of FORMAT to REPL as if
function `format' does.  FORMAT does not contain `%'

\(fn STRING FORMAT REPL)" nil nil)

(autoload 'YaTeX-replace-formats "yatexlib" "\


\(fn STRING REPLACE-LIST)" nil nil)

(autoload 'YaTeX-replace-format-args "yatexlib" "\
Translate the argument mark #1, #2, ... #n in the STRING into the
corresponding real arguments ARGS.

\(fn STRING &rest ARGS)" nil nil)

(autoload 'rindex "yatexlib" "\
Return the last position of STRING where character CHAR found.

\(fn STRING CHAR)" nil nil)

(autoload 'point-beginning-of-line "yatexlib" "\


\(fn)" nil nil)

(autoload 'point-end-of-line "yatexlib" "\


\(fn)" nil nil)

(autoload 'YaTeX-showup-buffer "yatexlib" "\
Make BUFFER show up in certain window (but current window)
that gives the maximum value by the FUNC.  FUNC should take an argument
of its window object.  Non-nil for optional third argument SELECT selects
that window.  This function never selects minibuffer window.

\(fn BUFFER &optional FUNC SELECT)" nil nil)

(autoload 'split-window-calculate-height "yatexlib" "\
Split current window wight specified HEIGHT.
If HEIGHT is number, make a new window that has HEIGHT lines.
If HEIGHT is string, make a new window that occupies HEIGT % of screen height.
Otherwise split window conventionally.

\(fn HEIGHT)" nil nil)

(autoload 'YaTeX-window-list "yatexlib" "\


\(fn)" nil nil)

(autoload 'substitute-all-key-definition "yatexlib" "\
Replace recursively OLDDEF with NEWDEF for any keys in KEYMAP now
defined as OLDDEF. In other words, OLDDEF is replaced with NEWDEF
where ever it appears.

\(fn OLDDEF NEWDEF KEYMAP)" nil nil)

(autoload 'YaTeX-match-string "yatexlib" "\
Return (buffer-substring (match-beginning n) (match-beginning m)).

\(fn N &optional M)" nil nil)

(autoload 'YaTeX-minibuffer-complete "yatexlib" "\
Complete in minibuffer.
  If the symbol 'delim is bound and is string, its value is assumed to be
the character class of delimiters.  Completion will be performed on
the last field separated by those delimiters.
  If the symbol 'quick is bound and is 't, when the try-completion results
in t, exit minibuffer immediately.

\(fn)" t nil)

(autoload 'completing-read-with-history "yatexlib" "\
Completing read with general history: gmhist, Emacs-19.

\(fn PROMPT TABLE &optional PREDICATE MUST-MATCH INITIAL HSYM)" nil nil)

(autoload 'read-from-minibuffer-with-history "yatexlib" "\
Read from minibuffer with general history: gmhist, Emacs-19.

\(fn PROMPT &optional INIT MAP READ HSYM)" nil nil)

(autoload 'read-string-with-history "yatexlib" "\
Read string with history: gmhist(Emacs-18) and Emacs-19.

\(fn PROMPT &optional INIT HSYM)" nil nil)

(fset 'YaTeX-rassoc (if (and nil (fboundp 'rassoc) (subrp (symbol-function 'rassoc))) (symbol-function 'rassoc) #'(lambda (key list) (let ((l list)) (catch 'found (while l (if (equal key (cdr (car l))) (throw 'found (car l))) (setq l (cdr l))))))))

(autoload 'YaTeX-delete1 "yatexlib" "\
Delete

\(fn ELT LIST)" nil nil)

(fset 'YaTeX-last-key (if (fboundp 'win:last-key) 'win:last-key #'(lambda nil (if (boundp 'last-command-char) last-command-char last-command-event))))

(autoload 'YaTeX-command-to-string "yatexlib" "\


\(fn CMD)" nil nil)

(autoload 'YaTeX-reindent "yatexlib" "\
Remove current indentation and reindento to COL column.

\(fn COL)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexlib" '("tfb-and-exit" "bcf-and-exit" "YaTeX-" "goto-buffer-window" "foreach-buffers" "latex-message-kanji-code")))

;;;***

;;;### (autoloads nil "yatexm-o" "yatexm-o.el" (0 0 0 0))
;;; Generated autoloads from yatexm-o.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexm-o" '("yatex-mode-")))

;;;***

;;;### (autoloads nil "yatexmth" "yatexmth.el" (0 0 0 0))
;;; Generated autoloads from yatexmth.el

(autoload 'YaTeX-toggle-math-mode "yatexmth" "\


\(fn &optional ARG)" t nil)

(autoload 'YaTeX-goto-corresponding-paren "yatexmth" "\
Go to corresponding mathematical parentheses.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexmth" '("YaTeX-")))

;;;***

;;;### (autoloads nil "yatexpkg" "yatexpkg.el" (0 0 0 0))
;;; Generated autoloads from yatexpkg.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexpkg" '("YaTeX-package-")))

;;;***

;;;### (autoloads nil "yatexprc" "yatexprc.el" (0 0 0 0))
;;; Generated autoloads from yatexprc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexprc" '("YaTeX-")))

;;;***

;;;### (autoloads nil "yatexsec" "yatexsec.el" (0 0 0 0))
;;; Generated autoloads from yatexsec.el

(autoload 'YaTeX-read-section-in-minibuffer "yatexsec" "\


\(fn PROMPT TABLE &optional DEFAULT DELIM)" t nil)

(autoload 'YaTeX-make-section-with-overview "yatexsec" "\
Input sectining command with previous overview.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatexsec" '("YaTeX-")))

;;;***

;;;### (autoloads nil nil ("yatex-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yatex-autoloads.el ends here
