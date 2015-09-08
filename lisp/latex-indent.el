;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; latex-indent.el
;;; Time-stamp: <07/04/06 09:48:31 wachi@closed.hit.ac.jp>
;;; create:	Aug. 27, 2001
;;; author:	WACHI, Akihito
;;; copyright (C) 2001, 2004, 2007 WACHI, Akihito
;;; $Id: latex-indent.el,v 1.5 2007/04/06 00:49:43 wachi Exp $
;;;
;;; This program provides indentation commands to `latex-mode'.
;;; To use them, put this file somewhere
;;; (`~/misc/latex-indent.el' below; it appears twice),
;;; and add the following commented-out code to your `.emacs.el'
;;; (with the leading semicolons removed).
;; ------------------------------------------------------------------ 
;; (autoload 'latex-indent-command "~/misc/latex-indent"
;;   "Indent current line accroding to LaTeX block structure.")
;; (autoload 'latex-indent-region-command "~/misc/latex-indent"
;;   "Indent each line in the region according to LaTeX block structure.")
;; (add-hook
;;  'latex-mode-hook
;;  '(lambda ()
;;     (define-key tex-mode-map "\t"       'latex-indent-command)
;;     (define-key tex-mode-map "\M-\C-\\" 'latex-indent-region-command)))

(defconst latex-indent-level 2)
(defconst latex-empty-line-re "[ \t]*$")
(defconst latex-indent-beg-re
  (concat "[ \t]*\\("
	  "\\\\begin{[a-zA-Z*]+}\\|"
	  (mapconcat (lambda (s) (regexp-quote s)) 
		     '( "(" "{" "[" "\\langle" "\\lbrack" "\\["
			"\\left(" "\\left\\{" "\\left[" "\\left." )
		     "\\|")
	  "\\)" ))
(defconst latex-indent-end-re
  (concat "[ \t]*\\("
	  "\\\\end{[a-zA-Z*]+}\\|"
	  (mapconcat (lambda (s) (regexp-quote s)) 
		     '( ")" "}" "]" "\\rangle" "\\rbrack" "\\]"
			"\\right)" "\\right\\}" "\\right]" "\\right." )
		     "\\|")
	  "\\)" ))
(defconst latex-skipped-begin-re
  (concat "[ \t]*"
	  "\\\\begin{\\("
	  (mapconcat (lambda (s) (regexp-quote s)) 
		     '( "document" "abstract" 
			"definition" "remark" "proposition" "theorem" "lemma"
			"corollary" "example" "proof" )
		     "\\|")
	  "\\)}" ))
(defconst latex-skipped-end-re
  (concat "[ \t]*"
	  "\\\\end{\\("
	  (mapconcat (lambda (s) (regexp-quote s)) 
		     '( "document" "abstract" 
			"definition" "remark" "proposition" "theorem" "lemma"
			"corollary" "example" "proof" )
		     "\\|")
	  "\\)}" ))

(defun latex-eat-indent-end ()
  (let ((num 0))
    (while (and (not (looking-at latex-skipped-end-re))
		(looking-at latex-indent-end-re))
      (setq num (1+ num))
      (goto-char (match-end 0)))
    num))

(defun latex-parse-line ()
  (save-excursion
    (beginning-of-line)
    (let ((lead-end-indent (latex-eat-indent-end))
	  (nest (/ (current-indentation) latex-indent-level))
	  (nest-add 0)
	  num-end)
      (while (not (eolp))
	(cond
	 ((and (not (looking-at latex-skipped-begin-re))
	       (looking-at latex-indent-beg-re))
	  (setq nest-add (1+ nest-add))
	  (goto-char (match-end 0)))
	 ((< 0 (setq num-end (latex-eat-indent-end)))
	  (setq nest-add (- nest-add num-end)))
	 (t
	  (forward-char))))
      (list nest lead-end-indent nest-add))))

(defun latex-calculate-indent (prev-parse cur-parse)
  (save-excursion
    (beginning-of-line)
    (if (not prev-parse)
	(setq prev-parse
	      (if (bobp)
		  '(0 0 0)
		(beginning-of-line 0)
		(while (and (not (bobp))
			    (looking-at latex-empty-line-re))
		  (beginning-of-line 0))
		(latex-parse-line))))
    (* latex-indent-level
       (- (+ (car prev-parse) (nth 2 prev-parse)) (cadr cur-parse)))))

(defun latex-indent-command ()
  "Indent current line accroding to LaTeX block structure."
  (interactive)
  (let ((old-col (current-column))
	beg-of-line
	old-indent
	new-indent)
    (beginning-of-line)
    (setq beg-of-line (point))
    (back-to-indentation)
    (setq old-indent (current-column))
    (delete-region beg-of-line (point))
    (beginning-of-line)
    (indent-to (latex-calculate-indent nil (latex-parse-line)))
    (setq new-indent (current-column))
    (if (< old-indent old-col)
	(move-to-column (+ old-col (- new-indent old-indent))))))

(defun latex-indent-region-command ()
  "Indent each line in the region according to LaTeX block structure."
  (interactive)
  (if (< (mark) (point))
      (exchange-point-and-mark))
  (beginning-of-line)
  (let ((region-beg (point))
	(line-beg (point))
	(parse (latex-parse-line))
	prev-parse
	cur-indent)
    (setq cur-indent (latex-calculate-indent nil parse))
    (back-to-indentation)
    (delete-region line-beg (point))
    (indent-to cur-indent)
    (while (progn (end-of-line) (< (point) (mark)))
      (beginning-of-line 2)
      (setq line-beg (point))
      (setq prev-parse (cons (/ cur-indent latex-indent-level) (cdr parse)))
      (setq parse (latex-parse-line))
      (setq cur-indent (latex-calculate-indent prev-parse parse))
      (back-to-indentation)
      (delete-region line-beg (point))
      (indent-to cur-indent))
    (goto-char region-beg)
    (exchange-point-and-mark)))
;;;
;;; end of latex-indent.el;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
