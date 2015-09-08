(add-to-list 'load-path "~/.emacs.d/")
;;; platfrom-p(http://ongaeshi.hatenablog.com/entry/20120725/1343232098)
(require 'platform-p)
;;; カッコのハイライト
(show-paren-mode t)

;;; 行番号の表示
;;;(line-number-mode t)
(require 'linum)
(global-linum-mode)

;;; 列番号の表示
;;;(column-number-mode t)

;;; スクロール時のカーソル位置の維持
(setq scroll-preserve-screen-position t)

;;; スクロール行数（一行ごとのスクロール）
(setq vertical-centering-font-regexp ".*")
(setq scroll-conservatively 35)
(setq scroll-margin 0)
(setq scroll-step 1)

;;; 画面スクロール時の重複行数
(setq next-screen-context-lines 1)

;;; 起動メッセージの非表示
(setq inhibit-startup-message t)

;;; スタートアップ時のエコー領域メッセージの非表示
(setq inhibit-startup-echo-area-message -1)

;;; @ backup

;;; 変更ファイルのバックアップ
(setq make-backup-files nil)

;;; 変更ファイルの番号つきバックアップ
(setq version-control nil)

;;; 編集中ファイルのバックアップ
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;;; 編集中ファイルのバックアップ先
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; 編集中ファイルのバックアップ間隔（秒）
(setq auto-save-timeout 30)

;;; 編集中ファイルのバックアップ間隔（打鍵）
(setq auto-save-interval 500)

;;;バックアップ世代数
(setq kept-old-versions 1)
(setq kept-new-versions 2)

;;; 上書き時の警告表示
;;; (setq trim-versions-without-asking nil)

;;; 古いバックアップファイルの削除
(setq delete-old-versions t)

(require 'cedet)



;;;(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)
;; 情報源として
    ;; * ac-source-filename
    ;; * ac-source-words-in-same-mode-buffers
    ;; を利用
(setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers))
(setq ac-auto-start 2) ;;;2文字以上で補完
(setq ac-delay 0.05) ;;;0.05秒後に補完開始
(setq ac-use-fuzzy t) ;;;曖昧補完
(setq ac-use-comphist t) ;;;補完推測機能有効
(setq ac-auto-show-menu 0.05) ;;;補完メニューを表示
(setq ac-quick-help-delay 0.5) ;;;クイックヘルプを表示
(setq ac-ignore-caes nil) ;;;大文字と小文字を区別する


;;; C++ style
(add-hook 'c++-mode-hook
          '(lambda()
             (c-set-style "stroustrup")
             (setq indent-tabs-mode nil)     ;; インデントは空白文字で行う（TABコードを空白に変換）
	     (setq tab-width 2)
             (c-set-offset 'innamespace 0)   ;;;namespace {}の中はインデントしない
             (c-set-offset 'arglist-close 0) ;;;関数の引数リストの閉じ括弧はインデントしない
             )
	  (semantic-mode 1)
	  ;;; (setq ac-sources (append ac-sources '(ac-source-semantic)))
	  (setq ac-sources (append ac-sources '(ac-source-semantic-raw)))
	  )


;;; smooth-scroll
(require 'smooth-scroll)
(smooth-scroll-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;;; one line at at time
(setq mouse-wheel-progressive-speed nil) ;;; dont accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;;; scroll window under mouse
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tool-bar-position (quote bottom)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;YaTexの設定
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode)  auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq load-path (cons (expand-file-name "~/.emacs.d/site-lisp/yatex") load-path))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)

;;;(setq tex-command "latexmk -pvc")  ;;保存したら自動で再コンパイル
(setq tex-command "latexmk")
(setq dvi2-command "evince")

;;;auto-complete latex
(require 'auto-complete-latex)
(setq ac-l-dict-directory "~/.emacs.d/ac-l-dict/")
(add-to-list 'ac-modes 'foo-mode)
(add-hook 'foo-mode-hook 'ac-l-setup)

(when platform-linux-p ; for GNU/Linux
;;; inverse search
  (require 'dbus)
  
  (defun un-urlify (fname-or-url)
	"A trivial function that replaces a prefix of file:/// with just /."
	(if (string= (substring fname-or-url 0 8) "file:///")
		(substring fname-or-url 7)
	  fname-or-url))
  
  (defun evince-inverse-search (file linecol &rest ignored)
	(let* ((fname (un-urlify file))
		   (buf (find-file fname))
		   (line (car linecol))
		   (col (cadr linecol)))
	  (if (null buf)
		  (message "[Synctex]: %s is not opened..." fname)
		(switch-to-buffer buf)
		(goto-line (car linecol))
		(unless (= col -1)
		  (move-to-column col)))))
  
  (dbus-register-signal
   :session nil "/org/gnome/evince/Window/0"
   "org.gnome.evince.Window" "SyncSource"
   'evince-inverse-search)
  )

;;; auto-install

(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;;; popwin
(require 'popwin)
(popwin-mode 1)
;;; popwin(for yatex)
(require 'popwin-yatex)
(push '("*YaTeX-typesetting*") popwin:special-display-config)


;;; color-theme
(load-theme 'deeper-blue t)

;;; flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; ;;; flycheck-pos-tip
;; (eval-after-load 'flycheck
;;   '(custom-set-variables
;;    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))


;;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; 
(defun in-directory (dir)
  "Runs execute-extended-command with default-directory set to the given directory."
  (interactive "DIn directory: ")
  (let ((default-directory dir))
	(call-interactively 'execute-extended-command)))

(global-set-key (kbd "M-X") 'in-directory)

; roslaunch highlighting
(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))

;; shellに色をつける
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-filter-hook
          '(lambda ()
             (let ((start-marker (make-marker))
                   (end-marker (process-mark (get-buffer-process (current-buffer)))))
               (set-marker start-marker (point-min))
               (ansi-color-apply-on-region start-marker end-marker))))

;; yatex-indent
(autoload 'latex-indent-command "~/misc/latex-indent"
  "Indent current line accroding to LaTeX block structure.")
(autoload 'latex-indent-region-command "~/misc/latex-indent"
  "Indent each line in the region according to LaTeX block structure.")
(add-hook
 'latex-mode-hook
 '(lambda ()
    (define-key tex-mode-map "\t"       'latex-indent-command)
    (define-key tex-mode-map "\M-\C-\\" 'latex-indent-region-command)))

;; elscreen.el
;;; プレフィクスキーはC-z
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)
;;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)
;;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))

;; smartparent
(smartparens-global-mode)
;; [DEL]キーもしくは[C-h]に当てられているdelete-backward-charにadviceをかけられて削除するたびにフリーズする．これを無効化.
(ad-disable-advice 'delete-backward-char 'before 'sp-delete-pair-advice)
(ad-activate 'delete-backward-char)

;; gnuplot-mode
(require 'gnuplot-mode)
(setq auto-mode-alist 
(append '(("\\.\\(gp\\|gnuplot\\|plt\\)$" . gnuplot-mode)) auto-mode-alist))

;; doc-view-modeのときに行番号を表示すると非常に重たい
(add-hook 'doc-view-mode-hook
		  (lambda ()
			(linum-mode -1)
			))
(add-hook 'pdf-view-mode-hook
		  (lambda ()
			(linum-mode -1)
			))
;; pdf-tools
(pdf-tools-install)
;;(setq revert-without-query 'yes)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
