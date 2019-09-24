;;; init.el ---
;;; Copyright (C) 2019  Ryodo Tanaka
;;; Author: Ryodo Tanaka
;;; Code:

;;;;;;;;;;;;;;;;;;
;; Requirements ;;
;;;;;;;;;;;;;;;;;;
(prog1 "Change user-emacs-directory"
  ;; enable debug
  (setq debug-on-error  t
        init-file-debug t)

  ;; you can run like 'emacs -q -l ~/hoge/init.el'
  (when load-file-name
    (setq user-emacs-directory
          (expand-file-name (file-name-directory load-file-name))))

  ;; change user-emacs-directory
  (setq user-emacs-directory
        (expand-file-name
         (format "local/%s.%s/"
                 emacs-major-version emacs-minor-version)
         user-emacs-directory))
  (make-directory user-emacs-directory t))

(prog1 "prepare leaf"
  (prog1 "package"
    (custom-set-variables
     '(package-archives '(("org"   . "https://orgmode.org/elpa/")
                          ("melpa" . "https://melpa.org/packages/")
                          ("gnu"   . "https://mirrors.163.com/elpa/gnu/"))))
    (package-initialize))

  (prog1 "leaf"
    (unless (package-installed-p 'leaf)
      (unless (assoc 'leaf package-archive-contents)
        (package-refresh-contents))
      (condition-case err
          (package-install 'leaf)
        (error
         (package-refresh-contents)       ; renew local melpa cache if fail
         (package-install 'leaf))))

    (leaf leaf-keywords
      :ensure t
      :config (leaf-keywords-init)))

  (prog1 "optional packages for leaf-keywords"
    ;; optional packages if you want to use :hydra, :el-get,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t
      :custom ((el-get-git-shallow-clone  . t)))))

;;;;;;;;;;;;;;;;;;;;;
;; Original Custom ;;
;;;;;;;;;;;;;;;;;;;;;
;;; Theme
;; テーマの設定
;; Doom Tomorrow Night
(leaf doom-themes
  :ensure t neotree 
  :custom
  (doom-themes-enable-italic . nil)
  (doom-themes-enable-bold . nil)
  :config
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  )

;;; User information setting
;; ユーザー情報の設定
(leaf *user-settings
  :config
  (setq user-full-name "Ryodo Tanaka"
        user-mail-address "GRoadPG@gmail.com")
  )

;;; Language setting
;; 言語，文字コード設定
(leaf *language-settings
  :config
  (set-language-environment 'Japanese) ;言語を日本語に
  (prefer-coding-system 'utf-8) ;極力UTF-8を使う
  (add-to-list 'default-frame-alist '(font . "hackgen-15")) ;フォント設定
  (leaf mozc ;; Mozc setting
    :ensure t
    :config
    (setq default-input-method "japanese-mozc")
    )
  )

;;; Editor setting
;; エディタ共通の設定
(leaf *editor-settings
  :config
  (set-frame-parameter nil 'alpha 98) ;背景透過
  (size-indication-mode t) ; ファイルサイズを表示
  (setq next-line-add-newlines nil) ;バッファの終わりでのnewlineを禁止する
  (global-font-lock-mode t) ;色分け設定
  (setq font-lock-support-mode 'jit-lock-mode) ;Just-In-Timeな文字装飾方式
  (keyboard-translate ?\C-h ?\C-?) ;C-hをバックスペースに変更
  )

;;; Start up setting
;; 起動時の設定
(leaf *startup-settings
  :config
  (setq inhibit-startup-message t) ;起動メッセージの非表示
  (setq inhibit-startup-echo-area-message -1) ;スタートアップ時のエコー領域メッセージの非表示
  )

;;; Backup setting
;; バッファのバックアップの設定
(leaf *backup-settings
  :config
  (setq make-backup-files nil) ;変更ファイルのバックアップ
  (setq version-control nil) ;変更ファイルの番号つきバックアップ
  (setq auto-save-list-file-name nil) ;編集中ファイルのバックアップ
  (setq auto-save-list-file-prefix nil)
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))) ;編集中ファイルのバックアップ先
  (setq auto-save-timeout 30) ;編集中ファイルのバックアップ間隔（秒）
  (setq auto-save-interval 500) ;編集中ファイルのバックアップ間隔（打鍵）
  (setq kept-old-versions 1) ;バックアップ世代数
  (setq kept-new-versions 2)
  (setq trim-versions-without-asking nil) ;上書き時の警告表示
  (setq delete-old-versions t) ;古いバックアップファイルの削除
  )

;;; Scroll setting
;; スクロールに関する設定
(leaf *scroll-settings
  :config
  (setq scroll-preserve-screen-position t) ;スクロール時のカーソル位置の維持
  ;; smooth-scroll
  ;; スクロールがスムーズになる
  (leaf smooth-scroll
    :ensure t
    :config
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
    (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
    (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
    (setq scroll-step 1) ;; keyboard scroll one line at a time
    )
  )

;;; Tab, Space setting
;; タブ，スペースに関する設定
(leaf *tab-space-settings
  :config
  (setq-default tab-width 4 indent-tabs-mode nil) ;タブにスペースを使用する
  (setq-default tab-width 4 indent-tabs-mode nil) ;タブにスペースを使用する
  )

;;; Line Setting
;; 行番号の設定
(leaf *line-settings
  :config
  ;; linum
  ;; 行番号の表示設定
  (leaf linum
    :ensure t
    :config
    (global-linum-mode t)
    (setq linum-format " %d")
    )
  )

;;; Mode Line settings
;; モードライン(下のバー)に関する設定
(leaf *modeline-settings
  :config
  ;; doom-modeline
  ;; doom を利用した mode-line
  (leaf doom-modeline
    :ensure t
    :custom
    (doom-modeline-buffer-file-name-style . 'truncate-with-project)
    (doom-modeline-icon . t)
    (doom-modeline-major-mode-icon . nil)
    (doom-modeline-minor-modes . nil)
    :hook (after-init-hook . doom-modeline-mode)
    :config
    (line-number-mode 0)
    (column-number-mode 0)
    (doom-modeline-def-modeline 'main
      '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
      '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))
    )
  ;; Hide mode line
  ;; 特定のモードでモードラインを非表示にする
  (leaf hide-mode-line
    :ensure t neotree minimap imenu-list
    :hook
    ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode)
    )
  )

;;; deliminator-settings
;; 括弧に関する設定
(leaf *deliminator-settings
  :config
  ;; rainbow-delimiters
  ;; 括弧を虹色に設定してくれる
  (leaf rainbow-delimiters
    :ensure t
    :hook
    (prog-mode-hook . rainbow-delimiters-mode)
    )
  ;; paren
  ;; 括弧を色付きにしてくれる
  (leaf paren
    :ensure t
    :hook
    (after-init-hook . show-paren-mode)
    :custom-face
    (show-paren-match . '((nil (:background "#44475a" :foreground "#f1fa8c"))))
    :custom ((show-paren-style . 'mixed)
             (show-paren-when-point-inside-paren . t)
             (show-paren-when-point-in-periphery . t))
    )
  )

;;; which-key
;; キーバインド覚えなくて良くするやつ
(leaf which-key
  :ensure t
  :hook (after-init-hook . which-key-mode)
  :config
  (which-key-setup-minibuffer)
  (setq which-key-idle-secondary-delay 0)
  )

;;; highlight-indent-guides
;; ソースコードのインデントを見やすくしてくれる
(leaf highlight-indent-guides
  :ensure t yaml-mode
  :custom (highlight-indent-guides-method . 'character)
  :hook
  (prog-mode-hook . highlight-indent-guides-mode)
  (yaml-mode-hook . highlight-indent-guides-mode)
  ;; :config
  ;; (setq highlight-indent-guides-method 'character)

  ) 

;;; neotree
;; ファイル階層を開いてくれる
;; F9 で開いたり閉じたりするように設定
(leaf neotree
  :ensure t
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :custom (neo-theme . 'nerd2)
  :bind
  ("<f9>" . neotree-projectile-toggle)
  :preface
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
         ;;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name)))))
    )
  )
;;; Emacs26 specified setting
;; Emacs 26.1以上に関する設定
(leaf *emacs26-settings
  :when (version<= "26.1" emacs-version)
  :config
  (setq default-mode-line-format (default-value 'mode-line-format))
  (add-to-list 'default-mode-line-format '(:eval (count-lines-and-chars)))
  )

;;;;;;;;;;;;;;;;;;;;;
;; IDE environment ;;
;;;;;;;;;;;;;;;;;;;;;
(leaf *lsp-basic-settings  
  :url "https://github.com/emacs-lsp/lsp-mode#supported-languages"
  :url "https://github.com/MaskRay/ccls/wiki/lsp-mode#find-definitionsreferences"
  :doc "lsp is language server protocol"
  :when (version<= "25.1" emacs-version)
  :ensure use-package
  :config
  ;; lsp-mode
  ;; LSPの基本パッケージ
  (use-package lsp-mode
    :ensure t
    :commands lsp
    :custom
    ((lsp-enable-snippet t)
     (lsp-enable-indentation nil)
     (lsp-prefer-flymake nil)
     (lsp-document-sync-method 'incremental)
     (lsp-inhibit-message t)
     (lsp-message-project-root-warning t)
     (create-lockfiles nil))
    :init
    (unbind-key "C-l")
    :bind
    (("C-l C-l"  . lsp)
     ("C-l h"    . lsp-describe-session)
     ("C-l t"    . lsp-goto-type-definition)
     ("C-l r"    . lsp-rename)
     ("C-l <f5>" . lsp-restart-workspace)
     ("C-l l"    . lsp-lens-mode))
    :hook
    (prog-major-mode . lsp-prog-major-mode-enable))
  ;; lsp-ui
  ;; LSPのカッチョ良いUIパッケージ
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :after lsp-mode
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-position 'top)
    (lsp-ui-doc-max-width  60)
    (lsp-ui-doc-max-height 20)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit nil)
    ;; lsp-ui-flycheck
    (lsp-ui-flycheck-enable t)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable t)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics t)
    (lsp-ui-sideline-show-code-actions t)
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable nil)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-always-show t)
    (lsp-ui-peek-peek-height 30)
    (lsp-ui-peek-list-width 30)
    (lsp-ui-peek-fontify 'always)
    :hook
    (lsp-mode . lsp-ui-mode)
    :bind
    (("C-l s"   . lsp-ui-sideline-mode)
     ("C-l C-d" . lsp-ui-peek-find-definitions)
     ("C-l C-r" . lsp-ui-peek-find-references)))
  ;; company-lsp
  ;; LSPベースの補間
  ;; company-lsp
  (use-package company-lsp
    :ensure t
    :commands company-lsp company
    :custom
    (company-lsp-cache-candidates nil)
    (company-lsp-async t)
    (company-lsp-enable-recompletion t)
    (company-lsp-enable-snippet t)
    :after
    (:all lsp-mode lsp-ui company yasnippet)
    :init
    (push 'company-lsp company-backends))
  ;; lsp-treema
  ;; LSP用treemacs
  (leaf lsp-treemacs :ensure t)  
  )

;;; company
;; 補間機能が使えるようにする
(use-package company
  :ensure t
  :custom
  (company-transformers '(company-sort-by-backend-importance))
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (completion-ignore-case t)
  :bind
  (("C-M-c" . company-complete))
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-s" . company-filter-candidates)
        ("C-i" . company-complete-selection)
        ([tab] . company-complete-selection))
  (:map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        )
  :hook (after-init-hook .  global-company-mode)
  :config
  ;; lowercaseを優先にするソート
  (defun my-sort-uppercase (candidates)
    (let (case-fold-search
          (re "\\`[[:upper:]]*\\'"))
      (sort candidates
            (lambda (s1 s2)
              (and (string-match-p re s2)
                   (not (string-match-p re s1)))))))
  (push 'my-sort-uppercase company-transformers)
  ;; yasnippetとの連携
  (defvar company-mode/enable-yas t)
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )

;;; git-complete
;; Gitから補間をしてくれる
(leaf git-complete
  :require t
  :package popup
  :el-get (zk-phi/git-complete :branch "master")
  :init (unbind-key "C-c C-c")
  :hook (after-init-hook . git-complete)
  :custom (git-complete-enable-autopair . t)
)

;;; yasnipet
;; スニペットを使えるようにする
(leaf *snipet-settings
  :ensure use-package
  :config
  (use-package yasnippet
    :ensure t
    :hook (after-init . yas-global-mode)
    :bind
    (:map yas-minor-mode-map
          ("C-x i n" . yas-new-snippet)
          ("C-x i v" . yas-visit-snippet-file)
          ("C-M-i"   . yas-insert-snippet))
    (:map yas-keymap
          ("<tab>" . nil)) ;; because of avoiding conflict with company keymap
    )
  )

;;; Yatex setting
;; Yatexの設定
(leaf yatex
  :ensure t
  :mode ("\\.tex$" . yatex-mode)
  :bind ("C-c C-t" . YaTeX-typeset-menu)
  :hook ((yatex-mode . turn-on-reftex)
         (latex-mode . (lambda ()
                         (define-key tex-mode-map "\t" 'latex-indent-command)
                         (define-key tex-mode-map "\M-\C-\\" 'latex-indent-region-command)))
         )
  :config
  (setq YaTeX-inhibit-prefix-letter t)
  (setq YaTeX-kanji-code nil)
  (setq YaTeX-latex-message-code 'utf-8)
  (setq tex-command "latexmk -pvc")  ;;保存したら自動で再コンパイル
  (setq dvi2-command "evince")
  (setq bibtex-command "pbibtex")     ; BibTeX のコマンド
  (when  (eq system-type 'gnu/linux) ; for GNU/Linux
    ;; inverse search
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
            (move-to-n col)))))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;
;; Language settings ;;
;;;;;;;;;;;;;;;;;;;;;;;
;;; nxml-mode
;; xml言語の設定
(leaf nxml-mode
  :mode (("\\.launch\\'")
         ("\\.xacro\\'")
         ("\\.urdf\\'")
         ("\\.config\\'")
         ("\\.sdf\\'")
         ("\\.world\\'"))
  )

;;; yaml-mode
;; yaml言語の設定
(leaf yaml-mode
  :ensure t;
  :mode (("\\.yml\\'")
         ("\\.yaml\\'"))
  )

;;; C, C++ style
;; C, C++言語の設定
(leaf c-c++
  :config
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.pde\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
  ;; original cc-mode hooks
  ;; オリジナルのcc-mode用hook
  (leaf cc-mode
    :require t
    :preface
    (defun ROS-c-mode-hook()
      (setq c-basic-offset 2)
      (setq indent-tabs-mode nil)
      (c-set-offset 'substatement-open 0)
      (c-set-offset 'innamespace 0)
      (c-set-offset 'case-label '+)
      (c-set-offset 'brace-list-open 0)
      (c-set-offset 'brace-list-intro '+)
      (c-set-offset 'brace-list-entry 0)
      (c-set-offset 'member-init-intro 0)
      (c-set-offset 'statement-case-open 0)
      (c-set-offset 'arglist-intro '+)
      (c-set-offset 'arglist-cont-nonempty '+)
      (c-set-offset 'arglist-close '+)
      (c-set-offset 'template-args-cont '+))
    :hook
    (c-mode-common-hook . ROS-c-mode-hook)
    (c++-mode-common-hook . ROS-c-mode-hook)
    )
  ;; google-c-style
  ;; Google-c-styleを利用する
  (leaf google-c-style
    :ensure t
    :hook
    (c-mode-common-hook . google-set-c-style)
    (c++-mode-common-hook . google-set-c-style)
    )
  ;; lsp setting
  ;; LSPに関する設定
  (leaf *lsp-setting
    :config
    ;; ccls
    ;; c,c++のLSP server
    (use-package ccls
      :custom
      (ccls-executable "/usr/local/bin/ccls")
      (ccls-sem-highlight-method 'font-lock)
      (ccls-use-default-rainbow-sem-highlight)
      :hook ((c-mode c++-mode objc-mode) .
             (lambda () (require 'ccls) (lsp))))
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto generated parameters         ;;
;; This part generates automatically ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)
;;; End:
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-buffer-file-name-style (quote truncate-with-project) t)
 '(doom-modeline-icon t t)
 '(doom-modeline-major-mode-icon nil t)
 '(doom-modeline-minor-modes nil t)
 '(doom-themes-enable-bold nil)
 '(doom-themes-enable-italic nil)
 '(el-get-git-shallow-clone t)
 '(git-complete-enable-autopair t t)
 '(highlight-indent-guides-method (quote character))
 '(neo-theme (quote nerd2) t)
 '(package-archives
   (quote
    (("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://mirrors.163.com/elpa/gnu/"))))
 '(package-selected-packages
   (quote
    (google-c-style yasnippet yaml-mode which-key use-package smooth-scroll rainbow-delimiters neotree mozc minimap lsp-ui lsp-treemacs leaf-keywords imenu-list highlight-indent-guides hide-mode-line el-get doom-themes doom-modeline ccls)))
 '(show-paren-style (quote mixed) t)
 '(show-paren-when-point-in-periphery t t)
 '(show-paren-when-point-inside-paren t t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))))
