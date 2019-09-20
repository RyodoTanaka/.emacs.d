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
  :config
  ;; lsp-mode
  ;; LSPの基本パッケージ
  (leaf lsp-mode
    :ensure t
    :hook ( (c-mode          . lsp)
           (c++-mode        . lsp)
           (prog-major-mode . lsp-prog-major-mode-enable))
    )
  ;; lsp-ui
  ;; LSPのカッチョ良いUIパッケージ
  (leaf lsp-ui
    :ensure t
    :hook (lsp-mode-hook . lsp-ui-mode)
    )
  ;; company-lsp
  ;; LSPベースの補間
  ;; (leaf company-lsp
  ;;   :ensure t
  ;;   :config
  ;;   (add-to-list 'company-backends 'company-lsp)
  ;;   )
  ;; lsp-treemacs
  ;; LSP用treemacs
  (leaf lsp-treemacs :ensure t)  
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
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto generated parameters         ;;
;; This part generates automatically ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ccls-executable "/usr/local/bin/ccls" t)
 '(doom-modeline-buffer-file-name-style (quote truncate-with-project))
 '(doom-modeline-icon t)
 '(doom-modeline-major-mode-icon nil)
 '(doom-modeline-minor-modes nil)
 '(doom-themes-enable-bold nil)
 '(doom-themes-enable-italic nil)
 '(el-get-git-shallow-clone t)
 '(highlight-indent-guides-method (quote character))
 '(neo-theme (quote nerd2) t)
 '(package-archives
   (quote
    (("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://mirrors.163.com/elpa/gnu/"))))
 '(package-selected-packages
   (quote
    (ccls google-c-style lsp-mode yaml-mode highlight-indent-guides which-key rainbow-delimiters imenu-list minimap hide-mode-line doom-modeline smooth-scroll mozc neotree doom-themes el-get hydra leaf-keywords leaf)))
 '(show-paren-style (quote mixed))
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))))

(provide 'init)
;;; End:
;;; init.el ends here
