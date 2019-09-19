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
                          ("gnu"   . "https://elpa.gnu.org/packages/"))))
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
;; Tomorrow Night
(leaf tomorrow-night-theme
  :when (version<= "24.1" emacs-version)
  :el-get (chriskempson/tomorrow-theme :branch "master" :load-path "GNU Emacs")
  :require t)
;; Gruvbox
;; (leaf gruvbox-theme
;;   :when (version<= "24.1" emacs-version)
;;   :ensure t
;;   :config
;;   (load-theme 'gruvbox-dark-hard t))

;;; User setting
(leaf *user-settings
  :config
  (setq user-full-name "Ryodo Tanaka"
        user-mail-address "GRoadPG@gmail.com")
  )

;;; Language setting
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
(leaf *editor-settings
  :config
  (show-paren-mode t) ;括弧のハイライト
  (set-frame-parameter nil 'alpha 95) ;背景透過
  (size-indication-mode t) ; ファイルサイズを表示
  (setq next-line-add-newlines nil) ;バッファの終わりでのnewlineを禁止する
  (global-font-lock-mode t) ;色分け設定
  (setq font-lock-support-mode 'jit-lock-mode) ;Just-In-Timeな文字装飾方式
  (keyboard-translate ?\C-h ?\C-?) ;C-hをバックスペースに変更
  )

;;; Start up setting
(leaf *startup-settings
  :config
  (setq inhibit-startup-message t) ;起動メッセージの非表示
  (setq inhibit-startup-echo-area-message -1) ;スタートアップ時のエコー領域メッセージの非表示
  )

;;; Scroll setting
(leaf *scroll-settings
  :config
  (setq scroll-preserve-screen-position t) ;スクロール時のカーソル位置の維持

  (leaf smooth-scroll
    :ensure t
    :config
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
    (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
    (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
    (setq scroll-step 1) ;; keyboard scroll one line at a time
    )
  )

;;; Backup setting
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

;;; Tab, Space setting
(leaf *tab-space-settings
  :config
  (setq-default tab-width 4 indent-tabs-mode nil) ;タブにスペースを使用する
  (setq-default tab-width 4 indent-tabs-mode nil) ;タブにスペースを使用する
  ;;(global-whitespace-mode 1) ;スペース、タブなどを可視化する
  )

;;; Line Setting
(leaf *line-settings
  :config
  (leaf linum ;行番号の表示
    :ensure t
    :config
    (global-linum-mode t)
    (setq linum-format " %d ")
    )
  )

;;; Emacs26 specified setting
(leaf *emacs26-settings
  :when (version<= "26.1" emacs-version)
  :config
  (setq default-mode-line-format (default-value 'mode-line-format))
  (add-to-list 'default-mode-line-format '(:eval (count-lines-and-chars)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom-set settings ;;                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This part will work with GUI setting on Emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(el-get-git-shallow-clone t)
 '(package-archives
   (quote
    (("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages (quote (gruvbox-theme el-get hydra leaf-keywords leaf))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; End:
;;; init.el ends here
