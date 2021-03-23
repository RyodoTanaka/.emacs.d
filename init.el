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
  (leaf all-the-icons
    :ensure t
    :custom
    (all-the-icons-scale-factor . 1.0)
    :config
    (let ((font-dest (cl-case window-system
                       ;; Default Linux install directories
                       (x  (concat (or (getenv "XDG_DATA_HOME")
                                       (concat (getenv "HOME") "/.local/share"))
                                   "/fonts/"))
                       (mac (concat (getenv "HOME") "/Library/Fonts/" ))
                       (ns (concat (getenv "HOME") "/Library/Fonts/" )))))
      )
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
  ;; Docモードやpdf-modeで行番号は付けない
  (add-hook 'doc-view-mode-hook
            (lambda ()
              (linum-mode -1)
              ))
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (linum-mode -1)
              ))
  ;; ファイルが #! から始まる場合， +x (実行権限) を付けて保存する
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)
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

;;; symbol-voerlay
;; 同じ名前のところを強調する
(leaf symbol-overlay
  :ensure t
  :bind (("M-i" . symbol-overlay-put)
         (symbol-overlay-map
          ((kbd "C-p") . symbol-overlay-jump-prev) ;; 次のシンボルへ
          ((kbd "C-n") . symbol-overlay-jump-next) ;; 前のシンボルへ
          ((kbd "C-g") . symbol-overlay-remove-all) ;; ハイライトキャンセル
          ))
  :hook ((prog-mode-hook . symbol-overlay-mode)
         (markdown-mode-hook . symbol-overlay-mode))
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
  :custom
  (highlight-indent-guides-method . 'character)
  (highlight-indent-guides-auto-enabled . nil)
  :hook
  (prog-mode-hook . highlight-indent-guides-mode)
  (yaml-mode-hook . highlight-indent-guides-mode)
  (text-mode-hook . highlight-indent-guides-mode)
  (web-mode-hook . highlight-indent-guides-mode)
  :config
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  ) 

;;; hs-minor-mode
;; コードの折りたたみ機能の追加
(leaf hs-minor-mode
  :hook
  (prog-mode-hook . (lambda () (hs-minor-mode 1))) 
  :bind (("C-c C-f" . hs-toggle-hiding)
         ("C-c C-a" . hs-show-all)
         ("C-c C-g" . hs-hide-all))
  )

;;; multiple-cursors
;; 複数行同時編集のためのパッケージ
;; (leaf multiple-cursors
;;   :bind ((kbd "C-x C-e") . mc/edit-lines)
;;   :ensure t
;;   )

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

;;; el-screen
;; タブ機能
(leaf elscreen
  :ensure t
  :preface
  (global-unset-key (kbd "C-z"))
  ;; :bind
  ;; (elscreen-prefix-key . (kbd "C-z"))
  :custom
  (elscreen-tab-display-kill-screen . nil)
  (elscreen-tab-display-control . nil)
  (elscreen-display-screen-number . nil)
  :hook (after-init-hook . elscreen-start)
  )

;;; ivy & swiper
;; 文字検索用
(leaf ivy
  :ensure t swiper counsel
  :hook (after-init-hook . ivy-mode)
  :custom
  (ivy-use-virtual-buffers . t)
  (enable-recursive-minibuffers . t)
  (ivy-height . 15) ;; minibufferのサイズを拡大！（重要）
  (ivy-extra-directories . nil)
  (ivy-re-builders-alist. '((t . ivy--regex-plus)))
  :config
  (global-set-key "\C-s" 'swiper)
  (global-set-key "\C-r" 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (defvar swiper-include-line-number-in-search t) ;; line-numberでも検索可能
  ;; 日本語でも検索可能に
  (leaf avy-migemo
    :ensure t
    :hook (ivy-mode-hook . avy-migemo-mode)
    )
  ;; ivy-rich
  (leaf ivy-rich
    :ensure t all-the-icons-ivy all-the-icons
    :hook (ivy-mode-hook . ivy-rich-mode)
    :preface
    (defun ivy-rich-switch-buffer-icon (candidate)
      (with-current-buffer
          (get-buffer candidate)
        (let ((icon (all-the-icons-icon-for-mode major-mode)))
          (if (symbolp icon)
              (all-the-icons-icon-for-mode 'fundamental-mode)
            icon))))
    :config
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    (setq ivy-rich--display-transformers-list
          '(ivy-switch-buffer
            (:columns
             ((ivy-rich-switch-buffer-icon :width 2)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand)))))
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
  :ensure flycheck
  :config
  ;; lsp-mode
  ;; LSPの基本パッケージ
  (leaf lsp-mode
    :ensure t
    :commands lsp
    :custom
    ((lsp-enable-snippet . t)
     (lsp-enable-indentation . nil)
     (lsp-prefer-flymake . nil)
     (lsp-document-sync-method . 'incremental)
     (lsp-inhibit-message . t)
     (lsp-message-project-root-warning . t)
     (create-lockfiles . nil)
     (lsp-file-watch-threshold .nil))
    :preface (global-unset-key (kbd "C-l"))
    :bind
    (("C-l C-l"  . lsp)
     ("C-l h"    . lsp-describe-session)
     ("C-l t"    . lsp-goto-type-definition)
     ("C-l r"    . lsp-rename)
     ("C-l <f5>" . lsp-restart-workspace)
     ("C-l l"    . lsp-lens-mode))
    :hook
    (prog-major-mode-hook . lsp-prog-major-mode-enable)
    )
  ;; lsp-ui
  ;; LSPのカッチョ良いUIパッケージ
  (leaf lsp-ui
    :ensure t 
    :commands lsp-ui-mode
    :after lsp-mode
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable . t)
    (lsp-ui-doc-header . t)
    (lsp-ui-doc-include-signature . t)
    (lsp-ui-doc-position . 'top)
    (lsp-ui-doc-max-width . 60)
    (lsp-ui-doc-max-height . 20)
    (lsp-ui-doc-use-childframe . t)
    (lsp-ui-doc-use-webkit . nil)
    ;; lsp-ui-flycheck
    (lsp-ui-flycheck-enable . t)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable . nil)
    (lsp-ui-sideline-ignore-duplicate . t)
    (lsp-ui-sideline-show-symbol . nil)
    (lsp-ui-sideline-show-hover . nil)
    (lsp-ui-sideline-show-diagnostics . nil)
    (lsp-ui-sideline-show-code-actions . nil)
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable . nil)
    (lsp-ui-imenu-kind-position . 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable . t)
    (lsp-ui-peek-always-show . t)
    (lsp-ui-peek-peek-height . 30)
    (lsp-ui-peek-list-width . 30)
    (lsp-ui-peek-fontify . 'always)
    :hook
    (lsp-mode-hook . lsp-ui-mode)
    :bind
    (("C-l s"   . lsp-ui-sideline-mode)
     ("C-l C-d" . lsp-ui-peek-find-definitions)
     ("C-l C-r" . lsp-ui-peek-find-references))
    )
  ;; company-lsp
  ;; LSPベースの補間
  (leaf company-lsp
    :url "https://github.com/tigersoldier/company-lsp"
    :commands company-lsp company
    :custom
    (company-lsp-cache-candidates . nil)
    (company-lsp-async . t)
    (company-lsp-enable-recompletion . t)
    (company-lsp-enable-snippet . t)
    :after
    (:all lsp-mode lsp-ui company yasnippet)
    ;; lsp-treema
    ;; LSP用treemacs
    (leaf lsp-treemacs :ensure t)  
    )
  )

;;; flycheck
;; 構文チェック
(leaf flycheck
  :ensure t
  :commands flycheck-mode
  :hook (prog-mode-hook . flycheck-mode) 
  :custom
  (flycheck-check-syntax-automatically . '(save new-line))
  (flycheck-idle-change-delay . 5.0)
  (flycheck-display-errors-delay . 0.9)
  (flycheck-highlighting-mode . 'symbols)
  (flycheck-indication-mode . 'left-fringe)
  (flycheck-standard-error-navigation . t)
  (flycheck-deferred-syntax-check . nil)
  (flycheck-completion-system . nil)
  )

;;; company
;; 補間機能が使えるようにする
(leaf company
  :ensure t company-irony slime-company company-c-headers company-auctex company-math
  :custom
  (company-transformers . '(company-sort-by-backend-importance))
  (company-idle-delay . 0)
  (company-echo-delay . 0)
  (company-minimum-prefix-length . 2)
  (company-selection-wrap-around . t)
  (completion-ignore-case . t)
  :bind
  (("C-M-c" . company-complete)
   (company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-s" . company-filter-candidates)
    ("C-i" . company-complete-selection)
    ([tab] . company-complete-selection))
   (company-search-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous))
   )
  :hook
  (after-init-hook . global-company-mode)
  (after-init-hook . company-mode)
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
  (company-auctex-init)
  (slime-setup '(slime-fancy slime-company))
  (add-to-list 'company-backends '(company-bbdb
                                   company-nxml
                                   company-css
                                   company-eclim
                                   company-semantic
                                   company-clang
                                   company-xcode
                                   company-cmake
                                   company-capf
                                   company-dabbrev-code
                                   company-gtags
                                   company-etags
                                   company-keywords
                                   company-oddmuse
                                   company-files
                                   company-dabbrev
                                   mapcar
                                   company-mode/backend-with-yas
                                   slime-company
                                   company-irony
                                   company-c-headers
                                   company-auctex
                                   company-math-symbols-unicode
                                   company-lsp
                                   company-elisp
                                   company-yasnippet)
               )
  )

;;; company-box
;; companyの起動時に出るboxの設定
(leaf company-box
  :ensure t
  :after (company all-the-icons)
  :hook ((company-mode-hook . company-box-mode))
  :custom
  (company-box-icons-alist . 'company-box-icons-all-the-icons)
  (company-box-doc-enable . nil))

;;; company-quickhelp
;; companyのhelpの設定
(leaf company-quickhelp
  :ensure t
  :hook (company-mode-hook . company-quickhelp-mode)
  )

;;; git-complete
;; Gitから補間をしてくれる
(leaf git-complete
  :require t
  :package popup
  :el-get (zk-phi/git-complete :branch "master")
  :preface (global-unset-key (kbd "C-c C-c"))
  :hook (after-init-hook . git-complete)
  :custom (git-complete-enable-autopair . t)
  )

;;; flyspell-mode
;; 動的にスペルチェックしてくれる
(leaf flyspell
  :ensure t
  :hook
  (text-mode-hook . flyspell-mode)
  (org-mode-hook . flyspell-mode)
  (prog-mode-hook . flyspell-prog-mode) 
  )

;;; ispell-check
;; 静的にスペルチェックしてくれる
(leaf *ispell-check
  :config
  (setq-default ispell-program-name "aspell")
  (with-eval-after-load "ispell"
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
  )

;;; yasnipet
;; スニペットを使えるようにする
(leaf *snipet-settings
  :config
  (leaf yasnippet
    :ensure t
    :hook (after-init . yas-global-mode)
    :bind
    ((yas-minor-mode-map
      ("C-x i n" . yas-new-snippet)
      ("C-x i v" . yas-visit-snippet-file)
      ("C-M-i"   . yas-insert-snippet))
     (yas-keymap
      ("<tab>" . nil)));; because of avoiding conflict with company keymap
    )
  )

;;; magit
;; magitの設定
(leaf magit
  :ensure t
  :bind ("C-x g" . magit-status)
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
  (setq YaTeX-use-font-lock t)
  (setq tex-command "latexmk -pvc -bibtex")  ;;保存したら自動で再コンパイル
  (setq dvi2-command "evince")
  (setq bibtex-command "pbibtex -kanji=utf8")     ; BibTeX のコマンド
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

;;; smart-jump
;; 定義ジャンプ
(leaf smart-jump
  :ensure t ivy
  :bind
  ("C-c C-j" . smart-jump-go)
  :custom
  (dumb-jump-mode . t)
  (dumb-jump-selector . 'ivy) ;; 候補選択をivyに任せます
  (dumb-jump-use-visible-window . nil)
  :config
  (smart-jump-setup-default-registers)
  )

;;;;;;;;;;;;;;;;;;;;;;;
;; Language settings ;;
;;;;;;;;;;;;;;;;;;;;;;;
;;; script-settings
;; scriptの設定
(leaf executable
  :ensure t
  :hook
  ;;ファイルが #! から始まる場合， +x (実行権限) を付けて保存する
  (after-save-hook . executable-make-buffer-file-executable-if-script-p)
  )

;;; nxml-mode
;; xml言語の設定
(leaf nxml-mode
  :mode (("\\.launch\\'")
         ("\\.xacro\\'")
         ("\\.urdf\\'")
         ("\\.config\\'")
         ("\\.sdf\\'")
         ("\\.world\\'")
         ("\\.test\\'"))
  )

;;; yaml-mode
;; yaml言語の設定
(leaf yaml-mode
  :ensure t;
  :mode (("\\.yml\\'")
         ("\\.yaml\\'")
         ("\\.cnoid\\'")
         )
  )

;;; cmake-mode
(leaf cmake-mode
  :ensure t
  :mode
  ("CMakeLists\\.txt\\'" . cmake-mode)
  ("\\.cmake\\'" . cmake-mode)
  
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
      (hide-ifdef-mode t)
      (hide-ifdefs)
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
  ;; clang-format
  ;; clang-formatを使用する
  (leaf clang-format
    :ensure t
    :custom
    (clang-format-style-option . "llvm")    
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
    (leaf ccls
      :ensure t
      :custom
      (ccls-executable .  "/usr/local/bin/ccls")
      ;; (ccls-sem-highlight-method . 'font-lock)
      ;; (ccls-use-default-rainbow-sem-highlight .)
      :hook ((c-mode-hook c++-mode-hook objc-mode-hook) .
             (lambda () (require 'ccls) (lsp))))
    )
  )

;;; web-mode
;; Web modeの設定
(leaf web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[gj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         )
  :custom
  (web-mode-engines-alist . '(("php"    . "\\.phtml\\'")
                              ("blade"  . "\\.blade\\.")))
  (web-mode-enable-current-element-highlight . t)
  :preface
  (defun my-web-mode-hook () "Hooks for Web mode." 
         (setq web-mode-markup-indent-offset 2) 
         (setq web-mode-markup-indent-offset 2)
         (setq web-mode-css-indent-offset 2)
         (setq web-mode-code-indent-offset 2)
         ) 
  :hook (web-mode-hook . my-web-mode-hook)
  )

;;; php-mode
;; PHP modeの設定
(leaf php-mode
  :ensure t
  )

;;; csharp-mode
;; C# modeの設定
(leaf csharp-mode
  :ensure t
  )

;;; Org mode
;; Org modeの設定
(leaf *org-mode-settings
  :config
  ;; ox-latex
  (leaf ox-latex
    :require t
    :custom (org-latex-listings . t)
    )
  ;; ox-gfm
  (leaf ox-gfm
    :ensure t
    )
  ;; org-mode
  (leaf org-mode
    :bind(("\C-cl" . org-store-link)
          ("\C-cc" . org-capture)
          ("\C-ca" . org-agenda)
          ("\C-cb" . org-iswitchb))
    :config
    ;; 拡張子
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
    (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
    (add-to-list 'auto-mode-alist '("\\.text\\'" . org-mode))
    ;; hyperref
    (add-to-list 'org-latex-packages-alist "\\hypersetup{colorlinks=true}" . org-mode)
    (add-to-list 'org-latex-packages-alist "\\hypersetup{citecolor=blue}" . org-mode)
    (add-to-list 'org-latex-packages-alist "\\hypersetup{linkcolor=red}" . org-mode)
    (add-to-list 'org-latex-packages-alist "\\hypersetup{urlcolor=orange}" . org-mode)
    :custom
    ;; 画像をインラインで表示
    (org-startup-with-inline-images . t)
    ;; 見出しの余分な*を消す
    (org-hide-leading-stars . t)
    ;; LOGBOOK drawerに時間を格納する
    (org-clock-into-drawer . t)
    ;; TODO状態
    (org-todo-keywords . '((sequence "TODO(t)" "WAIT(w)" "NOTE(n)"  "|" "DONE(d)" "SOMEDAY(s)" "CANCEL(c)")))
    ;; DONEの時刻を記録
    (org-log-done . 'time)
    ;; イメージサイズの大きさを画面に合わせる
    (org-image-actual-width . nil)
    ;; latexのコンパイラ設定
    (org-latex-pdf-process . '("latexmk -f %f"))
    ;; hperref の設定
    (org-latex-with-hyperref . nil)
    ;; Latex のHeader 設定
    (org-format-latex-header . '("\\documentclass[11pt,a4paper,dvipdfmx]{jarticle}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty} % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}z
\\addtolength{\\topmargin}{-2.54cm}
\\lstset{%
language={C},
basicstyle={\\small},%
identifierstyle={\\small},%
commentstyle={\\small\\itshape},%
keywordstyle={\\small\\bfseries},%
ndkeywordstyle={\\small},%
stringstyle={\\small\\ttfamily},
frame={tb},
breaklines=true,
columns=[l]{fullflexible},%
numbers=left,%
xrightmargin=0zw,%
xleftmargin=3zw,%
numberstyle={\\scriptsize},%
stepnumber=1,
numbersep=1zw,%
lineskip=-0.5ex}"))
    ;; Latex のデフォルトパッケージ設定
    (org-latex-default-packages-alist . (quote (("AUTO" "inputenc" t
                                                 ("pdflatex"))
                                                ("T1" "fontenc" t
                                                 ("pdflatex"))
                                                ("" "graphicx" t nil)
                                                ("" "grffile" t nil)
                                                ("" "longtable" nil nil)
                                                ("" "wrapfig" nil nil)
                                                ("" "rotating" nil nil)
                                                ("normalem" "ulem" t nil)
                                                ("" "amsmath" t nil)
                                                ("" "textcomp" t nil)
                                                ("" "amssymb" t nil)
                                                ("" "capt-of" nil nil)
                                                ("" "bm" nil nil)
                                                ("" "ascmac" nil nil)
                                                ("usenames" "color" nil nil)
                                                ("" "cite" nil nil)
                                                ("" "latexsym" nil nil)
                                                ("" "url" nil nil)
                                                ("" "algorithm" nil nil)
                                                ("" "algpseudocode" nil nil)
                                                ("" "examplep" nil nil)
                                                ("" "subfigure" nil nil)
                                                ("toc,page" "appendix" nil nil)
                                                ("" "forloop" nil nil)
                                                ("" "tablefootnote" nil nil)
                                                ("yyyymmdd" "datetime" nil nil)
                                                ("" "listings" nil nil)
                                                ("dvipdfmx" "hyperref" nil nil)
                                                ("dvipdfmx" "graphicx" nil nil)
                                                ("dvipdfmx" "xcolor" nil nil)
                                                ("" "cite" nil nil)
                                                ("" "caption" nil nil)
                                                ("" "subcaption" nil nil)
                                                ("" "siunitx" nil nil))
                                               )
                                      )
    )
  )

;;; dockerfile mode
;; Dockerfile 用の設定
(leaf dockerfile-mode
  :ensure t
  :mode (("Dockerfile" . dockerfile-mode))
  )

;;; Google Translate
;; Google Translate を使えるようにする
(leaf google-translate
  :ensure t
  :preface
  (defvar google-translate-english-chars "[:ascii:]’“”–"
    "これらの文字が含まれているときは英語とみなす")
  (defun google-translate-enja-or-jaen (&optional string)
    "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
    (interactive)
    (setq string
          (cond ((stringp string) string)
                (current-prefix-arg
                 (read-string "Google Translate: "))
                ((use-region-p)
                 (buffer-substring (region-beginning) (region-end)))
                (t
                 (save-excursion
                   (let (s)
                     (forward-char 1)
                     (backward-sentence)
                     (setq s (point))
                     (forward-sentence)
                     (buffer-substring s (point)))))))
    (let* ((asciip (string-match
                    (format "\\`[%s]+\\'" google-translate-english-chars)
                    string)))
      (run-at-time 0.1 nil 'deactivate-mark)
      (google-translate-translate
       (if asciip "en" "ja")
       (if asciip "ja" "en")
       string)))
  :config
  (global-set-key (kbd "C-c t") 'google-translate-enja-or-jaen)
  )


(provide 'init)
;;; End:
;;; init.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto generated parameters         ;;
;; This part generates automatically ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-themes-enable-bold nil)
 '(doom-themes-enable-italic nil)
 '(el-get-git-shallow-clone t)
 '(package-archives
   (quote
    (("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://mirrors.163.com/elpa/gnu/"))))
 '(package-selected-packages
   (quote
    (arduino-mode yatex yaml-mode which-key web-mode symbol-overlay smooth-scroll smart-jump slime-company rainbow-delimiters php-mode ox-gfm neotree mozc minimap magit lsp-ui leaf-keywords ivy-rich imenu-list hydra highlight-indent-guides hide-mode-line google-translate google-c-style flycheck elscreen el-get doom-themes doom-modeline dockerfile-mode csharp-mode counsel company-quickhelp company-math company-lsp company-irony company-c-headers company-box company-auctex cmake-mode clang-format ccls avy-migemo all-the-icons-ivy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))) nil "Customized with leaf in `paren' block at `/home/ryodo/.emacs.d/init.el'"))
