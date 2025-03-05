(load "vv.el") ;; load helpers
;;(load "~/.emacs.d/dape/dape.el")

;; set warning level
(setq warning-minimum-level :error)


;; .emacs variables

(defconst .emacs/default-face-size (cond  ((equal "COREI3" (system-name)) 190)
					  ((equal "THINKPAD" (system-name)) 190)
					  (t 230)))

(defconst .emacs/image-export-path "~/../../../photo-export-emacs")

(defconst .emacs/default-coding-system 'cp1251-dos)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(column-number-mode t)
 '(image-scaling-factor 0.5) ;; 0.7143
 '(cua-mode t nil (cua-base)))

;; Remove junk symbols in shell mode
(setenv "PS1" "\\[\\e[32m\\]\\u@\\h \\[\\e[33m\\]\\w\\[\\e[0m\\]\\n\\$")


;; ================== MELPA Package Support ===============

;; Enables basic packaging support
(require 'package)

;; Adds the Melpa archive to the list of available repositories

;;(add-to-list 'package-archives
;;             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;;(setq package-check-signatures nil) ;; disable signature checking

;; Installs packages
;; my packages list
(defvar my-packages
  '(
    ayu-theme       ;; Ayu theme
    use-package	    ;; Package configuration macros
    auto-complete
    paredit
    rainbow-delimiters
    which-key
    svg
    basic-mode
    org-download
    org-bullets
    company
    python
    jupyter
    google-translate
;;    ein
    ))

;; packages not tested on 29 version, but working on 28
(vv/when-version< "29.1"
  (let ((my-packages28 '(
			 ;; better-defaults ;; Set up some better Emacs defaults
			 ;; py-autopep8	 ;; Run autopep8 on save
			 ;; blacken	 ;; Black formatting on save
			 ;; pc-bufsw
			 ;; flycheck
			 ;; undo-fu
			 ;; markdown-mode
			 ;; yasnippet
			 ;; helm-lsp
			 ;; projectile
			 ;; hydra
			 ;; avy
			 ;; helm-xref
			 ;; dap-mode
			 ;; cython-mode
			 ;; flycheck-cython
			 ;; cmake-mode
			 ;; cmake-project
			 )))

    (setq my-packages (append my-packages my-packages28))))

;; Scans the list in my-packages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      my-packages)

;; =============== Basic Customization ===============

(menu-bar-mode 0)                   ;; Hide menu bar
(setq visible-bell t)               ;; enable visible bell
(setq ring-bell-function 'ignore)   ;; disable ring sound
(setq inhibit-startup-message t)    ;; Hide the startup message
(load-theme 'ayu-light t)
;; (load-theme 'material t)         ;; Load material theme
;; (load-theme 'spacemacs-light t)
;; (load-theme 'leuven t)
;; (global-linum-mode t)            ;; Enable line numbers globaly
(add-hook 'prog-mode-hook #'display-line-numbers-mode) ;; enable line numbers only in code buffers
(setq linum-offset t)
(set-default 'truncate-lines t)
(desktop-save-mode 1)               ;; save desktop on exit mode is ON
(put 'upcase-region 'disabled nil)
(scroll-bar-mode -1)                ;; Hide scrollbars
(tool-bar-mode 0)
(set-cursor-color "chocolate1")
(blink-cursor-mode 0)
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")  ;; tweek selection (region) attributes
(setq delete-by-moving-to-trash t)  ;; enable move to trash bin
(put 'narrow-to-region 'disabled nil) ;; enable narrowing

;; disable second selection shortcuts
(global-unset-key [M-mouse-1])
(global-unset-key [M-drag-mouse-1])
(global-unset-key [M-down-mouse-1])
(global-unset-key [M-mouse-3])
(global-unset-key [M-mouse-2])

;; setup for super key used in s-... shortcuts
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key
(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key
(setq w32-apps-modifier 'control) ; Menu key as Ctrl

(defun .emacs/display-formfeed-as-line-hook ()
  "Adds hook for displaying formfeed ^L chars as line when opening .el files"
  (when (string= (file-name-extension buffer-file-name) "el")
    (vv/display-formfeed-set-as-line)))
(add-hook 'find-file-hook '.emacs/display-formfeed-as-line-hook)

;;(pc-bufsw t) ;; Enables PC style quick buffer switcher for Emacs

(setq
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1) ;; Scrolling parameters

(defvar .emacs/default-face-properties-list
  (list :family "basis33" :foundry "outline" :antialias 'none :slant 'normal :weight 'normal :height .emacs/default-face-size :width 'normal))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 (list 'default (list (list t .emacs/default-face-properties-list))) ;; default face
 '(fixed-pitch ((t (:inherit default :family "basis33" :foreground "light green"))))
 '(fixed-pitch-serif ((t (:inherit default :foreground "light blue"))))
 ;;'(variable-pitch ((default nil) (nil nil)))
 '(variable-pitch ((t (:family "basis33"))))
 '(help-for-help-header ((t (:height 1.02))))
 '(helm-source-header ((t (:weight bold :height 1.05))))
 '(helm-ff-dotted-directory ((t (:background "gainsboro"))))
 '(tooltip ((default nil) (nil nil)))
 '(shadow ((t (:foreground "light blue"))))
 '(line-number ((t (:inherit default :foreground "#e3e4e5"))))
 '(line-number-current-line ((t (:inherit default :foreground "#ff772d"))))
 '(outline-2 ((t (:inherit font-lock-variable-name-face :foreground "salmon"))))
 '(org-block ((t (:inherit shadow :foreground "gray50" :background "white smoke"))))
 '(org-meta-line ((t (:background "gainsboro"))))
 '(org-level-1 ((t (:inherit outline-1 :weight bold :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :weight bold :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :weight bold :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :weight bold :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :weight bold :height 1.0))))
 '(org-level-6 ((t (:inherit outline-6 :weight bold :height 1.0))))
 '(org-level-7 ((t (:inherit outline-7 :weight bold :height 1.0))))
 '(org-level-8 ((t (:inherit outline-8 :weight bold :height 1.0)))))


(set-fontset-font "fontset-default" 'windows-1251 "basis33") ;; set font for russian characters

;; echo buffer tweeks
(defun .emacs/get-font-prop-list (size)
  (list (list 'default (list :height size))))

(with-current-buffer (get-buffer " *Echo Area 0*")
  (setq-local face-remapping-alist (.emacs/get-font-prop-list (/ 0.8 0.8))))

(with-current-buffer (get-buffer "*Messages*")
  (setq-local face-remapping-alist (.emacs/get-font-prop-list (/ 0.6 0.6))))

;; key bindings
(global-set-key (kbd "C-z") 'undo-fu-only-undo)
(global-set-key (kbd "C-y") 'undo-fu-only-redo)
(global-set-key (kbd "C-x f") 'find-file) ;; reset "C-x f" to find file same as "C-x C-f"
(global-set-key (kbd "C-x r") 'read-only-mode)
(global-set-key (kbd "C-x v") 'pyvenv-activate)
(global-set-key [(control tab)] 'ff-find-other-file)
(define-key lisp-interaction-mode-map (kbd "C-e") 'eval-print-last-sexp)

;; vv key bidings
(global-set-key (kbd "C-c v e") 'vv/ipynb-export-image-at-point) ;; export image from notebook at point

;; SVG support
(auto-image-file-mode 1)
(require 'svg)

;; benchmarking
(require 'benchmark)

;; ============== Coding system stuff =====================
(prefer-coding-system .emacs/default-coding-system)  ;; default coding system
;; (setq process-coding-system-alist
;;       (cons '("bash" . (cp1251-dos . cp1251-dos))
;; 	    process-coding-system-alist)) ;; coding system for eshell on windows
(add-to-list 'file-coding-system-alist '("\\.org" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.py" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.cpp" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.hpp" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.c" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.h" . utf-8))

;; IDO mode. Interactively do things.
(ido-mode 1)
(ido-everywhere) ;; disabled, because incomatible with helm-mode
(setq ido-enable-flex-matching t)
(fido-mode)

;; Show stray whitespace.
(setq-default show-trailing-whitespace nil)
(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries 'left)

(defun .emacs/show-trailing-whitespace ()
  "Enable `show-trailing-whitespace' in shell modes."
  (when (memq major-mode '(python-mode
			   emacs-lisp-mode
			   c++-mode
			   c-mode))
    (setq show-trailing-whitespace t
	  indicate-empty-lines t)))

(add-hook 'after-change-major-mode-hook '.emacs/show-trailing-whitespace)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)

;; Workaround for https://debbugs.gnu.org/34341 in GNU Emacs <= 26.3.
(when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Write customizations to a separate file instead of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Make dired open in the same window when using RET or ^
(require 'dired)
(put 'dired-find-alternate-file 'disabled nil) ; disables warning
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;; Enable Paredit.
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; Enable Rainbow Delimiters.
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;; Customize Rainbow Delimiters.
(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray

;; Auto complete stuff
(ac-config-default) ;; default auto-complete config
(setq tab-always-indent 'complete) ;; autocompletion visible in minibuffer
(add-to-list 'completion-styles 'initials t)
(add-hook 'c-mode-hook (lambda () (auto-complete-mode -1)))           ;; disable auto-complete
(add-hook 'c++-mode-hook (lambda () (auto-complete-mode -1)))         ;; in c/c++ modes
(add-hook 'python-mode-hook (lambda () (auto-complete-mode -1)))      ;; and python mode too

;; Which key mode - display available keybindings in popup
(which-key-mode)

;; Company mode
(use-package company
  :ensure t
  ;; :bind (:map company-mode-map
  ;; 	      ("<tab>" . company-complete))
  :config
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 1)
  ;;(global-set-key (kbd "<C-return>") 'company-complete)
  ;;(global-company-mode 1)
  )


;; ====================== emacs lisp setup ===================

(defun .emacs/elisp-mode-eval-buffer ()
  (interactive)
  (message "Evaluated buffer")
  (eval-buffer))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'.emacs/elisp-mode-eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") #'.emacs/elisp-mode-eval-buffer)

(add-hook 'emacs-lisp-mode-hook  (lambda ()
				   (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)))

;; ====================== C/C++ ==============================
(require 'cc-mode)

(defun .emacs/c-recompile-debug()
  (interactive)
  (call-interactively 'recompile))

(defun .emacs/c-debug ()
  (interactive)
  (condition-case nil
      (call-interactively 'gdb)
    (error nil)))

(defun .emacs/after-c-compile-debug ()
  (let ((file-windows-config "~/.emacs.d/gdb-windows.conf"))
    (when (file-exists-p file-windows-config)
      (gdb-load-window-configuration file-windows-config)
	;;(goto-char (point-max))
	)))

(advice-add '.emacs/c-recompile-debug :after '.emacs/c-debug)
(advice-add '.emacs/c-debug :after '.emacs/after-c-compile-debug)
;;(add-hook 'gdb-mode-hook #'.emacs/after-c-compile-debug)

(use-package c-mode
  :hook ((c-mode . eglot-ensure)
	 (c-mode . company-mode)
	 (c++-mode . eglot-ensure)
	 (c++-mode . company-mode))
  :bind (:map c-mode-map
	      (("<f5>" . compile)
	       ("C-<f5>" . .emacs/c-recompile-debug)
	       ("<f8>" . eglot-format))))

(setq c-default-style "linux")
;;(setq c-default-style "gnu")
(setq-default c-basic-offset 4)

;; hide-show mode
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

(font-lock-add-keywords 'c-mode '("\\<nil\\>"))

;; ;; company mode
;; (add-hook c-mode-common-hook 'company-mode)
;; (add-hook c-mode-hook 'company-mode)
;; (add-hook c++-mode-hook 'company-mode)

;; ;; eglot
;; (add-hook 'c-mode-common-hook 'eglot-ensure)
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)

;; (add-hook 'c++-mode-hook (lambda ()
;; 			   (modify-syntax-entry ?_ "w" c++-mode-hook)))

;; set  F5 key to run compile!
(global-set-key [f5] 'compile)


;; ======================== Basic-mode setup ===========

(autoload 'basic-generic-mode "basic-mode" "Major mode for editing BASIC code." t)
(add-to-list 'auto-mode-alist '("\\.bas\\'" . basic-generic-mode))

;; ======================== org-mode ===================

(setq org-startup-with-inline-images t)  ;; display inline images at startup
(setq org-support-shift-select 'always)  ;; enable selecting text with arrow keys using shift

;; org-download package
(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable) ;; Drag-and-drop to `dired`

;; custom functions
(defun org-custom-save-image-from-clipboard (dest-file-path)
  "This saves any image that might be in the windows clipboard to the file at DEST-FILE-PATH"
  (let* ((win-fname (replace-regexp-in-string "\n\\'" "" (shell-command-to-string (concat "cygpath -w " dest-file-path))))
         (cmd-str (concat "powershell.exe -Command \"(Get-Clipboard -Format image).Save('" win-fname "')\"")))
    (shell-command cmd-str)))

(use-package org-download
  :ensure t
  :after org
  :config
  (setq-default
   org-download-image-dir "assets"
   ;; Basename setting seems to be simply ignored.
   org-download-screenshot-basename ".org.png"
   org-download-timestamp "org_%Y%m%d-%H%M%S_"
   org-download-heading-lvl nil)
  :custom
  (org-download-screenshot-method #'org-custom-save-image-from-clipboard)
  :bind
  (:map org-mode-map
        (("C-M-y" . org-download-screenshot)
         ("s-y" . org-download-yank))))
;;(setq org-download-screenshot-method #'my-save-image-from-clipboard)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;;(setq org-bullets-bullet-list '("◉" "▸" "▹" "◦" "▪" "▫" "•"))
(setq org-bullets-bullet-list '("◉" "￮" "⊚" "⊛" "⊕" "⊙"))

;; set tags faces
(add-to-list 'org-tag-faces '("urgent" . (:foreground "red")))
(add-hook 'org-mode-hook
          (lambda ()
            ;; (push '("[ ]" .  "🞎") prettify-symbols-alist)
            ;; (push '("[X]" . "🗷" ) prettify-symbols-alist)
            ;; (push '("[-]" . "◫" ) prettify-symbols-alist)
	    (push '("urgent" . "☭" ) prettify-symbols-alist)  ;; replace tag with glyph symbol
            (prettify-symbols-mode)))

;; custom image-url org-mode link type for remote image urls
(org-add-link-type
 "image-url"
 (lambda (path)
   (let ((img (expand-file-name
               (concat "org-mode-image-url-temp" "." (file-name-extension path))
               temporary-file-directory)))
     (if (file-exists-p img)
	 (find-file img)
       (url-copy-file path img)
       (find-file img)))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((octave . t)
   (python . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (or  (string= lang "octave")
	    (string= lang "python"))))  ;don't ask for octave and python
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)

;; =====================================================

;; Hide/Show
(require 'hideshow)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))
(global-set-key (kbd "C-c h") 'hs-toggle-hiding)

;; ;; ======================  lsp mode and stuff ============================
;; ;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
;; (helm-mode)
;; (require 'helm-xref)
;; (define-key global-map [remap find-file] #'helm-find-files)
;; (define-key global-map [remap execute-extended-command] #'helm-M-x)
;; (define-key global-map [remap switch-to-buffer] #'helm-mini)

;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)

;; (setq gc-cons-threshold (* 100 1024 1024)
;;       read-process-output-max (* 1024 1024)
;;       treemacs-space-between-root-nodes nil
;;       company-idle-delay 0.0
;;       company-minimum-prefix-length 1
;;       lsp-idle-delay 0.1)  ;; clangd is fast

;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;   (require 'dap-cpptools)
;;   (yas-global-mode))

;; (setq lsp-keymap-prefix "s-n") ;; s-l used to locscreen in windows so rebind it to s-n

;; ;; ====================== cmake stuff ========================

;; (defun maybe-cmake-project-mode ()
;;   (if (or (file-exists-p "CMakeLists.txt")
;;           (file-exists-p (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
;;       (cmake-project-mode)))

;; (add-hook 'c-mode-hook 'maybe-cmake-project-mode)
;; (add-hook 'c++-mode-hook 'maybe-cmake-project-mode)

;; (add-hook 'find-file-hook (lambda ()
;; 			    (when (string= (buffer-file-name) "CMakeLists.txt")
;; 			      (cmake-mode))))

;; =========================== Eglot ========================
(use-package eglot
  :ensure t
  :defer t
  :hook ((python-mode . eglot-ensure)
	 ;;(python-mode . hs-minor-mode)
	 )
  :bind (:map eglot-mode-map
	      ("C-c d" . eldoc)
	      ("C-c a" . eglot-code-actions)
	      ("C-c r" . eglot-rename))
  ;;   (("<f7>" . dape-step-in)
  ;;    ("<f8>" . dape-next)
  ;;    ("<f9>" . dape-continue))
  )
;; (add-hook 'eglot-managed-mode-hook (lambda ()
;; 				     (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)
;; 				     (flymake-eslint-enable)))

;; ================== python development setup ===================
(use-package python-mode
  :hook ((python-mode . eglot-ensure)
	 (python-mode . company-mode)
	 (python-mode . (lambda ()
			  (setq-local compile-command
				      (concat "python "
					      (when buffer-file-name
						(
						 ;;shell-quote-argument
						 buffer-file-name)))))))
  :bind (:map python-mode-map
	      ("<f5>" . recompile)
	      ("<f6>" . eglot-format))
  :mode (("\\.py\\'" . python-mode)))

(use-package python
  :hook ((python-mode . hs-minor-mode)))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)
  (defun .emacs/set-python-shell-interpreter-to-jupyter()
    (setq python-shell-interpreter "Scripts/jupyter.exe"
	  python-shell-interpreter-args "console --simple-prompt"
	  python-shell-prompt-detect-failure-warning-warning nil))

  (defun .emacs/set-python-shell-interpreter-to-ipython()
    (setq python-shell-interpreter (concat pyvenv-virtual-env "Scripts/ipython.exe")
	  python-shell-interpreter-args "-i --simple-prompt"))

  (defun .emacs/set-python-shell-interpreter-to-python()
    (setq python-shell-interpreter "python"
	  python-shell-interpreter-args "-i"))

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
	(cond ((equal pyvenv-virtual-env-name "diffusers")
               (list '.emacs/set-python-shell-interpreter-to-jupyter))
	      (t (list '.emacs/set-python-shell-interpreter-to-python))))

  (setq pyvenv-post-deactivate-hooks
        (list '.emacs/set-python-shell-interpreter-to-python))
  ;; activate default environmen in this case `diffusers'
  (pyvenv-activate "~/.virtualenvs/diffusers/"))

;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")

;; ein custoize group properties
(setq ein:output-area-inlined-images t)

;; ;; Goto function definition kbd hook
;; (defun goto-def-or-rgrep ()
;;   "Go to definition of thing at point or do an rgrep in project if that fails"
;;   (interactive)
;;   (condition-case nil (elpy-goto-definition)
;;     (error (elpy-rgrep-symbol (thing-at-point 'symbol)))))
;; (define-key elpy-mode-map (kbd "M-.") 'goto-def-or-rgrep)

;; (use-package dape
;;   :config
;;   (add-to-list 'dape-configs
;;                `(debugpy
;;                  modes (python-ts-mode python-mode)
;;                  command "python3"
;;                  command-args ("-m" "debugpy.adapter")
;;                  :type "executable"
;;                  :request "launch"
;;                  :cwd dape-cwd-fn
;;                  :program dape-find-file-buffer-default)))

;; ========================= telega ======================================
;;(require 'telega)

;; ========================= Google Translate ============================
(require 'facemenu)
(require 'google-translate)
(require 'google-translate-smooth-ui)
(defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
(setq google-translate-backend-method 'curl)
(global-set-key "\C-ct" 'google-translate-smooth-translate)
(setq google-translate-translation-directions-alist
      '(("en" . "ru") ("ru" . "en")))
(setq google-translate-pop-up-buffer-set-focus nil)

(defun .emacs/google-translate-coding ()
  "Changes 'Google translate' window coding to utf-8"
  (set-buffer-file-coding-system 'utf-8)
  (when (and (string= (buffer-name) "*Google Translate*")
	     (not (string-search "1utf-8" (symbol-name buffer-file-coding-system))))
    (progn
      (setq inhibit-read-only 1)
      (save-restriction
	(widen)
	;; (encode-coding-region (point-min)
	;; 		      (point-max)
	;; 		      .emacs/default-coding-system)
	;; (decode-coding-region (point-min)
	;; 		      (point-max)
	;; 		      'utf-8)
	;;(set-buffer-file-coding-system 'utf-8)
	)
      (setq inhibit-read-only nil))
    ))
;;(add-hook 'buffer-list-update-hook '.emacs/google-translate-coding)
;;(add-hook 'help-mode-hook '(lambda () (set-buffer-file-coding-system 'utf-8-dos)))

;; ================ dirvish (improved dired) ============================
;;(dirvish-override-dired-mode)

;; ========================= Start server. ===============================
(require 'server)
(unless (server-running-p)
  (server-start))

(message "Configuration vv.emacs.windows10.el loaded!")
(if (daemonp)
    (message on!)
  (message "Loading in regular Emacs!"))
