(load "vv.el") ;; load helpers
(load "purge-folder.el") ;; purge-folder command


;; .emacs variables

(defconst .emacs/default-face-size (cond  ((equal "COREI3" (system-name)) 190)
					  ((equal "THINKPAD" (system-name)) 190)
					  (t 230)))

(defconst .emacs/image-export-path "~/../../../photo-export-emacs")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(package-selected-packages '(material-theme better-defaults)))

;; Remove junk symbols in shell mode
(setenv "PS1" "\\[\\e[32m\\]\\u@\\h \\[\\e[33m\\]\\w\\[\\e[0m\\]\\n\\$")


;; ================== MELPA Package Support ===============

;; Enables basic packaging support
(require 'package)

;; Adds the Melpa archive to the list of available repositories

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
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
  '(better-defaults ;; Set up some better Emacs defaults
    material-theme  ;; Theme
    spacemacs-theme ;; Spacemacs theme
    ayu-theme       ;; Ayu theme
    use-package	    ;; Package configuration macros
    py-autopep8	    ;; Run autopep8 on save
    blacken	    ;; Black formatting on save
    pc-bufsw
    flycheck
    elpy
    org-download
    auto-complete
    undo-fu
    markdown-mode
    paredit
    rainbow-delimiters
    yasnippet
    lsp-mode
    lsp-treemacs
    helm-lsp
    projectile
    hydra
    company
    avy
    which-key
    helm-xref
    dap-mode
    svg
    cython-mode
    flycheck-cython
    basic-mode
    org-bullets
    cmake-mode
    cmake-project
    python
    jupyter
    ein
    ))

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

(defun .emacs/display-formfeed-as-line-hook ()
  "Adds hook for displaying formfeed ^L chars as line when opening .el files"
  (when (string= (file-name-extension buffer-file-name) "el")
    (vv/display-formfeed-set-as-line)))
(add-hook 'find-file-hook '.emacs/display-formfeed-as-line-hook)

(pc-bufsw t) ;; Enables PC style quick buffer switcher for Emacs

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
 '(org-block ((t (:inherit shadow :foreground "gray50" :background "white smoke"))))
 '(org-meta-line ((t (:background "gainsboro"))))
 '(org-level-1 ((t (:inherit outline-1 :weight bold :height 1.1))))
 '(org-level-2 ((t (:inherit outline-2 :weight bold :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :weight bold :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :weight bold :height 1.05))))
 '(org-level-5 ((t (:inherit outline-5 :weight bold :height 1.05))))
 '(org-level-6 ((t (:inherit outline-6 :weight bold :height 1.05))))
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

;; vv key bidings
(global-set-key (kbd "C-c v e") 'vv/ipynb-export-image-at-point) ;; export image from notebook at point

;; SVG support
(auto-image-file-mode 1)
(require 'svg)

;; benchmarking
(require 'benchmark)

;; ============== Coding system stuff =====================
(prefer-coding-system 'cp1251-dos)  ;; default coding system
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
;; (ido-mode 1)
;; (ido-everywhere) ;; disabled, because incomatible with helm-mode
;; (setq ido-enable-flex-matching t)
;; (fido-mode)

;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

(defun .emacs/hide-trailing-whitespace ()
  "Disable `show-trailing-whitespace' in shell modes."
  (when (or (derived-mode-p 'shell-mode)
	    (equal major-mode 'eshell-mode)
	    (equal major-mode 'inferior-python-mode))
    (setq show-trailing-whitespace nil
	  indicate-empty-lines nil)))

(add-hook 'after-change-major-mode-hook '.emacs/hide-trailing-whitespace)

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
(add-hook 'c-mode-hook (lambda () (auto-complete-mode -1)))   ;; disable auto-complete
(add-hook 'c++-mode-hook (lambda () (auto-complete-mode -1))) ;; in c/c++ modes
(add-hook 'python-mode-hook (lambda () (auto-complete-mode -1))) ;; and python mode too
(add-hook 'cython-mode-hook (lambda () (auto-complete-mode -1))) ;; cython mode as well

;; ======================  lsp mode and stuff ============================
;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(setq lsp-keymap-prefix "s-n") ;; s-l used to locscreen in windows so rebind it to s-n

;; ====================== cmake stuff ========================

(defun maybe-cmake-project-mode ()
  (if (or (file-exists-p "CMakeLists.txt")
          (file-exists-p (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
      (cmake-project-mode)))

(add-hook 'c-mode-hook 'maybe-cmake-project-mode)
(add-hook 'c++-mode-hook 'maybe-cmake-project-mode)

(add-hook 'find-file-hook (lambda ()
			    (when (string= (buffer-file-name) "CMakeLists.txt")
			      (cmake-mode))))

;; ====================== emacs lisp setup ===================

(defun .emacs/elisp-mode-eval-buffer ()
  (interactive)
  (message "Evaluated buffer")
  (eval-buffer))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'.emacs/elisp-mode-eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") #'.emacs/elisp-mode-eval-buffer)

(add-hook 'emacs-lisp-mode-hook  (lambda ()
				   (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)))

;; ================== python development setup ===================

;; Enable elpy
(elpy-enable)
;;(use-package elpy
;;  :init (advice-add 'python-mode :before 'elpy-enable)
;;  :hook (elpy-mode . (lambda () (add-hook 'before-save-hook 'elpy-format-code)))
;;  :config (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;		)

;; ein custoize group properties
(setq ein:output-area-inlined-images t)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;(with-current-buffer (get-buffer " *Echo Area 0*")  ; the leading space character is correct
;;     (setq-local face-remapping-alist '((default (:height 0.9) variable-pitch)))) ; etc.

(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")

(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

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


(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; company auto completion backaend
  ;; (add-hook 'elpy-mode-hook
  ;; 	    (lambda ()
  ;; 	      (set (make-local-variable 'company-backends)
  ;; 		   '((company-dabbrev-code company-yasnippet elpy-company-backend)))))

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
	(cond ((equal pyvenv-virtual-env-name "transformers")
               (list '.emacs/set-python-shell-interpreter-to-jupyter))
	      (t (list '.emacs/set-python-shell-interpreter-to-python))))

  (setq pyvenv-post-deactivate-hooks
        (list '.emacs/set-python-shell-interpreter-to-python)))

;; Goto function definition kbd hook
(defun goto-def-or-rgrep ()
  "Go to definition of thing at point or do an rgrep in project if that fails"
  (interactive)
  (condition-case nil (elpy-goto-definition)
    (error (elpy-rgrep-symbol (thing-at-point 'symbol)))))
(define-key elpy-mode-map (kbd "M-.") 'goto-def-or-rgrep)

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-mode)


;; Cython-mode
(require 'cython-mode)
(when (require 'flycheck-cython nil t)
  (add-hook 'cython-mode-hook 'flycheck-mode))

;; Basic-mode setup
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

;; C/C++
(setq c-default-style "linux")
(setq-default c-basic-offset 4)

(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;; (add-hook 'c++-mode-hook (lambda ()
;; 			   (modify-syntax-entry ?_ "w" c++-mode-hook)))

;; set  F5 key to run compile!
(global-set-key [f5] 'compile)

;; ========================= Start server. ===============================
(require 'server)
(unless (server-running-p)
  (server-start))

(message "Configuration vv.emacs.windows10.el loaded!")
(if (daemonp)
    (message "Loading in the daemon!")
  (message "Loading in regular Emacs!"))
