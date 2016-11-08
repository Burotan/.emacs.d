;;; package --- Summary
;;; Setup package control
;;; Commentary:
;;; BS \ Emacs config
;;; Code:

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(defvar package-list)
(setq package-list '(use-package))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Specifies local directory to load packages from
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(let ((default-directory  "~/.emacs.d/packages/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'use-package)

;; Essential settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(tool-bar-mode -1) ; No toolbar
(scroll-bar-mode -1) ; Hide scrollbars
(menu-bar-mode -1) ; Hide menu bar
(show-paren-mode t) ; Highlights matching parenthesis
(electric-pair-mode t)
(setq initial-scratch-message "") ; No scratch text

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Temporarily set garbage collect threshold high to improve start time
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (string= (buffer-name) "*scratch*")
              (animate-string ";; Welcome back, sir! How can i help you today?" (/ (frame-height) 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(use-package sublime-themes
;  :ensure t
;  :config
;  (load-theme 'brin t)) ; Color theme

(use-package gotham-theme
  :ensure t
  :config
  (load-theme 'gotham t))

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
  (add-hook 'js-mode-hook 'js2-minor-mode))

(use-package skewer-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

(use-package php-mode
  :ensure t
  :config
  (add-hook 'php-mode-hook 'my-php-mode-hook)
  (defvar c-basic-indent)
  (defun my-php-mode-hook ()
    (setq indent-tabs-mode t)
    (let ((my-tab-width 4))
      (setq tab-width my-tab-width)
      (setq c-basic-indent my-tab-width)
      (set (make-local-variable 'tab-stop-list)
	   (number-sequence my-tab-width 200 my-tab-width)))))

(use-package vue-mode
  :ensure t)

;; Web major mode
(use-package web-mode
  :ensure t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
	  (add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-enable-css-colorization t)))))

(use-package zencoding-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'zencoding-mode))

;; Markdown formatting and preview
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-live-preview-delete-export 'delete-on-export))

(use-package haskell-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package erlang
  :ensure t)

(use-package google-this ;; Broken !
  :ensure t
  :init
  (google-this-mode 1)
  :bind
  ("C-x g" . google-this.mode-submap))

(use-package lorem-ipsum
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-mode t)
  (defvar ido-use-faces)
  (setq ido-use-faces t)
  (ido-vertical-mode t)
  :config
  (set-face-attribute 'ido-vertical-first-match-face nil
		      :background nil
		      :foreground "orange")

  (set-face-attribute 'ido-vertical-only-match-face nil
		      :background nil
		      :foreground nil)

  (set-face-attribute 'ido-vertical-match-face nil
		      :background nil
		      :foreground nil)
  
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-show-count t))

(use-package hl-line
  :ensure t
  :init
  (hl-line-mode t)
  :config (set-face-background 'hl-line "#232323"))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))
   
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq-default smooth-scroll-margin 4))

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ("C-c C-c M-x" . execute-extended-command))

;;(use-package powerline
;;  :ensure t
;;  :config
;;  (powerline-center-theme)
;;  (set-face-attribute 'mode-line nil
;;		      :foreground "White"
;;		      :background "DarkOrange"
;;		      :box nil))

(use-package recentf
  :init
  (recentf-mode 1)
  (defvar recentf-max-menu-items)
  (setq recentf-max-menu-items 25)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files))

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-current-symbol "")
  (global-linum-mode t)
  (linum-relative-mode))

;; Git porcelen
(use-package magit
  :ensure t)

;; Uses jedi server and company mode frameword for Python completion
(use-package company-jedi
  :ensure t
  :config
  (defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

;; On the fly syntax checking
(use-package flycheck
  :ensure t
  :config
  (progn
   ; (setq flycheck-display-errors-function nil)
    (add-hook 'after-init-hook 'global-flycheck-mode)))

;; Text manipulation
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-c C-v") 'neotree-toggle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE CONFIG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ox-twbs
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)))

(defun org-add-class(classname)
  (interactive "sClass Name:")
  (insert (concat"#+attr_html: :class " classname)))

(defun org-add-attr(key val)
  (interactive "sAttr Key:\nsAttr Val:")
  (if (string= "" val)
      (insert (concat "#+attr_html: :" key " \"\""))
    (insert (concat "#+attr_html: :" key " " val))))

(org-defkey org-mode-map (kbd "C-c h") 'org-html-export-to-html)
(org-defkey org-mode-map (kbd "C-c p") 'org-latex-export-to-pdf)
(org-defkey org-mode-map (kbd "C-c c") 'org-add-class)
(org-defkey org-mode-map (kbd "C-c a") 'org-add-attr)

;; Backup options
;; backup in one place. flat, no tree structure
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto/" t)))
(setq backup-by-copying t) ; Stop shinanigans with links

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-comment(name)
  "Adds elisp comment section"
  (interactive "sComment Section Name:")
  (insert
   (concat
    ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
    ";; " (upcase name) "\n"
    ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")))

(defun move-line-up()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADVICES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM KEYBINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "C-c C-a") 'add-comment)
(global-set-key (kbd "C-c C-k") 'compile)

;; FUCK Meta Keys
(global-set-key (kbd "M-f") 'ido-find-file)
(global-set-key (kbd "M-b") 'ido-switch-buffer)
(global-set-key (kbd "M-B") 'buffer-list)
(global-set-key (kbd "M-s") 'save-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATERIALIZE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun materialize-add-form-item(colsize  name)
  "Add materializecss form item by column."
  (interactive "sColumn Size:\nsItem Name:")
  (insert
   (concat
    "<div class=\"input-field col " colsize  "\">\n"
    "<input type=\"text\" id=\"" (downcase name) "\" name=\"" (downcase name) "\" />\n"
    "<label for=\"" (downcase name) "\">" name "</label>\n"
    "</div>"
    )))

(global-set-key (kbd "C-c C-b i") 'materialize-add-form-item)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (eq system-type 'windows-nt)
  (add-to-list 'default-frame-alist '(font . "Droid Sans Mono-11" ))
  (set-face-attribute 'default t :font "Droid Sans Mono-11" ))

(when (eq system-type 'darwin)
  (message "MACOSX"))

(when (eq system-type 'gnu/linux)
  (message "Welcome, Sir!"))

;;; init.el ends here
