;;; package --- Summary
;;; Setup package control
;;; Commentary:
;;; BS \ Emacs config
;;; Code:

;; Specifies local directory to load packages from
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(let ((default-directory  "~/.emacs.d/packages/"))
  (normal-top-level-add-subdirs-to-load-path))

(package-initialize)

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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (load custom-file)

(setq-default tab-width 2)

(mapc (lambda (hooksym)
	(add-hook hooksym
		  (lambda ()
		    (kill-local-variable 'indent-tabs-mode)
		    (kill-local-variable 'tab-width)
		    (local-set-key (kbd "TAB") 'self-insert-command))))
      '(c-mode-common-hook))

;; Backup options
;; backup in one place. flat, no tree structure
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto/" t)))
(setq backup-by-copying t) ; Stop shinanigans with links

;; Temporarily set garbage collect threshold high to improve start time
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (string= (buffer-name) "*scratch*")
              (animate-string ";; Welcome back, sir! How can i help you today?" (/ (frame-height) 2)))))

(use-package gotham-theme
  :ensure t
  :config
  (load-theme 'gotham t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package json-mode
  :ensure t)

(use-package lorem-ipsum
  :ensure t)

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ("C-c C-c M-x" . execute-extended-command))

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-current-symbol "")
  (global-linum-mode t)
  (linum-relative-mode))

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

;;; init-lite.el ends here
