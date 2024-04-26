;;; init.el --- Elan. The personal Emacs configuration of Henrik Kjerringvåg -*- lexical-binding: t; -*-

;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: configuration

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `init.el` 

;;; Code:

(setq user-full-name "Henrik Kjerringvåg"
      user-mail-address "henrik@kjerringvag.no")

(require 'elan-utilities)
(require 'elan-packaging)
(require 'elan-search)
(require 'elan-vc)
(require 'elan-llm)
(require 'elan-templates)
(require 'elan-completion)
(require 'elan-shell)
(require 'elan-aestetics)
(require 'elan-history)
(require 'elan-org)
(require 'elan-languages)
(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'elan-layout)
	    (require 'elan-bindings)))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Conditionally configure authentication sources on macOS platforms.
(when MACOS
  (add-to-list 'auth-sources 'macos-keychain-internet)
  (add-to-list 'auth-sources 'macos-keychain-generic))

;; Enable UTF-8 Support for Multibyte Characters in Emacs
;; Ensures proper handling and display of a wide range of international characters
;; by setting multibyte character support, preferring UTF-8 encoding, and aligning
;; the language environment with UTF-8 standards.
(setq-default default-enable-multibyte-characters t)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; Configure auto-reverting and file locking
(setq-default auto-revert-verbose nil
	      auto-revert-use-notify t
	      auto-revert-avoid-polling t
              global-auto-revert-non-file-buffers t
              create-lockfiles nil)
(global-auto-revert-mode t)

;; Enable automatic compression and decompression of files
(auto-compression-mode)

;; Automatically make file executable if the first line starts with a bang!
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq-default truncate-partial-width-windows nil
	      truncate-lines t)

(use-package no-littering
  :diminish
  :config
  (setq-default make-backup-files t
                backup-by-copying t
                backup-directory-alist `(("." . ,(no-littering-expand-var-file-name "backup")))
                auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file :noerror))

(recentf-mode 1)

(use-package gcmh
  :diminish
  :config
  (setq gcmh-idle-delay 0.3)
  (gcmh-mode t))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq-default wdired-allow-to-change-permissions t
		dired-find-subdir t
		dired-recursive-copies 'always
		dired-recursive-deletes 'always
		dired-dwim-target t
		dired-isearch-filenames t
		dired-listing-switches "-alh"))

(use-package dired-narrow)

(use-package compile
  :ensure nil
  :hook (compilation-filter . colorize-compilation-buffer)
  :init
  (defun ansi-color-buffer ()
    "Colorize ANSI escape-codes in the current buffer."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
	(ansi-color-buffer))))
  :config (setq-default compilation-auto-jump-to-first-error t
			compilation-scroll-output t))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-key helpful-at-point)
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         :map emacs-lisp-mode-map
         ("C-c C-d" . helpful-at-point))
  :config (setq-default help-window-select t))

(use-package ibuffer-vc
  :commands (ibuffer-vc)
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic))))
  :bind ([remap list-buffers] . ibuffer))

(use-package flymake
  :bind ("C-c f" . flymake-show-buffer-diagnostics))

;;; init.el ends here
