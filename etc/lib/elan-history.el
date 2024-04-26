;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

(save-place-mode t)

(use-package savehist
  :config
  (setq-default history-delete-duplicates t
                savehist-save-minibuffer-history t
                savehist-autosave-interval nil
                savehist-additional-variables
                '(kill-ring
                  mark-ring global-mark-ring
                  search-ring regexp-search-ring
		          shell-command-history))
  (savehist-mode t))

(use-package undo-fu
  :config
  (setq undo-fu-allow-undo-in-region t))

(provide 'elan-history)
