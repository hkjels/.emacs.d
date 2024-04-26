;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

(use-package eshell
  :ensure nil
  :hook (eshell-preoutput-filter-functions . ansi-color-filter-apply)
  :config
  (setq-default eshell-history-size nil ;; Default to $HISTSIZE
		eshell-scroll-to-bottom-on-input 'all
                eshell-kill-on-exit t
		eshell-delete-exited-processes t
                eshell-destroy-buffer-when-process-dies t
		eshell-cd-on-directory t
                eshell-hist-ignoredups t
                eshell-save-history-on-exit t
		eshell-buffer-shorthand t)
  (setq eshell-banner-message ""))

(provide 'elan-shell)
