;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

(use-package corfu
  :config
  (setq-default corfu-auto t
		corfu-auto-delay 0.0
		corfu-quit-at-boundary 'separator
		corfu-preselect-first nil)
  (global-corfu-mode)
  (corfu-history-mode))

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package icomplete
  :config
  (setq-default icomplete-prospects-height 10)
  (icomplete-vertical-mode 1))

(provide 'elan-completion)
