;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

(use-package svg-lib)

(use-package kind-icon
  :config
  (setq kind-icon-default-face 'corfu-default))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :commands (all-the-icons-dired-mode)
  :after (all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :config
  (setq all-the-icons-ibuffer-icon t
        all-the-icons-ibuffer-color-icon t)
  (all-the-icons-ibuffer-mode 1))

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config (all-the-icons-completion-mode))

(use-package notebook-light-theme
  :load-path "~/.emacs.d/etc/themes/notebook-theme"
  :config
  (load-theme 'notebook-light t))

(use-package notebook-dark-theme
  :load-path "~/.emacs.d/etc/themes/notebook-theme")

(provide 'elan-aestetics)
