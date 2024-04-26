;;; elan-navigation.el --- Enhance navigation in Emacs -*- lexical-binding: t -*-

;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (avy "0.5.0") (consult "0.9"))
;; Keywords: navigation, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `elan-navigation` provides convenient key bindings and configurations
;; to enhance navigation using popular packages like `avy` and `consult`.
;; It allows for quick movement and search within buffers and throughout projects.

;;; Code:

(use-package avy
  :functions avy-setup-default
  :config
  (avy-setup-default))

(use-package consult
  :bind (("M-b" . consult-buffer)
         ("M-f" . consult-find)
         ("M-g" . consult-grep)
         ("M-l" . consult-line)
         ("M-o" . consult-outline)))

(provide 'elan-navigation)

;;; elan-navigation.el ends here
