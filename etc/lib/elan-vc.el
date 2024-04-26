;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

(setq-default vc-follow-symlinks t)

(use-package git)

(use-package gist)

(use-package magit)

(use-package diff-hl
  :config
  (setq-default diff-hl-draw-borders nil)
  (global-diff-hl-mode))

(provide 'elan-vc)
