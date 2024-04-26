;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

(use-package rg
  :config
  (setq xref-search-program 'ripgrep)
  (rg-enable-default-bindings))

(use-package wgrep
  :config (setq wgrep-enable-key "e"
                wgrep-auto-save-buffer t))

(provide 'elan-search)
