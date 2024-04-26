;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

(defun elan-flymake-skip-test-files ()
  "Activate Flymake only for non-test files."
  (let ((is-test-file (or (string-match-p "-test\\.el\\'" buffer-file-name)
                          (string-match-p "/tests?/" buffer-file-name))))
    (flymake-mode (if is-test-file -1 1))))

(use-package flymake
  :ensure nil
  :hook (prog-mode . elan-flymake-skip-test-files))

(use-package editorconfig
  :diminish
  :config (editorconfig-mode 1))

;; Logs
(use-package logview
  :mode ("\\.log\\'" . logview-mode)
  :hook (logview-mode .  hl-line-mode)
  :config
  (setq datetime-timezone 'Europe/Oslo)
  (add-to-list 'logview-additional-submodes
               '("Simple"
                 (format . "TIMESTAMP: LEVEL: MESSAGE")
                 (levels . "SLF4J"))))

;; Clojure
(use-package clojure-mode
  :config
  (setq clojure-align-forms-automatically t)
  (setq clojure-indent-style ':always-align)
  (setq clojure--prettify-symbols-alist '((">=" . (?\s (Br . Bl) ?\s (Bc . Bc) ?≥))
                                          ("<=" . (?\s (Br . Bl) ?\s (Bc . Bc) ?≤))
                                          ("->" . (?- (Br . Bc) ?- (Br . Bc) ?>))
                                          ("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                                                         (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
                                                         (Bc . Bl) ?- (Br . Br) ?>))
                                          ("fn" . ?λ)
                                          ("comp" . ?∘)
                                          ("filter" . ?Ƒ)
                                          ("not=" . ?≠))))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :config
  (let ((repl-size 500))
    (setq cider-enrich-classpath t
          cider-use-overlays t
          cider-overlays-use-font-lock t
          cider-repl-use-clojure-font-lock t
          cider-popup-stacktraces t
          cider-popup-stacktraces-in-repl t
          cider-repl-buffer-size-limit repl-size
          cider-repl-history-size repl-size
          cider-repl-display-help-banner nil
          cider-repl-pop-to-buffer-on-connect nil
          cider-repl-use-pretty-printing t
          nrepl-hide-special-buffers t
          nrepl-log-messages nil)
    (add-to-list
     'display-buffer-alist
	 `("^\\*cider-repl"
	   (display-buffer-in-side-window)
       (shrink-window-if-larger-than-buffer)
	   (side . bottom)
	   (quit-restore ('window 'window nil nil))))))

(use-package jarchive
  :diminish
  :config (jarchive-setup))

(use-package html-to-hiccup
  :commands (html-to-hiccup-convert-region))

(use-package markdown-mode)

(use-package zig-mode)

(use-package cmake-mode)

(provide 'elan-languages)
