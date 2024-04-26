;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

(use-package org
  :hook ((org-mode . org-pretty-symbols-mode)
	 (org-mode . auto-fill-mode)
	 (org-mode . variable-pitch-mode))
  :init
  (defun org-pretty-symbols-mode ()
    (push '("#+title: "        . "") prettify-symbols-alist)
    (push '("#+subtitle: "     . "") prettify-symbols-alist)
    (push '("#+author: "       . "- ") prettify-symbols-alist)
    (push '(":properties:"     . ":") prettify-symbols-alist)
    (push '("#+begin_src"      . "…") prettify-symbols-alist)
    (push '("#+end_src"        . "⋱") prettify-symbols-alist)
    (push '("#+results:"       . "»") prettify-symbols-alist)
    (push '(":end:"            . "⋱") prettify-symbols-alist)
    (push '(":results:"        . "⋰") prettify-symbols-alist)
    (push '("#+name:"          . "-") prettify-symbols-alist)
    (push '("#+begin_example"  . "~") prettify-symbols-alist)
    (push '("#+end_example"    . "~") prettify-symbols-alist)
    (push '("#+tblfm:"         . "∫") prettify-symbols-alist)
    (push '("[X]"              . (?\[ (Br . Bl) ?✓ (Br . Bl) ?\])) prettify-symbols-alist)
    (push '("\\\\"             . "↩") prettify-symbols-alist)
    (prettify-symbols-mode t))
  (setq-default org-modules '(ol-doi ol-docview ol-info ol-mhe)
		org-display-inline-images t
		org-startup-with-inline-images t
		org-display-remote-inline-images t
		org-src-fontify-natively t
		org-fontify-quote-and-verse-blocks t
		org-src-window-setup 'current-window
		org-confirm-babel-evaluate nil
		org-hide-leading-stars t
		org-hide-emphasis-markers t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((calc . t)
     (emacs-lisp . t)
     (makefile . t)
     (shell . t)))
  (setq org-adapt-indentation nil
	org-edit-src-content-indentation 0
	org-src-preserve-indentation nil
	org-indent-indentation-per-level 0))

(use-package org-appear)

(use-package org-clock-today
  :config (org-clock-today-mode 1))

(provide 'elan-org)
