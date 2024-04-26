;;; elan-bindings.el --- Mix of Emacs and Evil bindings -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: configuration

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `elan-bindings.el`

;;; Code:

;; Bindings for Norwegian layout on a Keychron K3 for Mac
(when MACOS
  (setq mac-option-modifier 'super
	mac-command-modifier 'meta
	ns-function-modifier 'hyper
	mac-right-option-modifier 'none)
  (define-key global-map (kbd "s-(") "{")
  (define-key global-map (kbd "s-)") "}")
  (define-key global-map (kbd "s-8") "[")
  (define-key global-map (kbd "s-9") "]")
  (define-key global-map (kbd "s-7") "|")
  (define-key global-map (kbd "s-/") "\\"))

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config
  (evil-define-command evil-vsplit-buffer (buffer)
    "Splits window vertically and switches to another buffer."
    :repeat nil
    (interactive "<b>")
    (evil-window-vsplit)
    (if buffer
        (evil-buffer buffer)
      (ibuffer)))
  (evil-define-command evil-split-buffer (buffer)
    "Splits window horizontally and switches to another buffer."
    :repeat nil
    (interactive "<b>")
    (evil-window-split)
    (if buffer
        (evil-buffer buffer)
      (ibuffer)))
  (evil-define-command evil-vsplit-dired (&optional count dir)
    "Splits window vertically and opens the directory with dired."
    :repeat nil
    (interactive "P<f>")
    (evil-window-vsplit)
    (if dir
        (dired dir)
      (dired-home-or-current-directory)))
  (evil-define-command evil-split-dired (&optional count dir)
    "Splits window horizontally and opens the directory with dired."
    :repeat nil
    (interactive "P<f>")
    (evil-window-split)
    (if dir
        (dired dir)
      (dired-home-or-current-directory)))
  (evil-define-command evil-split-indirect-buffer ()
    "Splits window horizontally and opens an indirect buffer."
    :repeat nil
    (evil-window-split)
    (clone-indirect-buffer))
  (evil-ex-define-cmd "b[uffer]" 'ibuffer)
  (evil-ex-define-cmd "vb[uffer]" 'evil-vsplit-buffer)
  (evil-ex-define-cmd "sb[uffer]" 'evil-split-buffer)
  (evil-ex-define-cmd "d[ired]" 'dired-home-or-current-directory)
  (evil-ex-define-cmd "vd[ired]" 'evil-vsplit-dired)
  (evil-ex-define-cmd "sd[ired]" 'evil-split-dired)
  (evil-ex-define-cmd "vi[ndirect]" 'clone-indirect-buffer)
  (evil-ex-define-cmd "si[ndirect]" 'evil-split-indirect-buffer)
  (setq evil-vsplit-window-right t
        evil-split-window-below t
	evil-undo-system 'undo-fu)
  (evil-mode 1))

(use-package evil-collection
  :config
  (diminish 'evil-collection-unimpaired-mode) 
  (evil-set-leader 'normal " ")
  (evil-set-leader 'normal "," 'local-leader)
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :config
  (setq evil-visualstar/persistent t)
  (global-evil-visualstar-mode))

(use-package evil-numbers)

(use-package repeat
  :ensure nil
  :config
  (defvar evil-numbers-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "+") 'evil-numbers/inc-at-pt)
      (define-key map (kbd "-") 'evil-numbers/dec-at-pt)
      map))
  (dolist (cmd '(evil-numbers/inc-at-pt
                 evil-numbers/dec-at-pt))
    (put cmd 'repeat-map 'evil-numbers-repeat-map))
  (defvar text-scale-adjust-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "+") 'text-scale-increase)
      (define-key map (kbd "-") 'text-scale-decrease)))
  (dolist (cmd '(text-scale-increase text-scale-decrease))
    (put cmd 'repeat-map 'text-scale-adjust-map)))

(use-package paredit
  :diminish
  :hook ((lisp-mode emacs-lisp-mode clojure-mode) . paredit-mode)
  :config
  (show-paren-mode 1)
  (setq-default show-paren-style 'mixed))

(use-package which-key
  :diminish
  :config
  (setq which-key-idle-delay 0.35
	which-key-add-column-padding 10
	which-key-allow-imprecise-window-fit nil
	which-key-prefix-prefix ""
	which-key-min-column-description-width 18
	which-key-min-display-lines 2)
  (which-key-add-key-based-replacements
    "C-x RET" "encoding"
    "C-x a" "abbrev"
    "C-x n" "narrow"
    "C-x p" "project"
    "C-x r" "register"
    "C-x x" "buffer-actions"
    "C-x X" "edebug"
    "C-x t" "tab")
  (which-key-mode 1))

(use-package embark)

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config (setq marginalia-annotators-heavy t)
  :init (marginalia-mode 1))

;; Clean up which-key output by unbinding keys I don't use and/or have
;; replacements for.
(let ((keys-to-unbind '("C-x DEL" "C-x SPC"
			"C-x 0" "C-x 1" "C-x 2" "C-x 3" "C-x 4" "C-x 5" "C-x 6"
			"C-x (" "C-x )" "C-x +" "C-x -" "C-x <" "C-x =" "C-x >" "C-x ["
			"C-x \\" "C-x ]" "C-x ^" "C-x h" "C-x o" "C-x {" "C-x }"
			"C-x C-SPC" "C-x C-@" "C-x C-e" "C-x C-j" "C-x C-l" "C-x C-n"
			"C-x C-o" "C-x C-p" "C-x C-c" "C-x C-d" "C-x C-r" "C-x C-u" "C-x C-<left>" "C-x C-<right>"))
      (prefixes-to-unbind '("C-x w" "C-x 8" "C-x C-a"))
      (evil-window-unbind '("b" "c" "f" "p" "r" "R" "s" "S" "t" "v" "w" "W"
                     "C-b" "C-c" "C-f" "C-p" "C-r" "C-R" "C-s" "C-S"
                     "C-t" "C-v" "C-w" "C-W" "C-_" "C-n" "C-o" "C-S-b"
                     "C-S-c" "C-S-f" "C-S-p" "C-S-r" "C-S-R" "C-S-s"
                     "C-S-S" "C-S-t" "C-S-v" "C-S-w" "C-S-W" "C-S-_"
                     "C-S-n" "C-S-o" "C-S-h" "C-S-j" "C-S-k" "C-S-l")))
  (dolist (key keys-to-unbind)
    (global-unset-key (kbd key)))
  (dolist (prefix prefixes-to-unbind)
    (dolist (char (number-sequence 0 127))
      (global-unset-key (kbd (format "%s %c" prefix char))))
    (global-unset-key (kbd prefix)))
  (dolist (key evil-window-unbind)
    (define-key 'evil-window-map (kbd key) nil)))

;; Custom bindings

;; Embark
(with-eval-after-load 'embark
  (define-key evil-normal-state-map (kbd "M-.") 'embark-act)
  (define-key evil-visual-state-map (kbd "M-.") 'embark-act)
  (define-key evil-normal-state-map (kbd "M-RET") 'dwim-or-eval)
  (define-key evil-visual-state-map (kbd "M-RET") 'dwim-or-eval)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'embark-dwim)
  (define-key embark-expression-map (kbd "RET") 'dwim-or-eval)
  (define-key embark-file-map (kbd "X") 'open-in-xcode)
  (define-key embark-file-map (kbd "S") 'sudo-find-file)
  (define-key embark-buffer-map (kbd "S")
	      (lambda ()
                (interactive)
                (if-let ((file (buffer-file-name (embark-target-buffer))))
                    (sudo-find-file file)
                  (message "Buffer is not associated with a file.")))))

;; Dired
(global-set-key (kbd "C-x d") 'dired-home-or-current-directory)
(which-key-add-key-based-replacements "C-x d" "dired")
(define-key dired-mode-map [backspace] 'dired-up-directory)
(define-key dired-mode-map (kbd "C-c w") 'wdired-change-to-wdired-mode)

;; Eshell
(global-set-key (kbd "C-x C-x") 'eshell)

;; X-code
(global-set-key (kbd "C-c x") 'open-in-xcode)

;; Avy
(define-key evil-normal-state-map (kbd "SPC SPC") 'avy-goto-word-0)

;; Quick search
(define-key evil-visual-state-map (kbd "SPC s") 'kagi-search)
(define-key evil-normal-state-map (kbd "SPC s") 'kagi-search)

;; Quick note
(define-key evil-normal-state-map (kbd "SPC n") 'make-a-note)

(define-key icomplete-minibuffer-map (kbd "TAB") 'icomplete-force-complete)

(provide 'elan-bindings)

;;; elan-bindings.el ends here
