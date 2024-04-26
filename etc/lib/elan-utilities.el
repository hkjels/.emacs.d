;;; elan-utilities.el --- Utility functions -*- lexical-binding: t; -*-

;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: configuration

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `elan-utilities.el`

;;; Code:

(defun dired-home-or-current-directory (&optional prefix)
  "Open Dired in the current buffer's directory or prompt for a directory if PREFIX is provided.
Defaults to the home directory if the
current buffer isn't associated with a file."
  (interactive "P")
  (let ((dir (if prefix
                 (read-directory-name "Directory: ")
               (or (and (buffer-file-name)
                        (file-name-directory (buffer-file-name)))
                   "~"))))
    (when (and (stringp dir) (file-exists-p dir))
      (dired dir))))

(defun eval-dwim ()
  "Evaluate expressions based on context.
   Evaluate last sexp, entire buffer, current defun, or Org source block.
   Uses CIDER's evaluation functions in Clojure buffers."
  (interactive)
  (cond
   ;; CIDER evaluations in Clojure buffers
   ((and (bound-and-true-p cider-mode)
         (derived-mode-p 'clojure-mode))
    (cond ((looking-at-p "\\s-*)") (cider-eval-last-sexp))
          ((or (bobp) (eobp)) (cider-eval-buffer))
          (t (cider-eval-defun-at-point))))

   ;; Org source block evaluation
   ((and (derived-mode-p 'org-mode)
         (org-in-src-block-p))
    (org-babel-execute-src-block))

   ;; Standard evaluations for other buffers
   ((looking-at-p "\\s-*)") (eval-last-sexp nil))
   ((or (bobp) (eobp)) (eval-buffer))
   (t (eval-defun nil))))

(defun dwim-or-eval ()
  "Decide between 'eval-dwim' and 'embark-dwim' based on context."
  (interactive)
  (if (not (or (derived-mode-p 'emacs-lisp-mode 'clojure-mode 'lisp-mode 'scheme-mode)
               (and (derived-mode-p 'org-mode) (org-in-src-block-p))))
      (embark-dwim)
    (eval-dwim)))

(defun open-in-xcode ()
  "Open the current file in Xcode."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (shell-command (concat "open -a Xcode " (shell-quote-argument file-name)))
      (message "No file associated with this buffer."))))

(defun kagi-search (&optional prefix)
  "Search for the selected text or prompt if none is selected.
   With a prefix argument, search on GitHub code instead of the default kagi.com."
  (interactive "P")
  (let* ((base-url (if prefix
                       "https://github.com/search?q="
                     "https://kagi.com/search?q="))
         (query (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Search: "))))
    (browse-url (concat base-url (url-encode-url query)))))

(defun sudo-find-file (&optional file-name)
  "Open FILE-NAME as root, or prompt for a file if none is provided."
  (interactive
   (list (if-let ((buf-file (buffer-file-name)))
             buf-file
           (read-file-name "Sudo find file: "))))
  (unless (file-exists-p file-name)
    (error "File does not exist"))
  (find-alternate-file (concat "/sudo::" file-name)))

(defcustom notes-file "~/org/Notes.org"
  "Path to the global notes file."
  :type 'string
  :group 'elan)

(defun make-a-note ()
  "Open a project's Notes.org or Notes.md file if it exists, move to the end,
   and switch to insert mode. If no note file is found or not in a project, open `notes-file'."
  (interactive)
  (let* ((project (project-current nil))
         (root (when project (project-root project)))
         (notes-org (when root (concat root "Notes.org")))
         (notes-md (when root (concat root "Notes.md"))))
    (split-window-right)
    (other-window 1)
    ;; Determine which note file to open, or open the global file if not in a project
    (if (or (null project) (and (null notes-org) (null notes-md)))
        (find-file notes-file)
      (cond
       ((file-exists-p notes-org) (find-file notes-org))
       ((file-exists-p notes-md) (find-file notes-md))
       (t (find-file notes-file))))
    ;; Move to the end of the file, add a new line, and enter insert mode if using Evil
    (goto-char (point-max))
    (newline)
    (when (bound-and-true-p evil-mode)
      (evil-insert 1))))

(provide 'elan-utilities)

;;; elan-utilities.el ends here
