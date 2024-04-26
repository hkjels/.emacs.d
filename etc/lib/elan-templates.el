;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

(require 'tempo)

;; Clojure Templates

(defun elan-filepath-to-clojure-ns ()
  (let* ((path (file-name-sans-extension (buffer-file-name)))
         (segments (split-string path "/" t))
         (relevant-segments (seq-drop-while (lambda (s) (not (string= s "src"))) segments))
         (namespace (mapconcat 'identity (cdr relevant-segments) ".")))
    namespace))

(tempo-define-template
 "clojure-namespace"
 '("(ns " (elan-filepath-to-clojure-ns) ")\n\n")
 "Template that outputs namespace definition for Clojure files.")

(tempo-define-template
 "clojure-deps-edn"
 '("{:deps {" p "}\n"
   " :paths [\"src\"]\n"
   " :aliases {}}")
 "Template for starting a deps.edn file.")

(tempo-define-template
 "emacs-lisp-package"
  '((let ((package-name (read-string "Package name: "))
          (short-description (read-string "Short description: "))
          (author (read-string "Author: " user-full-name))
          (email (read-string "Email: " user-mail-address))
          (dependencies (read-string "Dependencies, e.g., emacs \"24.4\": " "emacs \"24.4\""))
          (keywords (read-string "Keywords: ")))
      (concat
       ";;; " package-name ".el --- " short-description " -*- lexical-binding: t; -*-\n"
       ";; Author: " author " <" email ">\n"
       ";; Version: 0.1.0\n"
       ";; Package-Requires: ((" dependencies "))\n"
       ";; Keywords: " keywords "\n\n"
       ";; This file is not part of GNU Emacs.\n\n"
       ";;; Commentary:\n\n"
       ";;\n\n"
       ";;; Code:\n\n\n"
       "(provide '" package-name ")\n"
       ";;; " package-name ".el ends here\n")))
  "Template for starting a new Emacs Lisp package.")

;; Auto-insert
(progn

  (setq auto-insert-query nil
	auto-insert-alist
	'((("\\.clj[cs]?\\'" . "Clojure file") . tempo-template-clojure-namespace)
	  (("deps.edn'" . "deps.edn file") . tempo-template-clojure-deps-edn)))

  (add-hook 'find-file-hook 'auto-insert))

(provide 'elan-templates)
