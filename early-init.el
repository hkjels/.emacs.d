;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

(startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory)))

(setq debug-on-error t)
(add-hook 'after-init-hook (lambda () (setq debug-on-error nil)))

(add-to-list 'load-path (expand-file-name "etc/lib" user-emacs-directory))

;; Temporary workaround to address path-related issues encountered
;; when using native compilation in Emacs, particularly in daemon
;; mode. The code explicitly sets the PATH environment variable within
;; Emacs, ensuring that the Emacs daemon can locate all necessary
;; executables for native compilation.
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default inhibit-splash-screen t)

(setq-default warning-minimum-level :error)

(setq-default ring-bell-function 'ignore)

(setq-default bidi-paragraph-direction 'left-to-right
	      bidi-inhibit-bpa t)

(when (boundp 'comp-speed)
 (setq-default comp-speed 2))

(defconst MACOS (eq system-type 'darwin))
(defconst LINUX (eq system-type 'gnu/linux))
(defconst WINDOWS (eq system-type 'windows-nt))
(defconst WSL (string-match "*WSL*" (shell-command-to-string "uname -a")))

(defconst KB 1024)
(defconst MB (* 1024 KB))

(setq-default gc-cons-threshold (* 1 MB))
(setq-default read-process-output-max (* 500 KB))
(setq-default undo-limit (* 256 MB))

(setq-default package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

(setq-default frame-title-format " ")
(setq-default frame-inhibit-implied-resize t)
(push `(title . ,frame-title-format) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(let ((coords '((top . 10) (left . 10) (width . 200) (height . 53))))
  (setq-default initial-frame-alist (nconc initial-frame-alist coords))
  (setq-default default-frame-alist (nconc default-frame-alist coords)))

(when MACOS
  (setq-default ns-use-proxy-icon nil)
  (setq-default ns-use-native-fullscreen t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(setq-default debug-on-error (getenv "DEBUG")
              init-file-debug (getenv "DEBUG"))
