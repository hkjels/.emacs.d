;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

(add-to-list 'display-buffer-alist
	     `("^\\(\\*\\(e?shell\\|Process List\\|Compile-Log\\|compilation\\|ripgrep-search\\)\\)"
	       (display-buffer-in-side-window)
	       (window-min-height . 20)
	       (window-max-height . 35)
	       (side . bottom)
	       (quit-restore ('window 'window nil nil))))

(provide 'elan-layout)
