;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

(use-package chatgpt-shell
  :config
  (setq chatgpt-shell-openai-key
	(auth-info-password (nth 0 (auth-source-search :user "hkjels@me.com" :port "open-ai")))))

(provide 'elan-llm)
