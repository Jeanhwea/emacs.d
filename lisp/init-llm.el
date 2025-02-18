(setq
  chatgpt-shell-openai-key (getenv "OPENAI_API_KEY")
  chatgpt-shell-api-url-base (getenv "OPENAI_API_BASE"))

(provide 'init-llm)
