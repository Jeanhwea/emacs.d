;; -----------------------------------------------------------------------------
;; projectile
;; -----------------------------------------------------------------------------
(when (require 'projectile)
  (projectile-mode 1)
  (setq-default
    projectile-mode-line-prefix " Proj"
    projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "M-9") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  ;; personal project structure
  (projectile-register-project-type 'py3code '("requirements.txt")
    :test "python -m unittest"
    :compile "pip install -r requirements.txt"
    :run "python -m unittest test/test_basic.py")
  (projectile-register-project-type 'yarn '("package.json")
    :compile "yarn install"
    :test "yarn test"
    :run "yarn start"
    :test-suffix ".spec")
  (projectile-register-project-type 'spring '("mvnw")
    :compile "mvnw compile package"
    :test "mvnw test"
    :run "mvnw spring-boot:run"
    :src-dir "src/main/"
    :test-dir "src/test/"
    :test-suffix "Tests"))


;; -----------------------------------------------------------------------------
;; engine-mode
;; -----------------------------------------------------------------------------
(when (require 'engine-mode)

  (defengine stackoverflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine baidu
    "https://www.baidu.com/s?wd=%s"
    :keybinding "b")

  (defengine npm
    "https://www.npmjs.com/search?q=%s"
    :keybinding "n")

  (engine-mode t))


;; -----------------------------------------------------------------------------
;; grip
;; -----------------------------------------------------------------------------
(defvar grip-buffer "*grip*")

(defun jh/grip (DIR FILENAME &optional HOST PORT)
  "start a grip in backgroud in DIR."
  (unless HOST (setq HOST "localhost"))
  (unless PORT (setq PORT 2758))
  (let ((default-directory DIR))
    (when (get-buffer-process grip-buffer)
      (quit-process grip-buffer))
    (start-process "grip" grip-buffer "grip" "-b" FILENAME
      (format "%s:%d" HOST PORT))))

(defun grip ()
  "start a grip daemon."
  (interactive)
  (let ((git-dir (jh/git-root-dir))
        (filename (jh/git-file-name)))
    (unless filename
      (error "filename is nil"))
    (if (string-match-p ".md\\'" filename)
      (when git-dir
        (jh/grip git-dir filename))
      (error "Buffer '%s' is not a Markdown file!" filename))))




(provide 'init-experimental)
