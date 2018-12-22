;; -----------------------------------------------------------------------------
;; projectile
;; -----------------------------------------------------------------------------
(when (require 'projectile)
  (projectile-mode 1)
  (setq-default
    projectile-mode-line-prefix " Proj"
    projectile-completion-system 'ivy
    projectile-create-missing-test-files t)
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

  ;; maven spring test
  (defun jh/springboot-test-command ()
    "Returns a String representing the test command to run for the given context"
    (when (eq major-mode 'java-mode)
      (let
        ((class-name (file-name-nondirectory (file-name-sans-extension (buffer-name))))
          (method-name (word-at-point)))
        (if (and class-name (string-match-p "^[a-zA-Z]*Test[0-9a-zA-Z]*$" class-name))
          (if (and method-name (string-match-p "^test[0-9A-Za-z]*$" method-name))
            (format "mvn test --batch-mode -Dtest=%s#%s" class-name method-name)
            (format "mvn test --batch-mode -Dtest=%s" class-name))
          "mvn test --batch-mode"))))

  (projectile-register-project-type 'spring '("mvnw")
    :compile "mvn compile package"
    :test 'jh/springboot-test-command
    :run "mvn spring-boot:run"
    :src-dir "src/main/"
    :test-dir "src/test/"
    :test-suffix "Test"))


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
;; web-mode
;; -----------------------------------------------------------------------------
(when (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode)))

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
