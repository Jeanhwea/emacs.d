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

  (defun projectile-test-command (compile-dir)
    "Override default `projectile-test-command', to remove revoking last test command"
    (or projectile-project-test-cmd
      (projectile-default-test-command (projectile-project-type))))

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
        ((java-class-name (file-name-nondirectory (file-name-sans-extension (buffer-name))))
          (java-method-name (word-at-point)))
        (if (and java-class-name (string-match-p "^[a-zA-Z]*Test[0-9a-zA-Z]*$" java-class-name))
          (if (and java-method-name (string-match-p "^test[0-9A-Za-z]*$" java-method-name))
            (format "mvn test --batch-mode -Dtest=%s#%s" java-class-name java-method-name)
            (format "mvn test --batch-mode -Dtest=%s" java-class-name))
          "mvn test --batch-mode"))))

  (projectile-register-project-type 'spring '("mvnw")
    :compile "mvn compile package"
    :run "mvn spring-boot:run"
    :test 'jh/springboot-test-command
    :src-dir "src/main/"
    :test-dir "src/test/"
    :test-suffix "Test"))

(provide 'init-projectile)
