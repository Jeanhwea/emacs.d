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
    :compile "pip install -r requirements.txt"
    :test "python -m unittest"
    :src-dir "gaintbrain/"
    :test-dir "test/"
    :test-prefix "test_"
    :run "python -m unittest test/test_basic.py")
  (projectile-register-project-type 'yarn '("package.json")
    :compile "yarn install"
    :test "yarn test"
    :run "yarn start"
    :test-suffix ".spec")

  ;; maven springboot test
  ;; http://maven.apache.org/surefire/maven-surefire-plugin/examples/single-test.html
  (defun jh/springboot-test-command ()
    "Returns a String representing the test command to run for the given context"
    (when (eq major-mode 'java-mode)
      (let
        ((java-class-name (file-name-nondirectory (file-name-sans-extension (buffer-name))))
          (java-method-name (word-at-point)))
        (if (and java-class-name (string-match-p "^[a-zA-Z][0-9a-zA-Z]*Test$" java-class-name))
          (if (and java-method-name (string-match-p "^test[0-9A-Za-z]*$" java-method-name))
            (format "mvn test -q -B -Dtest=%s#%s" java-class-name java-method-name)
            (format "mvn test -q -B -Dtest=%s" java-class-name))
          "mvn test -q -B"))))

  (projectile-register-project-type 'spring '("mvnw")
    :compile "mvn -B clean package"
    :run "mvn -B spring-boot:run"
    :test 'jh/springboot-test-command
    :src-dir "src/main/"
    :test-dir "src/test/"
    :test-suffix "Test"))

(provide 'init-projectile)
