;; -----------------------------------------------------------------------------
;; projectile
;; -----------------------------------------------------------------------------
(when (require 'projectile)
  (projectile-mode 1)
  (setq-default
    projectile-globally-ignored-directories '("node_modules" ".git")
    projectile-mode-line-prefix " Proj"
    projectile-completion-system 'ivy
    projectile-create-missing-test-files t)
  (define-key projectile-mode-map (kbd "M-9") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
  (projectile-register-project-type 'spring '("mvnw")
    :compile "mvn -B clean package"
    :run "mvn -B spring-boot:run"
    :src-dir "src/main/"
    :test-dir "src/test/"
    :test-suffix "Test"))

(provide 'init-projectile)
