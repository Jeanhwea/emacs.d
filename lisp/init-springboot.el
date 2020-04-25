;; -----------------------------------------------------------------------------
;;   .   ____          _            __ _ _
;;  /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
;; ( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
;;  \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
;;   '  |____| .__|_| |_|_| |_\__, | / / / /
;;  =========|_|==============|___/=/_/_/_/
;; -----------------------------------------------------------------------------
;;
;; - app-root
;;   +- module (业务模块包)
;;     +- controller (控制器类包)
;;     +- domain (问题模型包)
;;       +- entity (实体类包)
;;       +- repo (仓库类包)
;;         +- impl (仓库实现类包)
;;     +- service (服务类包)
;;       +- impl (服务实现类包)
;;
;; -----------------------------------------------------------------------------
(defun spt/prj-root (&optional dir)
  "Return project root dir."
  (let
    ((dir (or dir default-directory))
      (root))
    (setq root (jh/git-root dir))
    (or root
      (user-error "This `%s' is not a GIT repository!" dir))
    (or (file-exists-p (expand-file-name "pom.xml" root))
      (user-error "This `%s' seems not a maven project!" root))
    root))

(defun spt/src-root (&optional dir)
  "Return source root dir."
  (let
    ((dir (or dir (spt/prj-root)))
      (srcdir "src/main/java")
      (root))
    (setq root (file-name-as-directory (expand-file-name srcdir dir)))
    (or (file-exists-p root) (user-error "Folder `%s' is not exists!" root))
    root))

(defun spt/test-root (&optional dir)
  "Return test root dir."
  (let
    ((dir (or dir (spt/prj-root)))
      (testdir "src/test/java")
      (root))
    (setq root (file-name-as-directory (expand-file-name testdir dir)))
    (or (file-exists-p root) (user-error "Folder `%s' is not exists!" root))
    root))

(defun spt/app-root (&optional dir)
  "Return application root dir."
  (let
    ((dir (or dir (spt/src-root)))
      (appre "^.*Application.java$")
      (root))
    (setq root (car (directory-files-recursively dir appre)))
    (or root ;; if root is nil, then failed to get Application.java
      (user-error "Failed to get Application root of `%s'!" dir))
    (jh/parent-dir root)))

(defun spt/project-name ()
  "Return project name."
  (file-name-nondirectory (directory-file-name (spt/prj-root))))

(defun spt/read-entity-tabname (text)
  "Read entity table name. like `@Table(...)' "
  (let
    ((regexp "^[ \t]*@Table(name = \"\\([_A-Za-z0-9]+\\)\"")
      (addr 0)
      (tabname))
    (save-match-data
      (setq addr (string-match regexp text addr))
      (and addr (setq tabname (match-string 1 text))))
    tabname))

(defun spt/read-fields (text)
  "Read entity fields, like `@Column(...)' or `@JoinColumn(...)'. "
  (let
    ((regexp "^[ \t]*@\\(JoinColumn\\|Column\\)(name = \"\\([_A-Za-z0-9]+\\)\"")
      (addr 0)
      (fields))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          (let
            ((field (make-hash-table :test 'equal :size 2))
              (str1 (match-string 1 text))
              (str2 (match-string 2 text)))
            (puthash 'jointype str1 field)
            (puthash 'colname str2 field)
            (puthash 'addr addr field)
            (add-to-list 'fields field t))
          (setq addr (+ addr 1)))))
    fields))

;; -----------------------------------------------------------------------------
;;   ____ ___  __  __ ____   _    _   ___   __
;;  / ___/ _ \|  \/  |  _ \ / \  | \ | \ \ / /
;; | |  | | | | |\/| | |_) / _ \ |  \| |\ V /
;; | |__| |_| | |  | |  __/ ___ \| |\  | | |
;;  \____\___/|_|  |_|_| /_/   \_\_| \_| |_|
;; -----------------------------------------------------------------------------

(defun spt/company-jpa-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'spt/company-jpa-backend))
    (prefix
      (and (eq major-mode 'java-mode)
        (looking-back "find\\>")
        (match-string 0)))
    (candidates
      (when (equal arg "find")
        (list "findById" "findByCode" "findAllById" "findAllByCode")))
    (meta (format "This value is named %s" arg))))

;; (add-to-list 'company-backends 'spt/company-jpa-backend)

;; -----------------------------------------------------------------------------
;;  ____  _   _ _   _ _   _ _____ ____
;; |  _ \| | | | \ | | \ | | ____|  _ \
;; | |_) | | | |  \| |  \| |  _| | |_) |
;; |  _ <| |_| | |\  | |\  | |___|  _ <
;; |_| \_\\___/|_| \_|_| \_|_____|_| \_\ for Maven Springboot Test
;; -----------------------------------------------------------------------------

(defun spt/compilation-start (command &optional dir)
  "Run compilation command."
  (let ((default-directory (or dir (spt/prj-root))))
    (compilation-start command)))

(defun spt/current-test-method-name ()
  "Find current test method, the method name near cursor."
  (let*
    ((methods
       (sort
         (spt/read-junit-test-methods (jh/current-buffer))
         #'(lambda (a b) (< (gethash 'addr a) (gethash 'addr b)))))
      (current-point (point))
      (lookup
        (remove-if
          #'(lambda (method) (<= current-point (gethash 'addr method)))
          methods)))
    (and lookup (gethash 'funcname (car (last lookup))))))

;; http://maven.apache.org/surefire/maven-surefire-plugin/examples/single-test.html
(defun spt/maven-test-command (clzname &optional method)
  "Return maven test command."
  (let
    ((subjects (if method (concat clzname "#" method) clzname))
      (args '("-Dfile.encoding=UTF-8" "--quiet" "--batch-mode")))
    (format "mvn test -Dtest=%s %s" subjects (mapconcat 'identity args " "))))

(defun spt/run-test-class-command ()
  "Run a test command."
  (interactive)
  (let
    ((clzname
       (gethash 'clzname
         (spt/parse-java-frontinfo (jh/current-buffer)))))
    (and clzname (string-match-p "Test$" clzname)
      (spt/compilation-start (spt/maven-test-command clzname)))))

(defun spt/run-test-method-command ()
  "Run a test command."
  (interactive)
  (let
    ((clzname
       (gethash 'clzname
         (spt/parse-java-frontinfo (jh/current-buffer))))
      (method (spt/current-test-method-name)))
    (and clzname (string-match-p "Test$" clzname)
      (if method
        (spt/compilation-start (spt/maven-test-command clzname method))
        (spt/compilation-start (spt/maven-test-command clzname))))))

(provide 'init-springboot)
