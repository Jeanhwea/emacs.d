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
(defun spt/project-root ()
  "Return current project root dir."
  (or (jh/git-root default-directory)
    (error "This file is not inside a GIT repository")))

(defun spt/app-root (&optional entry)
  "Return current source root dir."
  (let*
    ((re (format "^.*%s\\.java$" (or entry "Application")))
      (dirs (directory-files-recursively (spt/src-root) re)))
    (or dirs (error "Failed to get application root!"))
    (jh/parent-dir (car dirs))))

(defun spt/app-test-root (&optional entry)
  "Return current source root dir."
  (let*
    ((re (format "^.*%s\\.java$" (or entry "ApplicationTest")))
      (dirs (directory-files-recursively (spt/test-root) re)))
    (or dirs (error "Failed to get application test root!"))
    (jh/parent-dir (car dirs))))

(defun spt/src-root ()
  "Return current source root dir."
  (let
    ((dir
       (file-name-as-directory
         (expand-file-name "src/main/java" (spt/project-root)))))
    (or (file-exists-p dir)
      (error "Folder `src/main/java' is not exists!"))
    dir))

(defun spt/test-root ()
  "Return current test case root dir."
  (let
    ((dir
       (file-name-as-directory
         (expand-file-name "src/test/java" (spt/project-root)))))
    (or (file-exists-p dir)
      (error "Folder `src/test/java' is not exists!"))
    dir))

(defun spt/project-name ()
  "Return project name."
  (let*
    ((root (directory-file-name (spt/project-root)))
      (parent (jh/parent-dir root)))
    (jh/re-replace "/$" "" (jh/re-replace parent "" root))))

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
  (let ((default-directory (or dir (spt/project-root))))
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
