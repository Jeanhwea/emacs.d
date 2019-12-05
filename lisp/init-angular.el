;; -----------------------------------------------------------------------------
;;     _                      _
;;    / \   _ __   __ _ _   _| | __ _ _ __
;;   / △ \ | '_ \ / _` | | | | |/ _` | '__|
;;  / ___ \| | | | (_| | |_| | | (_| | |
;; /_/   \_\_| |_|\__, |\__,_|_|\__,_|_|
;;                |___/
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;;  __  __      _        ___        __
;; |  \/  | ___| |_ __ _|_ _|_ __  / _| ___
;; | |\/| |/ _ \ __/ _` || || '_ \| |_ / _ \
;; | |  | |  __/ || (_| || || | | |  _| (_) |
;; |_|  |_|\___|\__\__,_|___|_| |_|_|  \___/
;; -----------------------------------------------------------------------------

(defun ng/project-root ()
  "Return current project root dir."
  (or (jh/git-project-root-dir default-directory)
    (error "This file is not inside a GIT repository")))

(defun ng/src-root ()
  "Return current source root dir."
  (let
    ((dir
       (file-name-as-directory
         (expand-file-name "src" (ng/project-root)))))
    (or (file-exists-p dir)
      (error "Folder `src' is not exists!"))
    dir))

(defun ng/app-root (&optional entry)
  "Return current source root dir."
  (let
    ((dir
       (file-name-as-directory
         (expand-file-name "app" (ng/src-root)))))
    (or (file-exists-p dir)
      (error "Failed to get application root!"))
    dir))


;; -----------------------------------------------------------------------------
;;  ____                             _____ _ _
;; / ___|  ___  _   _ _ __ ___ ___  |  ___(_) | ___  ___
;; \___ \ / _ \| | | | '__/ __/ _ \ | |_  | | |/ _ \/ __|
;;  ___) | (_) | |_| | | | (_|  __/ |  _| | | |  __/\__ \
;; |____/ \___/ \__,_|_|  \___\___| |_|   |_|_|\___||___/
;; -----------------------------------------------------------------------------

(defun ng/coerce-to-prefix (file)
  "Force to convert to prefix."
  (let
    ((re "\\(\\.html\\|\\.less\\|\\.spec\\.ts\\|\\.ts\\)$"))
    (replace-regexp-in-string re "" file)))

(defvar ng/filetype-suffix-alist
  '(('model . ".ts")
     ('view . ".html")
     ('style . ".less")
     ('test . ".spec.ts"))
  "File type suffix in angular project.")

(defun ng/find-alternative-file (filetype)
  "Find alternative filename with specific FILETYPE."
  (let
    ((prefix (ng/coerce-to-prefix (buffer-file-name)))
      (lookup (assoc filetype ng/filetype-suffix-alist)))
    (and lookup (concat prefix (cdr lookup)))))

(defun ng/switch-to (&optional filetype)
  "Switch to file type."
  (let
    ((alterfile (ng/find-alternative-file filetype)))
    (and (file-exists-p alterfile) (find-file alterfile))))


;; -----------------------------------------------------------------------------
;;  _  __            ____  _           _ _
;; | |/ /___ _   _  | __ )(_)_ __   __| (_)_ __   __ _ ___
;; | ' // _ \ | | | |  _ \| | '_ \ / _` | | '_ \ / _` / __|
;; | . \  __/ |_| | | |_) | | | | | (_| | | | | | (_| \__ \
;; |_|\_\___|\__, | |____/|_|_| |_|\__,_|_|_| |_|\__, |___/
;;           |___/                               |___/
;; -----------------------------------------------------------------------------

(progn
  ;; Leader Key
  (define-prefix-command 'ng/leader)

  ;; Switcher Keybinding
  (define-key ng/leader (kbd "t") #'(lambda () (interactive) (ng/switch-to 'model)))
  (define-key ng/leader (kbd "h") #'(lambda () (interactive) (ng/switch-to 'view)))
  (define-key ng/leader (kbd "l") #'(lambda () (interactive) (ng/switch-to 'style)))
  (define-key ng/leader (kbd "T") #'(lambda () (interactive) (ng/switch-to 'test))))
(global-set-key (kbd "M-n") 'ng/leader)


(provide 'init-angular)
