;;; github-mode.el --- Explore a Github repository on the fly -*- lexical-binding: t -*-

;; Copyright (C) 2019 Giap Tran <txgvnn@gmail.com>

;; Author: Giap Tran <txgvnn@gmail.com>
;; URL: https://github.com/TxGVNN/github-mode
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (request "0.1.0"))

;;; Commentary:
;; M-x github-go "txgvnn/github-mode"

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'json)

;; * TODO will also accept a full link https://github...

(defgroup github nil
  "Major mode of Github configuration file."
  :group 'languages
  :prefix "github-")

(defcustom github-mode-hook nil
  "*Hook run by `github-mode'."
  :type 'hook
  :group 'github)

(defcustom github-mode-name "Github"
  "*Modeline of `github-mode'."
  :type 'string
  :group 'github)

(defvar github-mode-map nil
  "Keymap for Github major mode.")

(defface github-directory-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for a directory.")

(defvar github-buffer-temp)

(defvar github-repository)

;;;###autoload
(defun github-go (&optional repo)
  "Go REPO github."
  (interactive (list (thing-at-point 'symbol)))
  (setq github-repository (read-string "Repository: " repo))
  (github--tree (format "https://api.github.com/repos/%s/git/trees/%s" github-repository "master") "/"))
;; (github-repo "melpa/melpa")

(defun github-go-at-point()
  "Go to path in buffer Github tree."
  (interactive)
  (let (url path (pos 0) matches repo base-path item)
    (setq item (get-text-property (line-beginning-position) 'invisible))
    (setq url (cdr (assoc 'url item)))
    (setq path (cdr (assoc 'path item)))

    (setq repo (buffer-name))
    (while (string-match ":\\(.+\\)\\*" repo pos)
      (setq matches (concat (match-string 0 repo)))
      (setq pos (match-end 0)))
    (setq base-path (substring matches 1 (- (length matches) 1)))
    (setq repo (car (split-string base-path ":")))
    (setq path (format "%s%s"  (car (cdr (split-string base-path ":"))) path))
    (if (string= (cdr (assoc 'type item)) "tree")
        (setq path (concat path "/")))
    (message "%s" path)
    (if (string= (cdr (assoc 'type item)) "tree")
        (github--tree url path)
      (github--raw repo path))))

(defun github--tree (url path)
  "Get trees by URL of PATH github.
This function will create *Github:REPO:* buffer"
  (setq github-buffer-temp (format "*%s:%s:%s*" github-mode-name github-repository path))
  (request
   url
   :parser 'buffer-string
   :success
   (function* (lambda (&key data &allow-other-keys)
                (when data
                  (with-current-buffer (get-buffer-create github-buffer-temp)
                    (let (github-object)
                      (read-only-mode -1)
                      (erase-buffer)
                      (insert data)
                      (switch-to-buffer (current-buffer))
                      (goto-char (point-min))
                      (setq github-object (json-read))
                      (erase-buffer)
                      (insert (format "[*] %s:%s\n" github-repository path))
                      (github--render-object github-object)
                      (goto-char (point-min))
                      (github-mode))))))
   :error
   (function* (lambda (&rest args &key error-thrown &allow-other-keys)
                (message "Got error: %S" error-thrown)))
   :complete (lambda (&rest _) (message "Finished!"))))

;; * QUESTION support revision? only master?
(defun github--raw (repo path)
  "Get raw of PATH in REPO github."
  (let (url)
    (setq url (format "https://raw.githubusercontent.com/%s/master%s" repo path))
    (setq github-buffer-temp (format "*%s:%s:%s*" github-mode-name repo path))
    (request
     url
     :parser 'buffer-string
     :success
     (function* (lambda (&key data &allow-other-keys)
                  (when data
                    (with-current-buffer (get-buffer-create github-buffer-temp)
                      (erase-buffer)
                      (insert data)
                      (pop-to-buffer (current-buffer))
                      (goto-char (point-min))))))
     :error
     (function* (lambda (&rest args &key error-thrown &allow-other-keys)
                  (message "Got error: %S" error-thrown)))
     :complete (lambda (&rest _) (message "Finished!")))))
;; (github--raw "melpa/melpa" "README.md")

(defun github-apply-auto-mode (&rest _)
  "Apply auto-mode for buffer Github.
pop-to-buffer(BUFFER-OR-NAME &OPTIONAL ACTION NORECORD)"
  (unless buffer-file-name
    (if (string-match-p (regexp-quote github-mode-name) (buffer-name))
        (let ((buffer-file-name (substring (buffer-name) 0 -1))) ;; remove * ending
          (set-auto-mode t)))))
(advice-add 'pop-to-buffer :after #'github-apply-auto-mode)


(defun github--item-path (item)
  "Get the path for an ITEM from Github."
  (cdr (assoc 'path item)))


(defun github--item-type (item)
  "Get the type for an ITEM from Github."
  (cdr (assoc 'type item)))


(defun github--render-object (github-object)
  "Render the GITHUB-OBJECT."
  (let ((trees (cdr (assoc 'tree github-object))))
	(cl-loop
	 for item across trees
	 for path = (github--item-path item)

	 if (string= (github--item-type item) "blob")
	 do (insert (format "  |----- %s" path))
	 else do
	 (insert (format "  |--[+] %s" path))
	 (left-char (length path))
	 (put-text-property (point) (+ (length path) (point)) 'face 'github-directory-face)

	 do
	 (put-text-property (line-beginning-position) (1+ (line-beginning-position)) 'invisible item)
	 (end-of-line)
	 (insert "\n"))))


(define-derived-mode github-mode fundamental-mode github-mode-name
  "Major mode for exploring Github repository on the fly"
  (setq buffer-auto-save-file-name nil
        buffer-read-only t)
  ;; Mode map
  (if (not github-mode-map)
      (progn
        (setq github-mode-map (make-sparse-keymap))
        (define-key github-mode-map (kbd "o") 'github-go-at-point)
        (define-key github-mode-map (kbd "RET") 'github-go-at-point)
        (define-key github-mode-map (kbd "n") 'next-line)
        (define-key github-mode-map (kbd "p") 'previous-line)))
  (use-local-map github-mode-map))


(provide 'github-mode)
;;; github-mode.el ends here
