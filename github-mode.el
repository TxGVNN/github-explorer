;;; github-mode.el --- Explore a Github repository on the fly -*- lexical-binding: t -*-

;; Copyright (C) 2019 Giap Tran <txgvnn@gmail.com>

;; Author: Giap Tran <txgvnn@gmail.com>
;; URL: https://github.com/TxGVNN/github-mode
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (request "0.1.0"))

;;; Commentary:
;; M-x github-go "txgvnn/github-mode"

;;; Code:

(require 'request)

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

(defun github-go (&optional repo)
  "Go REPO github."
  (interactive (list (thing-at-point 'symbol)))
  (setq github-repository (read-string "Repository: " repo))
  (github--tree (format "https://api.github.com/repos/%s/git/trees/%s" github-repository "master") "/"))
;; (github-repo "melpa/melpa")

(defun github-go-at-point()
  "Go to path in buffer Github tree."
  (interactive)
  (let (url path (pos 0) matches repo base-path)
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
  (setq github-buffer-temp (format "*Github:%s:%s*" github-repository path))
  (request
   url
   :parser 'buffer-string
   :success
   (cl-function (lambda (&key data &allow-other-keys)
                  (when data
                    (with-current-buffer (get-buffer-create github-buffer-temp)
                      (let (gh-object)
                        (erase-buffer)
                        (insert data)
                        (pop-to-buffer (current-buffer))
                        (beginning-of-buffer)
                        (setq gh-object (json-read))
                        (erase-buffer)
                        (github--render-object gh-object)
                        (github-mode)
                        )
                      ))))
   :error
   (cl-function (lambda (&key error-thrown &allow-other-keys&rest _)
                  (message "Got error: %S" error-thrown)))
   :complete (lambda (&rest _) (message "Finished!"))))

;; * QUESTION support revision? only master?
(defun github--raw (&optional repo path)
  "Get raw of PATH in REPO github."
  (let (url)
    (setq url (format "https://raw.githubusercontent.com/%s/master%s" repo path))
    (setq github-buffer-temp (format "*Github:%s:%s*" repo path))
    (request
     url
     :parser 'buffer-string
     :success
     (cl-function (lambda (&key data &allow-other-keys)
                    (when data
                      (with-current-buffer (get-buffer-create github-buffer-temp)
                        (erase-buffer)
                        (insert data)
                        (pop-to-buffer (current-buffer))))))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys&rest _)
                    (message "Got error: %S" error-thrown)))
     :complete (lambda (&rest _) (message "Finished!")))))
;; (github--raw "melpa/melpa" "README.md")


(defun github--render-object (gh-object)
  "Render the GH-OBJECT."
  (let (item i trees path)
    (setq trees (cdr (assoc 'tree gh-object)))
    (setq i 0)
    (while (< i (length trees))
      (setq item (elt trees i))
      (setq path (cdr (assoc 'path item)))
      (if (string= (cdr (assoc 'type item)) "blob")
          (insert (format "   --- %s" path))
        (insert (format "   [+] %s" path))
        (left-char (length path))
        (put-text-property (point) (+ (length path) (point)) 'face 'github-directory-face)
        )
      (put-text-property (line-beginning-position) (+ (line-beginning-position) 1) 'invisible item)
      (end-of-line)
      (insert "\n")
      (setq i (+ i 1)))))

;;;###autoload
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
  (use-local-map github-mode-map)
  )


(provide 'github-mode)
;;; github-mode.el ends here
