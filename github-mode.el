;;; github-mode.el --- Explore a GitHub repository on the fly -*- lexical-binding: t -*-

;; Copyright (C) 2019 Giap Tran <txgvnn@gmail.com>

;; Author: Giap Tran <txgvnn@gmail.com>
;; URL: https://github.com/TxGVNN/github-mode
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (request "0.1.0"))
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; M-x github-go "txgvnn/github-mode"

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'json)

(defgroup github nil
  "Major mode of GitHub configuration file."
  :group 'languages
  :prefix "github-")

(defcustom github-mode-hook nil
  "*Hook run by `github-mode'."
  :type 'hook
  :group 'github)

(defcustom github-mode-name "GitHub"
  "*Modeline of `github-mode'."
  :type 'string
  :group 'github)

(defvar github-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "o") 'github-go-at-point)
    (define-key keymap (kbd "RET") 'github-go-at-point)
    (define-key keymap (kbd "n") 'next-line)
    (define-key keymap (kbd "p") 'previous-line)
    keymap)
  "Keymap for GitHub major mode.")

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

(defun github-go-at-point()
  "Go to path in buffer GitHub tree."
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
This function will create *GitHub:REPO:* buffer"
  (setq github-buffer-temp (format "*%s:%s:%s*" github-mode-name github-repository path))
  (request
   url
   :parser 'buffer-string
   :success
   (cl-function (lambda (&key data &allow-other-keys)
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
   (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                  (message "Got error: %S" error-thrown)))))

(defun github--raw (repo path)
  "Get raw of PATH in REPO github."
  (let (url)
    (setq url (format "https://raw.githubusercontent.com/%s/master%s" repo path))
    (setq github-buffer-temp (format "*%s:%s:%s*" github-mode-name repo path))
    (request
     url
     :parser 'buffer-string
     :success
     (cl-function (lambda (&key data &allow-other-keys)
                    (when data
                      (with-current-buffer (get-buffer-create github-buffer-temp)
                        (erase-buffer)
                        (insert data)
                        (pop-to-buffer (current-buffer))
                        (goto-char (point-min))))))
     :error
     (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                    (message "Got error: %S" error-thrown))))))

(defun github-apply-auto-mode (&rest _)
  "Apply auto-mode for buffer GitHub.
pop-to-buffer(BUFFER-OR-NAME &OPTIONAL ACTION NORECORD)"
  (unless buffer-file-name
    (if (string-match-p (regexp-quote (format "*%s:" github-mode-name)) (buffer-name))
        (let ((buffer-file-name (substring (buffer-name) 0 -1))) ;; remove * ending
          (set-auto-mode t)))))
(advice-add 'pop-to-buffer :after #'github-apply-auto-mode)

(defun github--item-path (item)
  "Get the path for an ITEM from GitHub."
  (cdr (assoc 'path item)))

(defun github--item-type (item)
  "Get the type for an ITEM from GitHub."
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

(define-derived-mode github-mode special-mode github-mode-name
  "Major mode for exploring GitHub repository on the fly"
  (setq buffer-auto-save-file-name nil
        buffer-read-only t))

(provide 'github-mode)
;;; github-mode.el ends here
