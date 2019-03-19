;;; Code:

(require 'request)

;; * TODO create github-mode (major-mode)

;; DEBUG
(setq request-log-level 'trace)
(setq request-message-level 'trace)
(setq request-curl-options '("-k"))

;; * TODO will also accept a full link https://github...
;; * TODO entering to github-mode
(defun github-go (&optional repo)
  "Go REPO github."
  (interactive (list (thing-at-point 'symbol)))
  (setq repo (read-string "Repository: " repo))
  (github-tree repo "master"))
;; (github-go "melpa/melpa")

(defun github-tree (&optional repo tree)
  "Get TREE of REPO github.
This function will create *GITHUB:REPO:* buffer"
  (let (url)
    (setq url (format "https://api.github.com/repos/%s/git/trees/%s" repo tree))
    (setq buffer (format "*GITHUB:%s:*" repo))
    (request
     url
     :parser 'buffer-string
     :success
     (cl-function (lambda (&key data &allow-other-keys)
                    (when data
                      (with-current-buffer (get-buffer-create buffer)
                        (erase-buffer)
                        (insert data)
                        (pop-to-buffer (current-buffer))))))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys&rest _)
                    (message "Got error: %S" error-thrown)))
     :complete (lambda (&rest _) (message "Finished!")))))

(defun github-raw(&optional path)
  "Get raw of PATH in *GITHUB:REPO* buffer.
This function is have to called in *GITHUB:REPO* buffer"
  (interactive (list (thing-at-point 'symbol)))
  (setq path (read-string "Path: " path))
  (let ((pos 0) matches repo)
    (setq repo (buffer-name))
    (while (string-match ":\\(.+\\):" repo pos)
      (setq matches (concat (match-string 0 repo)))
      (setq pos (match-end 0)))
    (setq repo (substring matches 1 (- (length matches) 1)))
    (github--raw repo path)))
;; (github-tree "melpa/melpa" "2f6ab4e3eebf90341b56e0d672471ef017d10c4c")

;; * QUESTION support revision? only master?
(defun github--raw (&optional repo path)
  "Get raw of PATH in REPO github."
  (let (url buffer)
    (setq url (format "https://raw.githubusercontent.com/%s/master/%s" repo path))
    (setq buffer (format "*GITHUB:%s:%s*" repo path))
    (message "%s" buffer)
    (request
     url
     :parser 'buffer-string
     :success
     (cl-function (lambda (&key data &allow-other-keys)
                    (when data
                      (with-current-buffer (get-buffer-create buffer)
                        (erase-buffer)
                        (insert data)
                        (pop-to-buffer (current-buffer))))))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys&rest _)
                    (message "Got error: %S" error-thrown)))
     :complete (lambda (&rest _) (message "Finished!")))))

;; (github--raw "melpa/melpa" "README.md")

(provide 'github)
