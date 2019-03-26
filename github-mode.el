;;; Code:

(require 'request)

;; * TODO create github-mode (major-mode)

;; DEBUG
(setq request-log-level 'trace)
(setq request-message-level 'trace)
(setq request-curl-options '("-k"))

(defvar github-buffer-temp)
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
    (setq github-buffer-temp (format "*Github:%s:*" repo))
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
                          (github--render-object gh-object))
                        ))))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys&rest _)
                    (message "Got error: %S" error-thrown)))
     :complete (lambda (&rest _) (message "Finished!")))))

(defun github-raw(&optional path)
  "Get raw of PATH in *GITHUB:REPO* buffer.
This function is have to called in *GITHUB:REPO* buffer"
  (interactive (list (thing-at-point 'filename)))
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
  (let (url)
    (setq url (format "https://raw.githubusercontent.com/%s/master/%s" repo path))
    (setq github-buffer-temp (format "*Github:%s:%s*" repo path))
    (request
     url
     ;; "https://api.github.com/repos/txgvnn/dots/git/trees/master"
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
  (let (item i trees)
    (setq trees (cdr (assoc 'tree gh-object)))
    (setq i 0)
    (while (< i (length trees))
      (setq item (elt trees i))
      (if (string= (cdr (assoc 'type item)) "tree")
          (insert (format "[+] %s\n" (cdr (assoc 'path item))))
        (insert (format "--- %s\n" (cdr (assoc 'path item)))))
      (setq i (+ i 1))
      )
    )
  )

(provide 'github)
