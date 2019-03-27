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

(defface github-directory-face
  '((t (:inherit (dired-directory-face bold))))
  "Face for a directory.")


(defvar github-buffer-temp)
(defvar github-repository)

(defun github-repo (&optional repo)
  "Go REPO github."
  (interactive (list (thing-at-point 'symbol)))
  (setq github-repository (read-string "Repository: " repo))
  (github--tree (format "https://api.github.com/repos/%s/git/trees/%s" github-repository "master") "/"))
;; (github-repo "melpa/melpa")

(defun github-go()
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
  (let (item i trees)
    (setq trees (cdr (assoc 'tree gh-object)))
    (setq i 0)
    (while (< i (length trees))
      (setq item (elt trees i))
      (if (string= (cdr (assoc 'type item)) "tree")
          (insert (format "   [+] %s" (cdr (assoc 'path item))))
        (insert (format "   --- %s" (cdr (assoc 'path item)))))
      (put-text-property (line-beginning-position) (+ (line-beginning-position) 1) 'invisible item)
      (end-of-line)
      (insert "\n")
      (setq i (+ i 1))
      )
    ))

;;;###autoload
(define-derived-mode github-mode fundamental-mode github-mode-name
  "Major mode for exploring Github repository on the fly"
  (setq buffer-auto-save-file-name nil
        buffer-read-only t)
  )

(provide 'github)
