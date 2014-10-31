;;; helm-org-wiki.el --- Simple jump-to-org-files in a directory package

;;; Commentary:
;;

;;; Code:

(require 'helm)

(defgroup helm-org-wiki nil
  "Simple jump-to-org-file package."
  :group 'org
  :prefix "helm-org-wiki-")


(defcustom helm-org-wiki-directory "~/org/wiki/"
  "Directory where files for `helm-org-wiki' are stored."
  :group 'helm-org-wiki
  :type 'directory)

(defun helm-org-wiki-files ()
  "Return .org files in `helm-org-wiki-directory'."
  (let ((default-directory helm-org-wiki-directory))
    (mapcar #'file-name-sans-extension
            (file-expand-wildcards "*.org"))))

(defvar helm-source-org-wiki
  `((name . "Projects")
    (candidates . helm-org-wiki-files)
    (action . ,(lambda (x)
                       (find-file (expand-file-name
                                   (format "%s.org" x)
                                   helm-org-wiki-directory))))))

(defvar helm-source-org-wiki-not-found
  `((name . "Create org-wiki")
    (dummy)
    (action . (lambda (x)
                (helm-switch-to-buffer
                 (find-file
                  (format "%s/%s.org"
                          helm-org-wiki-directory x)))))))
;;;###autoload
(defun helm-org-wiki ()
  "Select an org-file to jump to."
  (interactive)
  (helm :sources
        '(helm-source-org-wiki
          helm-source-org-wiki-not-found)))


(provide 'helm-org-wiki)

;;; helm-org-wiki.el ends here
