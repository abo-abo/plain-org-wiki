;;; plain-org-wiki.el --- Simple jump-to-org-files in a directory package

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Version: 0.1.0
;; Keywords: completion

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'ivy)

(defgroup plain-org-wiki nil
  "Simple jump-to-org-file package."
  :group 'org
  :prefix "plain-org-wiki-")

(defcustom pow-directory "~/org/wiki/"
  "Directory where files for `plain-org-wiki' are stored."
  :type 'directory)

(defvar pow-extra-dirs nil
  "List of extra directories in addition to `pow-directory'.")

(defun pow-files-in-dir (dir)
  (let ((default-directory dir))
    (mapcar
     (lambda (x)
       (cons (file-name-sans-extension x)
             (expand-file-name x)))
     (append
      (file-expand-wildcards "*.org")
      (file-expand-wildcards "*.org.gpg")))))

(defun pow-files ()
  "Return .org files in `pow-directory'."
  (cl-mapcan #'pow-files-in-dir
             (cons pow-directory pow-extra-dirs)))

(defun pow-files-recursive ()
  "Return .org files in `pow-directory' and subdirectories."
  (let ((ffip-project-root pow-directory))
    (delq nil
          (mapcar (lambda (x)
                    (when (equal (file-name-extension (car x)) "org")
                      (file-name-sans-extension (car x))))
                  (ffip-project-files)))))

(defun pow-find-file (x)
  "Open X as a file with org extension in `pow-directory'."
  (when (consp x)
    (setq x (cdr x)))
  (with-ivy-window
    (if (file-exists-p x)
        (find-file x)
      (if (string-match "org$" x)
          (find-file
           (expand-file-name x pow-directory))
        (find-file
         (expand-file-name
          (format "%s.org" x) pow-directory))))))

;;;###autoload
(defun plain-org-wiki-helm ()
  "Select an org-file to jump to."
  (interactive)
  (require 'helm)
  (require 'helm-multi-match)
  (helm :sources
        '(((name . "Projects")
           (candidates . pow-files)
           (action . pow-find-file))
          ((name . "Create org-wiki")
           (dummy)
           (action . pow-find-file)))))

;;;###autoload
(defun plain-org-wiki ()
  "Select an org-file to jump to."
  (interactive)
  (ivy-read "pattern: " (pow-files)
            :action 'pow-find-file))

(provide 'plain-org-wiki)

;;; plain-org-wiki.el ends here
