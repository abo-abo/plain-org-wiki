;;; helm-org-wiki.el --- Simple jump-to-org-files in a directory package

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

(require 'helm)
(require 'helm-match-plugin)

(defgroup helm-org-wiki nil
  "Simple jump-to-org-file package."
  :group 'org
  :prefix "helm-org-wiki-")

(defcustom helm-org-wiki-directory "~/org/wiki/"
  "Directory where files for `helm-org-wiki' are stored."
  :type 'directory)

(defun helm-org-wiki-files ()
  "Return .org files in `helm-org-wiki-directory'."
  (let ((default-directory helm-org-wiki-directory))
    (mapcar #'file-name-sans-extension
            (file-expand-wildcards "*.org"))))

(defun helm-org-wiki-find-file (x)
  "Open X as a file with org extension in `helm-org-wiki-directory'."
  (find-file (expand-file-name
              (format "%s.org" x)
              helm-org-wiki-directory)))

(defvar helm-source-org-wiki
  '((name . "Projects")
    (candidates . helm-org-wiki-files)
    (action . helm-org-wiki-find-file)))

(defvar helm-source-org-wiki-not-found
  '((name . "Create org-wiki")
    (dummy)
    (action . helm-org-wiki-find-file)))

;;;###autoload
(defun helm-org-wiki ()
  "Select an org-file to jump to."
  (interactive)
  (helm :sources
        '(helm-source-org-wiki
          helm-source-org-wiki-not-found)))

(provide 'helm-org-wiki)

;;; helm-org-wiki.el ends here
