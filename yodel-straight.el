;;; yodel-straight.el --- straight.el extensions     -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Nicholas Vollmer

;; Author:  Nicholas Vollmer
;; URL: https://github.com/progfolio/yodel
;; Created: August 30, 2021
;; Keywords: convenience
;; Version: 0.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'yodel)
(require 'straight)

(defvar yodel--straight-host-url-formatters
  '(("github.com" identity (lambda (url commit) (concat url "/commit/" commit)))
    ("gitlab.com" identity (lambda (url commit) (concat url "/-/commit/" commit)))
    ("git.sr.ht"  identity (lambda (url commit) (concat url "/commit/" commit)))
    ("\\(git.savannah.gnu.org/\\)\\(git\\)" "\\1c\\2"
     (lambda (url commit) (concat url "/commit/?id=" commit))))
  "List of URL formatters for various hosts.
Each entry is a list form: (URL-REGEXP, URL-REPLACEMENT, COMMIT-URL-FORMATTER).")

(defun yodel-straight--repo-info (repo)
  "Return plist with branch, commit, commit date info for REPO."
  (let ((default-directory (straight--repos-dir repo)))
    (when (file-exists-p default-directory)
      (let* ((info (split-string (straight--process-output "git" "show" "-s"
                                                           "--format=%H %cs")
                                 " "))
             (commit (car info)))
        (list :branch (straight-vc-git--local-branch "HEAD")
              :commit commit
              :date   (cadr info))))))

(defun yodel-straight--format-urls (repo commit &optional host)
  "Return pair of URLS of form (REPO . COMMIT) considering HOST."
  (when host (setq repo (format "https://%s.com/%s"
                                (alist-get host '((github . "github")
                                                  (gitlab . "gitlab")))
                                repo)))
  (if (string-match-p "https?:" repo)
      (cl-some (lambda (formatter)
                 (when (string-match-p (car formatter) repo)
                   (let ((formatted
                          (replace-regexp-in-string (car formatter)
                                                    (cadr formatter) repo)))
                     (cons formatted (funcall (caddr formatter) formatted commit)))))
               yodel--straight-host-url-formatters)))

(defun yodel-straight--package-info ()
  "Return pacakge info plist for use with yodel."
  (let ((packages '()))
    (maphash (lambda (key val)
               (unless (member (intern key) (append straight-recipe-repositories
                                                    '(straight)))
                 (cl-destructuring-bind
                     (&key repo local-repo host &allow-other-keys &aux
                           (source (straight-recipe-source key))
                           (version (when local-repo (yodel-straight--repo-info local-repo)))
                           (urls (when repo
                                   (yodel-straight--format-urls repo
                                                                (plist-get version :commit)
                                                                host)))
                           (url (car urls)))
                     (nth 2 val) ;recipe
                   (setq version (plist-put version :commit-url (cdr urls)))
                   (push (list :name key :source source :repo repo
                               :local-repo local-repo :host host :url url :version version)
                         packages))))
             straight--build-cache)
    (nreverse packages)))


(provide 'yodel-straight)
;;; yodel-straight.el ends here
