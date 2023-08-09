;;; yodel-elpaca.el --- Yodel Elpaca integration     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; Keywords: convenience

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

(declare-function elpaca-generate-autoloads "elpaca")
(declare-function elpaca-process-queues "elpaca")
(declare-function elpaca "elpaca")
(declare-function elpaca-wait "elpaca")
(declare-function elpaca-get "elpaca")
(declare-function elpaca--queued "elpaca")
(declare-function elpaca-menu-item "elpaca")
(declare-function elpaca<-repo-dir "elpaca")
(declare-function elpaca<-recipe "elpaca")
(declare-function elpaca-process-output "elpaca-process")

(defvar yodel-elpaca--bootstrap
  '(progn
     (defvar elpaca-installer-version 0.5)
     (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
     (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
     (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
     (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                                   :ref nil
                                   :files (:defaults (:exclude "extensions"))
                                   :build (:not elpaca--activate-package)))
     (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
            (build (expand-file-name "elpaca/" elpaca-builds-directory))
            (order (cdr elpaca-order))
            (default-directory repo))
       (add-to-list 'load-path (if (file-exists-p build) build repo))
       (unless (file-exists-p repo)
         (make-directory repo t)
         (when (< emacs-major-version 28) (require 'subr-x))
         (condition-case-unless-debug err
             (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                      ((zerop (call-process "git" nil buffer t "clone"
                                            (plist-get order :repo) repo)))
                      ((zerop (call-process "git" nil buffer t "checkout"
                                            (or (plist-get order :ref) "--"))))
                      (emacs (concat invocation-directory invocation-name))
                      ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                            "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                      ((require 'elpaca))
                      ((elpaca-generate-autoloads "elpaca" repo)))
                 (progn (message "%s" (buffer-string)) (kill-buffer buffer))
               (error "%s" (with-current-buffer buffer (buffer-string))))
           ((error) (warn "%s" err) (delete-directory repo 'recursive))))
       (unless (require 'elpaca-autoloads nil t)
         (require 'elpaca)
         (elpaca-generate-autoloads "elpaca" repo)
         (load "./elpaca-autoloads")))
     (add-hook 'after-init-hook #'elpaca-process-queues)
     (elpaca `(,@elpaca-order))))

(defvar yodel-args)
(defun yodel-elpaca--install (&rest packages)
  "Install PACKAGES via Elpaca."
  (eval yodel-elpaca--bootstrap t)
  (mapc (lambda (declaration) (eval `(elpaca ,declaration) t)) packages)
  (unless (plist-get yodel-args :interactive) (elpaca-wait)))

(declare-function elpaca<-package "elpaca")
(defun yodel-elpaca--repo-info (e)
  "Return plist with branch, commit, commit date info for E."
  (if-let ((default-directory (elpaca<-repo-dir e))
           ((file-exists-p default-directory))
           (info (split-string (string-trim (elpaca-process-output "git" "show" "-s"
                                                                   "--format=%H %cs")
                                            " ")))
           (commit (car info)))
      (list :branch (string-trim (elpaca-process-output "git" "rev-parse" "--abbrev-ref" "HEAD"))
            :commit commit
            :date   (cadr info))
    (message "No repo info for %S" (elpaca<-package e))))

(defun yodel-elpaca--package-info ()
  "Return pacakge info plist for use with yodel."
  (cl-loop for (id . e) in (elpaca--queued)
           for recipe = (elpaca<-recipe e)
           for repo = (plist-get recipe :repo)
           for local-repo = (or (plist-get recipe :local-repo) repo)
           for host = (or (plist-get recipe :host) (plist-get recipe :fetcher))
           for version = (yodel-elpaca--repo-info e)
           for urls = (when repo (yodel--format-urls repo
                                                     (plist-get version :commit)
                                                     host))
           for url = (car urls)
           for menu-item = (elpaca-menu-item id)
           for menu-props = (cdr menu-item)
           collect
           (progn
             (setq version (plist-put version :commit-url (cdr urls)))
             (list :name (symbol-name id) :url (or url (plist-get menu-props :url))
                   :version version :repo repo :local-repo local-repo :host host
                   :source (plist-get menu-props :source)))))

(provide 'yodel-elpaca)
;;; yodel-elpaca.el ends here
