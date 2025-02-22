;;; yodel.el --- Communicable Elisp                    -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Nicholas Vollmer

;; Author:  Nicholas Vollmer
;; URL: https://github.com/progfolio/yodel
;; Created: August 30, 2021
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
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
;; The purpose of this package is to make it easier to reproduce elisp bugs.
;; Often on a mailing list or forge issue, I see people type out a series
;; of instructions which follow a common pattern:
;;
;; Given file "foo" with the following contents:
;;
;;--8<---------------cut here---------------start------------->8---
;; foo bar baz
;;--8<---------------cut here---------------end--------------->8---
;;
;; Execute the following:
;; 1. do this
;; 2. do that
;; 3. do this
;;
;; This is error prone and inefficient. It requires anyone on the other end to
;; prepare the environment (in this case a file) and manually execute the
;; reproduction steps. Yodel allows one to send a declarative form which describes
;; the environment and a program to execute within that environment. Others may
;; execute the form on their system and compare results easily via a consistently
;; formatted report.

;;; Code:
(require 'cl-lib)
(require 'pp)
(eval-when-compile (require 'subr-x))

(defgroup yodel nil
  "Communicable Elisp."
  :group 'yodel
  :prefix "yodel-")

(defconst yodel--process-end-text "YODEL--PROCESS-END"
  "String denoting end of process output and start of report form.")

(defvar yodel--default-args `("-Q" "--eval" "(setq debug-on-error t)" "-L" ,(file-name-directory (locate-library "yodel")) "--eval")
  "Arguments passed to the Emacs executable when testing.")

(defvar yodel-process-buffer "*yodel*"
  "Name of the yodel subprocess buffer.")

(eval-and-compile
  (defvar yodel-formatters nil
    "List of yodel report formatting functions."))

(defvar-local yodel--report nil
  "Report data structure.
Used for reformatting the report.")

(defvar-local yodel-args nil)

(defun yodel--pretty-print (form)
  "Convert elisp FORM into formatted string."
  (let* ((print-level nil)
         (print-length nil)
         (print-quoted t)
         (pp-escape-newlines nil))
    (with-temp-buffer
      (insert (pp-to-string form))
      (goto-char (point-min))
      (emacs-lisp-mode)
      (font-lock-mode 1)
      (font-lock-ensure)
      (mapc (lambda (target)
              (save-excursion
                (while (re-search-forward (car target) nil 'no-error)
                  (funcall (cdr target)))))
            '(("\\(?: \\(:[^z-a]*?\\) \\)";; keywords
               . (lambda ()
                   (when (eq (get-text-property (match-beginning 1) 'face)
                             'font-lock-builtin-face)
                     (replace-match
                      (concat "\n\\1" (if (string-suffix-p "*" (match-string 1)) "\n" " "))))))))
      ;; Remove empty lines.
      (goto-char (point-min))
      (flush-lines "\\(?:^[[:space:]]*$\\)")
      (let ((inhibit-message t))
        (indent-region (point-min) (point-max)))
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(defmacro yodel-formatter (name description &rest body)
  "Create a yodel formatting function with BODY and NAME.
Add the function to `yodel-formatters'.
Each function should accept a report plist as its sole argument.
DESCRIPTION is used as the docstring, and when prompting via `yodel-reformat'.
BODY will be executed in the context of an empty `yodel-process-buffer'.
`buffer-string' is returned after BODY is executed.
The following anaphoric bindings are available during BODY:

- stdout: The standard output of the subprocess.
- stderr: The errors output by the subprocess.
- report: The report form."
  (declare (indent defun))
  (let ((fn (intern (format "yodel-format-as-%s" name))))
    (when (fboundp fn) (makunbound fn))
    `(cl-pushnew
      (defun ,fn (report)
        ,(replace-regexp-in-string "report" #'upcase description)
        (cl-destructuring-bind (&key stdout stderr report) report
          (ignore report stdout stderr) ;no-op here to satisfy byte compiler.
          (erase-buffer)
          (goto-char (point-min))
          ,@body
          (buffer-string)))
      yodel-formatters)))

(defun yodel--format-link (format-string name url)
  "Replace NAME and URL in FORMAT-STRING."
  (replace-regexp-in-string
   "%url" url
   (replace-regexp-in-string "%name" name format-string)))

(defvar yodel--host-url-formatters
  '(("github.com" identity (lambda (url commit)
                             (concat (replace-regexp-in-string "\\.git$" "" url) "/commit/" commit)))
    ("gitlab.com" identity (lambda (url commit) (concat url "/-/commit/" commit)))
    ("git.sr.ht"  identity (lambda (url commit) (concat url "/commit/" commit)))
    ("\\(git.savannah.gnu.org/\\)\\(git\\)" "\\1c\\2"
     (lambda (url commit) (concat url "/commit/?id=" commit))))
  "List of URL formatters for various hosts.
Each entry is a list form: (URL-REGEXP, URL-REPLACEMENT, COMMIT-URL-FORMATTER).")

(defun yodel--format-urls (repo commit &optional host)
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
               yodel--host-url-formatters)))

(defun yodel--package-table-row (package &optional link-format short)
  "Return formatted table row for PACKAGE.
LINK-FORMAT is used to format links within the table row.
It must contain two substitution strings: %name and %url.
If SHORT is non-nil, abbreviated commits are used in links."
  (cl-destructuring-bind
      ( &key version url name source &allow-other-keys
        &aux
        ;;@TODO: link directly to branch
        (link-format (or link-format "[%name](%url)"))
        (name (if (and version url) (yodel--format-link link-format name url) name))
        (vc-info
         (apply #'format
                `("%-10s|%-10s|%s"
                  ,@(if version
                        (cl-destructuring-bind
                            ( &key commit ((:commit-url url)) date branch
                              &allow-other-keys
                              &aux
                              (abbrev (substring commit 0 10))
                              (commit
                               (if url
                                   (if (and
                                        (not short)
                                        (string-match-p "github.com" url))
                                       url
                                     (yodel--format-link link-format abbrev url))
                                 abbrev)))
                            version
                          (list branch commit date))
                      '("nil" "nil" "nil"))))))
      package
    (format "|%s|%s|%s|" name vc-info source)))

(eval-and-compile
  (yodel-formatter raw
    "Format report as a raw, readable plist."
    (insert (let (print-level print-length)
              (pp-to-string report)))))

(eval-and-compile
  (yodel-formatter mailing-list-message
    "Format report as a plain text email message."
    (message-mode)
    (cl-flet ((quoted (s)
                (with-temp-buffer
                  (with-silent-modifications ;otherwise we're prompted to save modified buffer
                    (message-mode)
                    (insert s)
                    (comment-region (point-min) (point-max))
                    (buffer-string))))
              (underline ()
                (insert (make-string (save-excursion (forward-line -1)
                                                     (- (line-end-position)
                                                        (line-beginning-position)))
                                     ?=))))
      (insert (format "Yodel[1] Report %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S"
                                          (plist-get report :yodel-time))))
      (underline)
      (insert "\n\n"
              (with-temp-buffer
                (insert (plist-get report :yodel-form) "\n")
                (when (fboundp 'message-mark-inserted-region)
                  (message-mark-inserted-region (point-min) (point-max)))
                (buffer-string)))
      (when stdout
        (insert "\nSTDOUT\n")
        (underline)
        (insert "\n\n" (quoted stdout) "\n"))
      (when stderr
        (insert "\nSTDERR\n")
        (underline)
        (insert "\n\n" (quoted stderr) "\n"))
      (insert "\nEnvironment\n")
      (underline)
      (insert
       "\n\n"
       (mapconcat (lambda (el) (format "- %s: %s" (car el) (cdr el)))
                  (list (cons "emacs version" (emacs-version))
                        (cons "system type" system-type))
                  "\n"))
      (when-let ((packages (plist-get report :packages)))
        (insert "\n\nPackages\n")
        (underline)
        (insert "\n")
        (mapc (lambda (package)
                (let ((longest (number-to-string
                                (length (plist-get
                                         (car (cl-sort (copy-tree packages)
                                                       #'string>
                                                       :key (lambda (p) (plist-get p :name))))
                                         :name)))))
                  (insert (format (concat "\n- %-" longest "s %s")
                                  (plist-get package :name)
                                  (or (plist-get (plist-get package :version) :commit-url)
                                      (plist-get package :url))))))
              (cl-sort (copy-tree packages) #'string< :key (lambda (p) (plist-get  p :name))))
        "\n\n")
      (insert "\n\n[1] https://www.github.com/progfolio/yodel"))
    (set-buffer-modified-p nil)))

(eval-and-compile
  (yodel-formatter org
    "Format REPORT in Org syntax."
    (when (fboundp 'org-mode) (org-mode))
    (let ((src-start "#+begin_src emacs-lisp :lexical t :results silent\n")
          (src-end "\n#+end_src")
          (packages (plist-get report :packages)))
      (insert
       (string-join
        `(,(format "* Yodel Report [%s]"
                   (format-time-string "%Y-%m-%d %H:%M:%S"
                                       (seconds-to-time (plist-get report :yodel-time))))
          ,(concat src-start (plist-get report :yodel-form) src-end)
          ,@(when stdout (list "** STDOUT:" (concat src-start stdout src-end)))
          ,@(when stderr (list "** STDERR:" (concat src-start stderr src-end)))
          "** Environment"
          ,(mapconcat (lambda (el) (format "- %s: %s" (car el) (cdr el)))
                      (list (cons "=emacs version=" (emacs-version))
                            (cons "=system type=" system-type))
                      "\n")
          ,@(when packages
              (list (concat
                     "*** Packages\n\n"
                     "| Name    | Branch  | Commit  | Date    | Source |\n"
                     "|---------|---------|---------|---------|--------|\n"
                     (mapconcat (lambda (p) (yodel--package-table-row p "[[%url][%name]]" 'short))
                                (cl-sort (copy-tree packages) #'string<
                                         :key (lambda (p) (plist-get p :name)))
                                "\n")))))
        "\n\n"))
      (when packages
        (when (fboundp 'org-cycle)
          (goto-char (point-max))
          (re-search-backward "^|")
          (org-cycle))
        (goto-char (point-min))))))

(eval-and-compile
  (yodel-formatter reddit-markdown
    "Format REPORT in reddit flavored markdown."
    (when (fboundp 'markdown-mode) (markdown-mode))
    (cl-flet ((indent (s) (with-temp-buffer
                            (insert s)
                            (let ((inhibit-message t))
                              (indent-rigidly (point-min) (point-max) 4))
                            (buffer-string))))
      (insert
       (string-join
        `(,(format "# [Yodel](https://github.com/progfolio/yodel) Report (%s):"
                   (format-time-string "%Y-%m-%d %H:%M:%S"
                                       (seconds-to-time (plist-get report :yodel-time))))
          ;;use four spaces because old reddit doesn't render code fences
          ,(indent (plist-get report :yodel-form))
          ,@(when stdout (list "## STDOUT:" (indent stdout)))
          ,@(when stderr (list "## STDERR:" (indent stderr)))
          "## Environment"
          ,(mapconcat (lambda (el) (format "- %s: %s" (car el) (cdr el)))
                      (list (cons "**emacs version**" (emacs-version))
                            (cons "**system type**" system-type))
                      "\n")
          ,(when-let ((packages (plist-get report :packages)))
             (concat
              "### Packages\n\n"
              "| Name    | Branch  | Commit  | Date    | Source |\n"
              "|---------|---------|---------|---------|--------|\n"
              (mapconcat (lambda (p) (yodel--package-table-row p nil 'short))
                         (cl-sort (copy-tree (plist-get report :packages))
                                  #'string<
                                  :key (lambda (it) (plist-get it :name)))
                         "\n"))))
        "\n\n")))
    (when (plist-get report :packages)
      (when (fboundp 'markdown-cycle)
        (goto-char (point-max))
        (re-search-backward "^|")
        (markdown-cycle))
      (goto-char (point-min)))))

(eval-and-compile
  (yodel-formatter github-markdown
    "Format REPORT in github flavored markdown."
    (when (fboundp 'markdown-mode) (markdown-mode))
    (let ((fence-start "\n```emacs-lisp\n")
          (fence-end "\n```\n")
          (packages (plist-get report :packages)))
      (insert
       (string-join
        `(,(format "[Yodel](https://github.com/progfolio/yodel) Report (%s):"
                   (format-time-string "%Y-%m-%d %H:%M:%S"
                                       (seconds-to-time (plist-get report :yodel-time))))
          ,(concat fence-start (or (plist-get report :yodel-form) "(yodel)") fence-end)
          ,@(when stdout (list "<details><summary>STDOUT:</summary>"
                               (concat fence-start stdout fence-end)
                               "</details>"))
          ,@(when stderr (list "<details><summary>STDERR:</summary>"
                               (concat fence-start stderr fence-end)
                               "</details>"))
          "<details><summary>Environment</summary>\n"
          ,(mapconcat (lambda (el) (format "- %s: %s" (car el) (cdr el)))
                      (list (cons "**emacs version**" (emacs-version))
                            (cons "**system type**" system-type))
                      "\n")
          ,@(when packages
              (list
               "\n<details open><summary>Packages</summary>\n"
               "| Name    | Branch  | Commit  | Date    | Source |"
               "|---------|---------|---------|---------|--------|"
               (mapconcat #'yodel--package-table-row (plist-get report :packages) "\n")
               "\n</details>"))
          "\n</details>")
        "\n"))
      (when (and packages (fboundp 'markdown-cycle))
        (goto-char (point-max))
        (re-search-backward "^|")
        (markdown-cycle))
      (goto-char (point-min)))))

(defcustom yodel-default-formatter #'yodel-format-as-org
  "Default report formatting function."
  :type 'function)

;; A variadic plist is a strict subset of a plist.
;; Its keys must be keywords, its values may not be keywords. Empty keys are ignored.
;; If a key is declared multiple times, it's last declaration is returned.
;; Keywords suffixed with "*" pack all values until the next keyword in a list.
(defun yodel-plist*-to-plist (plist*)
  "Convert PLIST* to plist."
  (when plist*
    (let (plist variadic keyword last)
      (unless (keywordp (car plist*))
        (signal 'wrong-type-argument `(keywordp ,(car plist*))))
      (dolist (el plist* plist)
        (if (keywordp el)
            (setq variadic (string-suffix-p "*" (symbol-name el))
                  keyword el)
          (setq plist
                (plist-put
                 plist keyword
                 (if variadic
                     (append (plist-get plist keyword) (list el))
                   (unless (keywordp last)
                     (error "Non-variadic key \"%S\" passed more than one value" keyword))
                   el))))
        (setq last el)))))

(defun yodel--position-point (indicator)
  "Replace point INDICATOR with actual point."
  (goto-char (point-min))
  (if (re-search-forward indicator nil 'noerror)
      (replace-match "")
    (goto-char (point-min))))

;;@TODO: Needs to be more robust when we encounter a read error.
(defun yodel--report (buffer)
  "Read the report from BUFFER."
  (if (get-buffer buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (list
         :stdout (let ((stdout
                        (string-trim
                         (buffer-substring
                          (point-min)
                          (and (re-search-forward yodel--process-end-text)
                               (line-beginning-position))))))
                   (unless (string-empty-p stdout) stdout))
         :report (and (forward-line) (read (current-buffer)))
         :stderr (let ((stderr (string-trim
                                (buffer-substring (1+ (point)) (point-max)))))
                   (unless (string-empty-p stderr) stderr))))
    (error "Report process buffer no longer live")))

;;@HACK:
;; Because we can't rely on the report buffer having lexical binding enabled
;; we store buffer-local variables in the process buffer prior to calling the sentinel.
;; ~ NV 2021-09-16
(defvar-local yodel--interactive nil)
(defvar-local yodel--raw nil)
(defvar-local yodel--save nil)
(defvar-local yodel--formatter nil)
(defvar-local yodel--emacs.d nil)

(defun yodel-reformat (formatter)
  "Reformat report with FORMATTER function."
  (interactive (progn
                 (or yodel--report
                     (user-error "No report associated with current buffer"))
                 (list
                  (let* ((candidates
                          (mapcar
                           (lambda (fn) (cons
                                         (format "%s -> %s"
                                                 (replace-regexp-in-string
                                                  "yodel-format-as-" ""
                                                  (symbol-name fn))
                                                 (car (split-string (documentation fn) "\n")))
                                         fn))
                           (cl-remove-if (lambda (formatter)
                                           (eq formatter
                                               (or yodel--formatter yodel-default-formatter)))
                                         yodel-formatters)))
                         (selection
                          (completing-read "formatter: "
                                           (setq candidates
                                                 (cl-sort candidates #'string< :key #'car))
                                           nil 'require-match)))
                    (alist-get selection candidates nil nil #'equal)))))
  (funcall formatter yodel--report)
  (setq yodel--formatter formatter))

(defun yodel--sentinel (process _event)
  "Pass PROCESS report to formatter."
  (when (memq (process-status process) '(exit signal))
    (let ((buffer (process-buffer process)))
      (with-current-buffer buffer
        ;;Bound to prevent buffer-local var wipe if formatter changes major mode.
        (let ((save yodel--save)
              (emacs.d yodel--emacs.d))
          (unless yodel--interactive
            (unless yodel--raw
              (setq yodel--report (yodel--report buffer))
              ;;Necessary to preserve in case formatter changes major mode
              (put 'yodel--report 'permanent-local t)
              (funcall yodel--formatter yodel--report)))
          (display-buffer buffer '(display-buffer-reuse-window))
          (unless save
            (when (file-exists-p emacs.d) (delete-directory emacs.d 'recursive))))))))

;;;###autoload
(defmacro yodel-file (path &rest args)
  "Create file at PATH and manipulate it according to ARGS.
If PATH is nil, a temporary file is created via `make-temp-file'.
Otherwise it is expanded relative to `default-directory'.
ARGS must be a plist* with any of the following keys:

:point

A regexp representing the initial point position in file's buffer.
An explicitly nil value will prevent the point from being searched for.

:with*

Contents which is converted into a string and inserted into file's buffer.
The first :point indicator is replaced and point is positioned there.
If no :point indicator is found, point is positioned at `point-min'.

:then*

Any number of forms which will be executed within the buffer.
The file's initial contents have been written at this point.
The result of the last form is returned.

:save

If this is non-nil, the file is saved to PATH.
Otherwise it is deleted after `yodel-file' finishes running.

:overwrite

If this is non-nil, allow overwriting PATH.
Otherwise throw an error if PATH exists."
  (declare (indent defun))
  (let ((file     (make-symbol "file"))
        (return   (make-symbol "return"))
        (buffer   (make-symbol "buffer"))
        (a        (make-symbol "args"))
        (with*    (make-symbol "with*"))
        (point    (make-symbol "point"))
        (then*    (make-symbol "then*"))
        (p        (make-symbol "path")))
    `(let* ((,p     ,path)
            (,a     (yodel-plist*-to-plist (if (or (stringp ,p) (null ,p))
                                               ',args
                                             (prog1
                                                 (append (list ,p) ',args)
                                               (setq ,p nil)))))
            (,point (plist-get ,a :point))
            (,then* (plist-get ,a :then*))
            (,file  (expand-file-name (or ,p (make-temp-name "yodel-"))
                                      (if ,p default-directory
                                        (temporary-file-directory))))
            ,return)
       (unless (plist-get ,a :overwrite)
         (when (file-exists-p ,file)
           (user-error "Cannot overwrite existing file: %S" ,file)))
       ;;create the dir if necessary
       ;;@TODO: keep track of what we're creating so that we can properly clean up
       (let ((dir (file-name-directory ,file)))
         (unless (file-exists-p dir) (make-directory dir t)))
       (let ((,buffer (find-file-noselect ,file)))
         (with-current-buffer ,buffer
           (when-let ((,with* (plist-get ,a :with*)))
             (erase-buffer)
             (insert (mapconcat (lambda (el) (if (stringp el) el
                                               ;;@TODO: abstract into macro?
                                               ;;(yodel--without-print-limits ....)
                                               (let (print-level
                                                     print-length
                                                     print-circle)
                                                 (prin1-to-string el))))
                                ,with* "\n")))
           (when ,point (yodel--position-point ,point))
           (when ,then* (setq ,return (eval `(progn ,@,then*) t))))
         (with-current-buffer ,buffer ;;rebound in case :then* chagned it
           (set-buffer-modified-p nil)
           ;;Avoiding write-file because it will add a final newline
           (with-file-modes #o0666
             (write-region (point-min) (point-max) ,file))
           (kill-buffer ,buffer)
           (unless (plist-get ,a :save) (delete-file ,file)))
         ,return))))

;;;###autoload
(defmacro yodel (&rest declaration)
  "Test elisp in a clean environment.
DECLARATION may be any of the following keywords and their respective values:
  - :pre* (Form)...
      Forms evaluated before launching Emacs.

  - :post* (Form)...
      Forms evaluated in the testing environment after bootstrapping.

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `yodel-process-buffer'
      Otherwise, the subprocess will be interactive.

  - :save Boolean or String
      If a string, use that string as the :user-dir argument and save.
      If otherwise non-nil, the :user-dir is not deleted after exiting.
      Otherwise, it is immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `yodel-process-buffer'. Otherwise, it is
      passed to the :formatter function.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temporary-file-directory'.
  - :clargs*
      Command line args for the child process.

  - :packages*
      Packages which are installed in the test enviornment.
      Packages are installed via elpaca.el, which see for recipe format.

DECLARATION is accessible within the :post* phase via the `yodel-args' plist."
  (declare (indent 0))
  (let* ((declaration (yodel-plist*-to-plist
                       (append declaration
                               (list :yodel-form
                                     (yodel--pretty-print (append '(yodel) declaration))))))
         (pre* (plist-get declaration :pre*)))
    `(let ((yodel-args ',declaration))
       (cl-destructuring-bind
           ( &key clargs* formatter interactive pre* post* raw save user-dir
             ((:executable emacs) (concat invocation-directory invocation-name))
             &allow-other-keys
             &aux
             (clargs (append (unless interactive '("--batch")) (or clargs* yodel--default-args)))
             (formatter (or formatter yodel-default-formatter #'yodel-format-as-raw))
             (emacs.d (expand-file-name
                       (or user-dir (and (stringp save) save)
                           (make-temp-file "yodel-" 'directory))
                       temporary-file-directory))
             program)
           yodel-args
         (unless (file-exists-p emacs.d)
           (make-directory emacs.d 'parents))
         (let ((default-directory emacs.d))
           (progn
             ,@pre*
             ;; Bind program after :pre* in case yodel-args has been modified.
             (setq program
                   (let ((print-level  nil)
                         (print-length nil)
                         (print-circle nil))
                     ;; Ensure default values are present even if not specified.
                     (setq yodel-args (plist-put yodel-args :user-dir emacs.d)
                           yodel-args (plist-put yodel-args :executable emacs))
                     (pp-to-string
                      ;; The top-level `let' is an intentional local
                      ;; variable binding. We want users of
                      ;; `yodel' to have access to their
                      ;; args within :pre*/:post* programs. Since
                      ;; we are binding with the package namespace, this
                      ;; should not overwrite other user bindings.
                      `(with-demoted-errors "%S"
                         (require 'yodel)
                         (setq yodel-args           ',yodel-args
                               user-emacs-directory ,emacs.d
                               default-directory    ,emacs.d
                               server-name          ,emacs.d
                               package-user-dir     (expand-file-name "elpa" ,emacs.d))
                         (unwind-protect (progn
                                           ;;@TOOD: simplify by moving to yodel-args?
                                           ,@',(when-let ((packages (plist-get declaration :packages*)))
                                                 `((require 'yodel-elpaca)
                                                   (apply #'yodel-elpaca--install ',packages)))
                                           ,@post*)
                           (plist-put yodel-args :yodel-time
                                      (string-to-number (format-time-string "%s")))
                           (message "%s" ,yodel--process-end-text)
                           (when (and (not ,interactive) (plist-get yodel-args :packages*))
                             (setq yodel-args (plist-put yodel-args :packages
                                                         (yodel-elpaca--package-info))))
                           (message "%S" yodel-args))))))))
         ;; Reset process buffer.
         (with-current-buffer (get-buffer-create yodel-process-buffer)
           (fundamental-mode) ; We want this to wipe out buffer local vars here
           (erase-buffer)
           (setq-local yodel--interactive interactive
                       yodel--raw raw
                       yodel--save save
                       yodel--formatter formatter
                       yodel--emacs.d emacs.d))
         (make-process
          :name    yodel-process-buffer
          :buffer  yodel-process-buffer
          :command `(,emacs ,@clargs ,program)
          :sentinel #'yodel--sentinel)
         (message "Running yodel in directory: %s" emacs.d)))))

;;@TODO: Ensure unique user-dir for each test?
;;;###autoload
(defmacro yodel-parallel (&rest args)
  "Test in parallel, reporting differences in output.
ARGS should be of the following form: (TEST... COMMON...)

Each TEST is a list of keywords and their values, which are passed to `yodel'.
COMMON are remaining keyword val pairs which are appened to each TEST.
e.g.

\\=(yodel-parallel
  (:user-dir \"A\" :packages* (example :branch \"develop\"))
  (:user-dir \"B\" :packages* (example :branch \"bugfix-example\"))
  :post*
  (example program))

Will run test A and B with the same :post* program."
  (let ((tests (let (forms)
                 (while (and (car args) (not (keywordp (car args))))
                   (push (append '(yodel) (pop args)) forms))
                 (nreverse forms))))
    `(progn ,@(mapcar (lambda (test)
                        `(let ((yodel-process-buffer
                                ,(let ((plist (yodel-plist*-to-plist (cdr test))))
                                   (or (plist-get plist :user-dir)
                                       (when-let ((save (plist-get plist :save)))
                                         (and (stringp save) save))))))
                           ,(append test args)))
                      tests))))

(provide 'yodel)
;;; LocalWords:  subprocess MERCHANTABILITY Vollmer elisp Elisp elpa emacs variadic baz eval plist ARGS args dir src formatter pre namespace metaprogram reddit
;;; yodel.el ends here
