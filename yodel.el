;;; yodel.el --- Communicable Elisp                    -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Nicholas Vollmer

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
;; %-----cut---start-----%
;; foo bar baz
;; %-----cut---end-------%
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
(eval-when-compile (require 'subr-x))

(defgroup yodel nil
  "Communicable Elisp."
  :group 'yodel
  :prefix "yodel-")

(defconst yodel--process-end-text "YODEL--PROCESS-END"
  "String denoting end of process output and start of report form.")

(defvar yodel--default-args `("-Q" "-L" ,(file-name-directory (locate-library "yodel")) "--eval")
  "Arguments passed to the Emacs executable when testing.")

(defvar yodel--process-buffer "*yodel*"
  "Name of the yodel subprocess buffer.")

(defvar yodel-formatters nil
  "List of yodel report formatting functions.")

(defvar-local yodel--report nil
  "Report data structure.
Used for reformatting the report.")

(defun yodel--pretty-print (form)
  "Convert elisp FORM into formatted string."
  (let* ((print-level nil)
         (print-length nil)
         (print-quoted t)
         (string (mapconcat
                  (lambda (el)
                    (concat (when (and el (listp el)) "\n")
                            (pp-to-string el)
                            (unless (keywordp el) "\n")))
                  form " ")))
    (with-temp-buffer
      (insert "(" string ")")
      (goto-char (point-min))
      ;; Replace dangling parenthesis.
      (save-excursion
        (while (re-search-forward "\\(?:\n[[:space:]]*)\\)" nil 'no-error)
          (replace-match ")")))
      (save-excursion
        (while (re-search-forward "\\(?:(let\\)" nil 'no-error)
          (forward-line)
          (join-line)))
      ;; Remove empty lines.
      (flush-lines "\\(?:^[[:space:]]*$\\)")
      (emacs-lisp-mode)
      (indent-region (point-min) (point-max))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defmacro yodel-formatter (name description &rest body)
  "Create a yodel formatting function with BODY and NAME.
Add the function to `yodel-formatters'.
Each function should accept a report plist as its sole argument.
DESCRIPTION is used as the docstring, and when prompting via `yodel-reformat'.
BODY will be executed in the context of an empty `yodel--process-buffer'.
`buffer-string' is returned after BODY is executed.
The following anaphoric bindings are available during BODY:

- stdout: The standard output of the subprocess.
- stderr: The errors output by the subprocess.
- report: The report form."
  (declare (indent defun))
  (let ((fn (intern (format "yodel--formatter-%s" name))))
    `(cl-pushnew
      (defun ,fn (report)
        ,(replace-regexp-in-string "report" #'upcase description)
        (cl-destructuring-bind (&key stdout stderr report) report
          (ignore report stdout stderr) ;no-op here to satisfy byte compiler.
          (with-current-buffer yodel--process-buffer
            (erase-buffer)
            (goto-char (point-min))
            ,@body
            (buffer-string))))
      yodel-formatters)))

(yodel-formatter raw
  "Format report as a raw, readable plist."
  (insert (let (print-level print-length)
            (pp-to-string report))))

(yodel-formatter org
  "Format REPORT in Org syntax."
  (when (fboundp 'org-mode) (org-mode))
  (insert
   (string-join
    `(,(format "* YODEL REPORT [%s]" (format-time-string "%Y-%m-%d %H:%M"))
      ,(concat
        "#+begin_src emacs-lisp :lexical t\n"
        (yodel--pretty-print (append '(yodel) (plist-get report :yodel-form)))
        "\n#+end_src")
      ,@(when stdout
          (list "** STDOUT:"
                (concat "#+begin_src emacs-lisp :lexical t\n"
                        (string-trim stdout)
                        "\n#+end_src")))
      ,@(when stderr
          (list "** STDERR:"
                (concat "#+begin_src emacs-lisp :lexical t\n"
                        stderr
                        "\n#+end_src")))
      "** Environment"
      ,(mapconcat (lambda (el) (format "- %s: %s" (car el) (cdr el)))
                  (list (cons "emacs version" (emacs-version))
                        (cons "system type" system-type))
                  "\n"))
    "\n\n")))

(yodel-formatter reddit-markdown
  "Format REPORT in reddit flavored markdown."
  (when (fboundp 'markdown-mode) (markdown-mode))
  (insert
   (string-join
    `(,(format "# [YODEL](https://github.com/progfolio/yodel) REPORT (%s):" (format-time-string "%Y-%m-%d %H:%M:%S"))
      ,(concat
        "\n"
        ;;use four spaces because old reddit doesn't render code fences
        (mapconcat (lambda (s) (format "    %s" s))
                   (split-string
                    (yodel--pretty-print
                     (append '(yodel) (plist-get report :yodel-form)))
                    "\n")
                   "\n")
        "\n")
      ,@(when stdout
          (list "## STDOUT:"
                (concat "\n    "
                        (string-trim stdout)
                        "\n")))
      ,@(when stderr
          (list "## STDERR:"
                (concat "\n    "
                        stderr
                        "\n")))
      "## Environment"
      ,(mapconcat (lambda (el) (format "- %s: %s" (car el) (cdr el)))
                  (list (cons "emacs version" (emacs-version))
                        (cons "system type" system-type))
                  "\n"))
    "\n\n")))

(declare-function yodel--formatter-org "yodel")
(declare-function yodel--formatter-raw "yodel")
(declare-function yodel--formatter-reddit-markdown "yodel")
(defcustom yodel-default-formatter #'yodel--formatter-org
  "Default report formatting function."
  :type 'function)

;; A variadic plist is a strict subset of a plist.
;; Its keys must be keywords, its values may not be keywords.
;; Empty keys are ignored.
;; If a key is declared multiple times, it's last declaration is returned.
;; If a keyword ends with "*" all values until the next keyword
;; are associated with it in a list.
(defun yodel-plist*-to-plist (plist*)
  "Convert PLIST* to plist."
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
      (setq last el))))

(defun yodel--position-point (indicator)
  "Replace point INDICATOR with actual point."
  (goto-char (point-min))
  (if (re-search-forward indicator nil 'noerror)
      (replace-match "")
    (goto-char (point-min))))

(defun yodel--report ()
  "Read the report from `yodel--process-buffer'."
  (if (get-buffer yodel--process-buffer)
      (with-current-buffer yodel--process-buffer
        (goto-char (point-min))
        (list
         :stdout (let ((stdout (buffer-substring
                                (point-min)
                                (and (re-search-forward yodel--process-end-text)
                                     (line-beginning-position)))))
                   (unless (string-empty-p  stdout) stdout))
         :report (and (forward-line) (read (current-buffer)))
         :stderr (let ((stderr (buffer-substring (1+ (point)) (1- (point-max)))))
                   (unless (string-empty-p (string-trim stderr)) stderr))))
    (error "Yodel process buffer no longer live")))

;;;###autoload
(defmacro yodel-file (path &rest args)
  "Create file at PATH and manipulate it according to ARGS.
If PATH is nil, a temporary file is created via `make-temp-file'.
ARGS must be a plist* with any of the following keys:

:point

A regexp representing the initial point position in file's buffer.
It defaults to \"|\".
An explicitly nil value will prevent the point from being searched for.

:contents

A string which is inserted into the file's buffer.
The first :point indicator is replaced and point is positioned there.
If no :point indicator is found, point is positioned at `point-min'.

:then*

Any number of forms which will be executed within the buffer.
The file's initial :contents has been written at this point.
The result of the last form is returned.

:save

If this is non-nil, the file is saved to PATH.
Otherwise it is deleted after `yodel-file' finishes running.

:overwrite

If this is non-nil, allow overwriting PATH.
Otherwise throw an error if PATH exists."
  (declare (indent defun))
  (let* ((pathp  (or (stringp path) (null path)))
         (args   (yodel-plist*-to-plist (if pathp args `(,path ,@args))))
         (point  (plist-get args :point))
         (then*  (plist-get args :then*))
         (file   (make-symbol "file"))
         (return (make-symbol "return"))
         (buffer (make-symbol "buffer")))
    (unless pathp (setq path nil))
    `(let ((,file (expand-file-name
                   ,(or path '(make-temp-name "yodel-"))
                   ,@(unless path '((temporary-file-directory)))))
           ,return)
       ,@(unless (plist-get args :overwrite)
           `((when (file-exists-p ,file)
               (user-error "Cannot overwrite existing file: %S" ,file))))
       (let ((,buffer (find-file-noselect ,file)))
         (with-current-buffer ,buffer
           ,@(when-let ((contents (plist-get args :contents)))
               `((insert ,contents)))
           ,@(unless (and point (null point))
               `((yodel--position-point ,(or point "|"))))
           ;;Avoiding write-file because it will add a final newline
           (write-region (point-min) (point-max) ,file)
           ,@(when then* `((setq ,return (progn ,@then*))))
           ,@(unless (plist-get args :save)
               `((when (buffer-name ,buffer)
                   (with-current-buffer ,buffer
                     (set-buffer-modified-p nil)
                     (kill-buffer ,buffer)))
                 (delete-file ,file)))))
       ,return)))

;;;###autoload
(defmacro yodel (&rest args)
  "Test elisp in a clean environment.
ARGS may be any of the following keywords and their respective values:
  - :pre* (Form)...
      Forms evaluated before launching Emacs.

  - :post* (Form)...
      Forms evaluated in the testing environment after bootstrapping.

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `yodel--process-buffer'
      Otherwise, the subprocess will be interactive.

  - :save Boolean
      If non-nil, the :user-dir is not deleted after exiting.
      Otherwise, it is immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `yodel--process-buffer'. Otherwise, it is
      passed to the :formatter function.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temporary-file-directory'.

ARGS are accessible within the :pre*/:post* phases via the
locally bound plist, yodel-args."
  (declare (indent defun))
  (unless lexical-binding
    (user-error "Lexical binding required for yodel"))
  (let* ((preserve-files    (make-symbol "preserve-files"))
         (emacs-dir         (make-symbol "emacs-dir"))
         (interactive       (make-symbol "interactive"))
         (emacs-args-symbol (make-symbol "emacs-args"))
         (emacs-executable  (make-symbol "emacs-executable"))
         (raw               (make-symbol "raw"))
         (formatter         (make-symbol "formatter"))
         (program           (make-symbol "program"))
         (yodel-form        args)
         (args              (yodel-plist*-to-plist args))
         (user-dir          (if-let ((dir (plist-get args :user-dir)))
                                (expand-file-name dir temporary-file-directory)
                              (make-temp-file "yodel-" 'directory)))
         (executable        (or (plist-get args :executable)
                                (concat invocation-directory invocation-name)))
         (declared-formatter (plist-get args :formatter))
         ;; Construct metaprogram to be evaluated by subprocess
         (metaprogram       (let ((print-level  nil)
                                  (print-length nil))
                              ;;@IDEA: modify args to ensure we've included default values?
                              ;;or store these in their own :yodel sub-plist?
                              (setq args (plist-put args :yodel-form (yodel--pretty-print yodel-form))
                                    args (plist-put args :user-dir user-dir)
                                    args (plist-put args :executable executable))
                              (pp-to-string
                               ;; The top-level `let' is an intentional local
                               ;; variable binding. We want users of
                               ;; `yodel' to have access to their
                               ;; args within :pre*/:post* programs. Since
                               ;; we are binding with the package namespace, this
                               ;; should not overwrite other user bindings.
                               `(with-demoted-errors "%S"
                                  (require 'yodel)
                                  ;;@TODO: Rename this to be consistent during runtime
                                  ;; and accessing during the report.
                                  ;; we need to clean up the terminology in general...
                                  (let ((yodel-args ',args))
                                    (setq user-emacs-directory ,user-dir
                                          default-directory    ,user-dir
                                          server-name          ,user-dir
                                          package-user-dir     (expand-file-name "elpa" ,user-dir))
                                    ;;@TODO: this needs to be earlier
                                    ;; Do we need to create the user dir prior to running this?:
                                    ;;(plist-get keywords :pre*)
                                    (unwind-protect
                                        (progn ,@(plist-get args :post*))
                                      (goto-char (point-max))
                                      (message "%s" ,yodel--process-end-text)
                                      (message "%s" yodel-args))))))))
    `(let* ((,preserve-files    ,(plist-get args :save))
            (,interactive       ,(plist-get args :interactive))
            (,emacs-executable  ,executable)
            (,emacs-args-symbol (append (unless ,interactive '("--batch")) ',yodel--default-args))
            (,raw               ,(plist-get args :raw))
            (,formatter         ,(or declared-formatter
                                     '(or yodel-default-formatter #'yodel--formatter-raw)))
            (,program           ,metaprogram)
            (,emacs-dir         ,user-dir))
       ;; Reset process buffer.
       (with-current-buffer (get-buffer-create yodel--process-buffer)
         (fundamental-mode)
         (erase-buffer))
       (make-process
        :name    yodel--process-buffer
        :buffer  yodel--process-buffer
        :command `(,,emacs-executable ,@,emacs-args-symbol ,,program)
        :sentinel
        (lambda (process _event)
          (unless ,interactive
            (when (memq (process-status process) '(exit signal))
              (unless ,raw
                (with-current-buffer yodel--process-buffer
                  (setq yodel--report (yodel--report))
                  ;;Necessary to preserve if major mode changes
                  (put 'yodel--report 'permanent-local t)
                  (funcall ,formatter yodel--report)))
              (run-with-idle-timer 1 nil (lambda () (display-buffer yodel--process-buffer)))
              (unless ,preserve-files
                (when (file-exists-p ,emacs-dir)
                  (delete-directory ,emacs-dir 'recursive)))))))
       (message "Running yodel in directory: %s" ,emacs-dir))))

(defun yodel-reformat (formatter)
  "Reformat report with FORMATTER function."
  (interactive (list
                (let* ((candidates
                        (mapcar
                         (lambda (fn) (cons
                                       (format "%s -> %s"
                                               (replace-regexp-in-string
                                                "yodel--formatter-" ""
                                                (symbol-name fn))
                                               (documentation fn))
                                       fn))
                         yodel-formatters))
                       (selection (completing-read "formatter: "
                                                   (cl-sort candidates #'string<)
                                                   nil 'require-match)))
                  (alist-get selection candidates nil nil #'equal))))
  (funcall formatter (if (get-buffer yodel--process-buffer)
                         (with-current-buffer yodel--process-buffer
                           yodel--report)
                       (user-error "%S buffer not live" yodel--process-buffer))))

(provide 'yodel)
;;; LocalWords:  subprocess MERCHANTABILITY Vollmer elisp Elisp elpa emacs variadic baz eval plist ARGS args dir src formatter pre namespace metaprogram reddit
;;; yodel.el ends here
