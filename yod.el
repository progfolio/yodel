;;; yod.el --- Communicable Elisp                    -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Nicholas Vollmer

;; Author:  Nicholas Vollmer
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
(require 'cl-lib)

(defgroup yodel nil
  "Communicable Elisp."
  :group 'yodel
  :prefix "yodel-")

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
The file has been written at this point.
The result of the last form is returned.

:save

If this is non-nil, the file is saved to PATH.
Otherwise it is deleted.

:overwrite

If this is non-nil, allow overwriting PATH.
Otherwise throw an error if PATH exists."
  (declare (indent 1))
  (let* ((pathp (or (stringp path) (null path)))
         (args (yodel-plist*-to-plist (if pathp args `(,path ,@args))))
         (point (plist-get args :point))
         (then* (plist-get args :then*))
         (file (make-symbol "file"))
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
           (write-file ,file)
           ,@(when then* `((setq ,return (progn ,@then*))))
           ,@(unless (plist-get args :save)
               `((when (buffer-name ,buffer)
                   (with-current-buffer ,buffer
                     (set-buffer-modified-p nil)
                     (kill-buffer ,buffer)))
                 (delete-file ,file)))))
       ,return)))

(provide 'yodel)
;;; yod.el ends here
