;;; yod.el --- communicable elisp                    -*- lexical-binding: t; -*-

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

;; A variadic plist is a strict subset of a plist.
;; Its keys must be keywords, its values may not be keywords.
;; Empty keys are ignored.
;; If a key is declared multiple times, it's last declaration is returned.
;; If a keyword ends with "*" all values until the next keyword
;; are associated with it in a list.
(defun yod-plist*-to-plist (plist*)
  "Convert PLIST* to plist."
  (let (plist variadic keyword last)
    (unless (keywordp (car plist*))
      (signal 'wrong-type-argument `(keywordp ,(car plist*))))
    (dolist (el plist* plist)
      (if (keywordp el)
          (setq variadic (string-suffix-p "*" (symbol-name el))
                keyword el)
        (setq plist (plist-put plist keyword
                               (if variadic
                                   (append (plist-get plist keyword) (list el))
                                 (unless (keywordp last)
                                   (error "Non-variadic key \"%S\" associated with more than one value" keyword))
                                 el))))
      (setq last el))))

(defun yod--position-point (indicator)
  "Replace point INDICATOR with actual point."
  (goto-char (point-min))
  (re-search-forward indicator nil 'noerror)
  (replace-match ""))

(defmacro yod-file (&rest args)
  "ARGS."
  (setq args (yod-plist*-to-plist args))
  `(with-temp-buffer
     (insert ,(plist-get args :contents))
     (yod--position-point ,(or (plist-get args :point) "|"))
     (progn ,@(plist-get args :then*))))

(provide 'yod)
;;; yod.el ends here
