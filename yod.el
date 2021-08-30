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

;; A vlist (variadic plist) is a special type of plist.
;; Its keys must be keywords, its values may not be keywords.
;; If a keyword ends with "*" all values until the next keyword
;; are associated with it in a list.
(defun yod-vlist-to-plist (vlist)
  "Convert VLIST to plist."
  (let (plist variadic keyword)
    (dolist (el vlist plist)
      (if (keywordp el)
          (setq variadic (string-suffix-p "*" (symbol-name el))
                keyword el)
        (setq plist (plist-put plist keyword
                               (if variadic
                                   (append (plist-get plist keyword) (list el))
                                 el)))))))

(defun yod--position-point (indicator)
  "Replace point INDICATOR with actual point."
  (goto-char (point-min))
  (re-search-forward indicator nil 'noerror)
  (replace-match ""))

(defmacro yod-file (&rest args)
  "ARGS."
  (setq args (yod-vlist-to-plist args))
  `(with-temp-buffer
     (insert ,(plist-get args :contents))
     (yod--position-point ,(or (plist-get args :point) "|"))
     (progn ,@(plist-get args :then*))))

(provide 'yod)
;;; yod.el ends here
