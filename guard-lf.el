;;; guard-lf.el --- Guard large files  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  JenChieh

;; Author: JenChieh <jcs090218@gmail.com>
;; Maintainer: JenChieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/guard-lf
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: help

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Guard large files.
;;

;;; Code:

(require 'so-long)

(defgroup guard-lf nil
  "Guard large files."
  :prefix "guard-lf-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/guard-lf"))

(defcustom guard-lf-major-mode #'fundamental-mode
  "Major mode to use when viewing large file."
  :type 'function
  :group 'guard-lf)

;;
;;; Entry

(defun guard-lf--enable ()
  "Enable `guard-lf-mode'."
  (advice-add 'find-file-noselect :around #'guard-lf--find-file)
  (advice-add 'set-auto-mode-1 :around #'guard-lf--set-auto-mode-1))

(defun guard-lf--disable ()
  "Disable `guard-lf-mode'."
  (advice-remove 'find-file-noselect #'guard-lf--find-file)
  (advice-remove 'set-auto-mode-1 #'guard-lf--set-auto-mode-1))

;;;###autoload
(define-minor-mode guard-lf-mode
  "Minor mode `guard-lf-mode'."
  :lighter " Guard-LF"
  :global t
  :group 'guard-lf
  (if guard-lf-mode (guard-lf--enable) (guard-lf--disable)))

;;
;;; Large File

;; NOTE: This section is copied around variable `large-file-warning-threshold'.

(defun guard-lf--file-size (filename)
  "Return the FILENAME size."
  (let ((attributes (file-attributes filename)))
    (file-attribute-size attributes)))

(defun guard-lf--file-too-large-p (filename)
  "Return non-nil if FILENAME's size is too large."
  (> (guard-lf--file-size filename) large-file-warning-threshold))

;;
;;; So long

(defun guard-lf--line-too-long-p (filename)
  "Return non-nil if FILENAME's line is too long."
  (with-temp-buffer
    (insert-file-contents filename)
    (funcall so-long-predicate)))

;;
;;; API

;;;###autoload
(defun guard-lf-p ()
  "Return non-nil if large file is detected."
  (or (guard-lf--file-too-large-p filename)
      (guard-lf--line-too-long-p filename)))

;;
;;; Core

(defvar guard-lf--detect-large-file nil
  "Set to t if larget file.")

(defun guard-lf--find-file (fnc &rest args)
  "Advice around `find-file-noselect'.

Arguments FNC and ARGS are used to call original operations."
  (let* ((filename (car args))
         (guard-lf--detect-large-file (guard-lf-p)))
    (apply fnc args)))

(defun guard-lf--set-auto-mode-1 (fnc &rest args)
  "Advice around function `set-auto-mode-1'.

Arguments FNC and ARGS are used to call original operations."
  (when guard-lf--detect-large-file
    (setq guard-lf--detect-large-file nil)  ; Revert back to `nil'
    (when guard-lf-major-mode
      (setf (nth 0 args) guard-lf-major-mode)))
  (apply fnc args))

(provide 'guard-lf)
;;; guard-lf.el ends here
