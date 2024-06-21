;;; guard-lf.el --- Guard large files  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  JenChieh

;; Author: JenChieh <jcs090218@gmail.com>
;; Maintainer: JenChieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/guard-lf
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
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

(defconst guard-lf-intact-major-modes
  '( fundamental-mode         ; Already in `fundamental-mode'? Ignore it!
     special-mode)
  "Do nothing for these major modes.")

;;
;;; Entry

(defun guard-lf--enable ()
  "Enable `guard-lf-mode'."
  (advice-add 'find-file-noselect :around #'guard-lf--find-file)
  (advice-add 'set-auto-mode-0 :around #'guard-lf--set-auto-mode-0))

(defun guard-lf--disable ()
  "Disable `guard-lf-mode'."
  (advice-remove 'find-file-noselect #'guard-lf--find-file)
  (advice-remove 'set-auto-mode-0 #'guard-lf--set-auto-mode-0))

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
  (when-let (((file-readable-p filename))
             (attributes (file-attributes filename)))
    (file-attribute-size attributes)))

(defun guard-lf--file-too-large-p (filename)
  "Return non-nil if FILENAME's size is too large."
  (when-let ((size (guard-lf--file-size filename)))
    (and large-file-warning-threshold
         (> size large-file-warning-threshold))))

;;
;;; So long

(defun guard-lf--line-too-long-p (filename)
  "Return non-nil if FILENAME's line is too long."
  (when (file-readable-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (funcall so-long-predicate))))

;;
;;; API

;;;###autoload
(defun guard-lf-p (filename)
  "Return non-nil if the large FILENAME is detected."
  (or (guard-lf--file-too-large-p filename)
      (guard-lf--line-too-long-p filename)))

;;
;;; Core

(defvar guard-lf--detect-large-file nil
  "Set to t if larget file.")

(defun guard-lf--find-file (fnc &rest args)
  "Advice around the function `find-file-noselect'.

Arguments FNC and ARGS are used to call original operations."
  (let* ((filename (car args))
         (guard-lf--detect-large-file (guard-lf-p filename)))
    (apply fnc args)))

(defun guard-lf--set-auto-mode-0 (fnc &rest args)
  "Advice around the function `set-auto-mode-0'.

Arguments FNC and ARGS are used to call original operations."
  (when guard-lf--detect-large-file
    (setq guard-lf--detect-large-file nil)  ; Revert back to `nil'
    (when (and guard-lf-major-mode
               (not (apply #'provided-mode-derived-p (cons (car args) guard-lf-intact-major-modes))))
      (message "[INFO] Large file detected; use the `%s' as the new major mode"
               guard-lf-major-mode)
      (setcar args guard-lf-major-mode)))
  (apply fnc args))

(provide 'guard-lf)
;;; guard-lf.el ends here
