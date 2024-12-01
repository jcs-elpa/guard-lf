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

(defgroup guard-lf nil
  "Guard large files."
  :prefix "guard-lf-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/guard-lf"))

(defcustom guard-lf-major-mode #'guard-lf-large-file-mode
  "Major mode to use when viewing large file."
  :type 'function
  :group 'guard-lf)

(defcustom guard-lf-intact-major-modes
  '(fundamental-mode ; Already in `fundamental-mode'? Ignore it!
    special-mode
    archive-mode
    tar-mode
    image-mode
    vlf-mode
    pcap-mode)
  "Do nothing for these major modes."
  :type '(repeat symbol)
  :group 'guard-lf)

;;
;;; Entry

;;;###autoload
(define-minor-mode guard-lf-mode
  "Minor mode `guard-lf-mode'."
  :lighter " Guard-LF"
  :global t
  :group 'guard-lf
  (if guard-lf-mode
      (advice-add 'set-auto-mode-0 :around #'guard-lf--set-auto-mode-0)
    (advice-remove 'set-auto-mode-0 #'guard-lf--set-auto-mode-0)))

;; This major mode is basically the same as `fundamental-mode'. It helps users
;; to distinguish if buffer mode has been set by `guard-lf' or not
(define-derived-mode guard-lf-large-file-mode fundamental-mode "guard-lf")

;;
;;; Large File

;; NOTE: This section is copied around variable `large-file-warning-threshold'.

(defun guard-lf--buffer-too-large-p (buffer)
  "Return non-nil if BUFFER's size is too large."
  (and large-file-warning-threshold
       (> (buffer-size buffer) large-file-warning-threshold)))

;;
;;; So long

(defun guard-lf--line-too-long-p (buffer)
  "Return non-nil if BUFFER's line is too long."
  (require 'so-long)
  (save-excursion
    (with-current-buffer buffer
      (funcall so-long-predicate))))

;;
;;; API

;;;###autoload
(defun guard-lf-p (&optional buffer)
  "Return non-nil if the BUFFER is large."
  (when-let ((buffer (or buffer (current-buffer))))
    (or (guard-lf--buffer-too-large-p buffer)
        (guard-lf--line-too-long-p buffer))))

;;
;;; Core

(defun guard-lf--set-auto-mode-0 (fnc &rest args)
  "Advice around the function `set-auto-mode-0'.

Arguments FNC and ARGS are used to call original operations."
  (let ((mode (car args)))
    (when (and guard-lf-major-mode
               mode ; nil can be passed for some special modes like `tar-mode'
               (not (apply #'provided-mode-derived-p
                           (append (list mode guard-lf-major-mode)
                                   guard-lf-intact-major-modes)))
               (guard-lf-p))
      (message "[guard-lf] Large file detected; using `%s' as major mode instead of `%s'"
               guard-lf-major-mode mode)
      (setcar args guard-lf-major-mode))
    (apply fnc args)))


(provide 'guard-lf)
;;; guard-lf.el ends here
