;;; notmuch-bookmarks.el --- add bookmark handling for notmuch buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: mail

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

;; This package adds a global minor mode which allows you to bookmark
;; notmuch buffers using the standard emacs bookmark functionality. A
;; `notmuch buffer' denotes either a notmuch tree view, a notmuch
;; search view or a notmuch show buffer (message view). With this
;; minor mode active, you can add these buffers to the bookmark list
;; and visit them using `bookmark-jump'.
;;
;; To activate the minor mode, add something like the following to
;; your init file:
;;
;; (use-package notmuch-bookmarks
;;   :after notmuch
;;   :config
;;   (notmuch-bookmarks))
;;


;;; Code:

(require 'notmuch)

(require 'cl-lib)    ;; cl-defun; cl-defgeneric; cl-defmethod; cl-assert; cl-case
(require 'seq)       ;; seq-doseq; seq-filter
(require 'uniquify)

;;; Custom Variables:

(defcustom notmuch-bookmark-prefix "notmuch: "
  "Prefix to add to new notmuch bookmarks, or nil.")


;;; Jumping to a Bookmark:

(defun notmuch-bookmarks-assert-major-mode (a-major-mode)
  "Throw an error if A-MAJOR-MODE is not supported by the package `notmuch-bookmarks'."
  (unless (seq-contains '(notmuch-show-mode notmuch-tree-mode notmuch-search-mode) a-major-mode)
    (user-error "Notmuch bookmarks does not support major mode '%s' " a-major-mode)))

(defun notmuch-bookmarks-create (query major-mode)
  "Create a notmuch buffer of type MAJOR-MODE for query."
  (cl-case major-mode
    (notmuch-tree-mode   (notmuch-tree query))
    (notmuch-show-mode   (notmuch-show query))
    (notmuch-search-mode (notmuch-search query))))

(cl-defun notmuch-bookmarks-jump-handler (bookmark)
  "Standard handler for opening notmuch bookmarks."
  (let* ((.filename    (bookmark-prop-get bookmark 'filename))
	 (.major-mode  (bookmark-prop-get bookmark 'major-mode))
	 (.buffer-name (bookmark-prop-get bookmark 'buffer-name)))
    ;; do some sanity checks:
    (notmuch-bookmarks-assert-major-mode .major-mode)
    (cl-assert (not (null .filename)) nil "Empty query string in bookmark record")
    (cl-assert (stringp .filename)    nil "Bad definition of bookmark query")
    ;; either open existing buffer or create fresh one:
    (if (not (get-buffer .buffer-name))
	(notmuch-bookmarks-create .filename .major-mode)
      (switch-to-buffer .buffer-name)
      (message "This buffer might not be up to date; you may want to refresh it"))))

;;; Creating a Bookmark:

(cl-defun notmuch-bookmarks-make-record (&key (handler 'notmuch-bookmarks-jump-handler)
					      (name    nil)
					      filename position annotation)
  "Turn argument list into a bookmark record list."
  `(,(concat notmuch-bookmark-prefix name)
    ((handler  . ,handler)
     (filename . ,filename)
     (major-mode . ,major-mode)
     (buffer-name . ,(or (uniquify-buffer-base-name)
			 (buffer-name)))
     (annotation . ,annotation)
     (position . ,position))))

(defun notmuch-bookmarks-record-p (bookmark)
  "Test whether BOOKMARK points to a notmuch query buffer."
  (eq 'notmuch-bookmarks-jump-handler (bookmark-prop-get bookmark 'handler)))

(cl-defgeneric notmuch-bookmarks-record ()
  "Return a bookmark record for the current notmuch buffer."
  (error "No bookmark handling defined for this major mode."))

(cl-defgeneric notmuch-bookmarks-record (&context (major-mode notmuch-tree-mode))
  "Return a bookmark record for the current notmuch tree buffer."
  (notmuch-bookmarks-make-record :filename (notmuch-tree-get-query)))
  
(cl-defgeneric notmuch-bookmarks-record (&context (major-mode notmuch-show-mode))
    "Return a bookmark record for the current notmuch show buffer."
  (notmuch-bookmarks-make-record :filename (notmuch-show-get-query)))
  
(cl-defgeneric notmuch-bookmarks-record (&context (major-mode notmuch-search-mode))
    "Return a bookmark record for the current notmuch search buffer."
  (notmuch-bookmarks-make-record :filename (notmuch-search-get-query)))

;; Install or uninstall the bookmark functionality:

(defun notmuch-bookmarks-set-record-fn ()
  "Set up notmuch bookmark handling for the current buffer.
Function to be added to a major mode hook."
  (notmuch-bookmarks-assert-major-mode major-mode)
  (setq-local bookmark-make-record-function 'notmuch-bookmarks-record))

(defun notmuch-bookmarks-install (&optional uninstall)
  "Add or optionally remove notmuch bookmark handlers."
  (let* ((hook-fn (if uninstall 'remove-hook 'add-hook)))
    (seq-doseq (hook-name '(notmuch-show-mode-hook
			    notmuch-search-mode-hook
			    notmuch-tree-mode-hook))
      (funcall hook-fn hook-name 'notmuch-bookmarks-set-record-fn))))

(define-minor-mode notmuch-bookmarks
  "Add notmuch specific bookmarks to the bookmarking system."
  :global t
  (notmuch-bookmarks-install (not notmuch-bookmarks)))

;; Convenience:

(defun notmuch-bookmarks-alist ()
  "Return a copy of `bookmark-alist' with notmuch bookmarks only."
  (seq-filter #'notmuch-bookmarks-record-p bookmark-alist))

;;;###autoload
(defun notmuch-bookmarks-counsel ()
  "Call `counsel-bookmarks' with a reduced bookmark set."
  (interactive)
  (if (not (require 'counsel nil t))
      (user-error "This function requires the package `counsel' to be installed")
    (let ((bookmark-alist (notmuch-bookmarks-alist)))
      (counsel-bookmark)))

(provide 'notmuch-bookmarks)
;;; notmuch-bookmarks.el ends here
