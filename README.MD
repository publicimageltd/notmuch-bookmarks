# notmuch-bookmarks
Bookmark handling for notmuch buffers.

This package adds a global minor mode which allows you to bookmark
 notmuch buffers via the standard emacs bookmark functionality. A
"notmuch buffer" denotes either a notmuch tree view, a notmuch search
 view or a notmuch show buffer (message view). With this minor mode
 active, you can add these buffers to the standard bookmark list and
 visit them, e.g. by using `bookmark-jump`.

 To activate the minor mode, add something like the following to
 your init file:

```
(use-package notmuch-bookmarks
   :after notmuch
   :config
   (notmuch-bookmarks-mode))
```

 This package is NOT part of the official notmuch emacs suite.

# Features

 - Specific jump handlers for notmuch buffers.
 - Interactive functions to change the bookmark.
 - Filter bookmark menu to display only notmuch bookmarks.

# Customizable Variables

## notmuch-bookmarks-prefix 

String which prefixes newly created bookmarks. If nil, do not prefix
anything. Defaults to `notmuch:`.

# Interactive Functions

## Generic bookmark handling

In order to use this package, you can use the ordinary bookmark
functionality shipped with vanilla emacs. That is, when in a notmuch
buffer, you might simply set a bookmark the usual way (via
`bookmark-set`). 

## Using the bookmark menu

The notmuch bookmarks are also included in the display of
`bookmark-bmenu-list`. The notmuch bookmarks mode implements a filter
function to display only notmuch bookmarks: In `bookmark-bmenu-mode',
just press 'N' to narrow to notmuch bookmarks.

## notmuch-bookmarks-edit-query

Edit the query with the bookmark the current buffer is visiting.

## notmuch-bookmarks-edit-name

Edit the name of the bookmark the current buffer is visiting.

## notmuch-bookmarks-set-search-type

Set a new search type for the bookmark the current buffer is visiting.
The new type can be either `notmuch tree` or `notmuch search`.
Switching to and from a notmuch show mode buffer is not possible.

# Hydra

``` emacs-lisp
;; Cancel hydra if not called in a bookmarked buffer:
(defun notmuch-bookmarks-quit-if-no-bookmark ()
    (interactive)
    (unless (notmuch-bookmarks-get-buffer-bookmark)
      (user-error "No bookmark associated with current buffer")))
	  
;; The hydra:
  (defhydra notmuch-bookmarks-shortcuts (:color blue :hint none
						:body-pre notmuch-bookmarks-quit-if-no-bookmark)
"
For current buffer:
[_e_] Edit query               [_s_] Set search type
[_n_] Edit name                [_q_]uit
"
    ("e" notmuch-bookmarks-edit-query)
    ("n" notmuch-bookmarks-edit-name)
    ("s" notmuch-bookmarks-set-search-type)
    ("q" nil))
```

# Known Limitations

 - Multiple buffer names (which are usually handled by `uniquify`) are
   not handled well. It's just too cumbersome to debug.

# What else?

There is an add-on package in production which allows to add "alerts"
to notmuch bookmarks, thus turning the bookmark feature into a viable
alternative to the notmuch hello buffer. 
