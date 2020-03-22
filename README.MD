# notmuch-bookmarks

Bookmark handling for notmuch buffers.

This package adds a global minor mode which allows you to bookmark
 notmuch buffers via the standard emacs bookmark functionality. A
 'notmuch buffer' denotes either a notmuch tree view, a notmuch search
 view or a notmuch show buffer (message view). With the minor mode
 active, you can add notmuch buffers to the standard bookmark list and
 visit them, e.g. by using `bookmark-jump`.

 To activate the minor mode, add something like the following to
 your `.init` file:

```
(use-package notmuch-bookmarks
   :after notmuch
   :config
   (notmuch-bookmarks-mode))
```

 This package is NOT part of the official notmuch emacs suite.

# Features

 - Specific jump handlers for notmuch buffers.
 - Interactive functions to edit the bookmark.
 - Filter the pre-installed emacs bookmark menu to display only
   notmuch bookmarks.

# Customizable Variables

## notmuch-bookmarks-prefix 

String which prefixes newly created bookmarks. If nil, do not prefix
anything. Defaults to `notmuch:`.

# Interactive Functions

## Generic bookmark handling

In order to use this package, you can use the ordinary bookmark
functionality shipped with vanilla emacs. That is, when in a notmuch
buffer, you might simply set a bookmark the usual way (via
`bookmark-set`).  Alternatively, you can set bookmarks using your
favorite completion system (i.e. ivy or counsel).

## Using the bookmark menu

Notmuch bookmarks are also included in the display of
`bookmark-bmenu-list`. The notmuch bookmarks mode implements a filter
function to display only notmuch bookmarks: In `bookmark-bmenu-mode',
just press 'N' to narrow to notmuch bookmarks.

To call the bookmark menu with only notmuch bookmarks displayed, use
the interactive command `notmuch-bookmarks-bmenu`.

## Editing a bookmark

There are a number of editing commands available if the current buffer
is a bookmarked notmuch buffer. You can call all commands
interactively, that is, with M-x. 

| Command                           | With current buffer and bookmark, do.....                 |
|-----------------------------------|----------------------------------------------------------|
| notmuch-bookmarks-edit-query      | ...edit the query.                                        |
| notmuch-bookmarks-edit-name       | ...edit the name.                                            |
| notmuch-bookmarks-set-search-type | ...choose between either `notmuch tree` or `notmuch search`. |

# Integrating notmuch bookmarks with completion frameworks

Since notmuch bookmarks are ordinary bookmarks, all completion
frameworks (like helm or ivy) recognize them as normal bookmark
fellows. There is no special handling necessary. However, we can add
some spice.

## Ivy 

If you use `ivy-rich`, notmuch bookmarks are recognized as special
bookmarks and thus displayed with the type marker `notmuch'`. Looks good.

## Helm

In helm, you can define a special source:

``` emacs-lisp
(setq helm-source-notmuch-bookmarks 
(helm-build-sync-source "helm-notmuch-bookmarks"
	  :candidates (lambda ()
			(seq-map
			 (lambda (b)
			   (cons (bookmark-name-from-full-record b)
				 b))
			 (seq-filter
			  #'notmuch-bookmarks-record-p
			  bookmark-alist)))
	  :action 'helm-bookmark-jump))
```

You can add this source to `helm-mini`, or bind it to a special key:

``` emacs-lisp
;; add the source to helm-mini:
  (add-to-list  'helm-mini-default-sources 'helm-source-notmuch-bookmarks)

;; or bind it to a special key:
 (global-set-key (kbd "<f4>") (lambda () (interactive)
                                (helm helm-source-notmuch-bookmarks)))
					
```

# Keybinding

The package does not bind any keys by itself. 

### Use a Hydra

For easier editing, I recommend to use a hydra or something similar.
This is one possible hydra:

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

There is an [add-on
package](https://github.com/publicimageltd/notmuch-alerts) which
allows to add "alerts" to notmuch bookmarks, thus turning the bookmark
feature into a viable alternative to the notmuch hello buffer.
Bookmarks can then be used to register new or unread mails. The
package is not in melpa yet, but I use it without any problems.
