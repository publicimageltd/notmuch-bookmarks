# notmuch-bookmarks

Bookmark handling for notmuch buffers.

This package adds a global minor mode which allows you to bookmark
 notmuch buffers via the standard emacs bookmark functionality. A
 'notmuch buffer' denotes either a notmuch tree view, a notmuch search
 view or a notmuch show buffer (message view). With the minor mode
 active, you can add notmuch buffers to the standard bookmark list and
 visit them, e.g. by using `bookmark-jump`.

There is experimental support for providing annotations which count
 the read and unread mails for the bookmarks. Currently, annotations
 do only work when the package `marginalia'`is installed.
 
 Furthermore, there is another global minor mode
 `notmuch-bookmarks-consult-mode` which toggles integration with the
 `consult` package. Currently, it adds a narrowing feature to
 `consult-bookmark': pressing 'n' will narrow to all notmuch bookmarks.

 To activate the minor mode, add something like the following to
 your `.init` file:

```emacs-lisp 
(use-package notmuch-bookmarks
   :after notmuch
   :config
   (notmuch-bookmarks-mode)
   ;; turn on annotations counting read/unread mails
   ;; currently requires 'marginalia' installed
   (notmuch-bookmarks-annotation-mode))
```

# Features

 - Specific jump handlers for notmuch buffers.
 - Edit name or query of bookmarked buffer directly.
 - Filter the pre-installed emacs bookmark menu to display only
   notmuch bookmarks.
 - Display annotations showing count of read, unread mails
   (experimental; currently only with support for `marginalia`).
 - Optional integration with `consult`

# Customizable Variables

## notmuch-bookmarks-prefix 

String which prefixes newly created bookmarks. If nil, do not prefix
anything. Defaults to `notmuch:`.

## notmuch-bookmarks-bmenu-filter-key

Key to narrow the bookmarks in the interactive bookmark menu (see below).

# Interactive Functions

## Generic bookmark handling

In order to use this package, you can use the ordinary bookmark
functionality shipped with vanilla emacs. That is, when in a notmuch
buffer, you might simply set a bookmark the usual way (via
`bookmark-set`).  Alternatively, you can set bookmarks using your
favorite completion system (i.e. ivy or counsel).

## Editing bookmarks in place

In a bookmarked buffer, use `notmuch-bookmarks-edit-name` or
`notmuch-bookmarks-edit-query` to edit the name or the query,
respectively.

## Using the bookmark menu

Notmuch bookmarks are also included in the `bookmark-bmenu-list`.
Notmuch bookmarks mode implements a filter function to display only
notmuch bookmarks: When visiting the bmenu list, just press 'N' to
narrow to notmuch bookmarks.

You can customize this by setting `notmuch-bookmarks-bmenu-filter-key`
to the key string. Turn the minor mode off and on again to see the
effect.

Setting `notmuch-bookmarks-bmenu-filter-key` to `nil` removes any
special key binding.

To call the restricted bookmark menu directly, use the interactive
command `notmuch-bookmarks-bmenu`.

# Keybinding

The package does not change any keybindings. Use the optional
`notmuch-bookmarks-consult-mode` to bind the prefix key 'n' in the
consult bookmarks selection to a narrowed view of all Notmuch
bookmarks..

# What else?

There is an [add-on package](https://github.com/publicimageltd/notmuch-alerts) which
allows to add "alerts" to notmuch bookmarks, thus turning the bookmark
feature into a viable alternative to the notmuch hello buffer.
Bookmarks can then be used to register new or unread mails. 
