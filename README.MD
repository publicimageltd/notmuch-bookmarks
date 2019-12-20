# notmuch-bookmarks
Bookmark handling for notmuch buffers

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

 - Specific jump handlers for notmuch buffers
 - Some interactive functions to ease working with these kind of
   bookmarks

# Customizable Variables

## notmuch-bookmarks-prefix 

String which prefixes newly created bookmarks. If nil, do not prefix
anything. Defaults to `notmuch:`.

# Interactive Functions

## Generic bookmark handling

In order to use this package, you can use the ordinary bookmark
functionality shipped with vanilla emacs. That is, when in a notmuch
buffer, you might simply set a bookmark the usual way (via
`bookmark-set`). The notmuch bookmarks are also included in the
display of `bookmark-bmenu-list`. You can use the search feature (key
'/') to narrow down the bookmark lists to all bookmarks with the prefix
`notmuch:`.

## notmuch-bookmarks-edit-query

Edit the query with the bookmark the current buffer is visiting.

## notmuch-bookmarks-edit-name

Edit the name of the bookmark the current buffer is visiting.

## notmuch-bookmarks-set-search-type

Set a new search type for the bookmark the current buffer is visiting.
The new type can be either `notmuch tree` or `notmuch search`.
Switching to and from a notmuch show mode buffer is not possible.

# Known Limitations

 - Multiple buffer names (which are usually handled by `uniquify`) are
   not handled well. It's just too cumbersome to debug.

# What else?

There is an add-on package in production which allows to add "alerts"
to notmuch bookmarks, thus turning the bookmark feature into a viable
alternative to the notmuch hello buffer. 
