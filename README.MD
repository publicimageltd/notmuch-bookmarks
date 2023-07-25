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

```emacs-lisp 
(use-package notmuch-bookmarks
   :after notmuch
   :config
   (notmuch-bookmarks-mode))
```

# Features

 - Specific jump handlers for notmuch buffers.
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

Notmuch bookmarks are also included in the `bookmark-bmenu-list`.
Notmuch bookmarks mode implements a filter function to display only
notmuch bookmarks: When visiting the bmenu list, ust press 'N' to
narrow to notmuch bookmarks.

You can customize this by setting `notmuch-bookmarks-bmenu-filter-key`
to the key string. Turn the minor mode off and on again to see the
effect.

Setting `notmuch-bookmarks-bmenu-filter-key` to `nil` removes any
special key binding.

To call the restricted bookmark menu directly, use the interactive
command `notmuch-bookmarks-bmenu`.

# Keybinding

The package does not bind any keys by itself. 

# What else?

There is an [add-on package](https://github.com/publicimageltd/notmuch-alerts) which
allows to add "alerts" to notmuch bookmarks, thus turning the bookmark
feature into a viable alternative to the notmuch hello buffer.
Bookmarks can then be used to register new or unread mails. The
package is not in melpa yet, but I use it without any problems.
