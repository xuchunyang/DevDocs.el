# DevDocs.el [![MELPA](http://melpa.org/packages/devdocs-badge.svg)](http://melpa.org/#/devdocs)

This is an Emacs package allowing you to easily search the
[DevDocs](http://devdocs.io) documentation.

## Usage

Type `M-x devdocs-search` to search something under point.

With prefix argument, you get a chance to edit search pattern. If the search
pattern contains whitespace, the first word of the search pattern will be used
to limit the search to a single documentation, for example, `js date` will
search `date` in JavaScript documentation.
