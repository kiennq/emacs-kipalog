# emacs-kipalog
Kipalog's plugin for Emacs

Let you write and post to [Kipalog](https://kipalog.com/) from right within `Emacs`

## Install
**Recommended**: Using [`straigh.el`](https://github.com/raxod502/straight.el) with [`use-package`](https://github.com/jwiegley/use-package)

``` emacs-lisp
(use-package kipalog
  :straight (:host github :repo "kiennq/emacs-kipalog ")
  :defer t
  :init
  (setq kipalog-token "YourKipalogToken") ; required
  (with-eval-after-load 'evil
    (evil-define-key 'normal global-map (kbd "SPC k k") 'kipalog-post-buffer)
    (evil-define-key 'normal global-map (kbd "SPC k p") 'kipalog-preview-buffer)))
```

Or clone this to your `load-path` and do setup properly.

## Usage

**You need to setup your Kipalog's token using `kipalog-token`**.

`kipalog-post-buffer` will post current buffer's content to Kipalog, and create a new post for it.
If the first 2 line in your buffer contains
``` markdown
<!-- Title: Your post title -->
<!-- Tags: tag1, tag2, tagx -->
```
in respected order, the title and tags of your post wil be automatically detected, else you will be asked to enter them.

`kipalog-preview-buffer` will show the preview of your buffer with Kipalog's rendering.
