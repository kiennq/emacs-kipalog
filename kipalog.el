;;; kipalog.el --- Emacs plugin for posting to Kipalog  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  Kien Nguyen <kien.n.quang@gmail.com>
;; Version: 0.0.1
;; Keywords: kipalog
;; Package-Requires: ((emacs "24") (request) (aio) (json))

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

;;

;;; Code:

(require 'request)
(require 'json)
(require 'aio)
(require 'shr)

(defgroup kipalog nil
  "kipalog Emacs plugin"
  :group 'blog
  :prefix "kipalog-")

(defcustom kipalog-token nil "Token for Kipalog."
  :type 'string
  :group 'kipalog)

(defcustom kipalog-url "https://kipalog.com"
  "Url for Kipalog."
  :type 'string
  :group 'kipalog)

;; (custom-set-variables '(request-backend 'url-retrieve))
(defvar kipalog--header
  `(("content-type" . "application/json")
    ("accept-charset" . "application/json")
    ("x-kipalog-token" . ,kipalog-token))
  "Common header for kipalog request.")

(defmacro kipalog--encode (str)
  `(encode-coding-string ,str 'utf-8 t))

(defmacro kipalog--decode (str)
  `(decode-coding-string ,str 'utf-8 t))

(aio-defun kipalog--post (title content status tags)
  "TITLE CONTENT STATUS TAGS."
  (let ((acallback (aio-make-callback)))
    (request (concat kipalog-url "/api/v1/post")
             :type "POST"
             :data (kipalog--encode (json-encode `((title . ,title)
                                                   (content . ,content)
                                                   (status . ,status)
                                                   (tag . ,tags))))
             :parser (lambda () (json-read-from-string (kipalog--decode (buffer-string))))
             :headers kipalog--header
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (funcall (car acallback) data)))
             :error
             (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown)))
             :complete (lambda (&rest _) (message "Finished!")))
    (car (aio-chain (cdr acallback)))))

(defun kipalog--find (part line)
  "Find metadata PART (title or tags) at LINE."
  (save-excursion
    (goto-line line)
    (save-match-data
      (let ((case-fold-search t)
            (current-string
             (buffer-substring (line-beginning-position) (line-end-position))))
        (and (string-match (concat "<!--\\s-*" part "\\s-*\\(.*?\\)\\s-*-->")
                           current-string)
             (match-string 1 current-string))))))

;;;###autoload
(defun kipalog-post-buffer (&optional title tags)
  "Post current buffer with TITLE and TAGS.
Using prefix will post as draft instead.
The TITLE and TAGS will be automatically decided from first two lines
of current buffer.
For example <!-- Title: My title --> at the first line will give the
post title `My title`."
  (interactive)
  (aio-with-async
    (let* ((title (or title
                      (kipalog--find "Title:" 1)
                      (read-string "Title: " nil t)))
           (tags (or tags
                     (kipalog--find "Tags:" 2)
                     (read-string "Tags (separated by comma): " nil t)))
           (res (aio-await (kipalog--post
                            title
                            (buffer-string)
                            (if current-prefix-arg "draft" "published")
                            tags))))
      (message "%d: %s"
               (alist-get 'status res) (alist-get 'cause res)))))

(aio-defun kipalog--preview (content)
  "CONTENT CALLBACK."
  (let* ((acallback (aio-make-callback)))
    (request (concat kipalog-url "/api/v1/post/preview")
             :type "POST"
             :data (kipalog--encode (json-encode `((content . ,content))))
             :parser (lambda () (json-read-from-string (kipalog--decode (buffer-string))))
             :headers kipalog--header
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (funcall (car acallback) data)))
             :error
             (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown)))
             :complete (lambda (&rest _) (message "Finished!")))
    (car (aio-chain (cdr acallback)))))

;;;###autoload
(defun kipalog-preview-buffer ()
  "Showing preview of current buffer when post."
  (interactive)
  (aio-with-async
    (let ((res (aio-await (kipalog--preview (buffer-string)))))
      (if (>= (alist-get 'status res) 400)
          ;; HTTP error
          (message "%d: %s"
                   (alist-get 'status res) (alist-get 'cause res))
        (save-excursion
          (with-current-buffer (get-buffer-create "*kipalog*")
            (erase-buffer)
            (insert (alist-get 'content res))
            (shr-render-buffer (current-buffer))))))))

(provide 'kipalog)
;;; kipalog.el ends here
