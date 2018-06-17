;;; kipalog.el --- Emacs plugin for posting to Kipalog  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  Kien Nguyen <kien.n.quang@gmail.com>
;; Keywords: kipalog
;; Package-Requires: ((emacs "24") (request) (deferred "0.4.0") (json))

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
(require 'deferred)
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

(custom-set-variables '(request-backend 'url-retrieve))

(defmacro kipalog--deferize (orig-func &rest args)
  "Change ORIG-FUNC (&rest ARGS CALLBACK) to deferred form."
  (let* ((d (deferred:new #'identity))
         (args (nconc args `((lambda (res)
                               (deferred:callback-post ,d res))))))
    `(progn
       (funcall ,orig-func ,@args)
       ,d)))

(defvar kipalog--header
  `(("Content-Type" . "application-json")
    ("Accept-Charset" . "application-json")
    ("X-Kipalog-Token" . ,kipalog-token))
  "Common header for kipalog request.")

(defun kipalog--post (title content status tags &optional callback)
  "TITLE CONTENT STATUS TAGS CALLBACK."
  (request (concat kipalog-url "/api/v1/post")
           :type "POST"
           :data (json-encode `((title . ,title)
                                (content . ,content)
                                (status . ,status)
                                (tag . ,tags)))
           :parser 'json-read
           :headers kipalog--header
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (if callback
                           (funcall callback data))))
           :error
           (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                          (message "Got error: %S" error-thrown)))
           :complete (lambda (&rest _) (message "Finished!"))))

;;;###autoload
(defun kipalog-post-buffer (title tags)
  "Post current buffer with TITLE and TAGS.
Using prefix will post as draft instead."
  (interactive
   (list (read-string "Title: " nil t)
         (read-string "Tags (separated by comma): " nil t)))
  (deferred:$
    (kipalog--deferize #'kipalog--post
                       title
                       (buffer-string)
                       (if current-prefix-arg "draft" "published")
                       tags)
    (deferred:nextc it
      #'(lambda (res)
          (message "%d: %s"
                   (alist-get 'status res) (alist-get 'cause res))))))

(defun kipalog--preview (content callback)
  "CONTENT CALLBACK."
  (request (concat kipalog-url "/api/v1/post/preview")
           :type "POST"
           :data (json-encode `((content . ,content)))
           :parser 'json-read
           :headers kipalog--header
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (if callback
                           (funcall callback data))))
           :error
           (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                          (message "Got error: %S" error-thrown)))
           :complete (lambda (&rest _) (message "Finished!"))))

;;;###autoload
(defun kipalog-preview-buffer ()
  "Showing preview of current buffer when post."
  (interactive)
  (deferred:$
    (kipalog--deferize #'kipalog--preview (buffer-string))
    (deferred:nextc it
      #'(lambda (res)
          (if (>= (alist-get 'status res) 400)
              ;; HTTP error
              (message "%d: %s"
                       (alist-get 'status res) (alist-get 'cause res))
            (save-excursion
              (with-current-buffer (get-buffer-create "*kipalog*")
                (erase-buffer)
                (insert (alist-get 'content res))
                (shr-render-buffer (current-buffer)))))))))

(provide 'kipalog)
;;; kipalog.el ends here
