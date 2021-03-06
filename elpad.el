;;; elpad web editor with emacs -*- lexical-binding: t -*-

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The Elpad server.  Stores elpads and runs the websocket server to
;; connect to them.

;;; Code:

(elnode-app elpad-dir cl elnode kv uuid websocket)

(defconst elpad/ws-port 9998
  "Default websocket port.")

(defconst elpad/ws-host "localhost"
  "Default websocket hostname")

(defgroup elpad nil
  "An Elnode pastebin that dynamically updates.

Elpad lets you share text editing across the web.  Users can send
a piece of text to an Elpad server and send it updates with the
websocket protocol."
  :group 'applications)

(defcustom elpad-auto-start nil
  "Whether to auto-start Elpad or not."
  :group 'elpad
  :type 'string)

(defcustom elpad-elnode-port 0
  "The TCP port used for the elpad HTTP server."
  :group 'elpad
  :type 'integer)

(defcustom elpad-elnode-host elpad/ws-host
  "The TCP port used for the elpad HTTP server."
  :group 'elpad
  :type 'string)

(defcustom elpad-websocket-host elpad/ws-host
  "The host used for the websocket server."
  :group 'elpad
  :type 'string)

(defcustom elpad-websocket-port elpad/ws-port
  "The TCP port used for the websocket server."
  :group 'elpad
  :type 'integer)

(defcustom elpad-websocket-host elpad/ws-host
  "The host used for the websocket server."
  :group 'elpad
  :type 'string)

(defcustom elpad-footer "
<small class=\"copyright\">(C) Nic Ferrier 2013</small>
<ul>
    <li><a href=\"http://github.com/nicferrier/elpad\">elpad github</a></li>
    <li><a href=\"http://github.com/nicferrier/elnode\">elnode github</a></li>
    <li><a href=\"http://elnode.org\">elnode site</a></li>
    <li><a href=\"http://emacswiki.org/emacs/NicFerrier\">nic's emacs-wiki</a></li>
    <li><a href=\"http://nic.ferrier.me.uk\">nic's blog</a></li>
</ul>"
  "Footer text for the HTML pages."
  :group 'elpad
  :type 'string)


(defvar elpad/ws-server nil
  "Elpad's websocket server.")

(defvar elpad/buffer-list (make-hash-table :test 'equal)
  "List of all the buffers the elpad server is holding.")

(defvar elpad/tags (make-hash-table :test 'equal)
  "Map of tags to lists of pad ids.")

(defvar elpad/users (make-hash-table :test 'equal)
  "Map of usernames to lists of pad ids.")


(defun* elpad/make-buffer (&key username text tags)
  "Add a new buffer to the buffer list.

USERNAME is an optional username to attach the resulting buffer
to.  TAGS is an optional list of tags with which to index the
buffer.

TEXT is optional text to add to the buffer.

Return the buffer's unique ID."
  (let* ((unique (uuid-string))
         (buf (get-buffer-create unique)))
    (puthash unique buf elpad/buffer-list)
    (when text
      (with-current-buffer buf
        (insert text)))
    (when tags
      (loop for tag in tags
         do (let ((buf-ids (gethash tag elpad/tags)))
              (unless (member unique buf-ids)
                (puthash
                 tag (append (list unique) buf-ids)
                 elpad/tags)))))
    (when username
      (let ((buf-ids (gethash username elpad/users)))
        (unless (member unique buf-ids)
          (puthash
           username (append (list unique) buf-ids)
           elpad/users)))
      unique)))

(defun elpad/buffer-list-entries ()
  "List the buffers controlled by the elpad server."
  (let ((ids->users
         (loop for (username . buf-list) in (kvhash->alist elpad/users)
            append
              (loop for buf in buf-list
                 collect (cons buf username))))
        (ids->tags
         (loop for (tag . buf-list) in (kvhash->alist elpad/tags)
            append
              (loop for buf in buf-list ; this is wrong - throws away tags
                 collect (cons buf tag)))))
    (loop for (id . buffer) in (kvhash->alist elpad/buffer-list)
       collect
         (list id
               (vector
                id (aget ids->users id) (or (aget ids->tags id) ""))))))

(defun elpad-show-buffer (id)
  "Show an elpad buffer from the list."
  (interactive
   (list
    (save-excursion
      (goto-char (line-beginning-position))
      (re-search-forward "\\([^ ]+\\)" nil t)
      (match-string 1))))
  (switch-to-buffer (get-buffer id)))

(define-derived-mode
    elpad-buffer-list-mode tabulated-list-mode "Elpad buffer list"
    "Major mode for listing Elpad buffers under the server."
    (setq tabulated-list-entries 'elpad/buffer-list-entries)
    (setq tabulated-list-format
          [("Buffer ID" 40 nil)
           ("User" 40 nil)
           ("Tags" 40 nil)])
    (define-key elpad-buffer-list-mode-map (kbd "\r") 'elpad-show-buffer)
    (tabulated-list-init-header))

(defun elpad-list-buffers ()
  "List the current buffers in elpad."
  (interactive)
  (with-current-buffer (get-buffer-create "*elpad-buffers*")
    (elpad-buffer-list-mode)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))


;; Websocket handling routines

(defun elpad/send (socket data)
  (websocket-send-text
   socket
   (json-encode data)))

(defvar elpad/buffer-websockets nil
  "Buffer local list of Websockets connected to this buffer.")

(make-variable-buffer-local 'elpad/buffer-websockets)

(defun elpad/on-message (socket frame)
  "Handle FRAME from SOCKET."
  (let* ((frame-data (websocket-frame-payload frame))
         (data (let ((json-array-type 'list))
                 (json-read-from-string frame-data))))
    (case (intern (car data))
      (connect
       (destructuring-bind (buf-handle) (cdr data)
         (let* ((buf (gethash buf-handle elpad/buffer-list))
                (str (with-current-buffer buf
                       ;; Really need to fix this to be add-to-list
                       (setq elpad/buffer-websockets
                             (append (list socket)
                                     elpad/buffer-websockets))
                       (buffer-substring-no-properties
                        (point-min) (point-max)))))
           (elpad/send socket (list 'yeah buf-handle str)))))
      (change
       (destructuring-bind (buf-handle beg end len str) (cdr data)
         ;; Update the copy we have here
         (with-current-buffer (gethash buf-handle elpad/buffer-list)
           (if (> len 0)
               (delete-region beg (+ end len))
               (goto-char beg)
               (insert str))
           ;; Then update the other sockets
           (loop for sock in elpad/buffer-websockets
              unless (equal socket sock)
              do
                (condition-case err
                    (elpad/send
                     sock (list 'sync buf-handle beg end len str))
                  (error (message "whoops! that one looks dead"))))))))))

(defun elpad/on-open (websocket)
  (message "elpad websocket open %S" websocket))

(defun elpad/on-close (websocket)
  (message "elpad websocket close"))

(defun elpad/ws-init ()
  "Initialize the websocket server."
  (setq elpad/ws-server
        (websocket-server
         elpad-websocket-port
         :on-message 'elpad/on-message
         :on-open 'elpad/on-open
         :on-close 'elpad/on-close)))


;; Elnode stuff

(defun elpad-pad (httpcon)
  "Find a particular pad or make a new one."
  (elnode-method httpcon
    (GET
     (let ((pad-id (elnode-http-mapping httpcon 1)))
       (cond
         ;; Do we have a pad of that ID? - send redirect to websocket
         ((and pad-id (gethash pad-id elpad/buffer-list))
          (elnode-send-redirect
           httpcon
           (format "ws://%s:%s;id=%s"
                   elpad-websocket-host
                   elpad-websocket-port
                   pad-id)))
         ;; We have no pad-id - possibly we could send list of recent pads?
         ((not pad-id)
          (elnode-send-json httpcon '(nothing)))
         (t
          (elnode-send-404 httpcon "No such pad.")))))
    (POST
     (let* ((username (elnode-http-param httpcon "username" ""))
            (tags (elnode-http-param httpcon "tags" ""))
            (text (elnode-http-param httpcon "text" ""))
            (handle
             (elpad/make-buffer
              :username (unless (equal "" username) username)
              :tags (unless (equal "" tags) (split-string tags))
              :text (unless (equal "" text) text))))
       (elnode-send-redirect httpcon (format "/pad/%s/" handle))))))

(defun elpad/email->username (email)
  (when (string-match "\\([^@]+\\)@.*" email)
    (match-string 1 email)))

(defun elpad-user (httpcon)
  "Present list of pads for the user."
  (let* ((user (elnode-http-mapping httpcon 1))
         (buffers-json (json-encode (gethash user elpad/users)))
         (elnode-replacements-pattern "{\\[\\([^]]+\\)\\]}"))
    (elnode-send-file
     httpcon (concat elpad-dir "template.html")
     :replacements
     `(("title" . ,user)
       ("username" . ,(elpad/email->username user))
       ("pads" . ,buffers-json)
       ("footer" . ,elpad-footer)))))

;;;###autoload
(defun elpad-handler (httpcon)
  "The Elpad server."
  (unless (equal 'listen (process-status elpad/ws-server))
    (elpad/ws-init))
  (let ((webserver (elnode-webserver-handler-maker elpad-dir)))
    (elnode-dispatcher
     httpcon
     `(("^/$" . ,(elnode-make-send-file
                  (concat elpad-dir "index.html")
                  :replacements
                  (lambda ()
                    `(("footer" . ,elpad-footer)))
                  :replacements-pattern "{\\[\\([^]]+\\)\\]}"))
       ("^/-/\\(.*\\)$" . ,webserver)
       ("^/pad/\\([^/]*\\).*" . elpad-pad)
       ("^/user/\\([^/]+\\).*" . elpad-user)
       ("^/app.js" . ,(elnode-make-send-file "app.js"))
       ("^/diff.js" . ,(elnode-make-send-file "diff.js"))
       ("^/jquery.js" . ,(elnode-make-send-file "jquery.js"))))))

(defun elpad-stop ()
  "Stop the elpad websocket server."
  (interactive)
  (websocket-server-close elpad/ws-server))

;;;###autoload
(defun elpad-auto-start ()
  "Auto-start the elpad server.

You still have to call this function, but it is auto-loaded and
it does the necessary tests on the custom variables."
  (interactive)
  (when (and (boundp 'elpad-auto-start)
             (> elpad-elnode-port 0)
             elpad-auto-start)
    (elnode-start
     'elpad-handler
     :port elpad-elnode-port
     :host elpad-elnode-host)))

(provide 'elpad)

;;; elpad.el ends here
