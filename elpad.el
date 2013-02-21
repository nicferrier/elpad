;;; elpad web editor with emacs -*- lexical-binding: t -*-

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: hypermedia
;; Package-Requires: ((websocket "1.0")(elnode "0.9.9.6.4")(uuid "0.0.3"))
;; Version: 0.0.0.201302211612

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

;; A client for elpad.

;;; Code:

(require 'elnode)
(require 'kv)
(require 'uuid)

(defvar elpad/ws-server nil
  "Elpad's websocket server.")

(defconst elpad/ws-port 9998)
(defconst elpad/ws-host "localhost")

(defconst elpad/buffer-list (make-hash-table :test 'equal)
  "List of all the buffers the elpad server is holding.")

(defun elpad/make-buffer ()
  "Add a new buffer to the buffer list.

Return the buffer's unique ID."
  (let* ((unique (uuid-string))
         (buf (get-buffer-create unique)))
    (puthash unique buf elpad/buffer-list)
    unique))

;; (setq nic-buf (elpad/make-buffer))

(defun elpad/send (socket data)
  (websocket-send-text
   socket
   (json-encode data)))

(defun elpad/on-message (socket frame)
  "Handle FRAME from SOCKET."
  (let* ((fd (websocket-frame-payload frame))
         (data (let ((json-array-type 'list))
                 (json-read-from-string fd))))
    (case (intern (car data))
      (connect
       (destructuring-bind (buf-handle) (cdr data)
         (let* ((buf (gethash buf-handle elpad/buffer-list))
                (str (with-current-buffer buf
                       (buffer-substring-no-properties
                        (point-min) (point-max)))))
           (elpad/send socket (list 'yeah buf-handle str)))))
      (change
       ;;(message "elpad/on-message change %S" (cdr data))
       (destructuring-bind (buf-handle beg end len str) (cdr data)
         (with-current-buffer (gethash buf-handle elpad/buffer-list)
           (if (> len 0)
               (delete-region beg (+ end len))
               (goto-char beg)
               (insert str))))))))

(defun elpad/on-open (websocket)
  (message "elpad websocket open %S" websocket))

(defun elpad/on-close (websocket)
  (message "elpad websocket close"))

(defun elpad/ws-init ()
  "Initialize the websocket server."
  (setq elpad/ws-server
        (websocket-server
         elpad/ws-port
         :on-message 'elpad/on-message
         :on-open 'elpad/on-open
         :on-close 'elpad/on-close)))

(defun elpad-pad (httpcon)
  "Find a particular pad."
  ;; This should probably also support POST
  ;; POST to it, get back the new pad-id from elpad/make-buffer
  (let ((pad-id (elnode-http-mapping httpcon 1)))
    (cond
      ;; Do we have a pad of that ID? - send redirect to websocket
      ((and pad-id (gethash pad-id elpad/buffer-list))
        (elnode-send-redirect
         httpcon
         (format "ws://%s:%s;id=%s" elpad/ws-host elpad/ws-port pad-id)))
      ;; We have no pad-id - possibly we could send list of recent pads?
      ((not pad-id)
       (elnode-send-json httpcon '(nothing)))
      (t
       (elnode-send-404 httpcon "No such pad.")))))

(defun elpad-handler (httpcon)
  ;; Initialize the websocket server socket
  (unless (equal 'listen (process-status elpad/ws-server))
    (elpad/ws-init))
  (elnode-dispatcher
   httpcon
   `(("^/$" . ,(elnode-make-send-file "~/work/elpad/index.html"))
     ("^/pad/\\([^/]*\\).*" . elpad-pad)
     ("^/app.js" . ,(elnode-make-send-file "app.js"))
     ("^/diff.js" . ,(elnode-make-send-file "diff.js"))
     ("^/jquery.js" . ,(elnode-make-send-file "jquery.js")))))

(defun elpad-stop ()
  "Stop the elpad websocket server."
  (interactive)
  (websocket-server-close elpad/ws-server))

;;; elpad.el ends here
