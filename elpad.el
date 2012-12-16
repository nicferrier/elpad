;;; elpad web editor with emacs -*- lexical-binding: t -*-

(require 'elnode)

(defun elpad-pad-change (httpcon)
  (let* ((pos
          (string-to-int (elnode-http-param httpcon "pos")))
         (change (elnode-http-param httpcon "change"))
         (change-char (string-to-int change)))
    (with-current-buffer (get-buffer-create "*test*")
      (goto-char pos)
      (case change-char
        (8 (delete-char -1))
        (13 (newline))
        (t (insert (make-char 'ascii change-char)))))
    (elnode-send-json httpcon '(("status" . "done")))))

(defun elpad-pad (httpcon)
  "Get the contents of the pad."
  (with-current-buffer (get-buffer-create "*test*")
    (elnode-send-json
     httpcon
     (list (cons "text" (buffer-substring (point-min)(point-max)))))))

(defun elpad-handler (httpcon)
  (elnode-dispatcher
   httpcon
   `(("^/$" . ,(elnode-make-send-file "~/work/elpad/index.html"))
     ("^/change/" . elpad-pad-change)
     ("^/pad/" . elpad-pad)
     ("^/app.js" . ,(elnode-make-send-file "app.js"))
     ("^/diff.js" . ,(elnode-make-send-file "diff.js"))
     ("^/jquery.js" . ,(elnode-make-send-file "jquery.js")))))

;;; elpad.el ends here
