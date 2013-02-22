;;; tests for elpad

(require 'ert)
(require 'elpad)

(defmacro elpad/unwind-buffers (&rest body)
  "Ensure buffers are trashed."
  (declare (debug (&rest form))
           (indent 0))
  `(unwind-protect
        ,@body
     (maphash (lambda (k v)
                (kill-buffer v)) elpad/buffer-list)))

(ert-deftest elpad/make-buffer ()
  "Test that the buffer creation interface works."
  (let ((elpad/buffer-list (make-hash-table :test 'equal))
        (elpad/users (make-hash-table :test 'equal))
        (elpad/tags (make-hash-table :test 'equal)))
    (flet ((uuid-string () "" "1234-test"))
      (uuid-string)
      ;; Straight buffer create
      (elpad/unwind-buffers
        (elpad/make-buffer)
        (should (bufferp (gethash "1234-test" elpad/buffer-list))))
      ;; Create with some text
      (elpad/unwind-buffers
        (elpad/make-buffer :text "hello!")
        (should
         (equal
          "hello!"
          (with-current-buffer (gethash "1234-test" elpad/buffer-list)
            (buffer-substring (point-min)(point-max))))))
      ;; Create with a user
      (elpad/unwind-buffers
        (elpad/make-buffer :username "nic")
        (should
         (equal (list "1234-test") (gethash "nic" elpad/users))))
      ;; Create with tags
      (elpad/unwind-buffers
        (elpad/make-buffer :tags '("test" "check"))
        (should
         (equal (list "1234-test") (gethash "test" elpad/tags)))
        (should
         (equal (list "1234-test") (gethash "check" elpad/tags)))))))

(provide 'elpad-tests)

;;; elpad-tests.el ends here
