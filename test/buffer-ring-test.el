
;; Add source paths to load path so the tests can find the source files
;; Adapted from:
;; https://github.com/Lindydancer/cmake-font-lock/blob/47687b6ccd0e244691fb5907aaba609e5a42d787/test/cmake-font-lock-test-setup.el#L20-L27
(defvar buffer-ring-test-setup-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".."))
  (add-to-list 'load-path
               (concat buffer-ring-test-setup-directory dir)))

;;

(require 'buffer-ring)
(require 'cl)

;;
;; Fixtures
;;

(defvar bfr-test-name-prefix "bfr-test")
(defvar bfr-test-ring-name "bfr-test-ring")

;; fixture recipe from:
;; https://www.gnu.org/software/emacs/manual/html_node/ert/Fixtures-and-Test-Suites.html
(defun fixture-0-ring (body)
  (unwind-protect
      (let ((ring (make-bfr-ring bfr-test-ring-name)))
        (funcall body))))

(defun fixture-1-ring (body)
  (unwind-protect
      (let* ((ring (make-bfr-ring bfr-test-ring-name))
             (buffer (generate-new-buffer bfr-test-name-prefix)))
        (bfr-ring-add-buffer ring buffer)
        (funcall body))))

(defun fixture-2-ring (body)
  (unwind-protect
      (let* ((ring (make-bfr-ring bfr-test-ring-name))
             (buf1 (generate-new-buffer bfr-test-name-prefix))
             (bfr-ring-add-buffer ring buf1)
             (buf2 (generate-new-buffer bfr-test-name-prefix))
             (bfr-ring-add-buffer ring buf2))
        (funcall body))))

(defun fixture-3-ring (body)
  (unwind-protect
      (let* ((ring (make-bfr-ring bfr-test-ring-name))
             (buf1 (generate-new-buffer bfr-test-name-prefix))
             (bfr-ring-add-buffer ring buf1)
             (buf2 (generate-new-buffer bfr-test-name-prefix))
             (bfr-ring-add-buffer ring buf2)
             (buf3 (generate-new-buffer bfr-test-name-prefix))
             (bfr-ring-add-buffer ring buf3))
        (funcall body))))

;;
;; Test utilities
;;



;;
;; Tests
;;

(ert-deftest bfr-ring-test ()
  ;; null constructor
  (should (make-bfr-ring bfr-test-ring-name))

  ;; bfr-ring-name
  (let ((ring (make-bfr-ring bfr-test-ring-name)))
    (should (equal bfr-test-ring-name (bfr-ring-name ring))))

  ;; bfr-ring-ring
  (let ((ring (make-bfr-ring bfr-test-ring-name)))
    (should (bfr-ring-ring ring))))
