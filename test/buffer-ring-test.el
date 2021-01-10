
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

;;
;; Fixtures
;;

(defvar bfr-test-name-prefix "bfr-test")
(defvar bfr-0-ring-name "bfr-test-ring-0")
(defvar bfr-1-ring-name "bfr-test-ring-1")
(defvar bfr-2-ring-name "bfr-test-ring-2")

;; fixture recipe from:
;; https://www.gnu.org/software/emacs/manual/html_node/ert/Fixtures-and-Test-Suites.html
(defun fixture-0 (body)
  ;; no buffer rings present
  (unwind-protect
      (progn (setq buffer-ring-torus (make-dyn-ring))
             (funcall body))
    (dyn-ring-destroy buffer-ring-torus)))

(defun fixture-1 (body)
  ;; 1 empty buffer ring
  (let ((bring nil))
    (unwind-protect
        (progn (setq buffer-ring-torus (make-dyn-ring))
               (let ((bring (bfr-torus-get-ring bfr-0-ring-name)))
                 (funcall body)))
      (dyn-ring-destroy buffer-ring-torus)
      (dyn-ring-destroy bring))))

(defun fixture-2 (body)
  ;; 2 buffer rings: empty, 1 element
  (let ((bring0 nil)
        (bring1 nil)
        (buffer nil))
    (unwind-protect
        (progn
          (setq buffer-ring-torus (make-dyn-ring))
          (let* ((bring0 (bfr-torus-get-ring bfr-0-ring-name))
                 (bring1 (bfr-torus-get-ring bfr-1-ring-name))
                 (buffer (generate-new-buffer bfr-test-name-prefix)))
            (with-current-buffer buffer
              (buffer-ring-add (bfr-ring-name bring1)))
            (funcall body)))
      (dyn-ring-destroy buffer-ring-torus)
      (dyn-ring-destroy bring0)
      (dyn-ring-destroy bring1)
      (kill-buffer buffer))))

(defun fixture-2-1-1 (body2)
  ;; 2 buffer rings: empty, 1 element
  ;; add a buffer to the empty ring
  (fixture-2
   (lambda ()
     (with-current-buffer buffer
       (buffer-ring-add (bfr-ring-name bring0)))
     (funcall body2))))

(defun fixture-3 (body)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  (let ((bring0 nil)
        (bring1 nil)
        (bring2 nil)
        (buf1 nil)
        (buf2 nil)
        (buf3 nil))
    (unwind-protect
        (progn
          (setq buffer-ring-torus (make-dyn-ring))
          (let* ((bring0 (bfr-torus-get-ring bfr-0-ring-name))
                 (bring1 (bfr-torus-get-ring bfr-1-ring-name))
                 (bring2 (bfr-torus-get-ring bfr-2-ring-name))
                 (buf1 (generate-new-buffer bfr-test-name-prefix))
                 (buf2 (generate-new-buffer bfr-test-name-prefix))
                 (buf3 (generate-new-buffer bfr-test-name-prefix)))
            (with-current-buffer buf1
              (buffer-ring-add (bfr-ring-name bring1)))
            (with-current-buffer buf2
              (buffer-ring-add (bfr-ring-name bring2)))
            (with-current-buffer buf3
              (buffer-ring-add (bfr-ring-name bring2)))
            (funcall body)))
      (dyn-ring-destroy buffer-ring-torus)
      (dyn-ring-destroy bring0)
      (dyn-ring-destroy bring1)
      (dyn-ring-destroy bring2)
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3))))

(defun fixture-3-1-1-2 (body2)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; add a buffer to the empty ring
  (fixture-3
   (lambda ()
     (let ((buffer buf1))
       (with-current-buffer buffer
         (buffer-ring-add (bfr-ring-name bring0)))
       (funcall body2)))))

(defun fixture-3-0-2-2 (body2)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; add a buffer to the 1 ring
  (fixture-3
   (lambda ()
     (let ((buffer buf2))
       (with-current-buffer buffer
         (buffer-ring-add (bfr-ring-name bring1)))
       (funcall body2)))))

(defun fixture-3-0-1-3 (body2)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; add a buffer to the 2 ring
  (fixture-3
   (lambda ()
     (let ((buffer buf1))
       (with-current-buffer buffer
         (buffer-ring-add (bfr-ring-name bring2)))
       (funcall body2)))))

;;
;; Test utilities
;;



;;
;; Tests
;;

(ert-deftest bfr-ring-test ()
  ;; null constructor
  (should (make-bfr-ring bfr-0-ring-name))

  ;; bfr-ring-name
  (let ((bfr-ring (make-bfr-ring bfr-0-ring-name)))
    (should (equal bfr-0-ring-name (bfr-ring-name bfr-ring))))

  ;; bfr-ring-ring
  (let ((bfr-ring (make-bfr-ring bfr-0-ring-name)))
    (should (bfr-ring-ring bfr-ring))))

(ert-deftest buffer-ring-add-test ()
  (fixture-0
   (lambda ()
     (let ((buffer (generate-new-buffer bfr-test-name-prefix)))
       (buffer-ring-add "new-ring" buffer)
       (should (dyn-ring-contains-p buffer-ring-torus
                                    (car (bfr-get-rings buffer))))
       (should (dyn-ring-contains-p (bfr-ring-ring (bfr-current-ring))
                                    buffer))
       (should (= 1 (bfr-ring-size)))
       ;; cleanup
       (dyn-ring-destroy (bfr-ring-ring (bfr-current-ring)))
       (kill-buffer buffer))))

  (fixture-1
   (lambda ()
     (let ((buffer (generate-new-buffer bfr-test-name-prefix)))
       (buffer-ring-add (bfr-ring-name bring)
                        buffer)
       (should (dyn-ring-contains-p buffer-ring-torus
                                    (car (bfr-get-rings buffer))))
       (should (dyn-ring-contains-p (bfr-ring-ring (bfr-current-ring))
                                    buffer))
       (should (= 1 (bfr-ring-size)))
       ;; cleanup
       (kill-buffer buffer))))

  (fixture-2-1-1
   (lambda ()
     (should (dyn-ring-contains-p (bfr-ring-ring bring0)
                                  buffer))
     (should (dyn-ring-contains-p (bfr-ring-ring bring1)
                                  buffer))))

  (fixture-3-1-1-2
   (lambda ()
     (should (dyn-ring-contains-p (bfr-ring-ring bring0)
                                  buffer))
     (should (dyn-ring-contains-p (bfr-ring-ring bring1)
                                  buffer))
     (should-not (dyn-ring-contains-p (bfr-ring-ring bring2)
                                      buffer))))

  (fixture-3-0-2-2
   (lambda ()
     (should-not (dyn-ring-contains-p (bfr-ring-ring bring0)
                                      buffer))
     (should (dyn-ring-contains-p (bfr-ring-ring bring1)
                                  buffer))
     (should (dyn-ring-contains-p (bfr-ring-ring bring2)
                                  buffer))))

  (fixture-3-0-1-3
   (lambda ()
     (should-not (dyn-ring-contains-p (bfr-ring-ring bring0)
                                      buffer))
     (should (dyn-ring-contains-p (bfr-ring-ring bring1)
                                  buffer))
     (should (dyn-ring-contains-p (bfr-ring-ring bring2)
                                  buffer)))))
