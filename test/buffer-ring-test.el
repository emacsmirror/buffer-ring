
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
(defvar bfr-new-ring-name "bfr-test-new-ring")
(defvar bfr-0-ring-name "bfr-test-ring-0")
(defvar bfr-1-ring-name "bfr-test-ring-1")
(defvar bfr-2-ring-name "bfr-test-ring-2")

;; fixture recipe from:
;; https://www.gnu.org/software/emacs/manual/html_node/ert/Fixtures-and-Test-Suites.html
(defun fixture-0 (body)
  ;; no buffer rings present
  ;; an unaffiliated buffer
  (let ((buffer nil))
    (unwind-protect
        (progn (setq buffer-ring-torus (make-dyn-ring))
               (setq buffer-rings (ht))
               (setq buffer (generate-new-buffer bfr-test-name-prefix))
               (funcall body))
      (kill-buffer buffer)
      (let ((bring (bfr-torus-get-ring bfr-new-ring-name)))
        (when bring
          (dyn-ring-destroy (bfr-ring-ring bring))))
      (dyn-ring-destroy buffer-ring-torus))))

(defun fixture-1-0 (body)
  ;; 1 empty buffer ring
  ;; an unaffiliated buffer
  (let ((bring nil)
        (buffer nil))
    (unwind-protect
        (progn (setq buffer-ring-torus (make-dyn-ring))
               (setq buffer-rings (ht))
               (setq bring (bfr-torus-get-ring bfr-0-ring-name)
                     buffer (generate-new-buffer bfr-test-name-prefix))
               (funcall body))
      (kill-buffer buffer)
      (dyn-ring-destroy buffer-ring-torus)
      (dyn-ring-destroy (bfr-ring-ring bring)))))

(defun fixture-1-1 (body2)
  (fixture-1-0
   (lambda ()
     (buffer-ring-add (bfr-ring-name bring)
                      buffer)
     (funcall body2))))

(defun fixture-1-2 (body3)
  (fixture-1-1
   (lambda ()
     (let ((buf2 nil))
       (unwind-protect
           (progn
             (setq buf2 (generate-new-buffer bfr-test-name-prefix))
             (buffer-ring-add (bfr-ring-name bring)
                              buf2)
             (funcall body3))
         (kill-buffer buf2))))))

(defun fixture-1-3 (body4)
  (fixture-1-2
   (lambda ()
     (let ((buf3 nil))
       (unwind-protect
           (progn
             (setq buf3 (generate-new-buffer bfr-test-name-prefix))
             (buffer-ring-add (bfr-ring-name bring)
                              buf3)
             (funcall body4))
         (kill-buffer buf3))))))

(defun fixture-2-0-1 (body)
  ;; 2 buffer rings: empty, 1 element
  (let ((bring0 nil)
        (bring1 nil)
        (buffer nil))
    (unwind-protect
        (progn
          (setq buffer-ring-torus (make-dyn-ring))
          (setq buffer-rings (ht))
          (setq bring0 (bfr-torus-get-ring bfr-0-ring-name)
                bring1 (bfr-torus-get-ring bfr-1-ring-name)
                buffer (generate-new-buffer bfr-test-name-prefix))
          (buffer-ring-add (bfr-ring-name bring1)
                           buffer)
          (funcall body))
      (kill-buffer buffer)
      (dyn-ring-destroy buffer-ring-torus)
      (dyn-ring-destroy (bfr-ring-ring bring0))
      (dyn-ring-destroy (bfr-ring-ring bring1)))))

(defun fixture-2-1-1 (body2)
  ;; 2 buffer rings: empty, 1 element
  ;; add a buffer to the empty ring
  (fixture-2-0-1
   (lambda ()
     (buffer-ring-add (bfr-ring-name bring0)
                      buffer)
     (funcall body2))))

(defun fixture-3-0-1-0 (body)
  ;; 3 buffer rings: empty, 1 element, empty
  (let ((bring0 nil)
        (bring1 nil)
        (bring2 nil)
        (buf1 nil))
    (unwind-protect
        (progn
          (setq buffer-ring-torus (make-dyn-ring))
          (setq buffer-rings (ht))
          (setq bring0 (bfr-torus-get-ring bfr-0-ring-name)
                bring1 (bfr-torus-get-ring bfr-1-ring-name)
                bring2 (bfr-torus-get-ring bfr-2-ring-name)
                buf1 (generate-new-buffer bfr-test-name-prefix))
          (buffer-ring-add (bfr-ring-name bring1)
                           buf1)
          (funcall body))
      (kill-buffer buf1)
      (dyn-ring-destroy buffer-ring-torus)
      (dyn-ring-destroy (bfr-ring-ring bring0))
      (dyn-ring-destroy (bfr-ring-ring bring1))
      (dyn-ring-destroy (bfr-ring-ring bring2)))))

(defun fixture-3-0-1-1 (body2)
  ;; 3 buffer rings: empty, 1 element, 1 element
  (fixture-3-0-1-0
   (lambda ()
     (let ((buf2 nil))
       (unwind-protect
           (progn
             (setq buf2 (generate-new-buffer bfr-test-name-prefix))
             (buffer-ring-add (bfr-ring-name bring2)
                              buf2)
             (funcall body2))
         (kill-buffer buf2))))))

(defun fixture-3-0-1-2 (body3)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  (fixture-3-0-1-1
   (lambda ()
     (let ((buf3 nil))
       (unwind-protect
           (progn
             (setq buf3 (generate-new-buffer bfr-test-name-prefix))
             (buffer-ring-add (bfr-ring-name bring2)
                              buf3)
             (funcall body3))
         (kill-buffer buf3))))))

(defun fixture-3-1-1-2 (body4)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; add a buffer to the empty ring
  (fixture-3-0-1-2
   (lambda ()
     (let ((buffer buf1))
       (buffer-ring-add (bfr-ring-name bring0)
                        buffer)
       (funcall body4)))))

(defun fixture-3-0-2-2 (body4)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; add a buffer to the 1 ring
  (fixture-3-0-1-2
   (lambda ()
     (let ((buffer buf2))
       (buffer-ring-add (bfr-ring-name bring1)
                        buffer)
       (funcall body4)))))

(defun fixture-3-0-1-3 (body4)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; add a buffer to the 2 ring
  (fixture-3-0-1-2
   (lambda ()
     (let ((buffer buf1))
       (buffer-ring-add (bfr-ring-name bring2)
                        buffer)
       (funcall body4)))))

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
     (buffer-ring-add bfr-new-ring-name buffer)
     (should (dyn-ring-contains-p buffer-ring-torus
                                  (car (bfr-get-rings buffer))))))
  (fixture-0
   (lambda ()
     (buffer-ring-add bfr-new-ring-name buffer)
     (should (dyn-ring-contains-p (bfr-ring-ring (bfr-current-ring))
                                  buffer))
     (should (= 1 (bfr-ring-size)))))
  (fixture-0
   (lambda ()
     (buffer-ring-add bfr-new-ring-name buffer)
     (should (= 1 (bfr-ring-size)))))
  (fixture-0
   (lambda ()
     (buffer-ring-add bfr-new-ring-name buffer)
     (should (eq (bfr-torus-get-ring bfr-new-ring-name)
                 (car (bfr-get-rings buffer))))))

  (fixture-1-0
   (lambda ()
     (buffer-ring-add (bfr-ring-name bring)
                      buffer)
     (should (dyn-ring-contains-p buffer-ring-torus
                                  (car (bfr-get-rings buffer))))))
  (fixture-1-0
   (lambda ()
     (buffer-ring-add (bfr-ring-name bring)
                      buffer)
     (should (dyn-ring-contains-p (bfr-ring-ring (bfr-current-ring))
                                  buffer))))
  (fixture-1-0
   (lambda ()
     (buffer-ring-add (bfr-ring-name bring)
                      buffer)
     (should (= 1 (bfr-ring-size)))))
  (fixture-1-0
   (lambda ()
     (buffer-ring-add (bfr-ring-name bring)
                      buffer)
     (should (eq bring
                 (car (bfr-get-rings buffer))))))

  ;; should not add when already present
  (fixture-2-0-1
   (lambda ()
     (should-not (buffer-ring-add (bfr-ring-name bring1)
                                  buffer))))

  (fixture-2-1-1
   (lambda ()
     (should (dyn-ring-contains-p (bfr-ring-ring bring0)
                                  buffer))))
  (fixture-2-1-1
   (lambda ()
     (should (dyn-ring-contains-p (bfr-ring-ring bring1)
                                  buffer))))
  (fixture-2-1-1
   (lambda ()
     (should (member bring0 (bfr-get-rings buffer)))))
  (fixture-2-1-1
   (lambda ()
     (should (member bring1 (bfr-get-rings buffer)))))
  (fixture-2-1-1
   (lambda ()
     (should-not (buffer-ring-add (bfr-ring-name bring1)
                                  buffer))))

  (fixture-3-1-1-2
   (lambda ()
     (should (dyn-ring-contains-p (bfr-ring-ring bring0)
                                  buffer))))
  (fixture-3-1-1-2
   (lambda ()
     (should (dyn-ring-contains-p (bfr-ring-ring bring1)
                                  buffer))))
  (fixture-3-1-1-2
   (lambda ()
     (should-not (dyn-ring-contains-p (bfr-ring-ring bring2)
                                      buffer))))
  (fixture-3-1-1-2
   (lambda ()
     (should (member bring0 (bfr-get-rings buffer)))))
  (fixture-3-1-1-2
   (lambda ()
     (should (member bring1 (bfr-get-rings buffer)))))
  (fixture-3-1-1-2
   (lambda ()
     (should-not (member bring2 (bfr-get-rings buffer)))))
  (fixture-3-1-1-2
   (lambda ()
     (should-not (buffer-ring-add (bfr-ring-name bring1)
                                  buffer))))

  (fixture-3-0-2-2
   (lambda ()
     (should-not (dyn-ring-contains-p (bfr-ring-ring bring0)
                                      buffer))))
  (fixture-3-0-2-2
   (lambda ()
     (should (dyn-ring-contains-p (bfr-ring-ring bring1)
                                  buffer))))
  (fixture-3-0-2-2
   (lambda ()
     (should (dyn-ring-contains-p (bfr-ring-ring bring2)
                                  buffer))))
  (fixture-3-0-2-2
   (lambda ()
     (should-not (member bring0 (bfr-get-rings buffer)))))
  (fixture-3-0-2-2
   (lambda ()
     (should (member bring1 (bfr-get-rings buffer)))))
  (fixture-3-0-2-2
   (lambda ()
     (should (member bring2 (bfr-get-rings buffer)))))
  (fixture-3-0-2-2
   (lambda ()
     (should-not (buffer-ring-add (bfr-ring-name bring1)
                                  buffer))))

  (fixture-3-0-1-3
   (lambda ()
     (should-not (dyn-ring-contains-p (bfr-ring-ring bring0)
                                      buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (should (dyn-ring-contains-p (bfr-ring-ring bring1)
                                  buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (should (dyn-ring-contains-p (bfr-ring-ring bring2)
                                  buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (should-not (member bring0 (bfr-get-rings buffer)))))
  (fixture-3-0-1-3
   (lambda ()
     (should (member bring1 (bfr-get-rings buffer)))))
  (fixture-3-0-1-3
   (lambda ()
     (should (member bring2 (bfr-get-rings buffer)))))
  (fixture-3-0-1-3
   (lambda ()
     (should-not (buffer-ring-add (bfr-ring-name bring1)
                                  buffer)))))

(ert-deftest buffer-ring-delete-test ()
  (fixture-0
   (lambda ()
     (should-not (buffer-ring-delete buffer))))

  (fixture-1-0
   (lambda ()
     (should-not (buffer-ring-delete buffer))))

  (fixture-1-1
   (lambda ()
     (should (buffer-ring-delete buffer))))
  (fixture-1-1
   (lambda ()
     (buffer-ring-delete buffer)
     (should-not (dyn-ring-contains-p (bfr-ring-ring bring)
                                      buffer))))
  (fixture-1-1
   (lambda ()
     (buffer-ring-delete buffer)
     (should (= 0 (dyn-ring-size (bfr-ring-ring bring))))))

  (fixture-3-0-1-3
   (lambda ()
     (bfr-torus-switch-to-ring bfr-1-ring-name)
     (should (buffer-ring-delete buffer))
     (should-not (dyn-ring-contains-p (bfr-ring-ring bring1)
                                      buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (bfr-torus-switch-to-ring bfr-1-ring-name)
     (should (buffer-ring-delete buffer))
     (should-not (member bring1 (bfr-get-rings buffer)))))
  (fixture-3-0-1-3
   (lambda ()
     (bfr-torus-switch-to-ring bfr-1-ring-name)
     (should (buffer-ring-delete buffer))
     (should (= 0 (dyn-ring-size (bfr-ring-ring bring1))))))
  (fixture-3-0-1-3
   (lambda ()
     (bfr-torus-switch-to-ring bfr-2-ring-name)
     (should (buffer-ring-delete buffer))
     (should-not (dyn-ring-contains-p (bfr-ring-ring bring2)
                                      buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (bfr-torus-switch-to-ring bfr-2-ring-name)
     (should (buffer-ring-delete buffer))
     (should-not (member bring2 (bfr-get-rings buffer)))))
  (fixture-3-0-1-3
   (lambda ()
     (bfr-torus-switch-to-ring bfr-2-ring-name)
     (should (buffer-ring-delete buffer))
     (should (= 2 (dyn-ring-size (bfr-ring-ring bring2)))))))

(ert-deftest buffer-ring-next-buffer-test ()

  (setq sut #'buffer-ring-next-buffer)

  (fixture-0
   (lambda ()
     (should-not (funcall sut))))

  (fixture-1-0
   (lambda ()
     (with-current-buffer buffer
       (should-not (funcall sut))
       (should (eq (current-buffer) buffer)))))

  (fixture-1-1
   (lambda ()
     (with-current-buffer buffer
       (should (funcall sut))
       (should (eq (current-buffer) buffer)))))

  ;; when abroad, rotating the ring changes to a ring buffer
  (fixture-1-1
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (should (funcall sut))
             (should (eq (current-buffer) buffer)))
         (kill-buffer new-buf)))))

  ;; head is buf2 initially
  (fixture-1-2
   (lambda ()
     (should (funcall sut))
     (should (eq (current-buffer) buffer))))
  (fixture-1-2
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf2))))

  ;; head is buf3 initially
  (fixture-1-3
   (lambda ()
     (should (funcall sut))
     (should (eq (current-buffer) buf2))))
  (fixture-1-3
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buffer))))
  (fixture-1-3
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf3)))))

(ert-deftest buffer-ring-prev-buffer-test ()

  (setq sut #'buffer-ring-prev-buffer)

  (fixture-0
   (lambda ()
     (should-not (funcall sut))))

  (fixture-1-0
   (lambda ()
     (with-current-buffer buffer
       (should-not (funcall sut))
       (should (eq (current-buffer) buffer)))))

  (fixture-1-1
   (lambda ()
     (with-current-buffer buffer
       (should (funcall sut))
       (should (eq (current-buffer) buffer)))))

  ;; head is buf2 initially
  (fixture-1-2
   (lambda ()
     (should (funcall sut))
     (should (eq (current-buffer) buffer))))
  (fixture-1-2
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf2))))

  ;; head is buf3 initially
  (fixture-1-3
   (lambda ()
     (should (funcall sut))
     (should (eq (current-buffer) buffer))))
  (fixture-1-3
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf2))))
  (fixture-1-3
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf3)))))

(ert-deftest buffer-torus-next-ring-test ()

  (setq sut #'buffer-torus-next-ring)

  (fixture-0
   (lambda ()
     (should-not (funcall sut))))

  (fixture-1-0
   (lambda ()
     (with-current-buffer buffer
       (should (funcall sut))
       (should (eq (current-buffer) buffer)))))

  (fixture-1-1
   (lambda ()
     (with-current-buffer buffer
       (should (funcall sut))
       (should (eq (current-buffer) buffer)))))

  ;; when abroad, rotating the torus changes to a ring buffer
  (fixture-1-1
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (should (funcall sut))
             (should (eq (current-buffer) buffer)))
         (kill-buffer new-buf)))))

  (fixture-2-0-1
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring-add bfr-0-ring-name)
             (bfr-torus-switch-to-ring bfr-0-ring-name)
             (should (funcall sut))
             (should (eq (current-buffer) buffer)))
         (kill-buffer new-buf)))))

  ;; if buffer is present in the new ring but not at head,
  ;; remain on it in the new ring
  (fixture-2-1-1
   ;; buffer is on both rings
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring-add bfr-0-ring-name)
             (bfr-torus-switch-to-ring bfr-0-ring-name)
             ;; go to a buffer not on ring 1
             (dyn-ring-rotate-until (bfr-ring-ring (bfr-current-ring))
                                    #'dyn-ring-rotate-right
                                    (lambda (bfr)
                                      (not (eq buffer bfr))))
             ;; buffer is the only buffer in ring 1
             ;; so it should become current
             (bfr-torus-switch-to-ring bfr-1-ring-name)
             ;; now switching back would a priori return to the head,
             ;; which isn't buffer. But since buffer is present in
             ;; the destination ring, it should remain current
             (should (funcall sut))
             (should (eq (current-buffer) buffer)))
         (kill-buffer new-buf)))))

  ;; does not rotate if all other rings are empty
  (fixture-3-0-1-0
   (lambda ()
     (bfr-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf1
       (should-not (funcall sut))
       (should (eq (current-buffer) buf1))
       (should (eq bring1 (bfr-current-ring))))))

  ;; rotation with an empty ring
  (fixture-3-0-1-1
   (lambda ()
     (bfr-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf1
       (should (funcall sut))
       (should (eq (current-buffer) buf2))
       (should (eq bring2 (bfr-current-ring))))))
  (fixture-3-0-1-1
   (lambda ()
     (bfr-torus-switch-to-ring bfr-2-ring-name)
     (with-current-buffer buf2
       (should (funcall sut))
       (should (eq (current-buffer) buf1))
       (should (eq bring1 (bfr-current-ring))))))

  ;; rotation with same buffer in multiple rings
  (fixture-3-0-1-3
   (lambda ()
     (bfr-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf1
       (should (funcall sut))
       (should (eq (current-buffer) buf1)) ; stays in same buffer
       (should (eq bring2 (bfr-current-ring))))))
  (fixture-3-0-1-3
   (lambda ()
     (bfr-torus-switch-to-ring bfr-2-ring-name)
     (with-current-buffer buf1
       (should (funcall sut))
       (should (eq (current-buffer) buf1)) ; stays in same buffer
       (should (eq bring1 (bfr-current-ring)))))))

;;
;; External triggers
;;

(ert-deftest buffer-is-killed-test ()
  (fixture-1-0
   (lambda ()
     (kill-buffer buffer)
     (should (dyn-ring-empty-p (bfr-ring-ring (bfr-current-ring))))))

  (fixture-1-1
   (lambda ()
     (kill-buffer buffer)
     (should-not (dyn-ring-contains-p (bfr-ring-ring bring)
                                      buffer))))
  (fixture-1-1
   (lambda ()
     (let ((key (bfr-registry-get-key buffer)))
       (kill-buffer buffer)
       (should-not (member bring (ht-get buffer-rings key))))))
  (fixture-1-1
   (lambda ()
     (kill-buffer buffer)
     (should (= 0 (dyn-ring-size (bfr-ring-ring bring))))))

  (fixture-3-0-1-3
   (lambda ()
     (kill-buffer buffer)
     (should-not (dyn-ring-contains-p (bfr-ring-ring bring1)
                                      buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (kill-buffer buffer)
     (should-not (dyn-ring-contains-p (bfr-ring-ring bring2)
                                      buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (let ((key (bfr-registry-get-key buffer)))
       (kill-buffer buffer)
       (should-not (member bring1 (ht-get buffer-rings key))))))
  (fixture-3-0-1-3
   (lambda ()
     (let ((key (bfr-registry-get-key buffer)))
       (kill-buffer buffer)
       (should-not (member bring2 (ht-get buffer-rings key))))))
  (fixture-3-0-1-3
   (lambda ()
     (kill-buffer buffer)
     (should (= 0 (dyn-ring-size (bfr-ring-ring bring1))))))
  (fixture-3-0-1-3
   (lambda ()
     (kill-buffer buffer)
     (should (= 2 (dyn-ring-size (bfr-ring-ring bring2)))))))
