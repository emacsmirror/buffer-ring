;; Note: we want to retain dynamic binding for these tests because the
;; ERT "fixtures" rely on it.

;; To run the tests from within Emacs, you must `eval-buffer` this test
;; buffer first. Then, run tests using `ert-run-tests-interactively`.

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

(require 'dynaring)
(require 'buffer-ring)
(require 'ht)

;;
;; Fixtures
;;

;; TODO: maybe write macros to spin up fixtures based on
;; a conveniently indicated specification, since in these
;; tests we'd like to be able to say, this many rings with
;; this many buffers (as we are already doing) but also,
;; it'd be nice to indicate _which_ buffers go in which
;; rings without having to manually code that
;; so instead of fixture-N-M-...
;; maybe fixture-N-ABC-AB-0

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
        (progn (setq buffer-ring-torus (dynaring-make))
               (setq buffer-rings (ht))
               (setq buffer (generate-new-buffer bfr-test-name-prefix))
               (funcall body))
      (kill-buffer buffer)
      (let ((bring0 (buffer-ring-torus-get-ring bfr-new-ring-name)))
        (when bring0
          (dynaring-destroy (buffer-ring-ring-ring bring0))))
      (dynaring-destroy buffer-ring-torus))))

(defun fixture-1-0 (body)
  ;; 1 empty buffer ring
  ;; an unaffiliated buffer
  (let ((bring0 nil)
        (buffer nil))
    (unwind-protect
        (progn (setq buffer-ring-torus (dynaring-make))
               (setq buffer-rings (ht))
               (setq bring0 (buffer-ring-torus-get-ring bfr-0-ring-name)
                     buffer (generate-new-buffer bfr-test-name-prefix))
               (funcall body))
      (kill-buffer buffer)
      (dynaring-destroy buffer-ring-torus)
      (dynaring-destroy (buffer-ring-ring-ring bring0)))))

(defun fixture-1-A (body2)
  (fixture-1-0
   (lambda ()
     (buffer-ring--add-buffer-to-ring buffer bring0)
     (funcall body2))))

(defun fixture-1-AB (body3)
  (fixture-1-A
   (lambda ()
     (let ((buf2 nil))
       (unwind-protect
           (progn
             (setq buf2 (generate-new-buffer bfr-test-name-prefix))
             (buffer-ring--add-buffer-to-ring buf2 bring0)
             (funcall body3))
         (kill-buffer buf2))))))

(defun fixture-1-ABC (body4)
  (fixture-1-AB
   (lambda ()
     (let ((buf3 nil))
       (unwind-protect
           (progn
             (setq buf3 (generate-new-buffer bfr-test-name-prefix))
             (buffer-ring--add-buffer-to-ring buf3 bring0)
             (funcall body4))
         (kill-buffer buf3))))))

(defun fixture-2-0-1 (body)
  ;; 2 buffer rings: empty, 1 element
  (let ((bring0 nil)
        (bring1 nil)
        (buffer nil))
    (unwind-protect
        (progn
          (setq buffer-ring-torus (dynaring-make))
          (setq buffer-rings (ht))
          (setq bring0 (buffer-ring-torus-get-ring bfr-0-ring-name)
                bring1 (buffer-ring-torus-get-ring bfr-1-ring-name)
                buffer (generate-new-buffer bfr-test-name-prefix))
          (buffer-ring--add-buffer-to-ring buffer bring1)
          (funcall body))
      (kill-buffer buffer)
      (dynaring-destroy buffer-ring-torus)
      (dynaring-destroy (buffer-ring-ring-ring bring0))
      (dynaring-destroy (buffer-ring-ring-ring bring1)))))

(defun fixture-2-1-1 (body2)
  ;; 2 buffer rings: empty, 1 element
  ;; add a buffer to the empty ring
  (fixture-2-0-1
   (lambda ()
     (buffer-ring--add-buffer-to-ring buffer bring0)
     (funcall body2))))

(defun fixture-3-0-1-0 (body)
  ;; 3 buffer rings: empty, 1 element, empty
  ;; [2] - 1 - 0 - [2]
  (let ((bring0 nil)
        (bring1 nil)
        (bring2 nil)
        (buf1 nil))
    (unwind-protect
        (progn
          (setq buffer-ring-torus (dynaring-make))
          (setq buffer-rings (ht))
          (setq bring0 (buffer-ring-torus-get-ring bfr-0-ring-name)
                bring1 (buffer-ring-torus-get-ring bfr-1-ring-name)
                bring2 (buffer-ring-torus-get-ring bfr-2-ring-name)
                buf1 (generate-new-buffer bfr-test-name-prefix))
          (buffer-ring--add-buffer-to-ring buf1 bring1)
          (funcall body))
      (kill-buffer buf1)
      (dynaring-destroy buffer-ring-torus)
      (dynaring-destroy (buffer-ring-ring-ring bring0))
      (dynaring-destroy (buffer-ring-ring-ring bring1))
      (dynaring-destroy (buffer-ring-ring-ring bring2)))))

(defun fixture-3-0-1-1 (body2)
  ;; 3 buffer rings: empty, 1 element, 1 element
  ;; [2] - 1 - 0 - [2]
  (fixture-3-0-1-0
   (lambda ()
     (let ((buf2 nil))
       (unwind-protect
           (progn
             (setq buf2 (generate-new-buffer bfr-test-name-prefix))
             (buffer-ring--add-buffer-to-ring buf2 bring2)
             (funcall body2))
         (kill-buffer buf2))))))

(defun fixture-3-1-1-1 (body3)
  ;; 3 buffer rings: 1 distinct buffer in each
  ;; [2] - 1 - 0 - [2]
  (fixture-3-0-1-1
   (lambda ()
     (let ((buf3 nil))
       (unwind-protect
           (progn
             (setq buf3 (generate-new-buffer bfr-test-name-prefix))
             (buffer-ring--add-buffer-to-ring buf3 bring0)
             (funcall body3))
         (kill-buffer buf3))))))

(defun fixture-3-0-1-2 (body3)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  (fixture-3-0-1-1
   (lambda ()
     (let ((buf3 nil))
       (unwind-protect
           (progn
             (setq buf3 (generate-new-buffer bfr-test-name-prefix))
             (buffer-ring--add-buffer-to-ring buf3 bring2)
             (funcall body3))
         (kill-buffer buf3))))))

(defun fixture-3-1-1-2 (body4)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  ;; add a buffer to the empty ring
  (fixture-3-0-1-2
   (lambda ()
     (let ((buffer buf1))
       (buffer-ring--add-buffer-to-ring buffer bring0)
       (funcall body4)))))

(defun fixture-3-0-2-2 (body4)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  ;; add a buffer to the 1 ring
  (fixture-3-0-1-2
   (lambda ()
     (let ((buffer buf2))
       (buffer-ring--add-buffer-to-ring buffer bring1)
       (funcall body4)))))

(defun fixture-3-0-1-3 (body4)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  ;; add a buffer to the 2 ring
  (fixture-3-0-1-2
   (lambda ()
     (let ((buffer buf1))
       (buffer-ring--add-buffer-to-ring buffer bring2)
       (funcall body4)))))

;;
;; Test utilities
;;



;;
;; Tests
;;

(ert-deftest buffer-ring-test ()
  ;; null constructor
  (should (buffer-ring-make-ring bfr-0-ring-name))

  ;; buffer-ring-ring-name
  (let ((bfr-ring (buffer-ring-make-ring bfr-0-ring-name)))
    (should (equal bfr-0-ring-name (buffer-ring-ring-name bfr-ring))))

  ;; buffer-ring-ring-ring
  (let ((bfr-ring (buffer-ring-make-ring bfr-0-ring-name)))
    (should (buffer-ring-ring-ring bfr-ring))))

(ert-deftest buffer-torus-test ()
  ;; create new ring
  (should (buffer-ring-torus-get-ring bfr-0-ring-name))

  ;; get existing ring
  (let ((bfr-ring (buffer-ring-torus-get-ring bfr-0-ring-name)))
    (should (eq bfr-ring (buffer-ring-torus-get-ring bfr-0-ring-name)))))

(ert-deftest buffer-ring-torus-switch-to-ring-test ()
  ;; does not switch to non-existent ring
  (fixture-2-0-1
   (lambda ()
     (should-not (buffer-ring-torus-switch-to-ring "non-existent"))))

  ;; switch to ring preserves buffer if present
  (fixture-2-1-1
   ;; buffer is on both rings
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring--add-buffer-to-ring new-buf bring0)
             (buffer-ring-torus-switch-to-ring bfr-0-ring-name)
             ;; go to a buffer not on ring 1
             (dynaring-rotate-until (buffer-ring-ring-ring (buffer-ring-current-ring))
                                    #'dynaring-rotate-right
                                    (lambda (bfr)
                                      (not (eq buffer bfr))))
             ;; buffer is the only buffer in ring 1
             ;; so it should become current
             (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
             ;; now switching back would a priori return to the head,
             ;; which isn't buffer. But since buffer is present in
             ;; the destination ring, it should remain current
             (should (buffer-ring-torus-switch-to-ring bfr-0-ring-name))
             (should (eq (current-buffer) buffer)))
         (kill-buffer new-buf)))))

  ;; reorders ring to account for recency
  (fixture-3-1-1-1
   ;; [2] - 1 - 0 - [2]
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     ;; now changes to [1] - 2 - 0 - [1]
     ;; which is a different ordering under the
     ;; same rotation
     (should (eq bring1 (buffer-ring-current-ring)))
     (buffer-ring-torus-next-ring)
     (should (eq bring2 (buffer-ring-current-ring))))))

(ert-deftest buffer-ring-torus-delete-ring-test ()
  (fixture-2-1-1
   (lambda ()
     (should (buffer-ring-torus-delete-ring))
     (should (= 1 (dynaring-size buffer-ring-torus)))))

  ;; changes ring if current one is deleted
  (fixture-2-1-1
   (lambda ()
     (should (buffer-ring-torus-delete-ring))
     (should (eq bring0 (buffer-ring-current-ring))))))

(ert-deftest buffer-ring-add-test ()
  (fixture-0
   (lambda ()
     (buffer-ring-add bfr-new-ring-name buffer)
     (should (dynaring-contains-p buffer-ring-torus
                                  (car (buffer-ring-get-rings buffer))))))
  (fixture-0
   (lambda ()
     (buffer-ring-add bfr-new-ring-name buffer)
     (should (dynaring-contains-p (buffer-ring-ring-ring (buffer-ring-current-ring))
                                  buffer))
     (should (= 1 (buffer-ring-size)))))
  (fixture-0
   (lambda ()
     (buffer-ring-add bfr-new-ring-name buffer)
     (should (= 1 (buffer-ring-size)))))
  (fixture-0
   (lambda ()
     (buffer-ring-add bfr-new-ring-name buffer)
     (should (eq (buffer-ring-torus-get-ring bfr-new-ring-name)
                 (car (buffer-ring-get-rings buffer))))))

  (fixture-1-0
   (lambda ()
     (buffer-ring-add (buffer-ring-ring-name bring0)
                      buffer)
     (should (dynaring-contains-p buffer-ring-torus
                                  (car (buffer-ring-get-rings buffer))))))
  (fixture-1-0
   (lambda ()
     (buffer-ring-add (buffer-ring-ring-name bring0)
                      buffer)
     (should (dynaring-contains-p (buffer-ring-ring-ring (buffer-ring-current-ring))
                                  buffer))))
  (fixture-1-0
   (lambda ()
     (buffer-ring-add (buffer-ring-ring-name bring0)
                      buffer)
     (should (= 1 (buffer-ring-size)))))
  (fixture-1-0
   (lambda ()
     (buffer-ring-add (buffer-ring-ring-name bring0)
                      buffer)
     (should (eq bring0
                 (car (buffer-ring-get-rings buffer))))))

  ;; should not add when already present
  (fixture-2-0-1
   (lambda ()
     (should-not (buffer-ring-add (buffer-ring-ring-name bring1)
                                  buffer))))

  (fixture-2-1-1
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring bring0)
                                  buffer))))
  (fixture-2-1-1
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring bring1)
                                  buffer))))
  (fixture-2-1-1
   (lambda ()
     (should (member bring0 (buffer-ring-get-rings buffer)))))
  (fixture-2-1-1
   (lambda ()
     (should (member bring1 (buffer-ring-get-rings buffer)))))
  (fixture-2-1-1
   (lambda ()
     (should-not (buffer-ring-add (buffer-ring-ring-name bring1)
                                  buffer))))
  (fixture-2-1-1
   (lambda ()
     ;; current ring is bring1 - the buffer is already present
     ;; on both bring0 and bring1. we add it to bring0.
     ;; it should fail to add, yet, it should switch to bring0
     ;; as the active ring
     (buffer-ring-add (buffer-ring-ring-name bring0) buffer)
     (should (eq (buffer-ring-current-ring) bring0))))

  (fixture-3-1-1-2
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring bring0)
                                  buffer))))
  (fixture-3-1-1-2
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring bring1)
                                  buffer))))
  (fixture-3-1-1-2
   (lambda ()
     (should-not (dynaring-contains-p (buffer-ring-ring-ring bring2)
                                      buffer))))
  (fixture-3-1-1-2
   (lambda ()
     (should (member bring0 (buffer-ring-get-rings buffer)))))
  (fixture-3-1-1-2
   (lambda ()
     (should (member bring1 (buffer-ring-get-rings buffer)))))
  (fixture-3-1-1-2
   (lambda ()
     (should-not (member bring2 (buffer-ring-get-rings buffer)))))
  (fixture-3-1-1-2
   (lambda ()
     (should-not (buffer-ring-add (buffer-ring-ring-name bring1)
                                  buffer))))

  (fixture-3-0-2-2
   (lambda ()
     (should-not (dynaring-contains-p (buffer-ring-ring-ring bring0)
                                      buffer))))
  (fixture-3-0-2-2
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring bring1)
                                  buffer))))
  (fixture-3-0-2-2
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring bring2)
                                  buffer))))
  (fixture-3-0-2-2
   (lambda ()
     (should-not (member bring0 (buffer-ring-get-rings buffer)))))
  (fixture-3-0-2-2
   (lambda ()
     (should (member bring1 (buffer-ring-get-rings buffer)))))
  (fixture-3-0-2-2
   (lambda ()
     (should (member bring2 (buffer-ring-get-rings buffer)))))
  (fixture-3-0-2-2
   (lambda ()
     (should-not (buffer-ring-add (buffer-ring-ring-name bring1)
                                  buffer))))

  (fixture-3-0-1-3
   (lambda ()
     (should-not (dynaring-contains-p (buffer-ring-ring-ring bring0)
                                      buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring bring1)
                                  buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring bring2)
                                  buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (should-not (member bring0 (buffer-ring-get-rings buffer)))))
  (fixture-3-0-1-3
   (lambda ()
     (should (member bring1 (buffer-ring-get-rings buffer)))))
  (fixture-3-0-1-3
   (lambda ()
     (should (member bring2 (buffer-ring-get-rings buffer)))))
  (fixture-3-0-1-3
   (lambda ()
     (should-not (buffer-ring-add (buffer-ring-ring-name bring1)
                                  buffer))))

  ;; should change to a ring when a buffer is added to it
  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-add (buffer-ring-ring-name bring0)
                      buffer)
     (should (eq bring0 (buffer-ring-current-ring))))))

(ert-deftest buffer-ring-delete-test ()
  (fixture-0
   (lambda ()
     (should-not (buffer-ring-delete buffer))))

  (fixture-1-0
   (lambda ()
     (should-not (buffer-ring-delete buffer))))

  (fixture-1-A
   (lambda ()
     (should (buffer-ring-delete buffer))))
  (fixture-1-A
   (lambda ()
     (buffer-ring-delete buffer)
     (should-not (dynaring-contains-p (buffer-ring-ring-ring bring0)
                                      buffer))))
  (fixture-1-A
   (lambda ()
     (buffer-ring-delete buffer)
     (should (= 0 (dynaring-size (buffer-ring-ring-ring bring0))))))

  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (should (buffer-ring-delete buffer))
     (should-not (dynaring-contains-p (buffer-ring-ring-ring bring1)
                                      buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (should (buffer-ring-delete buffer))
     (should-not (member bring1 (buffer-ring-get-rings buffer)))))
  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (should (buffer-ring-delete buffer))
     (should (= 0 (dynaring-size (buffer-ring-ring-ring bring1))))))
  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (should (buffer-ring-delete buffer))
     (should-not (dynaring-contains-p (buffer-ring-ring-ring bring2)
                                      buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (should (buffer-ring-delete buffer))
     (should-not (member bring2 (buffer-ring-get-rings buffer)))))
  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (should (buffer-ring-delete buffer))
     (should (= 2 (dynaring-size (buffer-ring-ring-ring bring2)))))))

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

  (fixture-1-A
   (lambda ()
     (with-current-buffer buffer
       (should (funcall sut))
       (should (eq (current-buffer) buffer)))))

  ;; when abroad, rotating the ring changes to a native buffer
  (fixture-1-A
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (should (funcall sut))
             (should (eq (current-buffer) buffer)))
         (kill-buffer new-buf)))))

  ;; head is buf2 initially
  (fixture-1-AB
   (lambda ()
     (should (funcall sut))
     (should (eq (current-buffer) buffer))))
  (fixture-1-AB
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf2))))

  ;; head is buf3 initially
  (fixture-1-ABC
   (lambda ()
     (should (funcall sut))
     (should (eq (current-buffer) buf2))))
  (fixture-1-ABC
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buffer))))
  (fixture-1-ABC
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

  (fixture-1-A
   (lambda ()
     (with-current-buffer buffer
       (should (funcall sut))
       (should (eq (current-buffer) buffer)))))

  ;; head is buf2 initially
  (fixture-1-AB
   (lambda ()
     (should (funcall sut))
     (should (eq (current-buffer) buffer))))
  (fixture-1-AB
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf2))))

  ;; head is buf3 initially
  (fixture-1-ABC
   (lambda ()
     (should (funcall sut))
     (should (eq (current-buffer) buffer))))
  (fixture-1-ABC
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf2))))
  (fixture-1-ABC
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf3)))))

(ert-deftest buffer-ring-torus-next-ring-test ()

  (setq sut #'buffer-ring-torus-next-ring)

  (fixture-0
   (lambda ()
     (should-not (funcall sut))))

  (fixture-1-0
   (lambda ()
     (with-current-buffer buffer
       (should (funcall sut))
       (should (eq (current-buffer) buffer)))))

  (fixture-1-A
   (lambda ()
     (with-current-buffer buffer
       (should (funcall sut))
       (should (eq (current-buffer) buffer)))))

  ;; when abroad, rotating the torus changes to a native buffer
  (fixture-1-A
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
             (buffer-ring--add-buffer-to-ring new-buf bring0)
             (buffer-ring-torus-switch-to-ring bfr-0-ring-name)
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
             (buffer-ring--add-buffer-to-ring new-buf bring0)
             (buffer-ring-torus-switch-to-ring bfr-0-ring-name)
             ;; go to a buffer not on ring 1
             (dynaring-rotate-until (buffer-ring-ring-ring (buffer-ring-current-ring))
                                    #'dynaring-rotate-right
                                    (lambda (bfr)
                                      (not (eq buffer bfr))))
             ;; buffer is the only buffer in ring 1
             ;; so it should become current
             (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
             ;; now switching back would a priori return to the head,
             ;; which isn't buffer. But since buffer is present in
             ;; the destination ring, it should remain current
             (should (funcall sut))
             (should (eq (current-buffer) buffer)))
         (kill-buffer new-buf)))))

  ;; does not rotate if all other rings are empty
  (fixture-3-0-1-0
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf1
       (should-not (funcall sut))
       (should (eq (current-buffer) buf1))
       (should (eq bring1 (buffer-ring-current-ring))))))

  ;; rotation with an empty ring
  (fixture-3-0-1-1
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf1
       (should (funcall sut))
       (should (eq (current-buffer) buf2))
       (should (eq bring2 (buffer-ring-current-ring))))))
  (fixture-3-0-1-1
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (with-current-buffer buf2
       (should (funcall sut))
       (should (eq (current-buffer) buf1))
       (should (eq bring1 (buffer-ring-current-ring))))))

  ;; simple rotation with a 3-ring
  (fixture-3-1-1-1
   (lambda ()
     ;; current ring is 2
     ;; current buffer is buf2
     (with-current-buffer buf2
       (should (funcall sut))
       (should (eq (current-buffer) buf1))
       (should (eq bring1 (buffer-ring-current-ring))))))
  (fixture-3-1-1-1
   (lambda ()
     (with-current-buffer buf2
       (should (progn (funcall sut)
                      (funcall sut)))
       (should (eq (current-buffer) buf3))
       (should (eq bring0 (buffer-ring-current-ring))))))
  (fixture-3-1-1-1
   (lambda ()
     (with-current-buffer buf2
       (should (progn (funcall sut)
                      (funcall sut)
                      (funcall sut)))
       (should (eq (current-buffer) buf2))
       (should (eq bring2 (buffer-ring-current-ring))))))

  ;; rotation with same buffer in multiple rings
  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf1
       (should (funcall sut))
       (should (eq (current-buffer) buf1)) ; stays in same buffer
       (should (eq bring2 (buffer-ring-current-ring))))))
  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (with-current-buffer buf1
       (should (funcall sut))
       (should (eq (current-buffer) buf1)) ; stays in same buffer
       (should (eq bring1 (buffer-ring-current-ring)))))))

(ert-deftest buffer-ring-torus-prev-ring-test ()

  (setq sut #'buffer-ring-torus-prev-ring)

  (fixture-0
   (lambda ()
     (should-not (funcall sut))))

  (fixture-1-0
   (lambda ()
     (with-current-buffer buffer
       (should (funcall sut))
       (should (eq (current-buffer) buffer)))))

  (fixture-1-A
   (lambda ()
     (with-current-buffer buffer
       (should (funcall sut))
       (should (eq (current-buffer) buffer)))))

  ;; when abroad, rotating the torus changes to a native buffer
  (fixture-1-A
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
             (buffer-ring--add-buffer-to-ring new-buf bring0)
             (buffer-ring-torus-switch-to-ring bfr-0-ring-name)
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
             (buffer-ring--add-buffer-to-ring new-buf bring0)
             (buffer-ring-torus-switch-to-ring bfr-0-ring-name)
             ;; go to a buffer not on ring 1
             (dynaring-rotate-until (buffer-ring-ring-ring (buffer-ring-current-ring))
                                    #'dynaring-rotate-right
                                    (lambda (bfr)
                                      (not (eq buffer bfr))))
             ;; buffer is the only buffer in ring 1
             ;; so it should become current
             (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
             ;; now switching back would a priori return to the head,
             ;; which isn't buffer. But since buffer is present in
             ;; the destination ring, it should remain current
             (should (funcall sut))
             (should (eq (current-buffer) buffer)))
         (kill-buffer new-buf)))))

  ;; does not rotate if all other rings are empty
  (fixture-3-0-1-0
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf1
       (should-not (funcall sut))
       (should (eq (current-buffer) buf1))
       (should (eq bring1 (buffer-ring-current-ring))))))

  ;; rotation with an empty ring
  (fixture-3-0-1-1
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf1
       (should (funcall sut))
       (should (eq (current-buffer) buf2))
       (should (eq bring2 (buffer-ring-current-ring))))))
  (fixture-3-0-1-1
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (with-current-buffer buf2
       (should (funcall sut))
       (should (eq (current-buffer) buf1))
       (should (eq bring1 (buffer-ring-current-ring))))))

  ;; simple rotation with a 3-ring
  (fixture-3-1-1-1
   (lambda ()
     ;; current ring is 2
     ;; current buffer is buf2
     (with-current-buffer buf2
       (should (funcall sut))
       (should (eq (current-buffer) buf3))
       (should (eq bring0 (buffer-ring-current-ring))))))
  (fixture-3-1-1-1
   (lambda ()
     (with-current-buffer buf2
       (should (progn (funcall sut)
                      (funcall sut)))
       (should (eq (current-buffer) buf1))
       (should (eq bring1 (buffer-ring-current-ring))))))
  (fixture-3-1-1-1
   (lambda ()
     (with-current-buffer buf2
       (should (progn (funcall sut)
                      (funcall sut)
                      (funcall sut)))
       (should (eq (current-buffer) buf2))
       (should (eq bring2 (buffer-ring-current-ring))))))

  ;; rotation with same buffer in multiple rings
  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf1
       (should (funcall sut))
       (should (eq (current-buffer) buf1)) ; stays in same buffer
       (should (eq bring2 (buffer-ring-current-ring))))))
  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (with-current-buffer buf1
       (should (funcall sut))
       (should (eq (current-buffer) buf1)) ; stays in same buffer
       (should (eq bring1 (buffer-ring-current-ring)))))))

;;
;; External triggers
;;

(ert-deftest buffer-is-killed-test ()
  (fixture-1-0
   (lambda ()
     (kill-buffer buffer)
     (should (dynaring-empty-p (buffer-ring-ring-ring (buffer-ring-current-ring))))))

  (fixture-1-A
   (lambda ()
     (kill-buffer buffer)
     (should-not (dynaring-contains-p (buffer-ring-ring-ring bring0)
                                      buffer))))
  (fixture-1-A
   (lambda ()
     (let ((key (buffer-ring-registry-get-key buffer)))
       (kill-buffer buffer)
       (should-not (member bring0 (ht-get buffer-rings key))))))
  (fixture-1-A
   (lambda ()
     (kill-buffer buffer)
     (should (= 0 (dynaring-size (buffer-ring-ring-ring bring0))))))

  (fixture-3-0-1-3
   (lambda ()
     (kill-buffer buffer)
     (should-not (dynaring-contains-p (buffer-ring-ring-ring bring1)
                                      buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (kill-buffer buffer)
     (should-not (dynaring-contains-p (buffer-ring-ring-ring bring2)
                                      buffer))))
  (fixture-3-0-1-3
   (lambda ()
     (let ((key (buffer-ring-registry-get-key buffer)))
       (kill-buffer buffer)
       (should-not (member bring1 (ht-get buffer-rings key))))))
  (fixture-3-0-1-3
   (lambda ()
     (let ((key (buffer-ring-registry-get-key buffer)))
       (kill-buffer buffer)
       (should-not (member bring2 (ht-get buffer-rings key))))))
  (fixture-3-0-1-3
   (lambda ()
     (kill-buffer buffer)
     (should (= 0 (dynaring-size (buffer-ring-ring-ring bring1))))))
  (fixture-3-0-1-3
   (lambda ()
     (kill-buffer buffer)
     (should (= 2 (dynaring-size (buffer-ring-ring-ring bring2)))))))

(ert-deftest buffer-ring-visit-buffer-test ()
  ;; if buffer is unaffiliated, nothing happens
  (fixture-2-0-1
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring-visit-buffer)
             (should (eq bring1 (buffer-ring-current-ring))))
         (kill-buffer new-buf)))))

  ;; if buffer is in current ring and already at head, nothing happens
  (fixture-3-0-1-3
   (lambda ()
     (with-current-buffer buffer
       (buffer-ring-visit-buffer)
       (should (eq bring2 (buffer-ring-current-ring)))
       (should (eq buffer (buffer-ring-current-buffer))))))

  ;; if buffer is in current ring and not at head, it is reinserted
  (fixture-3-0-1-3
   (lambda ()
     (with-current-buffer buf3
       (buffer-ring-visit-buffer)
       (should (eq bring2 (buffer-ring-current-ring)))
       (should (eq buf3 (buffer-ring-current-buffer))))))

  ;; if buffer is in a different ring, current ring is changed
  (fixture-3-0-1-2
   (lambda ()
     (with-current-buffer buf1
       (buffer-ring-visit-buffer)
       (should (eq bring1 (buffer-ring-current-ring)))
       (should (eq buf1 (buffer-ring-current-buffer))))))

  ;; buffer is current in every ring that it is part of
  (fixture-3-0-1-3
   (lambda ()
     (with-current-buffer buf1
       (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
       (buffer-ring-next-buffer)
       ;; validate that buf1 is no longer current in ring 2
       (should-not (eq buf1 (buffer-ring-current-buffer bring2)))
       (buffer-ring-torus-switch-to-ring bfr-0-ring-name)
       (buffer-ring-visit-buffer buf1)
       (should (eq buf1 (buffer-ring-current-buffer bring1)))
       (should (eq buf1 (buffer-ring-current-buffer bring2)))))))

(ert-deftest buffer-ring-set-buffer-context-test ()
  ;; if buffer is in a different ring, current ring is changed
  ;; and it is placed at the head
  (fixture-3-0-1-2
   (lambda ()
     (with-current-buffer buf1
       (buffer-ring-set-buffer-context)
       (should (eq bring1 (buffer-ring-current-ring)))
       (should (eq buf1 (buffer-ring-current-buffer)))))))

(ert-deftest buffer-ring-surface-ring-test ()
  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-surface-ring bring1)
     (should (eq bring1 (car (buffer-ring-get-rings buf1))))))
  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-surface-ring bring1)
     (dolist (buf (list buf1 buf2 buf3))
       (buffer-ring-register-ring buf bring1))
     (buffer-ring-surface-ring bring2)
     (should (eq bring2 (car (buffer-ring-get-rings buf1))))
     (should (eq bring2 (car (buffer-ring-get-rings buf2))))
     (should (eq bring2 (car (buffer-ring-get-rings buf3)))))))

;;
;; "Integration" tests
;;

(ert-deftest buffer-ring-switching-buffers-and-rings-test ()
  (fixture-3-0-1-3
   (lambda ()
     (buffer-ring-initialize)
     (let ((bring3 (buffer-ring-torus-get-ring "new-ring"))
           (new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring-add "new-ring" new-buf)
             (should (eq bring3 (buffer-ring-current-ring)))
             ;; switch to buf2 in bring2 so it's at head
             (switch-to-buffer buf2)
             ;; switch to bring1, buf1 now points to bring1
             (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
             (should (eq bring1 (car (buffer-ring-get-rings buf1))))
             ;; then switch to the new buffer in bring3
             (switch-to-buffer new-buf)
             (should (eq bring3 (buffer-ring-current-ring)))
             ;; then switch to bring2 which should set buffer to buf2
             (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
             (should (eq buf2 (current-buffer)))
             ;; ensure that buf1 (not active but in ring) points to bring2
             (should (eq bring2 (car (buffer-ring-get-rings buf1)))))
         (kill-buffer new-buf)
         (dynaring-destroy (buffer-ring-ring-ring bring3))))
     (buffer-ring-disable))))
