;; Note: we want to retain dynamic binding for these tests because the
;; ERT "fixtures" rely on it.

;; To run the tests from within Emacs, you must `eval-buffer` this test
;; buffer first. Then, run tests using `ert-run-tests-interactively`.

;; Note: if you see "lisp nesting exceeds max-lisp-eval-depth"
;; while running these tests, it could be that you have a duplicate
;; "body" invocation within one of the nested fixtures. Since these
;; are dynamically bound, every fixture needs to have a distinct
;; name for the body argument.

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

(defvar bfr-test-name-prefix "bfr-test")
(defvar bfr-new-ring-name "bfr-test-new-ring")
(defvar bfr-0-ring-name "bfr-test-ring-0")
(defvar bfr-1-ring-name "bfr-test-ring-1")
(defvar bfr-2-ring-name "bfr-test-ring-2")

;; fixture recipe from:
;; https://www.gnu.org/software/emacs/manual/html_node/ert/Fixtures-and-Test-Suites.html
(defun fixture-buffers-A (body-buf-a)
  (let* ((buf-A nil))
    (unwind-protect
        (progn (setq buf-A (generate-new-buffer bfr-test-name-prefix))
               (funcall body-buf-a))
      (kill-buffer buf-A))))

(defun fixture-buffers-AB (body-buf-ab)
  (fixture-buffers-A
   (lambda ()
     (let ((buf-B nil))
       (unwind-protect
           (progn (setq buf-B (generate-new-buffer bfr-test-name-prefix))
                  (funcall body-buf-ab))
         (kill-buffer buf-B))))))

(defun fixture-buffers-ABC (body-buf-abc)
  (fixture-buffers-AB
   (lambda ()
     (let ((buf-C nil))
       (unwind-protect
           (progn (setq buf-C (generate-new-buffer bfr-test-name-prefix))
                  (funcall body-buf-abc))
         (kill-buffer buf-C))))))

(defun fixture-0 (body-0)
  ;; no buffer rings present
  (unwind-protect
      (progn (setq buffer-ring-torus (dynaring-make))
             (setq buffer-rings (ht))
             (funcall body-0))
    (let ((r1 (buffer-ring-torus-get-ring bfr-new-ring-name)))
      (when r1
        (dynaring-destroy (buffer-ring-ring-ring r1))))
    (dynaring-destroy buffer-ring-torus)))

(defun fixture-0+A (body-0+a)
  ;; no buffer rings present
  ;; an unaffiliated buffer
  (fixture-0
   (lambda ()
     (fixture-buffers-A
      (lambda ()
        (funcall body-0+a))))))

(defun fixture-1 (body-1)
  ;; 1 empty buffer ring
  ;; an unaffiliated buffer
  (fixture-0
   (lambda ()
     (let ((r1 nil))
       (unwind-protect
           (progn (setq r1 (buffer-ring-torus-get-ring bfr-0-ring-name))
                  (funcall body-1))
         (dynaring-destroy (buffer-ring-ring-ring r1)))))))

(defun fixture-1-0+A (body-10+a)
  ;; 1 empty buffer ring
  ;; an unaffiliated buffer
  (fixture-1
   (lambda ()
     (fixture-buffers-A
      (lambda ()
        (funcall body-10+a))))))

(defun fixture-1-A (body-1a)
  (fixture-1
   (lambda ()
     (fixture-buffers-A
      (lambda ()
        (buffer-ring--add-buffer-to-ring buf-A r1)
        (funcall body-1a))))))

(defun fixture-1-AB (body-1ab)
  (fixture-1
   (lambda ()
     (fixture-buffers-AB
      (lambda ()
        (buffer-ring--add-buffer-to-ring buf-A r1)
        (buffer-ring--add-buffer-to-ring buf-B r1)
        (funcall body-1ab))))))

(defun fixture-1-ABC (body-1abc)
  (fixture-1
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add-buffer-to-ring buf-A r1)
        (buffer-ring--add-buffer-to-ring buf-B r1)
        (buffer-ring--add-buffer-to-ring buf-C r1)
        (funcall body-1abc))))))

(defun fixture-2 (body-2)
  ;; 2 buffer rings: both empty
  (fixture-1
   (lambda ()
     (let ((r2 nil))
       (unwind-protect
           (progn
             (setq r2 (buffer-ring-torus-get-ring bfr-1-ring-name))
             (funcall body-2))
         (dynaring-destroy (buffer-ring-ring-ring r2)))))))

(defun fixture-2-0-A (body-20a)
  ;; 2 buffer rings: empty, 1 element
  (fixture-2
   (lambda ()
     (fixture-buffers-A
      (lambda ()
        (buffer-ring--add-buffer-to-ring buf-A r2)
        (funcall body-20a))))))

(defun fixture-2-A-A (body-2aa)
  ;; 2 buffer rings: empty, 1 element
  ;; add a buffer to the empty ring
  (fixture-2
   (lambda ()
     (fixture-buffers-A
      (lambda ()
        (buffer-ring--add-buffer-to-ring buf-A r1)
        (buffer-ring--add-buffer-to-ring buf-A r2)
        (funcall body-2aa))))))

(defun fixture-3 (body-3)
  ;; 3 buffer rings
  ;; [2] - 1 - 0 - [2]
  (fixture-2
   (lambda ()
     (let ((r3 nil))
       (unwind-protect
           (progn
             (setq r3 (buffer-ring-torus-get-ring bfr-2-ring-name))
             (funcall body-3))
         (dynaring-destroy (buffer-ring-ring-ring r3)))))))

(defun fixture-3-0-A-0 (body-30a0)
  ;; 3 buffer rings: empty, 1 element, empty
  ;; [2] - 1 - 0 - [2]
  (fixture-3
   (lambda ()
     (fixture-buffers-A
      (lambda ()
        (buffer-ring--add-buffer-to-ring buf-A r2)
        (funcall body-30a0))))))

(defun fixture-3-0-A-B (body-30ab)
  ;; 3 buffer rings: empty, 1 element, 1 element
  ;; [2] - 1 - 0 - [2]
  (fixture-3
   (lambda ()
     (fixture-buffers-AB
      (lambda ()
        (buffer-ring--add-buffer-to-ring buf-A r2)
        (buffer-ring--add-buffer-to-ring buf-B r3)
        (funcall body-30ab))))))

(defun fixture-3-C-A-B (body-3cab)
  ;; 3 buffer rings: 1 distinct buffer in each
  ;; [2] - 1 - 0 - [2]
  (fixture-3
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add-buffer-to-ring buf-A r2)
        (buffer-ring--add-buffer-to-ring buf-B r3)
        (buffer-ring--add-buffer-to-ring buf-C r1)
        (funcall body-3cab))))))

(defun fixture-3-0-A-BC (body-3-0-a-bc)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  (fixture-3
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add-buffer-to-ring buf-A r2)
        (buffer-ring--add-buffer-to-ring buf-B r3)
        (buffer-ring--add-buffer-to-ring buf-C r3)
        (funcall body-3-0-a-bc))))))

(defun fixture-3-A-A-BC (body-3-a-a-bc)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  ;; add a buffer to the empty ring
  (fixture-3
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add-buffer-to-ring buf-A r1)
        (buffer-ring--add-buffer-to-ring buf-A r2)
        (buffer-ring--add-buffer-to-ring buf-B r3)
        (buffer-ring--add-buffer-to-ring buf-C r3)
        (funcall body-3-a-a-bc))))))

(defun fixture-3-0-AB-BC (body-3-0-ab-bc)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  ;; add a buffer to the 1 ring
  (fixture-3
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add-buffer-to-ring buf-A r2)
        (buffer-ring--add-buffer-to-ring buf-B r2)
        (buffer-ring--add-buffer-to-ring buf-B r3)
        (buffer-ring--add-buffer-to-ring buf-C r3)
        (funcall body-3-0-ab-bc))))))

(defun fixture-3-0-A-ABC (body-3-0-a-abc)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  ;; add a buffer to the 2 ring
  (fixture-3
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add-buffer-to-ring buf-A r2)
        (buffer-ring--add-buffer-to-ring buf-A r3)
        (buffer-ring--add-buffer-to-ring buf-B r3)
        (buffer-ring--add-buffer-to-ring buf-C r3)
        (funcall body-3-0-a-abc))))))

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
  (fixture-2-0-A
   (lambda ()
     (should-not (buffer-ring-torus-switch-to-ring "non-existent"))))

  ;; switch to ring preserves buffer if present
  (fixture-2-A-A
   ;; buffer is on both rings
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring--add-buffer-to-ring new-buf r1)
             (buffer-ring-torus-switch-to-ring bfr-0-ring-name)
             ;; go to a buffer not on ring 1
             (dynaring-rotate-until (buffer-ring-ring-ring (buffer-ring-current-ring))
                                    #'dynaring-rotate-right
                                    (lambda (bfr)
                                      (not (eq buf-A bfr))))
             ;; buffer is the only buffer in ring 1
             ;; so it should become current
             (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
             ;; now switching back would a priori return to the head,
             ;; which isn't buffer. But since buffer is present in
             ;; the destination ring, it should remain current
             (should (buffer-ring-torus-switch-to-ring bfr-0-ring-name))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  ;; reorders ring to account for recency
  (fixture-3-C-A-B
   ;; [2] - 1 - 0 - [2]
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     ;; now changes to [1] - 2 - 0 - [1]
     ;; which is a different ordering under the
     ;; same rotation
     (should (eq r2 (buffer-ring-current-ring)))
     (buffer-ring-torus-next-ring)
     (should (eq r3 (buffer-ring-current-ring))))))

(ert-deftest buffer-ring-torus-delete-ring-test ()
  (fixture-2-A-A
   (lambda ()
     (should (buffer-ring-torus-delete-ring))
     (should (= 1 (dynaring-size buffer-ring-torus)))))

  ;; changes ring if current one is deleted
  (fixture-2-A-A
   (lambda ()
     (should (buffer-ring-torus-delete-ring))
     (should (eq r1 (buffer-ring-current-ring))))))

(ert-deftest buffer-ring-add-test ()
  (fixture-0+A
   (lambda ()
     (buffer-ring-add bfr-new-ring-name buf-A)
     (should (dynaring-contains-p buffer-ring-torus
                                  (car (buffer-ring-get-rings buf-A))))))
  (fixture-0+A
   (lambda ()
     (buffer-ring-add bfr-new-ring-name buf-A)
     (should (dynaring-contains-p (buffer-ring-ring-ring (buffer-ring-current-ring))
                                  buf-A))
     (should (= 1 (buffer-ring-size)))))
  (fixture-0+A
   (lambda ()
     (buffer-ring-add bfr-new-ring-name buf-A)
     (should (= 1 (buffer-ring-size)))))
  (fixture-0+A
   (lambda ()
     (buffer-ring-add bfr-new-ring-name buf-A)
     (should (eq (buffer-ring-torus-get-ring bfr-new-ring-name)
                 (car (buffer-ring-get-rings buf-A))))))

  (fixture-1-0+A
   (lambda ()
     (buffer-ring-add (buffer-ring-ring-name r1)
                      buf-A)
     (should (dynaring-contains-p buffer-ring-torus
                                  (car (buffer-ring-get-rings buf-A))))))
  (fixture-1-0+A
   (lambda ()
     (buffer-ring-add (buffer-ring-ring-name r1)
                      buf-A)
     (should (dynaring-contains-p (buffer-ring-ring-ring (buffer-ring-current-ring))
                                  buf-A))))
  (fixture-1-0+A
   (lambda ()
     (buffer-ring-add (buffer-ring-ring-name r1)
                      buf-A)
     (should (= 1 (buffer-ring-size)))))
  (fixture-1-0+A
   (lambda ()
     (buffer-ring-add (buffer-ring-ring-name r1)
                      buf-A)
     (should (eq r1
                 (car (buffer-ring-get-rings buf-A))))))

  ;; should not add when already present
  (fixture-2-0-A
   (lambda ()
     (should-not (buffer-ring-add (buffer-ring-ring-name r2)
                                  buf-A))))

  (fixture-2-A-A
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring r1)
                                  buf-A))))
  (fixture-2-A-A
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring r2)
                                  buf-A))))
  (fixture-2-A-A
   (lambda ()
     (should (member r1 (buffer-ring-get-rings buf-A)))))
  (fixture-2-A-A
   (lambda ()
     (should (member r2 (buffer-ring-get-rings buf-A)))))
  (fixture-2-A-A
   (lambda ()
     (should-not (buffer-ring-add (buffer-ring-ring-name r2)
                                  buf-A))))
  (fixture-2-A-A
   (lambda ()
     ;; current ring is r2 - the buffer is already present
     ;; on both r1 and r2. we add it to r1.
     ;; it should fail to add, yet, it should switch to r1
     ;; as the active ring
     (buffer-ring-add (buffer-ring-ring-name r1) buf-A)
     (should (eq (buffer-ring-current-ring) r1))))

  (fixture-3-A-A-BC
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring r1)
                                  buf-A))))
  (fixture-3-A-A-BC
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring r2)
                                  buf-A))))
  (fixture-3-A-A-BC
   (lambda ()
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r3)
                                      buf-A))))
  (fixture-3-A-A-BC
   (lambda ()
     (should (member r1 (buffer-ring-get-rings buf-A)))))
  (fixture-3-A-A-BC
   (lambda ()
     (should (member r2 (buffer-ring-get-rings buf-A)))))
  (fixture-3-A-A-BC
   (lambda ()
     (should-not (member r3 (buffer-ring-get-rings buf-A)))))
  (fixture-3-A-A-BC
   (lambda ()
     (should-not (buffer-ring-add (buffer-ring-ring-name r2)
                                  buf-A))))

  (fixture-3-0-AB-BC
   (lambda ()
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r1)
                                      buf-A))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r1)
                                      buf-B))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r1)
                                      buf-C))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring r2)
                                  buf-A))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring r2)
                                  buf-B))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r2)
                                      buf-C))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r3)
                                      buf-A))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring r3)
                                  buf-B))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring r3)
                                  buf-C))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should-not (member r1 (buffer-ring-get-rings buf-A)))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should (member r2 (buffer-ring-get-rings buf-A)))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should-not (member r3 (buffer-ring-get-rings buf-A)))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should-not (member r1 (buffer-ring-get-rings buf-B)))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should (member r2 (buffer-ring-get-rings buf-B)))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should (member r3 (buffer-ring-get-rings buf-B)))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should-not (member r1 (buffer-ring-get-rings buf-C)))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should-not (member r2 (buffer-ring-get-rings buf-C)))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should (member r3 (buffer-ring-get-rings buf-C)))))
  (fixture-3-0-AB-BC
   (lambda ()
     (should-not (buffer-ring-add (buffer-ring-ring-name r2)
                                  buf-A))))

  (fixture-3-0-A-ABC
   (lambda ()
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r1)
                                      buf-A))))
  (fixture-3-0-A-ABC
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring r2)
                                  buf-A))))
  (fixture-3-0-A-ABC
   (lambda ()
     (should (dynaring-contains-p (buffer-ring-ring-ring r3)
                                  buf-A))))
  (fixture-3-0-A-ABC
   (lambda ()
     (should-not (member r1 (buffer-ring-get-rings buf-A)))))
  (fixture-3-0-A-ABC
   (lambda ()
     (should (member r2 (buffer-ring-get-rings buf-A)))))
  (fixture-3-0-A-ABC
   (lambda ()
     (should (member r3 (buffer-ring-get-rings buf-A)))))
  (fixture-3-0-A-ABC
   (lambda ()
     (should-not (buffer-ring-add (buffer-ring-ring-name r2)
                                  buf-A))))

  ;; should change to a ring when a buffer is added to it
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-add (buffer-ring-ring-name r1)
                      buf-A)
     (should (eq r1 (buffer-ring-current-ring))))))

(ert-deftest buffer-ring-delete-test ()
  (fixture-0+A
   (lambda ()
     (should-not (buffer-ring-delete buf-A))))

  (fixture-1-0+A
   (lambda ()
     (should-not (buffer-ring-delete buf-A))))

  (fixture-1-A
   (lambda ()
     (should (buffer-ring-delete buf-A))))
  (fixture-1-A
   (lambda ()
     (buffer-ring-delete buf-A)
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r1)
                                      buf-A))))
  (fixture-1-A
   (lambda ()
     (buffer-ring-delete buf-A)
     (should (= 0 (dynaring-size (buffer-ring-ring-ring r1))))))

  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (should (buffer-ring-delete buf-A))
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r2)
                                      buf-A))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (should (buffer-ring-delete buf-A))
     (should-not (member r2 (buffer-ring-get-rings buf-A)))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (should (buffer-ring-delete buf-A))
     (should (= 0 (dynaring-size (buffer-ring-ring-ring r2))))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (should (buffer-ring-delete buf-A))
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r3)
                                      buf-A))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (should (buffer-ring-delete buf-A))
     (should-not (member r3 (buffer-ring-get-rings buf-A)))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (should (buffer-ring-delete buf-A))
     (should (= 2 (dynaring-size (buffer-ring-ring-ring r3)))))))

(ert-deftest buffer-ring-next-buffer-test ()

  (setq sut #'buffer-ring-next-buffer)

  (fixture-0+A
   (lambda ()
     (should-not (funcall sut))))

  (fixture-1-0+A
   (lambda ()
     (with-current-buffer buf-A
       (should-not (funcall sut))
       (should (eq (current-buffer) buf-A)))))

  (fixture-1-A
   (lambda ()
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-A)))))

  ;; when abroad, rotating the ring changes to a native buffer
  (fixture-1-A
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (should (funcall sut))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  ;; head is buf-B initially
  (fixture-1-AB
   (lambda ()
     (should (funcall sut))
     (should (eq (current-buffer) buf-A))))
  (fixture-1-AB
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf-B))))

  ;; head is buf-C initially
  (fixture-1-ABC
   (lambda ()
     (should (funcall sut))
     (should (eq (current-buffer) buf-B))))
  (fixture-1-ABC
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf-A))))
  (fixture-1-ABC
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf-C)))))

(ert-deftest buffer-ring-prev-buffer-test ()

  (setq sut #'buffer-ring-prev-buffer)

  (fixture-0+A
   (lambda ()
     (should-not (funcall sut))))

  (fixture-1-0+A
   (lambda ()
     (with-current-buffer buf-A
       (should-not (funcall sut))
       (should (eq (current-buffer) buf-A)))))

  (fixture-1-A
   (lambda ()
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-A)))))

  ;; head is buf-B initially
  (fixture-1-AB
   (lambda ()
     (should (funcall sut))
     (should (eq (current-buffer) buf-A))))
  (fixture-1-AB
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf-B))))

  ;; head is buf-C initially
  (fixture-1-ABC
   (lambda ()
     (should (funcall sut))
     (should (eq (current-buffer) buf-A))))
  (fixture-1-ABC
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf-B))))
  (fixture-1-ABC
   (lambda ()
     (should
      (progn (funcall sut)
             (funcall sut)
             (funcall sut)))
     (should (eq (current-buffer) buf-C)))))

(ert-deftest buffer-ring-torus-next-ring-test ()

  (setq sut #'buffer-ring-torus-next-ring)

  (fixture-0+A
   (lambda ()
     (should-not (funcall sut))))

  (fixture-1-0+A
   (lambda ()
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-A)))))

  (fixture-1-A
   (lambda ()
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-A)))))

  ;; when abroad, rotating the torus changes to a native buffer
  (fixture-1-A
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (should (funcall sut))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  (fixture-2-0-A
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring--add-buffer-to-ring new-buf r1)
             (buffer-ring-torus-switch-to-ring bfr-0-ring-name)
             (should (funcall sut))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  ;; if buffer is present in the new ring but not at head,
  ;; remain on it in the new ring
  (fixture-2-A-A
   ;; buffer is on both rings
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring--add-buffer-to-ring new-buf r1)
             (buffer-ring-torus-switch-to-ring bfr-0-ring-name)
             ;; go to a buffer not on ring 1
             (dynaring-rotate-until (buffer-ring-ring-ring (buffer-ring-current-ring))
                                    #'dynaring-rotate-right
                                    (lambda (bfr)
                                      (not (eq buf-A bfr))))
             ;; buffer is the only buffer in ring 1
             ;; so it should become current
             (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
             ;; now switching back would a priori return to the head,
             ;; which isn't buffer. But since buffer is present in
             ;; the destination ring, it should remain current
             (should (funcall sut))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  ;; does not rotate if all other rings are empty
  (fixture-3-0-A-0
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf-A
       (should-not (funcall sut))
       (should (eq (current-buffer) buf-A))
       (should (eq r2 (buffer-ring-current-ring))))))

  ;; rotation with an empty ring
  (fixture-3-0-A-B
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-B))
       (should (eq r3 (buffer-ring-current-ring))))))
  (fixture-3-0-A-B
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (with-current-buffer buf-B
       (should (funcall sut))
       (should (eq (current-buffer) buf-A))
       (should (eq r2 (buffer-ring-current-ring))))))

  ;; simple rotation with a 3-ring
  (fixture-3-C-A-B
   (lambda ()
     ;; current ring is 2
     ;; current buffer is buf-B
     (with-current-buffer buf-B
       (should (funcall sut))
       (should (eq (current-buffer) buf-A))
       (should (eq r2 (buffer-ring-current-ring))))))
  (fixture-3-C-A-B
   (lambda ()
     (with-current-buffer buf-B
       (should (progn (funcall sut)
                      (funcall sut)))
       (should (eq (current-buffer) buf-C))
       (should (eq r1 (buffer-ring-current-ring))))))
  (fixture-3-C-A-B
   (lambda ()
     (with-current-buffer buf-B
       (should (progn (funcall sut)
                      (funcall sut)
                      (funcall sut)))
       (should (eq (current-buffer) buf-B))
       (should (eq r3 (buffer-ring-current-ring))))))

  ;; rotation with same buffer in multiple rings
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-A)) ; stays in same buffer
       (should (eq r3 (buffer-ring-current-ring))))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-A)) ; stays in same buffer
       (should (eq r2 (buffer-ring-current-ring)))))))

(ert-deftest buffer-ring-torus-prev-ring-test ()

  (setq sut #'buffer-ring-torus-prev-ring)

  (fixture-0+A
   (lambda ()
     (should-not (funcall sut))))

  (fixture-1-0+A
   (lambda ()
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-A)))))

  (fixture-1-A
   (lambda ()
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-A)))))

  ;; when abroad, rotating the torus changes to a native buffer
  (fixture-1-A
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (should (funcall sut))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  (fixture-2-0-A
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring--add-buffer-to-ring new-buf r1)
             (buffer-ring-torus-switch-to-ring bfr-0-ring-name)
             (should (funcall sut))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  ;; if buffer is present in the new ring but not at head,
  ;; remain on it in the new ring
  (fixture-2-A-A
   ;; buffer is on both rings
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring--add-buffer-to-ring new-buf r1)
             (buffer-ring-torus-switch-to-ring bfr-0-ring-name)
             ;; go to a buffer not on ring 1
             (dynaring-rotate-until (buffer-ring-ring-ring (buffer-ring-current-ring))
                                    #'dynaring-rotate-right
                                    (lambda (bfr)
                                      (not (eq buf-A bfr))))
             ;; buffer is the only buffer in ring 1
             ;; so it should become current
             (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
             ;; now switching back would a priori return to the head,
             ;; which isn't buffer. But since buffer is present in
             ;; the destination ring, it should remain current
             (should (funcall sut))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  ;; does not rotate if all other rings are empty
  (fixture-3-0-A-0
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf-A
       (should-not (funcall sut))
       (should (eq (current-buffer) buf-A))
       (should (eq r2 (buffer-ring-current-ring))))))

  ;; rotation with an empty ring
  (fixture-3-0-A-B
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-B))
       (should (eq r3 (buffer-ring-current-ring))))))
  (fixture-3-0-A-B
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (with-current-buffer buf-B
       (should (funcall sut))
       (should (eq (current-buffer) buf-A))
       (should (eq r2 (buffer-ring-current-ring))))))

  ;; simple rotation with a 3-ring
  (fixture-3-C-A-B
   (lambda ()
     ;; current ring is 2
     ;; current buffer is buf-B
     (with-current-buffer buf-B
       (should (funcall sut))
       (should (eq (current-buffer) buf-C))
       (should (eq r1 (buffer-ring-current-ring))))))
  (fixture-3-C-A-B
   (lambda ()
     (with-current-buffer buf-B
       (should (progn (funcall sut)
                      (funcall sut)))
       (should (eq (current-buffer) buf-A))
       (should (eq r2 (buffer-ring-current-ring))))))
  (fixture-3-C-A-B
   (lambda ()
     (with-current-buffer buf-B
       (should (progn (funcall sut)
                      (funcall sut)
                      (funcall sut)))
       (should (eq (current-buffer) buf-B))
       (should (eq r3 (buffer-ring-current-ring))))))

  ;; rotation with same buffer in multiple rings
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-A)) ; stays in same buffer
       (should (eq r3 (buffer-ring-current-ring))))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-A)) ; stays in same buffer
       (should (eq r2 (buffer-ring-current-ring)))))))

;;
;; External triggers
;;

(ert-deftest buffer-is-killed-test ()
  (fixture-1-0+A
   (lambda ()
     (kill-buffer buf-A)
     (should (dynaring-empty-p (buffer-ring-ring-ring (buffer-ring-current-ring))))))

  (fixture-1-A
   (lambda ()
     (kill-buffer buf-A)
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r1)
                                      buf-A))))
  (fixture-1-A
   (lambda ()
     (let ((key (buffer-ring-registry-get-key buf-A)))
       (kill-buffer buf-A)
       (should-not (member r1 (ht-get buffer-rings key))))))
  (fixture-1-A
   (lambda ()
     (kill-buffer buf-A)
     (should (= 0 (dynaring-size (buffer-ring-ring-ring r1))))))

  (fixture-3-0-A-ABC
   (lambda ()
     (kill-buffer buf-A)
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r2)
                                      buf-A))))
  (fixture-3-0-A-ABC
   (lambda ()
     (kill-buffer buf-A)
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r3)
                                      buf-A))))
  (fixture-3-0-A-ABC
   (lambda ()
     (let ((key (buffer-ring-registry-get-key buf-A)))
       (kill-buffer buf-A)
       (should-not (member r2 (ht-get buffer-rings key))))))
  (fixture-3-0-A-ABC
   (lambda ()
     (let ((key (buffer-ring-registry-get-key buf-A)))
       (kill-buffer buf-A)
       (should-not (member r3 (ht-get buffer-rings key))))))
  (fixture-3-0-A-ABC
   (lambda ()
     (kill-buffer buf-A)
     (should (= 0 (dynaring-size (buffer-ring-ring-ring r2))))))
  (fixture-3-0-A-ABC
   (lambda ()
     (kill-buffer buf-A)
     (should (= 2 (dynaring-size (buffer-ring-ring-ring r3)))))))

(ert-deftest buffer-ring-visit-buffer-test ()
  ;; if buffer is unaffiliated, nothing happens
  (fixture-2-0-A
   (lambda ()
     (let ((new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring-visit-buffer)
             (should (eq r2 (buffer-ring-current-ring))))
         (kill-buffer new-buf)))))

  ;; if buffer is in current ring and already at head, nothing happens
  (fixture-3-0-A-ABC
   (lambda ()
     (with-current-buffer buf-A
       (buffer-ring-visit-buffer)
       (should (eq r3 (buffer-ring-current-ring)))
       (should (eq buf-A (buffer-ring-current-buffer))))))

  ;; if buffer is in current ring and not at head, it is reinserted
  (fixture-3-0-A-ABC
   (lambda ()
     (with-current-buffer buf-C
       (buffer-ring-visit-buffer)
       (should (eq r3 (buffer-ring-current-ring)))
       (should (eq buf-C (buffer-ring-current-buffer))))))

  ;; if buffer is in a different ring, current ring is changed
  (fixture-3-0-A-BC
   (lambda ()
     (with-current-buffer buf-A
       (buffer-ring-visit-buffer)
       (should (eq r2 (buffer-ring-current-ring)))
       (should (eq buf-A (buffer-ring-current-buffer))))))

  ;; buffer is current in every ring that it is part of
  (fixture-3-0-A-ABC
   (lambda ()
     (with-current-buffer buf-A
       (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
       (buffer-ring-next-buffer)
       ;; validate that buf-A is no longer current in ring 2
       (should-not (eq buf-A (buffer-ring-current-buffer r3)))
       (buffer-ring-torus-switch-to-ring bfr-0-ring-name)
       (buffer-ring-visit-buffer buf-A)
       (should (eq buf-A (buffer-ring-current-buffer r2)))
       (should (eq buf-A (buffer-ring-current-buffer r3)))))))

(ert-deftest buffer-ring-set-buffer-context-test ()
  ;; if buffer is in a different ring, current ring is changed
  ;; and it is placed at the head
  (fixture-3-0-A-BC
   (lambda ()
     (with-current-buffer buf-A
       (buffer-ring-set-buffer-context)
       (should (eq r2 (buffer-ring-current-ring)))
       (should (eq buf-A (buffer-ring-current-buffer)))))))

(ert-deftest buffer-ring-surface-ring-test ()
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-surface-ring r2)
     (should (eq r2 (car (buffer-ring-get-rings buf-A))))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-surface-ring r2)
     (dolist (buf (list buf-A buf-B buf-C))
       (buffer-ring-register-ring buf r2))
     (buffer-ring-surface-ring r3)
     (should (eq r3 (car (buffer-ring-get-rings buf-A))))
     (should (eq r3 (car (buffer-ring-get-rings buf-B))))
     (should (eq r3 (car (buffer-ring-get-rings buf-C)))))))

;;
;; "Integration" tests
;;

(ert-deftest buffer-ring-switching-buffers-and-rings-test ()
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-initialize)
     (let ((r4 (buffer-ring-torus-get-ring "new-ring"))
           (new-buf (generate-new-buffer bfr-test-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring-add "new-ring" new-buf)
             (should (eq r4 (buffer-ring-current-ring)))
             ;; switch to buf-B in r3 so it's at head
             (switch-to-buffer buf-B)
             ;; switch to r2, buf-A now points to r2
             (buffer-ring-torus-switch-to-ring bfr-1-ring-name)
             (should (eq r2 (car (buffer-ring-get-rings buf-A))))
             ;; then switch to the new buffer in r4
             (switch-to-buffer new-buf)
             (should (eq r4 (buffer-ring-current-ring)))
             ;; then switch to r3 which should set buffer to buf-B
             (buffer-ring-torus-switch-to-ring bfr-2-ring-name)
             (should (eq buf-B (current-buffer)))
             ;; ensure that buf-A (not active but in ring) points to r3
             (should (eq r3 (car (buffer-ring-get-rings buf-A)))))
         (kill-buffer new-buf)
         (dynaring-destroy (buffer-ring-ring-ring r4))))
     (buffer-ring-disable))))
