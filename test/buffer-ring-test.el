;; Note: we want to retain dynamic binding for these tests because the
;; ERT "fixtures" rely on it.

;; To run the tests from within Emacs, you must `eval-buffer` this test
;; buffer first. Then, run tests using `ert-run-tests-interactively`.
;; But, to avoid having to evaluate the changes (which may affect the live
;; environment), it may be preferable to `make test` at the shell, instead.

;; Notes:
;; - If you see "lisp nesting exceeds max-lisp-eval-depth"
;;   while running these tests, it could be that you have a duplicate
;;   "body" invocation within one of the nested fixtures. Since these
;;   are dynamically bound, every fixture needs to have a distinct
;;   name for the body argument.
;; - If you see errors like "(void-function t)", "(void-function nil)"
;;   and "invalid function nil . 0"
;;   then you probably are using a fixture without wrapping the body
;;   in a lambda
;; - if there are errors in tests and you're left with a mess of unclosed
;;   test buffers, use this to get rid of them:
;;   (kill-matching-buffers (concat "^" fixture-buffer-name-prefix) nil t)

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

(defvar fixture-buffer-name-prefix "bfr-test")
(defvar fixture-new-ring-name "bfr-test-new-ring")
(defvar fixture-ring-1-name "bfr-test-ring-1")
(defvar fixture-ring-2-name "bfr-test-ring-2")
(defvar fixture-ring-3-name "bfr-test-ring-3")

;; fixture recipe from:
;; https://www.gnu.org/software/emacs/manual/html_node/ert/Fixtures-and-Test-Suites.html
(defun fixture-buffers-A (body-buf-a)
  (let* ((buf-A nil))
    (unwind-protect
        (progn (setq buf-A (generate-new-buffer fixture-buffer-name-prefix))
               (funcall body-buf-a))
      (kill-buffer buf-A))))

(defun fixture-buffers-AB (body-buf-ab)
  (fixture-buffers-A
   (lambda ()
     (let ((buf-B nil))
       (unwind-protect
           (progn (setq buf-B (generate-new-buffer fixture-buffer-name-prefix))
                  (funcall body-buf-ab))
         (kill-buffer buf-B))))))

(defun fixture-buffers-ABC (body-buf-abc)
  (fixture-buffers-AB
   (lambda ()
     (let ((buf-C nil))
       (unwind-protect
           (progn (setq buf-C (generate-new-buffer fixture-buffer-name-prefix))
                  (funcall body-buf-abc))
         (kill-buffer buf-C))))))

(defun fixture-0 (body-0)
  ;; no buffer rings present
  (unwind-protect
      (progn (setq buffer-ring-torus (dynaring-make))
             (setq buffer-rings (ht))
             (funcall body-0))
    (let ((r1 (buffer-ring-torus-get-ring fixture-new-ring-name)))
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
           (progn (setq r1 (buffer-ring-torus-get-ring fixture-ring-1-name))
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
        (buffer-ring--add buf-A r1)
        (funcall body-1a))))))

(defun fixture-1-AB (body-1ab)
  (fixture-1
   (lambda ()
     (fixture-buffers-AB
      (lambda ()
        (buffer-ring--add buf-A r1)
        (buffer-ring--add buf-B r1)
        (funcall body-1ab))))))

(defun fixture-1-ABC (body-1abc)
  (fixture-1
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add buf-A r1)
        (buffer-ring--add buf-B r1)
        (buffer-ring--add buf-C r1)
        (funcall body-1abc))))))

(defun fixture-2 (body-2)
  ;; 2 buffer rings: both empty
  (fixture-1
   (lambda ()
     (let ((r2 nil))
       (unwind-protect
           (progn
             (setq r2 (buffer-ring-torus-get-ring fixture-ring-2-name))
             (funcall body-2))
         (dynaring-destroy (buffer-ring-ring-ring r2)))))))

(defun fixture-2-0-A (body-20a)
  ;; 2 buffer rings: empty, 1 element
  (fixture-2
   (lambda ()
     (fixture-buffers-A
      (lambda ()
        (buffer-ring--add buf-A r2)
        (funcall body-20a))))))

(defun fixture-2-A-A (body-2aa)
  ;; 2 buffer rings: empty, 1 element
  ;; add a buffer to the empty ring
  (fixture-2
   (lambda ()
     (fixture-buffers-A
      (lambda ()
        (buffer-ring--add buf-A r1)
        (buffer-ring--add buf-A r2)
        (funcall body-2aa))))))

(defun fixture-2-AB-AC (body-2-ab-ac)
  ;; 2 buffer rings, each with 2 buffers, AB and AC
  (fixture-2
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add buf-A r1)
        (buffer-ring--add buf-B r1)
        (buffer-ring--add buf-A r2)
        (buffer-ring--add buf-C r2)
        (funcall body-2-ab-ac))))))

(defun fixture-3 (body-3)
  ;; 3 buffer rings
  ;; [2] - 1 - 0 - [2]
  (fixture-2
   (lambda ()
     (let ((r3 nil))
       (unwind-protect
           (progn
             (setq r3 (buffer-ring-torus-get-ring fixture-ring-3-name))
             (funcall body-3))
         (dynaring-destroy (buffer-ring-ring-ring r3)))))))

(defun fixture-3-0-A-0 (body-30a0)
  ;; 3 buffer rings: empty, 1 element, empty
  ;; [2] - 1 - 0 - [2]
  (fixture-3
   (lambda ()
     (fixture-buffers-A
      (lambda ()
        (buffer-ring--add buf-A r2)
        (funcall body-30a0))))))

(defun fixture-3-0-A-B (body-30ab)
  ;; 3 buffer rings: empty, 1 element, 1 element
  ;; [2] - 1 - 0 - [2]
  (fixture-3
   (lambda ()
     (fixture-buffers-AB
      (lambda ()
        (buffer-ring--add buf-A r2)
        (buffer-ring--add buf-B r3)
        (funcall body-30ab))))))

(defun fixture-3-C-A-B (body-3cab)
  ;; 3 buffer rings: 1 distinct buffer in each
  ;; [2] - 1 - 0 - [2]
  (fixture-3
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add buf-A r2)
        (buffer-ring--add buf-B r3)
        (buffer-ring--add buf-C r1)
        (funcall body-3cab))))))

(defun fixture-3-0-A-BC (body-3-0-a-bc)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  (fixture-3
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add buf-A r2)
        (buffer-ring--add buf-B r3)
        (buffer-ring--add buf-C r3)
        (funcall body-3-0-a-bc))))))

(defun fixture-3-A-A-BC (body-3-a-a-bc)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  ;; add a buffer to the empty ring
  (fixture-3
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add buf-A r1)
        (buffer-ring--add buf-A r2)
        (buffer-ring--add buf-B r3)
        (buffer-ring--add buf-C r3)
        (funcall body-3-a-a-bc))))))

(defun fixture-3-0-AB-BC (body-3-0-ab-bc)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  ;; add a buffer to the 1 ring
  (fixture-3
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add buf-A r2)
        (buffer-ring--add buf-B r2)
        (buffer-ring--add buf-B r3)
        (buffer-ring--add buf-C r3)
        (funcall body-3-0-ab-bc))))))

(defun fixture-3-0-A-ABC (body-3-0-a-abc)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  ;; add a buffer to the 2 ring
  (fixture-3
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add buf-A r2)
        (buffer-ring--add buf-A r3)
        (buffer-ring--add buf-B r3)
        (buffer-ring--add buf-C r3)
        (funcall body-3-0-a-abc))))))

(defun fixture-3-AB-BC-AC (body-3-ab-bc-ac)
  (fixture-3
   (lambda ()
     (fixture-buffers-ABC
      (lambda ()
        (buffer-ring--add buf-A r1)
        (buffer-ring--add buf-A r3)
        (buffer-ring--add buf-B r1)
        (buffer-ring--add buf-B r2)
        (buffer-ring--add buf-C r2)
        (buffer-ring--add buf-C r3)
        (funcall body-3-ab-bc-ac))))))

;;
;; Test utilities
;;



;;
;; Tests
;;

(ert-deftest buffer-ring-test ()
  ;; null constructor
  (should (buffer-ring-make-ring fixture-ring-1-name))

  ;; buffer-ring-ring-name
  (let ((bfr-ring (buffer-ring-make-ring fixture-ring-1-name)))
    (should (equal fixture-ring-1-name (buffer-ring-ring-name bfr-ring))))

  ;; buffer-ring-ring-ring
  (let ((bfr-ring (buffer-ring-make-ring fixture-ring-1-name)))
    (should (buffer-ring-ring-ring bfr-ring))))

(ert-deftest buffer-torus-test ()
  ;; create new ring
  (should (buffer-ring-torus-get-ring fixture-ring-1-name))

  ;; get existing ring
  (let ((bfr-ring (buffer-ring-torus-get-ring fixture-ring-1-name)))
    (should (eq bfr-ring (buffer-ring-torus-get-ring fixture-ring-1-name)))))

(ert-deftest buffer-ring-torus-switch-to-ring-test ()
  ;; does not switch to non-existent ring
  (fixture-2-0-A
   (lambda ()
     (should-not (buffer-ring-torus-switch-to-ring "non-existent"))))

  ;; switch to ring preserves buffer if present
  (fixture-2-A-A
   ;; buffer is on both rings
   (lambda ()
     (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring--add new-buf r1)
             (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
             ;; go to a buffer not on ring 1
             (dynaring-rotate-until (buffer-ring-ring-ring (buffer-ring-current-ring))
                                    #'dynaring-rotate-right
                                    (lambda (bfr)
                                      (not (eq buf-A bfr))))
             ;; buffer is the only buffer in ring 1
             ;; so it should become current
             (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
             ;; now switching back would a priori return to the head,
             ;; which isn't buffer. But since buffer is present in
             ;; the destination ring, it should remain current
             (should (buffer-ring-torus-switch-to-ring fixture-ring-1-name))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  ;; reorders ring to account for recency
  (fixture-3-C-A-B
   ;; [2] - 1 - 0 - [2]
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
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
     (buffer-ring-add fixture-new-ring-name buf-A)
     (should (dynaring-contains-p buffer-ring-torus
                                  (car (buffer-ring-get-rings buf-A))))))
  (fixture-0+A
   (lambda ()
     (buffer-ring-add fixture-new-ring-name buf-A)
     (should (dynaring-contains-p (buffer-ring-ring-ring (buffer-ring-current-ring))
                                  buf-A))
     (should (= 1 (buffer-ring-size)))))
  (fixture-0+A
   (lambda ()
     (buffer-ring-add fixture-new-ring-name buf-A)
     (should (= 1 (buffer-ring-size)))))
  (fixture-0+A
   (lambda ()
     (buffer-ring-add fixture-new-ring-name buf-A)
     (should (eq (buffer-ring-torus-get-ring fixture-new-ring-name)
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
     ;; interface accepts buffer name as string
     (buffer-ring-add (buffer-ring-ring-name r1)
                      (buffer-name buf-A))
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
     ;; current buffer is A
     ;; current ring is r2 - the buffer is already present
     ;; on both r1 and r2. we add it to r1.
     ;; it should fail to add, yet, it should switch to r1
     ;; as the active ring
     (switch-to-buffer buf-A)
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
     ;; initial ring is 3
     (switch-to-buffer buf-A)
     (buffer-ring-add (buffer-ring-ring-name r1)
                      buf-A)
     (should (eq r1 (buffer-ring-current-ring)))))

  ;; should NOT change to a ring when a buffer is added to it
  ;; if the buffer isn't the current buffer
  (fixture-3-0-A-ABC
   (lambda ()
     ;; initial ring is 3
     (buffer-ring-add (buffer-ring-ring-name r1)
                      buf-A)
     (should (eq r3 (buffer-ring-current-ring))))))

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
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
     (should (buffer-ring-delete buf-A))
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r2)
                                      buf-A))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
     (should (buffer-ring-delete buf-A))
     (should-not (member r2 (buffer-ring-get-rings buf-A)))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
     (should (buffer-ring-delete buf-A))
     (should (= 0 (dynaring-size (buffer-ring-ring-ring r2))))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
     (should (buffer-ring-delete buf-A))
     (should-not (dynaring-contains-p (buffer-ring-ring-ring r3)
                                      buf-A))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
     (should (buffer-ring-delete buf-A))
     (should-not (member r3 (buffer-ring-get-rings buf-A)))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
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
     (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
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
     (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (should (funcall sut))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  (fixture-2-0-A
   (lambda ()
     (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring--add new-buf r1)
             (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
             (should (funcall sut))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  ;; if buffer is present in the new ring but not at head,
  ;; remain on it in the new ring
  (fixture-2-A-A
   ;; buffer is on both rings
   (lambda ()
     (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring--add new-buf r1)
             (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
             ;; go to a buffer not on ring 1
             (dynaring-rotate-until (buffer-ring-ring-ring (buffer-ring-current-ring))
                                    #'dynaring-rotate-right
                                    (lambda (bfr)
                                      (not (eq buf-A bfr))))
             ;; buffer is the only buffer in ring 1
             ;; so it should become current
             (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
             ;; now switching back would a priori return to the head,
             ;; which isn't buffer. But since buffer is present in
             ;; the destination ring, it should remain current
             (should (funcall sut))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  ;; does not rotate if all other rings are empty
  (fixture-3-0-A-0
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
     (with-current-buffer buf-A
       (should-not (funcall sut))
       (should (eq (current-buffer) buf-A))
       (should (eq r2 (buffer-ring-current-ring))))))

  ;; rotation with an empty ring
  (fixture-3-0-A-B
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-B))
       (should (eq r3 (buffer-ring-current-ring))))))
  (fixture-3-0-A-B
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
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
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-A)) ; stays in same buffer
       (should (eq r3 (buffer-ring-current-ring))))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
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
     (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (should (funcall sut))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  (fixture-2-0-A
   (lambda ()
     (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring--add new-buf r1)
             (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
             (should (funcall sut))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  ;; if buffer is present in the new ring but not at head,
  ;; remain on it in the new ring
  (fixture-2-A-A
   ;; buffer is on both rings
   (lambda ()
     (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring--add new-buf r1)
             (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
             ;; go to a buffer not on ring 1
             (dynaring-rotate-until (buffer-ring-ring-ring (buffer-ring-current-ring))
                                    #'dynaring-rotate-right
                                    (lambda (bfr)
                                      (not (eq buf-A bfr))))
             ;; buffer is the only buffer in ring 1
             ;; so it should become current
             (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
             ;; now switching back would a priori return to the head,
             ;; which isn't buffer. But since buffer is present in
             ;; the destination ring, it should remain current
             (should (funcall sut))
             (should (eq (current-buffer) buf-A)))
         (kill-buffer new-buf)))))

  ;; does not rotate if all other rings are empty
  (fixture-3-0-A-0
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
     (with-current-buffer buf-A
       (should-not (funcall sut))
       (should (eq (current-buffer) buf-A))
       (should (eq r2 (buffer-ring-current-ring))))))

  ;; rotation with an empty ring
  (fixture-3-0-A-B
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-B))
       (should (eq r3 (buffer-ring-current-ring))))))
  (fixture-3-0-A-B
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
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
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-A)) ; stays in same buffer
       (should (eq r3 (buffer-ring-current-ring))))))
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
     (with-current-buffer buf-A
       (should (funcall sut))
       (should (eq (current-buffer) buf-A)) ; stays in same buffer
       (should (eq r2 (buffer-ring-current-ring)))))))

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

(ert-deftest buffer-ring-surface-buffer-test ()
  (fixture-3-AB-BC-AC
   (lambda ()
     (dynaring-rotate-until r1
                            #'dynaring-rotate-left
                            (lambda (x)
                              (not (eq x buf-A))))
     (dynaring-rotate-until r3
                            #'dynaring-rotate-left
                            (lambda (x)
                              (not (eq x buf-A))))
     (buffer-ring-surface-buffer buf-A)
     (should (eq buf-A (buffer-ring-current-buffer r1)))
     (should (eq buf-A (buffer-ring-current-buffer r3))))))

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

(ert-deftest buffer-ring-synchronize-buffer-test ()
  ;; if buffer is unaffiliated, nothing happens
  (fixture-2-0-A
   (lambda ()
     (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
       (unwind-protect
           (with-current-buffer new-buf
             (buffer-ring-synchronize-buffer)
             (should (eq r2 (buffer-ring-current-ring))))
         (kill-buffer new-buf)))))

  ;; if buffer is in current ring and already at head, nothing happens
  (fixture-3-0-A-ABC
   (lambda ()
     (with-current-buffer buf-A
       (buffer-ring-synchronize-buffer)
       (should (eq r3 (buffer-ring-current-ring)))
       (should (eq buf-A (buffer-ring-current-buffer))))))

  ;; if buffer is in current ring and not at head, it is reinserted
  (fixture-3-0-A-ABC
   (lambda ()
     (with-current-buffer buf-C
       (buffer-ring-synchronize-buffer)
       (should (eq r3 (buffer-ring-current-ring)))
       (should (eq buf-C (buffer-ring-current-buffer))))))

  ;; if buffer is in a different ring, current ring is changed
  ;; and it is placed at the head
  (fixture-3-0-A-BC
   ;; initial ring is r3
   (lambda ()
     (with-current-buffer buf-A
       (buffer-ring-synchronize-buffer)
       (should (eq r2 (buffer-ring-current-ring)))
       (should (eq buf-A (buffer-ring-current-buffer))))))

  ;; buffer is current in every ring that it is part of
  (fixture-3-0-A-ABC
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
     (buffer-ring-next-buffer)
     ;; validate that buf-A is no longer current in ring 3
     (should-not (eq buf-A (buffer-ring-current-buffer r3)))
     (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
     (with-current-buffer buf-A
       (buffer-ring-synchronize-buffer))
     (should (eq buf-A (buffer-ring-current-buffer r2)))
     (should (eq buf-A (buffer-ring-current-buffer r3)))))

  ;; recent ring is surfaced in all of its buffers
  (fixture-3-AB-BC-AC
   (lambda ()
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
     ;; ring 3 is most recent for buffer A
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
     ;; ring 2 is most recent for buffers B and C
     (with-current-buffer buf-A
       (buffer-ring-synchronize-buffer buf-A))
     ;; ring 3 was most recent for buf A, so it should,
     ;; as current ring, be surfaced as most recent for
     ;; buf C as well, even though C was last visited on r2
     (should (eq r3 (car (buffer-ring-get-rings buf-A))))
     (should (eq r3 (car (buffer-ring-get-rings buf-C)))))))

(ert-deftest buffer-ring-synchronize-ring-test ()
  (fixture-2-AB-AC
   ;; switches to the head buffer
   (lambda ()
     (buffer-ring-synchronize-ring r1)
     (should (eq buf-B (current-buffer)))))

  (fixture-2-AB-AC
   ;; surfaces the ring in all of its buffers
   (lambda ()
     (buffer-ring-synchronize-ring r1)
     (should (eq r1 (car (buffer-ring-get-rings buf-A))))
     (should (eq r1 (car (buffer-ring-get-rings buf-B))))))

  (fixture-2-0-A
   ;; should not switch buffer if the new ring is empty
   (lambda ()
     (buffer-ring-synchronize-ring r2)
     (should (eq buf-A (current-buffer)))
     (buffer-ring-synchronize-ring r1)
     (should (eq buf-A (current-buffer))))))

;;
;; "Integration" tests
;;

(ert-deftest buffer-torus-switch-ring-integration-test ()
  ;; check that it surfaces ring and buffer
  (fixture-3-AB-BC-AC
   ;; surfaces ring
   ;; switching to a ring causes all of its buffers to show that
   ;; ring as most recent, even if the last time those buffers were
   ;; active was on another ring
   (lambda ()
     (buffer-ring-initialize)
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
     ;; towards "the last time we visited B was on ring 2"
     (switch-to-buffer buf-B)
     ;; rotate to C to ensure we remain on C when we switch
     ;; to r3
     (buffer-ring-next-buffer)
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
     ;; now switch to A, which should position it at head
     ;; on both r3 and r1
     (switch-to-buffer buf-A)
     ;; switch ring to r1
     (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
     ;; verify that we are on A
     (should (eq buf-A (current-buffer)))
     ;; the last time we visited B was on ring 2, yet
     ;; r1 should now reflect as most recent ring on it
     (should (eq r1 (car (buffer-ring-get-rings buf-A))))
     (should (eq r1 (car (buffer-ring-get-rings buf-B))))
     (buffer-ring-disable)))

  (fixture-3-AB-BC-AC
   ;; surfaces head buffer
   ;; switching to a ring causes its head buffer to be head
   ;; in all of its rings
   (lambda ()
     (buffer-ring-initialize)
     (buffer-ring-torus-switch-to-ring fixture-ring-1-name) ; on B
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name) ; still on B
     (buffer-ring-next-buffer) ; now on C on r2
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name) ; on C in r3
     (buffer-ring-torus-switch-to-ring fixture-ring-1-name) ; B is at head
     (should (eq buf-B (current-buffer)))
     (should (eq buf-B (buffer-ring-current-buffer r1)))
     (should (eq buf-B (buffer-ring-current-buffer r2)))
     (buffer-ring-disable))))

(ert-deftest buffer-torus-rotation-integration-test ()
  ;; check that it surfaces ring and buffer
  (fixture-3-AB-BC-AC
   ;; surfaces ring
   ;; rotating to a ring causes all of its buffers to show that
   ;; ring as most recent, even if the last time those buffers were
   ;; active was on another ring
   (lambda ()
     (buffer-ring-initialize)
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name) ; reorders
     ;; towards "the last time we visited B was on ring 2"
     (switch-to-buffer buf-B)
     ;; rotate to C to ensure we remain on C when we switch
     ;; to r3
     (buffer-ring-next-buffer)
     (buffer-ring-torus-next-ring) ; puts us on r3
     ;; now switch to A, which should position it at head
     ;; on both r3 and r1
     (switch-to-buffer buf-A)
     ;; switch ring to r1
     (buffer-ring-torus-next-ring) ; puts us on r1
     ;; verify that we are on A
     (should (eq buf-A (current-buffer)))
     ;; the last time we visited B was on ring 2, yet
     ;; r1 should now reflect as most recent ring on it
     (should (eq r1 (car (buffer-ring-get-rings buf-A))))
     (should (eq r1 (car (buffer-ring-get-rings buf-B))))
     (buffer-ring-disable)))

  (fixture-2-AB-AC
   ;; surfaces head buffer
   ;; rotating to a ring causes its head buffer to be head
   ;; in all of its rings
   (lambda ()
     (buffer-ring-initialize)
     (buffer-ring-next-buffer) ; now A, on r2
     (buffer-ring-torus-next-ring) ; r2 -> r1, remaining on A
     (buffer-ring-next-buffer) ; now B
     (buffer-ring-torus-prev-ring) ; back on A in r2
     ;; A should be head again in r1
     (should (eq buf-A (buffer-ring-current-buffer r2)))
     (should (eq buf-A (buffer-ring-current-buffer r1)))
     (buffer-ring-disable))))

(ert-deftest buffer-ring-rotation-integration-test ()
  ;; check that it surfaces the buffer
  (fixture-2-AB-AC
   ;; rotating the ring ensures that the new head buffer
   ;; is at the head of all rings that it is part of
   (lambda ()
     (buffer-ring-initialize)
     ;; switch to B and C in the rings
     ;; then switch to A in one by rotating.
     ;; without switching to the other ring,
     ;; check that A is at its head
     (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
     (switch-to-buffer buf-B)
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
     (switch-to-buffer buf-C)

     (buffer-ring-next-buffer)
     (should (eq buf-A (buffer-ring-current-buffer r1)))
     (should (eq buf-A (buffer-ring-current-buffer r2)))
     (buffer-ring-disable))))

(ert-deftest buffer-ring-switch-to-buffer-directly-test ()
  (fixture-3-AB-BC-AC
   ;; switching to a buffer directly remains on current ring
   ;; if the buffer is a member
   (lambda ()
     (buffer-ring-initialize)
     ;; start on ring 1 with buffers A and B
     (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
     (buffer-ring-next-buffer) ; rotate to A
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
     (buffer-ring-next-buffer) ; rotate A -> C
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
     (switch-to-buffer buf-B)
     ;; we should remain on r2 since B is on the current ring
     (should (eq fixture-ring-2-name (buffer-ring-current-ring-name)))
     (buffer-ring-disable)))

  (fixture-3-AB-BC-AC
   ;; switching to a buffer directly switches to its most recent ring
   (lambda ()
     (buffer-ring-initialize)
     ;; start on ring 1 with buffers A and B
     (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
     (buffer-ring-next-buffer) ; rotate to A
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name) ; on C
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name) ; still on C
     ;; the last time we saw B was on r1
     (switch-to-buffer buf-B)
     ;; but ring 2 was more recent so it should become current
     (should (eq fixture-ring-2-name (buffer-ring-current-ring-name)))
     (buffer-ring-disable)))

  (fixture-3-AB-BC-AC
   ;; switching to a buffer directly surfaces its most recent ring
   (lambda ()
     (buffer-ring-initialize)
     ;; start on ring 1 with buffers A and B
     (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
     (buffer-ring-next-buffer) ; rotate to A
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name) ; on C
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name) ; still on C
     ;; the last time we saw B was on r1
     (switch-to-buffer buf-B)
     ;; but ring 2 was more recent so it should be surfaced
     ;; i.e. should show as most recent for both B and C
     (should (eq r2 (car (buffer-ring-get-rings buf-B))))
     (should (eq r2 (car (buffer-ring-get-rings buf-C))))
     (buffer-ring-disable)))

  (fixture-3-AB-BC-AC
   ;; switching to a buffer directly surfaces it in all of its rings
   (lambda ()
     (buffer-ring-initialize)
     ;; start on ring 1 with buffers A and B
     (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
     (buffer-ring-next-buffer) ; rotate to A
     (buffer-ring-torus-switch-to-ring fixture-ring-2-name) ; on C
     (buffer-ring-torus-switch-to-ring fixture-ring-3-name) ; still on C
     ;; the last time we were on r1, it was on buffer A
     ;; and in r2, on buffer C
     (switch-to-buffer buf-B)
     ;; but B should be surfaced in all of its rings,
     ;; i.e. it should now be at head in both r1 and r2
     (should (eq buf-B (buffer-ring-current-buffer r1)))
     (should (eq buf-B (buffer-ring-current-buffer r2)))
     (buffer-ring-disable))))
