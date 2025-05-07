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

(defmacro with-fixture (fixture &rest test)
  "Run TEST using FIXTURE."
  (declare (indent 1))
  `(,fixture
    (lambda ()
      ,@test)))

(defun fixture-buffers-A (body-buf-a)
  (let* ((buf-A nil))
    (unwind-protect
        (progn (setq buf-A (generate-new-buffer
                            (concat fixture-buffer-name-prefix "-" "A")))
               (funcall body-buf-a))
      (kill-buffer buf-A))))

(defun fixture-buffers-AB (body-buf-ab)
  (with-fixture fixture-buffers-A
    (let ((buf-B nil))
      (unwind-protect
          (progn (setq buf-B (generate-new-buffer
                              (concat fixture-buffer-name-prefix "-" "B")))
                 (funcall body-buf-ab))
        (kill-buffer buf-B)))))

(defun fixture-buffers-ABC (body-buf-abc)
  (with-fixture fixture-buffers-AB
    (let ((buf-C nil))
      (unwind-protect
          (progn (setq buf-C (generate-new-buffer
                              (concat fixture-buffer-name-prefix "-" "C")))
                 (funcall body-buf-abc))
        (kill-buffer buf-C)))))

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
  (with-fixture fixture-0
    (with-fixture fixture-buffers-A
      (funcall body-0+a))))

(defun fixture-1 (body-1)
  ;; 1 empty buffer ring
  ;; an unaffiliated buffer
  (with-fixture fixture-0
    (let ((r1 nil))
      (unwind-protect
          (progn (setq r1 (buffer-ring-torus-get-ring fixture-ring-1-name))
                 (funcall body-1))
        (dynaring-destroy (buffer-ring-ring-ring r1))))))

(defun fixture-1-0+A (body-10+a)
  ;; 1 empty buffer ring
  ;; an unaffiliated buffer
  (with-fixture fixture-1
    (with-fixture fixture-buffers-A
      (funcall body-10+a))))

(defun fixture-1-A (body-1a)
  (with-fixture fixture-1
    (with-fixture fixture-buffers-A
      (buffer-ring--add buf-A r1)
      (funcall body-1a))))

(defun fixture-1-AB (body-1ab)
  (with-fixture fixture-1
    (with-fixture fixture-buffers-AB
      (buffer-ring--add buf-A r1)
      (buffer-ring--add buf-B r1)
      (funcall body-1ab))))

(defun fixture-1-ABC (body-1abc)
  (with-fixture fixture-1
    (with-fixture fixture-buffers-ABC
      (buffer-ring--add buf-A r1)
      (buffer-ring--add buf-B r1)
      (buffer-ring--add buf-C r1)
      (funcall body-1abc))))

(defun fixture-2 (body-2)
  ;; 2 buffer rings: both empty
  (with-fixture fixture-1
    (let ((r2 nil))
      (unwind-protect
          (progn
            (setq r2 (buffer-ring-torus-get-ring fixture-ring-2-name))
            (funcall body-2))
        (dynaring-destroy (buffer-ring-ring-ring r2))))))

(defun fixture-2-0-A (body-20a)
  ;; 2 buffer rings: empty, 1 element
  (with-fixture fixture-2
    (with-fixture fixture-buffers-A
      (buffer-ring--add buf-A r2)
      (funcall body-20a))))

(defun fixture-2-A-A (body-2aa)
  ;; 2 buffer rings: empty, 1 element
  ;; add a buffer to the empty ring
  (with-fixture fixture-2
    (with-fixture fixture-buffers-A
      (buffer-ring--add buf-A r1)
      (buffer-ring--add buf-A r2)
      (funcall body-2aa))))

(defun fixture-2-AB-AC (body-2-ab-ac)
  ;; 2 buffer rings, each with 2 buffers, AB and AC
  (with-fixture fixture-2
    (with-fixture fixture-buffers-ABC
      (buffer-ring--add buf-A r1)
      (buffer-ring--add buf-B r1)
      (buffer-ring--add buf-A r2)
      (buffer-ring--add buf-C r2)
      (funcall body-2-ab-ac))))

(defun fixture-3 (body-3)
  ;; 3 buffer rings
  ;; [2] - 1 - 0 - [2]
  (with-fixture fixture-2
    (let ((r3 nil))
      (unwind-protect
          (progn
            (setq r3 (buffer-ring-torus-get-ring fixture-ring-3-name))
            (funcall body-3))
        (dynaring-destroy (buffer-ring-ring-ring r3))))))

(defun fixture-3-0-A-0 (body-30a0)
  ;; 3 buffer rings: empty, 1 element, empty
  ;; [2] - 1 - 0 - [2]
  (with-fixture fixture-3
    (with-fixture fixture-buffers-A
      (buffer-ring--add buf-A r2)
      (funcall body-30a0))))

(defun fixture-3-0-A-B (body-30ab)
  ;; 3 buffer rings: empty, 1 element, 1 element
  ;; [2] - 1 - 0 - [2]
  (with-fixture fixture-3
    (with-fixture fixture-buffers-AB
      (buffer-ring--add buf-A r2)
      (buffer-ring--add buf-B r3)
      (funcall body-30ab))))

(defun fixture-3-C-A-B (body-3cab)
  ;; 3 buffer rings: 1 distinct buffer in each
  ;; [2] - 1 - 0 - [2]
  (with-fixture fixture-3
    (with-fixture fixture-buffers-ABC
      (buffer-ring--add buf-A r2)
      (buffer-ring--add buf-B r3)
      (buffer-ring--add buf-C r1)
      (funcall body-3cab))))

(defun fixture-3-0-A-BC (body-3-0-a-bc)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  (with-fixture fixture-3
    (with-fixture fixture-buffers-ABC
      (buffer-ring--add buf-A r2)
      (buffer-ring--add buf-B r3)
      (buffer-ring--add buf-C r3)
      (funcall body-3-0-a-bc))))

(defun fixture-3-A-A-BC (body-3-a-a-bc)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  ;; add a buffer to the empty ring
  (with-fixture fixture-3
    (with-fixture fixture-buffers-ABC
      (buffer-ring--add buf-A r1)
      (buffer-ring--add buf-A r2)
      (buffer-ring--add buf-B r3)
      (buffer-ring--add buf-C r3)
      (funcall body-3-a-a-bc))))

(defun fixture-3-0-AB-BC (body-3-0-ab-bc)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  ;; add a buffer to the 1 ring
  (with-fixture fixture-3
    (with-fixture fixture-buffers-ABC
      (buffer-ring--add buf-A r2)
      (buffer-ring--add buf-B r2)
      (buffer-ring--add buf-B r3)
      (buffer-ring--add buf-C r3)
      (funcall body-3-0-ab-bc))))

(defun fixture-3-0-A-ABC (body-3-0-a-abc)
  ;; 3 buffer rings: empty, 1 element, 2 elements
  ;; [2] - 1 - 0 - [2]
  ;; add a buffer to the 2 ring
  (with-fixture fixture-3
    (with-fixture fixture-buffers-ABC
      (buffer-ring--add buf-A r2)
      (buffer-ring--add buf-A r3)
      (buffer-ring--add buf-B r3)
      (buffer-ring--add buf-C r3)
      (funcall body-3-0-a-abc))))

(defun fixture-3-AB-BC-AC (body-3-ab-bc-ac)
  (with-fixture fixture-3
    (with-fixture fixture-buffers-ABC
      (buffer-ring--add buf-A r1)
      (buffer-ring--add buf-A r3)
      (buffer-ring--add buf-B r1)
      (buffer-ring--add buf-B r2)
      (buffer-ring--add buf-C r2)
      (buffer-ring--add buf-C r3)
      (funcall body-3-ab-bc-ac))))

;;
;; Test utilities
;;

(defmacro br-define-test-suite (name docstring &rest body)
  "Define a test suite."
  ;; for now, just rewrite to the existing code,
  ;; but later, rewrite to individual tests and ignore the
  ;; "suite" abstraction
  ;; TODO: this macro doesn't work, so it is unused at present
  (declare (indent 1))
  `(ert-deftest name ()
     ,docstring
     ,@body))

(defmacro br-define-test (name docstring fixture &rest body)
  "Define a unit test.

This abstracts away the boilerplate of using the fixtures, which must
be used as (fixture-fn (lambda () body ...)). Besides being
distracting this boilerplate is also prone to user error, e.g. in
adding a body without a wrapping lambda."
  (declare (indent 1))
  `(,fixture
    (lambda ()
      ,@body)))

(defmacro br-define-integration-test (name docstring fixture &rest body)
  "Define an integration test.

Similar to `br-define-test`, this abstracts away the boilerplate of
using fixtures, and also initializes advice etc. at the start of the
test, which are used by buffer-ring at runtime, and disables these at
the end of the test."
  `(,fixture
    (lambda ()
      (buffer-ring-initialize)
      ,@body
      (buffer-ring-disable))))

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
  "Test buffer-ring-torus-switch-to-ring"

  (br-define-test test1
    "Does not switch to non-existent ring."
    fixture-2-0-A
    (should-not (buffer-ring-torus-switch-to-ring "non-existent")))

  (br-define-test test2
    "Switch to ring preserves buffer if present."
    fixture-2-A-A
    ;; buffer is on both rings
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
        (kill-buffer new-buf))))

  (br-define-test test3
    "Reorders ring to account for recency."
    fixture-3-C-A-B
    ;; [2] - 1 - 0 - [2]
    (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
    ;; now changes to [1] - 2 - 0 - [1]
    ;; which is a different ordering under the
    ;; same rotation
    (should (eq r2 (buffer-ring-current-ring)))
    (buffer-ring-torus-next-ring)
    (should (eq r3 (buffer-ring-current-ring)))))

(ert-deftest buffer-ring-torus-delete-ring-test ()
  "Tests for buffer-ring-torus-delete-ring"

  (br-define-test test1
    ""
    fixture-2-A-A
    (should (buffer-ring-torus-delete-ring))
    (should (= 1 (dynaring-size buffer-ring-torus))))

  (br-define-test test2
    "Changes ring if current one is deleted."
    fixture-2-A-A
    (should (buffer-ring-torus-delete-ring))
    (should (eq r1 (buffer-ring-current-ring)))))

(ert-deftest buffer-ring-add-test ()
  "Tests for buffer-ring-add."

  (br-define-test test1
    ""
    fixture-0+A
    (buffer-ring-add fixture-new-ring-name buf-A)
    (should (dynaring-contains-p buffer-ring-torus
                                 (car (buffer-ring-get-rings buf-A)))))
  (br-define-test test2
    ""
    fixture-0+A
    (buffer-ring-add fixture-new-ring-name buf-A)
    (should (dynaring-contains-p (buffer-ring-ring-ring (buffer-ring-current-ring))
                                 buf-A))
    (should (= 1 (buffer-ring-size))))

  (br-define-test test3
    ""
    fixture-0+A
    (buffer-ring-add fixture-new-ring-name buf-A)
    (should (= 1 (buffer-ring-size))))
  (br-define-test test4
    ""
    fixture-0+A
    (buffer-ring-add fixture-new-ring-name buf-A)
    (should (eq (buffer-ring-torus-get-ring fixture-new-ring-name)
                (car (buffer-ring-get-rings buf-A)))))

  (br-define-test test5
    ""
    fixture-1-0+A
    (buffer-ring-add (buffer-ring-ring-name r1)
                     buf-A)
    (should (dynaring-contains-p buffer-ring-torus
                                 (car (buffer-ring-get-rings buf-A)))))

  (br-define-test test6
    ""
    fixture-1-0+A
    (buffer-ring-add (buffer-ring-ring-name r1)
                     buf-A)
    (should (dynaring-contains-p (buffer-ring-ring-ring (buffer-ring-current-ring))
                                 buf-A)))
  (br-define-test test7
    ""
    fixture-1-0+A
    ;; interface accepts buffer name as string
    (buffer-ring-add (buffer-ring-ring-name r1)
                     (buffer-name buf-A))
    (should (dynaring-contains-p (buffer-ring-ring-ring (buffer-ring-current-ring))
                                 buf-A)))
  (br-define-test test8
    ""
    fixture-1-0+A
    (buffer-ring-add (buffer-ring-ring-name r1)
                     buf-A)
    (should (= 1 (buffer-ring-size))))

  (br-define-test test9
    ""
    fixture-1-0+A
    (buffer-ring-add (buffer-ring-ring-name r1)
                     buf-A)
    (should (eq r1
                (car (buffer-ring-get-rings buf-A)))))

  (br-define-test test10
    "Should not add when already present."
    fixture-2-0-A
    (should-not (buffer-ring-add (buffer-ring-ring-name r2)
                                 buf-A)))

  (br-define-test test11
    ""
    fixture-2-A-A
    (should (dynaring-contains-p (buffer-ring-ring-ring r1)
                                 buf-A)))

  (br-define-test test12
    ""
    fixture-2-A-A
    (should (dynaring-contains-p (buffer-ring-ring-ring r2)
                                 buf-A)))
  (br-define-test test13
    ""
    fixture-2-A-A
    (should (member r1 (buffer-ring-get-rings buf-A))))

  (br-define-test test14
    ""
    fixture-2-A-A
    (should (member r2 (buffer-ring-get-rings buf-A))))

  (br-define-test test15
    ""
    fixture-2-A-A
    (should-not (buffer-ring-add (buffer-ring-ring-name r2)
                                 buf-A)))
  (br-define-test test16
    ""
    fixture-2-A-A
    ;; current buffer is A
    ;; current ring is r2 - the buffer is already present
    ;; on both r1 and r2. we add it to r1.
    ;; it should fail to add, yet, it should switch to r1
    ;; as the active ring
    (switch-to-buffer buf-A)
    (buffer-ring-add (buffer-ring-ring-name r1) buf-A)
    (should (eq (buffer-ring-current-ring) r1)))

  (br-define-test test17
    ""
    fixture-3-A-A-BC
    (should (dynaring-contains-p (buffer-ring-ring-ring r1)
                                 buf-A)))
  (br-define-test test18
    ""
    fixture-3-A-A-BC
    (should (dynaring-contains-p (buffer-ring-ring-ring r2)
                                 buf-A)))
  (br-define-test test19
    ""
    fixture-3-A-A-BC
    (should-not (dynaring-contains-p (buffer-ring-ring-ring r3)
                                     buf-A)))
  (br-define-test test20
    ""
    fixture-3-A-A-BC
    (should (member r1 (buffer-ring-get-rings buf-A))))

  (br-define-test test21
    ""
    fixture-3-A-A-BC
    (should (member r2 (buffer-ring-get-rings buf-A))))

  (br-define-test test22
    ""
    fixture-3-A-A-BC
    (should-not (member r3 (buffer-ring-get-rings buf-A))))

  (br-define-test test23
    ""
    fixture-3-A-A-BC
    (should-not (buffer-ring-add (buffer-ring-ring-name r2)
                                 buf-A)))

  (br-define-test test24
    ""
    fixture-3-0-AB-BC
    (should-not (dynaring-contains-p (buffer-ring-ring-ring r1)
                                     buf-A)))

  (br-define-test test25
    ""
    fixture-3-0-AB-BC
    (should-not (dynaring-contains-p (buffer-ring-ring-ring r1)
                                     buf-B)))

  (br-define-test test26
    ""
    fixture-3-0-AB-BC
    (should-not (dynaring-contains-p (buffer-ring-ring-ring r1)
                                     buf-C)))

  (br-define-test test27
    ""
    fixture-3-0-AB-BC
    (should (dynaring-contains-p (buffer-ring-ring-ring r2)
                                 buf-A)))

  (br-define-test test28
    ""
    fixture-3-0-AB-BC
    (should (dynaring-contains-p (buffer-ring-ring-ring r2)
                                 buf-B)))

  (br-define-test test29
    ""
    fixture-3-0-AB-BC
    (should-not (dynaring-contains-p (buffer-ring-ring-ring r2)
                                     buf-C)))

  (br-define-test test30
    ""
    fixture-3-0-AB-BC
    (should-not (dynaring-contains-p (buffer-ring-ring-ring r3)
                                     buf-A)))

  (br-define-test test31
    ""
    fixture-3-0-AB-BC
    (should (dynaring-contains-p (buffer-ring-ring-ring r3)
                                 buf-B)))
  (br-define-test test32
    ""
    fixture-3-0-AB-BC
    (should (dynaring-contains-p (buffer-ring-ring-ring r3)
                                 buf-C)))

  (br-define-test test33
    ""
    fixture-3-0-AB-BC
    (should-not (member r1 (buffer-ring-get-rings buf-A))))

  (br-define-test test34
    ""
    fixture-3-0-AB-BC
    (should (member r2 (buffer-ring-get-rings buf-A))))

  (br-define-test test35
    ""
    fixture-3-0-AB-BC
    (should-not (member r3 (buffer-ring-get-rings buf-A))))

  (br-define-test test36
    ""
    fixture-3-0-AB-BC
    (should-not (member r1 (buffer-ring-get-rings buf-B))))

  (br-define-test test37
    ""
    fixture-3-0-AB-BC
    (should (member r2 (buffer-ring-get-rings buf-B))))

  (br-define-test test38
    ""
    fixture-3-0-AB-BC
    (should (member r3 (buffer-ring-get-rings buf-B))))

  (br-define-test test39
    ""
    fixture-3-0-AB-BC
    (should-not (member r1 (buffer-ring-get-rings buf-C))))

  (br-define-test test40
    ""
    fixture-3-0-AB-BC
    (should-not (member r2 (buffer-ring-get-rings buf-C))))

  (br-define-test test41
    ""
    fixture-3-0-AB-BC
    (should (member r3 (buffer-ring-get-rings buf-C))))

  (br-define-test test42
    ""
    fixture-3-0-AB-BC
    (should-not (buffer-ring-add (buffer-ring-ring-name r2)
                                 buf-A)))

  (br-define-test test43
    ""
    fixture-3-0-A-ABC
    (should-not (dynaring-contains-p (buffer-ring-ring-ring r1)
                                     buf-A)))

  (br-define-test test44
    ""
    fixture-3-0-A-ABC
    (should (dynaring-contains-p (buffer-ring-ring-ring r2)
                                 buf-A)))

  (br-define-test test45
    ""
    fixture-3-0-A-ABC
    (should (dynaring-contains-p (buffer-ring-ring-ring r3)
                                 buf-A)))

  (br-define-test test46
    ""
    fixture-3-0-A-ABC
    (should-not (member r1 (buffer-ring-get-rings buf-A))))

  (br-define-test test47
    ""
    fixture-3-0-A-ABC
    (should (member r2 (buffer-ring-get-rings buf-A))))

  (br-define-test test48
    ""
    fixture-3-0-A-ABC
    (should (member r3 (buffer-ring-get-rings buf-A))))

  (br-define-test test49
    ""
    fixture-3-0-A-ABC
    (should-not (buffer-ring-add (buffer-ring-ring-name r2)
                                 buf-A)))

  (br-define-test test50
    "Should change to a ring when a buffer is added to it."
    fixture-3-0-A-ABC
    ;; initial ring is 3
    (switch-to-buffer buf-A)
    (buffer-ring-add (buffer-ring-ring-name r1)
                     buf-A)
    (should (eq r1 (buffer-ring-current-ring))))

  (br-define-test test51
    "Should NOT change to a ring when a buffer is added to it if the
buffer isn't the current buffer."
    fixture-3-0-A-ABC
    ;; initial ring is 3
    (buffer-ring-add (buffer-ring-ring-name r1)
                     buf-A)
    (should (eq r3 (buffer-ring-current-ring)))))

(ert-deftest buffer-ring-delete-test ()
  "Tests for buffer-ring-delete."

  (br-define-test test1
    ""
    fixture-0+A
    (should-not (buffer-ring-delete buf-A)))

  (br-define-test test2
    ""
    fixture-1-0+A
    (should-not (buffer-ring-delete buf-A)))

  (br-define-test test3
    ""
    fixture-1-A
    (should (buffer-ring-delete buf-A)))

  (br-define-test test4
    ""
    fixture-1-A
    (buffer-ring-delete buf-A)
    (should-not (dynaring-contains-p (buffer-ring-ring-ring r1)
                                     buf-A)))

  (br-define-test test5
    ""
    fixture-1-A
    (buffer-ring-delete buf-A)
    (should (= 0 (dynaring-size (buffer-ring-ring-ring r1)))))

  (br-define-test test6
    ""
    fixture-3-0-A-ABC
    (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
    (should (buffer-ring-delete buf-A))
    (should-not (dynaring-contains-p (buffer-ring-ring-ring r2)
                                     buf-A)))

  (br-define-test test7
    ""
    fixture-3-0-A-ABC
    (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
    (should (buffer-ring-delete buf-A))
    (should-not (member r2 (buffer-ring-get-rings buf-A))))

  (br-define-test test8
    ""
    fixture-3-0-A-ABC
    (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
    (should (buffer-ring-delete buf-A))
    (should (= 0 (dynaring-size (buffer-ring-ring-ring r2)))))

  (br-define-test test9
    ""
    fixture-3-0-A-ABC
    (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
    (should (buffer-ring-delete buf-A))
    (should-not (dynaring-contains-p (buffer-ring-ring-ring r3)
                                     buf-A)))

  (br-define-test test10
    ""
    fixture-3-0-A-ABC
    (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
    (should (buffer-ring-delete buf-A))
    (should-not (member r3 (buffer-ring-get-rings buf-A))))

  (br-define-test test11
    ""
    fixture-3-0-A-ABC
    (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
    (should (buffer-ring-delete buf-A))
    (should (= 2 (dynaring-size (buffer-ring-ring-ring r3))))))

(ert-deftest buffer-ring-next-buffer-test ()
  "Tests for buffer-ring-next-buffer."

  (setq sut #'buffer-ring-next-buffer)

  (br-define-test test1
    ""
    fixture-0+A
    (should-not (funcall sut)))

  (br-define-test test2
    ""
    fixture-1-0+A
    (with-current-buffer buf-A
      (should-not (funcall sut))
      (should (eq (current-buffer) buf-A))))

  (br-define-test test3
    ""
    fixture-1-A
    (with-current-buffer buf-A
      (should (funcall sut))
      (should (eq (current-buffer) buf-A))))

  (br-define-test test4
    "When abroad, rotating the ring changes to a native buffer."
    fixture-1-A
    (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
      (unwind-protect
          (with-current-buffer new-buf
            (should (funcall sut))
            (should (eq (current-buffer) buf-A)))
        (kill-buffer new-buf))))

  ;; head is buf-B initially
  (br-define-test test5
    ""
    fixture-1-AB
    (should (funcall sut))
    (should (eq (current-buffer) buf-A)))

  (br-define-test test6
    ""
    fixture-1-AB
    (should
     (progn (funcall sut)
            (funcall sut)))
    (should (eq (current-buffer) buf-B)))

  ;; head is buf-C initially
  (br-define-test test7
    ""
    fixture-1-ABC
    (should (funcall sut))
    (should (eq (current-buffer) buf-B)))

  (br-define-test test8
    ""
    fixture-1-ABC
    (should
     (progn (funcall sut)
            (funcall sut)))
    (should (eq (current-buffer) buf-A)))

  (br-define-test test9
    ""
    fixture-1-ABC
    (should
     (progn (funcall sut)
            (funcall sut)
            (funcall sut)))
    (should (eq (current-buffer) buf-C))))

(ert-deftest buffer-ring-prev-buffer-test ()
  "Tests for buffer-ring-prev-buffer."

  (setq sut #'buffer-ring-prev-buffer)

  (br-define-test test1
    ""
    fixture-0+A
    (should-not (funcall sut)))

  (br-define-test test2
    ""
    fixture-1-0+A
    (with-current-buffer buf-A
      (should-not (funcall sut))
      (should (eq (current-buffer) buf-A))))

  (br-define-test test3
    ""
    fixture-1-A
    (with-current-buffer buf-A
      (should (funcall sut))
      (should (eq (current-buffer) buf-A))))

  ;; head is buf-B initially
  (br-define-test test4
    ""
    fixture-1-AB
    (should (funcall sut))
    (should (eq (current-buffer) buf-A)))

  (br-define-test test5
    ""
    fixture-1-AB
    (should
     (progn (funcall sut)
            (funcall sut)))
    (should (eq (current-buffer) buf-B)))

  ;; head is buf-C initially
  (br-define-test test6
    ""
    fixture-1-ABC
    (should (funcall sut))
    (should (eq (current-buffer) buf-A)))

  (br-define-test test7
    ""
    fixture-1-ABC
    (should
     (progn (funcall sut)
            (funcall sut)))
    (should (eq (current-buffer) buf-B)))

  (br-define-test test8
    ""
    fixture-1-ABC
    (should
     (progn (funcall sut)
            (funcall sut)
            (funcall sut)))
    (should (eq (current-buffer) buf-C))))

(ert-deftest buffer-ring-rotate-to-buffer-test ()
  "Tests for buffer-ring-rotate-to-buffer."

  (setq sut #'buffer-ring-rotate-to-buffer)

  (br-define-test test1
    "Rotating to head does nothing."
    fixture-1-ABC
    (should (funcall sut buf-C))
    (should (eq (current-buffer) buf-C))
    (should (equal (dynaring-values (buffer-ring-ring-ring (buffer-ring-current-ring))) (list buf-A buf-B buf-C))))

  (br-define-test test2
    "Rotating to another buffer preserves the order wherever that buffer may be on the ring."
    fixture-1-ABC
    (should (funcall sut buf-A))
    (should (eq (current-buffer) buf-A))
    (should (equal (dynaring-values (buffer-ring-ring-ring (buffer-ring-current-ring))) (list buf-B buf-C buf-A))))

  (br-define-test test3
    "Rotating to another buffer preserves the order wherever that buffer may be on the ring."
    fixture-1-ABC
    (should (funcall sut buf-B))
    (should (eq (current-buffer) buf-B))
    (should (equal (dynaring-values (buffer-ring-ring-ring (buffer-ring-current-ring))) (list buf-C buf-A buf-B)))))

(ert-deftest buffer-ring-torus-next-ring-test ()
  "Tests for buffer-ring-torus-next-ring."

  (setq sut #'buffer-ring-torus-next-ring)

  (br-define-test test1
    ""
    fixture-0+A
    (should-not (funcall sut)))

  (br-define-test test2
    ""
    fixture-1-0+A
    (with-current-buffer buf-A
      (should (funcall sut))
      (should (eq (current-buffer) buf-A))))

  (br-define-test test3
    ""
    fixture-1-A
    (with-current-buffer buf-A
      (should (funcall sut))
      (should (eq (current-buffer) buf-A))))

  (br-define-test test4
    "When abroad, rotating the torus changes to a native buffer."
    fixture-1-A
    (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
      (unwind-protect
          (with-current-buffer new-buf
            (should (funcall sut))
            (should (eq (current-buffer) buf-A)))
        (kill-buffer new-buf))))

  (br-define-test test5
    ""
    fixture-2-0-A
    (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
      (unwind-protect
          (with-current-buffer new-buf
            (buffer-ring--add new-buf r1)
            (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
            (should (funcall sut))
            (should (eq (current-buffer) buf-A)))
        (kill-buffer new-buf))))

  (br-define-test test6
    "If buffer is present in the new ring but not at head, remain on
it in the new ring."
    fixture-2-A-A
    ;; buffer is on both rings
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
        (kill-buffer new-buf))))

  (br-define-test test7
    "Do not rotate if all other rings are empty."
    fixture-3-0-A-0
    (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
    (with-current-buffer buf-A
      (should-not (funcall sut))
      (should (eq (current-buffer) buf-A))
      (should (eq r2 (buffer-ring-current-ring)))))

  (br-define-test test8
    "Rotation with an empty ring."
    fixture-3-0-A-B
    (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
    (with-current-buffer buf-A
      (should (funcall sut))
      (should (eq (current-buffer) buf-B))
      (should (eq r3 (buffer-ring-current-ring)))))

  (br-define-test test9
    ""
    fixture-3-0-A-B
    (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
    (with-current-buffer buf-B
      (should (funcall sut))
      (should (eq (current-buffer) buf-A))
      (should (eq r2 (buffer-ring-current-ring)))))

  (br-define-test test10
    "Simple rotation with a 3-ring."
    fixture-3-C-A-B
    ;; current ring is 2
    ;; current buffer is buf-B
    (with-current-buffer buf-B
      (should (funcall sut))
      (should (eq (current-buffer) buf-A))
      (should (eq r2 (buffer-ring-current-ring)))))

  (br-define-test test11
    ""
    fixture-3-C-A-B
    (with-current-buffer buf-B
      (should (progn (funcall sut)
                     (funcall sut)))
      (should (eq (current-buffer) buf-C))
      (should (eq r1 (buffer-ring-current-ring)))))

  (br-define-test test12
    ""
    fixture-3-C-A-B
    (with-current-buffer buf-B
      (should (progn (funcall sut)
                     (funcall sut)
                     (funcall sut)))
      (should (eq (current-buffer) buf-B))
      (should (eq r3 (buffer-ring-current-ring)))))

  (br-define-test test13
    "Rotation with same buffer in multiple rings."
    fixture-3-0-A-ABC
    (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
    (with-current-buffer buf-A
      (should (funcall sut))
      (should (eq (current-buffer) buf-A)) ; stays in same buffer
      (should (eq r3 (buffer-ring-current-ring)))))

  (br-define-test test14
    ""
    fixture-3-0-A-ABC
    (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
    (with-current-buffer buf-A
      (should (funcall sut))
      (should (eq (current-buffer) buf-A)) ; stays in same buffer
      (should (eq r2 (buffer-ring-current-ring))))))

(ert-deftest buffer-ring-torus-prev-ring-test ()
  "Tests for buffer-ring-torus-prev-ring."

  (setq sut #'buffer-ring-torus-prev-ring)

  (br-define-test test1
    ""
    fixture-0+A
    (should-not (funcall sut)))

  (br-define-test test2
    ""
    fixture-1-0+A
    (with-current-buffer buf-A
      (should (funcall sut))
      (should (eq (current-buffer) buf-A))))

  (br-define-test test3
    ""
    fixture-1-A
    (with-current-buffer buf-A
      (should (funcall sut))
      (should (eq (current-buffer) buf-A))))

  (br-define-test test4
    "When abroad, rotating the torus changes to a native buffer."
    fixture-1-A
    (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
      (unwind-protect
          (with-current-buffer new-buf
            (should (funcall sut))
            (should (eq (current-buffer) buf-A)))
        (kill-buffer new-buf))))

  (br-define-test test5
    ""
    fixture-2-0-A
    (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
      (unwind-protect
          (with-current-buffer new-buf
            (buffer-ring--add new-buf r1)
            (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
            (should (funcall sut))
            (should (eq (current-buffer) buf-A)))
        (kill-buffer new-buf))))

  (br-define-test test6
    "If buffer is present in the new ring but not at head, remain on
it in the new ring."
    fixture-2-A-A
    ;; buffer is on both rings
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
        (kill-buffer new-buf))))

  (br-define-test test7
    "Does not rotate if all other rings are empty."
    fixture-3-0-A-0
    (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
    (with-current-buffer buf-A
      (should-not (funcall sut))
      (should (eq (current-buffer) buf-A))
      (should (eq r2 (buffer-ring-current-ring)))))

  (br-define-test test8
    "Rotation with an empty ring."
    fixture-3-0-A-B
    (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
    (with-current-buffer buf-A
      (should (funcall sut))
      (should (eq (current-buffer) buf-B))
      (should (eq r3 (buffer-ring-current-ring)))))

  (br-define-test test9
    ""
    fixture-3-0-A-B
    (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
    (with-current-buffer buf-B
      (should (funcall sut))
      (should (eq (current-buffer) buf-A))
      (should (eq r2 (buffer-ring-current-ring)))))

  (br-define-test test10
    "Simple rotation with a 3-ring."
    fixture-3-C-A-B
    ;; current ring is 2
    ;; current buffer is buf-B
    (with-current-buffer buf-B
      (should (funcall sut))
      (should (eq (current-buffer) buf-C))
      (should (eq r1 (buffer-ring-current-ring)))))

  (br-define-test test11
    ""
    fixture-3-C-A-B
    (with-current-buffer buf-B
      (should (progn (funcall sut)
                     (funcall sut)))
      (should (eq (current-buffer) buf-A))
      (should (eq r2 (buffer-ring-current-ring)))))

  (br-define-test test12
    ""
    fixture-3-C-A-B
    (with-current-buffer buf-B
      (should (progn (funcall sut)
                     (funcall sut)
                     (funcall sut)))
      (should (eq (current-buffer) buf-B))
      (should (eq r3 (buffer-ring-current-ring)))))

  (br-define-test test13
    "Rotation with same buffer in multiple rings."
    fixture-3-0-A-ABC
    (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
    (with-current-buffer buf-A
      (should (funcall sut))
      (should (eq (current-buffer) buf-A)) ; stays in same buffer
      (should (eq r3 (buffer-ring-current-ring)))))

  (br-define-test test14
    ""
    fixture-3-0-A-ABC
    (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
    (with-current-buffer buf-A
      (should (funcall sut))
      (should (eq (current-buffer) buf-A)) ; stays in same buffer
      (should (eq r2 (buffer-ring-current-ring))))))

(ert-deftest buffer-ring-surface-ring-test ()
  "Tests for buffer-ring-surface-ring."

  (br-define-test test1
    ""
    fixture-3-0-A-ABC
    (buffer-ring-surface-ring r2)
    (should (eq r2 (car (buffer-ring-get-rings buf-A)))))

  (br-define-test test2
    ""
    fixture-3-0-A-ABC
    (buffer-ring-surface-ring r2)
    (dolist (buf (list buf-A buf-B buf-C))
      (buffer-ring-register-ring buf r2))
    (buffer-ring-surface-ring r3)
    (should (eq r3 (car (buffer-ring-get-rings buf-A))))
    (should (eq r3 (car (buffer-ring-get-rings buf-B))))
    (should (eq r3 (car (buffer-ring-get-rings buf-C))))))

(ert-deftest buffer-ring-surface-buffer-test ()
  "Tests for buffer-ring-surface-buffer-test."

  (br-define-test test1
    ""
    fixture-3-AB-BC-AC
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
    (should (eq buf-A (buffer-ring-current-buffer r3)))))

;;
;; External triggers
;;

(ert-deftest buffer-is-killed-test ()
  "Tests for buffer-is-killed."

  (br-define-test test1
    ""
    fixture-1-0+A
    (kill-buffer buf-A)
    (should (dynaring-empty-p (buffer-ring-ring-ring (buffer-ring-current-ring)))))

  (br-define-test test2
    ""
    fixture-1-A
    (kill-buffer buf-A)
    (should-not (dynaring-contains-p (buffer-ring-ring-ring r1)
                                     buf-A)))

  (br-define-test test3
    ""
    fixture-1-A
    (let ((key (buffer-ring-registry-get-key buf-A)))
      (kill-buffer buf-A)
      (should-not (member r1 (ht-get buffer-rings key)))))

  (br-define-test test4
    ""
    fixture-1-A
    (kill-buffer buf-A)
    (should (= 0 (dynaring-size (buffer-ring-ring-ring r1)))))

  (br-define-test test5
    ""
    fixture-3-0-A-ABC
    (kill-buffer buf-A)
    (should-not (dynaring-contains-p (buffer-ring-ring-ring r2)
                                     buf-A)))

  (br-define-test test6
    ""
    fixture-3-0-A-ABC
    (kill-buffer buf-A)
    (should-not (dynaring-contains-p (buffer-ring-ring-ring r3)
                                     buf-A)))

  (br-define-test test7
    ""
    fixture-3-0-A-ABC
    (let ((key (buffer-ring-registry-get-key buf-A)))
      (kill-buffer buf-A)
      (should-not (member r2 (ht-get buffer-rings key)))))

  (br-define-test test8
    ""
    fixture-3-0-A-ABC
    (let ((key (buffer-ring-registry-get-key buf-A)))
      (kill-buffer buf-A)
      (should-not (member r3 (ht-get buffer-rings key)))))

  (br-define-test test9
    ""
    fixture-3-0-A-ABC
    (kill-buffer buf-A)
    (should (= 0 (dynaring-size (buffer-ring-ring-ring r2)))))

  (br-define-test test10
    ""
    fixture-3-0-A-ABC
    (kill-buffer buf-A)
    (should (= 2 (dynaring-size (buffer-ring-ring-ring r3))))))

(ert-deftest buffer-ring-synchronize-buffer-test ()
  "Tests for buffer-ring-synchronize-buffer."

  (br-define-test test1
    "If buffer is unaffiliated, nothing happens."
    fixture-2-0-A
    (let ((new-buf (generate-new-buffer fixture-buffer-name-prefix)))
      (unwind-protect
          (with-current-buffer new-buf
            (buffer-ring-synchronize-buffer)
            (should (eq r2 (buffer-ring-current-ring))))
        (kill-buffer new-buf))))

  (br-define-test test2
    "If buffer is in current ring and already at head, nothing happens."
    fixture-3-0-A-ABC
    (with-current-buffer buf-A
      (buffer-ring-synchronize-buffer)
      (should (eq r3 (buffer-ring-current-ring)))
      (should (eq buf-A (buffer-ring-current-buffer)))))

  (br-define-test test3
    "If buffer is in current ring and not at head, it is reinserted."
    fixture-3-0-A-ABC
    (with-current-buffer buf-C
      (buffer-ring-synchronize-buffer)
      (should (eq r3 (buffer-ring-current-ring)))
      (should (eq buf-C (buffer-ring-current-buffer)))))

  (br-define-test test4
    "If buffer is in a different ring, current ring is changed and it
is placed at the head."
    fixture-3-0-A-BC
    ;; initial ring is r3
    (with-current-buffer buf-A
      (buffer-ring-synchronize-buffer)
      (should (eq r2 (buffer-ring-current-ring)))
      (should (eq buf-A (buffer-ring-current-buffer)))))

  (br-define-test test5
    "Buffer is current in every ring that it is part of."
    fixture-3-0-A-ABC
    (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
    (buffer-ring-next-buffer)
    ;; validate that buf-A is no longer current in ring 3
    (should-not (eq buf-A (buffer-ring-current-buffer r3)))
    (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
    (with-current-buffer buf-A
      (buffer-ring-synchronize-buffer))
    (should (eq buf-A (buffer-ring-current-buffer r2)))
    (should (eq buf-A (buffer-ring-current-buffer r3))))

  (br-define-test test6
    "Recent ring is surfaced in all of its buffers."
    fixture-3-AB-BC-AC
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
    (should (eq r3 (car (buffer-ring-get-rings buf-C))))))

(ert-deftest buffer-ring-synchronize-ring-test ()
  "Tests for buffer-ring-synchronize-ring-test."

  (br-define-test test1
    "Switches to the head buffer."
    fixture-2-AB-AC
    (buffer-ring-synchronize-ring r1)
    (should (eq buf-B (current-buffer))))

  (br-define-test test2
    "Surfaces the ring in all of its buffers."
    fixture-2-AB-AC
    (buffer-ring-synchronize-ring r1)
    (should (eq r1 (car (buffer-ring-get-rings buf-A))))
    (should (eq r1 (car (buffer-ring-get-rings buf-B)))))

  (br-define-test test3
    "Should not switch buffer if the new ring is empty."
    fixture-2-0-A
    (buffer-ring-synchronize-ring r2)
    (should (eq buf-A (current-buffer)))
    (buffer-ring-synchronize-ring r1)
    (should (eq buf-A (current-buffer)))))

;;
;; "Integration" tests
;;

(ert-deftest buffer-torus-switch-ring-integration-test ()
  "Integration tests for buffer-torus-switch-to-ring, to ensure that
it surfaces both ring and buffer."

  (br-define-integration-test test1
    "Surfaces ring."
    fixture-3-AB-BC-AC
    ;; switching to a ring causes all of its buffers to show that
    ;; ring as most recent, even if the last time those buffers were
    ;; active was on another ring
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
    (should (eq r1 (car (buffer-ring-get-rings buf-B)))))

  (br-define-integration-test test2
    "Surfaces head buffer."
    fixture-3-AB-BC-AC
    ;; switching to a ring causes its head buffer to be head
    ;; in all of its rings
    (buffer-ring-torus-switch-to-ring fixture-ring-1-name) ; on B
    (buffer-ring-torus-switch-to-ring fixture-ring-2-name) ; still on B
    (buffer-ring-next-buffer) ; now on C on r2
    (buffer-ring-torus-switch-to-ring fixture-ring-3-name) ; on C in r3
    (buffer-ring-torus-switch-to-ring fixture-ring-1-name) ; B is at head
    (should (eq buf-B (current-buffer)))
    (should (eq buf-B (buffer-ring-current-buffer r1)))
    (should (eq buf-B (buffer-ring-current-buffer r2)))))

(ert-deftest buffer-torus-rotation-integration-test ()
  "Integration tests for buffer torus rotation, to check that it
surfaces both ring and buffer."

  (br-define-integration-test test1
    "Surfaces ring."
    fixture-3-AB-BC-AC
    ;; rotating to a ring causes all of its buffers to show that
    ;; ring as most recent, even if the last time those buffers were
    ;; active was on another ring
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
    (should (eq r1 (car (buffer-ring-get-rings buf-B)))))

  (br-define-integration-test test2
    "Surfaces head buffer."
    fixture-2-AB-AC
    ;; rotating to a ring causes its head buffer to be head
    ;; in all of its rings
    (buffer-ring-next-buffer) ; now A, on r2
    (buffer-ring-torus-next-ring) ; r2 -> r1, remaining on A
    (buffer-ring-next-buffer) ; now B
    (buffer-ring-torus-prev-ring) ; back on A in r2
    ;; A should be head again in r1
    (should (eq buf-A (buffer-ring-current-buffer r2)))
    (should (eq buf-A (buffer-ring-current-buffer r1)))))

(ert-deftest buffer-ring-rotation-integration-test ()
  "Integration tests for buffer ring rotation."

  (br-define-integration-test test1
    "Check that it surfaces the buffer."
    fixture-2-AB-AC
    ;; rotating the ring ensures that the new head buffer
    ;; is at the head of all rings that it is part of
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
    (should (eq buf-A (buffer-ring-current-buffer r2)))))

(ert-deftest buffer-ring-switch-to-buffer-directly-test ()
  "Integration tests for switching to a buffer 'out of band', i.e. directly."

  (br-define-integration-test test2
    "Switching to a buffer directly remains on current ring if the buffer is a member."
    fixture-3-AB-BC-AC
    ;; start on ring 1 with buffers A and B
    (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
    (buffer-ring-next-buffer) ; rotate to A
    (buffer-ring-torus-switch-to-ring fixture-ring-3-name)
    (buffer-ring-next-buffer) ; rotate A -> C
    (buffer-ring-torus-switch-to-ring fixture-ring-2-name)
    (switch-to-buffer buf-B)
    ;; we should remain on r2 since B is on the current ring
    (should (eq fixture-ring-2-name (buffer-ring-current-ring-name))))

  (br-define-integration-test test3
    "Switching to a buffer directly switches to its most recent ring."
    fixture-3-AB-BC-AC
    ;; start on ring 1 with buffers A and B
    (buffer-ring-torus-switch-to-ring fixture-ring-1-name)
    (buffer-ring-next-buffer) ; rotate to A
    (buffer-ring-torus-switch-to-ring fixture-ring-2-name) ; on C
    (buffer-ring-torus-switch-to-ring fixture-ring-3-name) ; still on C
    ;; the last time we saw B was on r1
    (switch-to-buffer buf-B)
    ;; but ring 2 was more recent so it should become current
    (should (eq fixture-ring-2-name (buffer-ring-current-ring-name))))

  (br-define-integration-test test4
    "Switching to a buffer directly surfaces its most recent ring."
    fixture-3-AB-BC-AC
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
    (should (eq r2 (car (buffer-ring-get-rings buf-C)))))

  (br-define-integration-test test5
    "Switching to a buffer directly surfaces it in all of its rings."
    fixture-3-AB-BC-AC
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
    (should (eq buf-B (buffer-ring-current-buffer r2)))))
