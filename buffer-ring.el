;;; buffer-ring.el --- Rings and tori for buffer navigation -*- lexical-binding: t -*-

;; Copyright (C) 2009 Mike Mattie
;; Author: Mike Mattie codermattie@gmail.com
;; Maintainer: Mike Mattie codermattie@gmail.com
;; URL: https://github.com/countvajhula/buffer-ring
;; Created: 2009-4-16
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (dynamic-ring "0.0.2") (s "1.12.0"))

;; This file is NOT a part of Gnu Emacs.

;; License: GPL-v3

;; buffer-ring.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Rings of buffers and tori of buffer rings.

;;; Code:

(defconst buffer-ring-version "0.1.1" "buffer-ring version")
(require 'dynamic-ring)
(require 's)

;;
;; default keymap
;;

(global-set-key (kbd "C-c C-b b") 'buffer-ring-list-buffers)
(global-set-key (kbd "C-c C-b r") 'buffer-torus-list-rings)

(global-set-key (kbd "C-c C-b a") 'buffer-ring-add)
(global-set-key (kbd "C-c C-b d") 'buffer-ring-delete)
(global-set-key (kbd "C-c C-b c") 'buffer-ring-drop-buffer)

(global-set-key (kbd "C-c C-b f") 'buffer-ring-next-buffer)
(global-set-key (kbd "C-c C-b b") 'buffer-ring-prev-buffer)

(global-set-key (kbd "C-c C-b n") 'buffer-torus-next-ring)
(global-set-key (kbd "C-c C-b p") 'buffer-torus-prev-ring)
(global-set-key (kbd "C-c C-b e") 'buffer-torus-delete-ring)

(defvar buffer-ring-torus (make-dyn-ring)
  "a global ring of all the buffer rings. A torus I believe.")

(defun buffer-ring-initialize ()
  "Set up any hooks needed for buffer rings."
  (interactive)
  ;; TODO: if we want to add all buffers to a "primary"
  ;; ring, we should also hook into buffer-list-changed-hook
  ;; or maybe find-file-hook in addition here
  ;; TODO: should this be buffer-local? in that case it can
  ;; be added at the time that the buffer is adding to a ring
  (advice-add 'switch-to-buffer
              :after #'buffer-ring-set-buffer-context))

(defun buffer-ring-disable ()
  "Remove hooks, etc."
  (interactive)
  (advice-remove 'switch-to-buffer #'buffer-ring-set-buffer-context))

;;
;;  buffer ring structure
;;

(defun make-bfr-ring (name)
  (cons name (make-dyn-ring)))

(defun bfr-ring-name (buffer-ring)
  (car buffer-ring))

(defun bfr-ring-ring (buffer-ring)
  (cdr buffer-ring))

;;
;; buffer rings registry
;;
;; TODO: consider buffer local variables
(defvar buffer-rings
  (ht)
  "Buffer to rings hash.")

(defun bfr-registry-get-key (buffer)
  "Key to use for BUFFER in the buffer registry."
  (buffer-name buffer))

(defun bfr-get-rings (&optional buffer)
  "All rings that BUFFER is part of."
  (let ((buffer (or buffer (current-buffer))))
    (ht-get buffer-rings (bfr-registry-get-key buffer))))

(defun bfr-register-ring (buffer bfr-ring)
  "Register that BUFFER has been added to BFR-RING."
  (let ((key (bfr-registry-get-key buffer)))
    (ht-set! buffer-rings
             key
             (delete-dups
              (cons bfr-ring
                    (ht-get buffer-rings
                            key))))))

(defun bfr-delete-ring (buffer bfr-ring)
  "Delete BFR-RING from the list of rings for BUFFER.

This does NOT delete the buffer from the ring, only the ring
identifier from the buffer. It should be called only as part
of doing the former."
  (let ((key (buffer-name buffer)))
    (ht-set! buffer-rings
             key
             (remq bfr-ring
                   (ht-get buffer-rings
                           key)))))

;;
;; buffer ring interface
;;

(defun bfr-ring-size ()
  "bfr-ring-size

   Returns the number of buffers in the current ring.
   If there is no active buffer ring, it returns -1 so that
   you can always use a numeric operator.
  "
  (let ((ring (bfr-ring-ring (bfr-current-ring))))
    (if ring
        (dyn-ring-size ring)
      -1)))

(defun bfr--add-buffer-to-ring (buffer bfr-ring)
  "Add BUFFER to BFR-RING."
  (let ((ring (bfr-ring-ring bfr-ring)))
    (dyn-ring-insert ring buffer)
    (bfr-register-ring buffer bfr-ring)
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook 'buffer-ring-drop-buffer t t))))

(defun buffer-ring-add (ring-name &optional buffer)
  "buffer-ring-add RING-NAME BUFFER

   Add the buffer to a ring. It will prompt for the ring
   to add the buffer to, and assumes the current buffer
   if none is provided.
  "
  (interactive "sAdd to ring ? ")
  (let* ((bfr-ring (bfr-torus-get-ring ring-name))
         (ring (bfr-ring-ring bfr-ring))
         (buffer (or buffer (current-buffer))))
    (cond ((dyn-ring-contains-p ring buffer)
           (message "buffer %s is already in ring \"%s\"" (buffer-name)
                    ring-name)
           nil)
          (t (bfr--add-buffer-to-ring buffer bfr-ring)
             (bfr-torus-switch-to-ring ring-name)
             t))))

(defun buffer-ring-delete (&optional buffer)
  "buffer-ring-delete

   Delete the current buffer from the current ring.
   This modifies the ring, it does not kill the buffer.
  "
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (if (bfr-current-ring)
        (let ((ring (bfr-ring-ring (bfr-current-ring))))
          (if (dyn-ring-delete ring buffer)
              (progn
                (bfr-delete-ring buffer (bfr-current-ring))
                (message "Deleted buffer %s from ring %s"
                         buffer
                         (bfr-current-ring-name)))
            (message "This buffer is not in the current ring")
            nil))
      (message "No active buffer ring.")
      nil)))

(defun buffer-ring-drop-buffer ()
  "Drop buffer from all rings.

Not to be confused with the little-known evil cousin
to the koala buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (save-excursion
      (dolist (bfr-ring (bfr-get-rings buffer))
        ;; TODO: this may muddle torus recency
        (bfr-torus-switch-to-ring (bfr-ring-name bfr-ring))
        (buffer-ring-delete buffer)))
    (remove-hook 'kill-buffer-hook 'buffer-ring-drop-buffer t)))

(defun buffer-ring-list-buffers ()
  "buffer-ring-list-buffers

   List the buffers in the current buffer ring.
  "
  (interactive)
  (let* ((bfr-ring (bfr-current-ring))
         (ring (bfr-ring-ring bfr-ring)))
    (if bfr-ring
        (let ((result (dyn-ring-traverse-collect ring #'buffer-name)))
          (if result
              (message "buffers in [%s]: %s" (bfr-ring-name bfr-ring) result)
            (message "Buffer ring is empty.")))
      (message "No active buffer ring."))) )

;; TODO: standardize interface names
(defun bfr-ring--rotate (direction)
  (let ((bfr-ring (bfr-current-ring)))
    (when bfr-ring
      (let ((ring (bfr-ring-ring bfr-ring)))
        (unless (dyn-ring-empty-p ring)
          (when (= 1 (dyn-ring-size ring))
            (message "There is only one buffer in the ring."))
          (funcall direction ring)
          (switch-to-buffer (dyn-ring-value ring)))))))

(defun buffer-ring-prev-buffer ()
  "buffer-ring-prev-buffer

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (bfr-ring--rotate #'dyn-ring-rotate-left))

(defun buffer-ring-next-buffer ()
  "buffer-ring-next-buffer

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (bfr-ring--rotate #'dyn-ring-rotate-right))

(defun buffer-ring-set-buffer-context (&rest args)
  "If a buffer is visited directly without rotating
   to it, it should modify the ring structure so that
   recency is accounted for correctly."
  (let* ((buffer (current-buffer))
         (bfr-rings (bfr-get-rings buffer)))
    (when bfr-rings
      (let ((ring (bfr-ring-ring (bfr-current-ring))))
        (if (dyn-ring-contains-p ring buffer)
            ;; if it is already at the head, we don't
            ;; need to do anything, and we probably arrived
            ;; here via a buffer-ring interface
            (unless (eq buffer (dyn-ring-value ring))
              (dyn-ring-break-insert ring buffer))
          (bfr-torus-switch-to-ring
           (bfr-ring-name (car bfr-rings))))))))

(defun buffer-ring-surface-ring (&optional bfr-ring)
  "Make BFR-RING the most recent ring in all member buffers.

We'd want to do this each time the ring becomes current, so that
ring recency is consistent across the board."
  (let ((bfr-ring (or bfr-ring (bfr-current-ring))))
    (dolist (buffer (dyn-ring-values (bfr-ring-ring bfr-ring)))
      (bfr-register-ring buffer bfr-ring))))

;;
;; buffer torus interface
;;

(defun bfr-torus--create-ring (name)
  "Create ring with name NAME."
  (let ((bfr-ring (make-bfr-ring name)))
    (dyn-ring-insert buffer-ring-torus bfr-ring)
    bfr-ring))

(defun bfr-torus-get-ring (name)
  "bfr-torus-get-ring NAME

   Find a existing buffer ring, or create a new buffer ring with name.
   The buffer-ring is returned.
  "
  (let ((segment (dyn-ring-find-forwards buffer-ring-torus
                                         (lambda (r)
                                           (string= name
                                                    (bfr-ring-name r))))))
    (if segment
        (progn
          (message "Found existing ring: %s" name)
          (dyn-ring-segment-value segment))
      (message "Creating a new ring \"%s\"" name)
      (bfr-torus--create-ring name))))

(defun bfr-torus-switch-to-ring (name)
  "Switch to ring NAME."
  (interactive "sSwitch to ring ? ")
  (let ((buffer (current-buffer))
        (segment (dyn-ring-find-forwards buffer-ring-torus
                                         (lambda (r)
                                           (string= name
                                                    (bfr-ring-name r))))))
    (when segment
      (let ((bfr-ring (dyn-ring-segment-value segment)))
        ;; switch to ring and reinsert it at the head
        (dyn-ring-break-insert buffer-ring-torus
                               bfr-ring)
        (bfr-torus--switch-ring bfr-ring buffer)))))

(defun bfr-current-ring ()
  (dyn-ring-value buffer-ring-torus))

(defun bfr-current-ring-name ()
  (bfr-ring-name (bfr-current-ring)))

(defun bfr-ring-current-buffer (&optional bfr-ring)
  "Current buffer in BFR-RING."
  (let ((bfr-ring (or bfr-ring (bfr-current-ring))))
    (dyn-ring-value (bfr-ring-ring bfr-ring))))

(defun bfr-torus--switch-ring (bfr-ring buffer)
  "Perform any actions in connection with switching to a new ring.

BFR-RING is the new ring switched to, and BUFFER is the original buffer."
  (let ((ring (bfr-ring-ring bfr-ring)))
    ;; bring the buffer ring "to the surface" across all
    ;; member buffers, as the most recent one
    (buffer-ring-surface-ring bfr-ring)
    ;; if original buffer is in the new ring, stay there
    ;; and reinsert it to account for recency
    (when (dyn-ring-contains-p ring buffer)
      (dyn-ring-break-insert ring buffer))
    (switch-to-buffer
     (bfr-ring-current-buffer bfr-ring))))


(defun bfr-torus--rotate (direction)
  (let ((buffer (current-buffer))
        (initial-bfr-ring (bfr-current-ring)))
    (cond ((dyn-ring-empty-p buffer-ring-torus)
           (message "There are no rings in the buffer torus.")
           nil)
          ((= 1 (dyn-ring-size buffer-ring-torus))
           (message "There is only one buffer ring.")
           (unless (dyn-ring-empty-p (bfr-ring-ring (bfr-current-ring)))
             (switch-to-buffer
              (bfr-ring-current-buffer (bfr-current-ring))))
           t)
          (t
           ;; rotate past any empties
           (if (dyn-ring-rotate-until buffer-ring-torus
                                      direction
                                      (lambda (bfr-ring)
                                        ;; we want to rotate at least once
                                        (and (not (eq initial-bfr-ring
                                                      bfr-ring))
                                             (not (dyn-ring-empty-p
                                                   (bfr-ring-ring bfr-ring))))))
               (progn
                 (message "switching to ring %s" (bfr-current-ring-name))
                 (bfr-torus--switch-ring (bfr-current-ring) buffer))
             (message "All of the buffer rings are empty. Keeping the current ring position")
             nil)))))

(defun buffer-torus-next-ring ()
  "buffer-torus-next-ring

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (bfr-torus--rotate 'dyn-ring-rotate-right))

(defun buffer-torus-prev-ring ()
  "buffer-torus-prev-ring

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (bfr-torus--rotate 'dyn-ring-rotate-left))

(defun buffer-torus-list-rings ()
  "buffer-torus-list-rings.

   List the buffer rings in the buffer torus.
  "
  (interactive)
  (message "buffer rings: %s"
           (s-join ", " (dyn-ring-traverse-collect buffer-ring-torus
                                                   #'bfr-ring-name))))

(defun buffer-torus-delete-ring ()
  "buffer-torus-delete-ring

   Delete the entire current buffer-ring.
  "
  (interactive)
  (let ((bfr-ring (bfr-current-ring)))
    (dyn-ring-delete buffer-ring-torus
                     bfr-ring)
    (dyn-ring-destroy (bfr-ring-ring bfr-ring))))

(provide 'buffer-ring)
;;; buffer-ring.el ends here
