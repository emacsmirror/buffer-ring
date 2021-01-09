;;; buffer-ring.el --- A torus for buffer navigation. A ring of buffers, and a ring of buffer rings. -*- lexical-binding: t -*-

;; Copyright (C) 2009 Mike Mattie
;; Author: Mike Mattie codermattie@gmail.com
;; Maintainer: Mike Mattie codermattie@gmail.com
;; Created: 2009-4-16
;; Version: 0.1.0
;; Package-Requires: ((dynamic-ring "0.0.2") (s "1.12.0"))

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

(defvar buffer-ring-default nil
  "The default buffer ring")

(defun buffer-ring-initialize ()
  "Set up any hooks needed for buffer rings."
  (interactive)
  (advice-add 'switch-to-buffer
              :around #'buffer-ring-jump-to-buffer)
  ;; TODO: if we want to add all buffers to a "primary"
  ;; ring, we should also hook into buffer-list-changed-hook
  ;; or maybe find-file-hook in addition here
  )

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

(defun bfr-get-rings (&optional buffer)
  "All rings that BUFFER is part of."
  (let ((buffer (or buffer (current-buffer))))
    (ht-get buffer-rings (buffer-name buffer))))

(defun bfr-register-ring (buffer bfr-ring)
  "Register that BUFFER has been added to BFR-RING."
  (let* ((key (buffer-name buffer))
         (rings (ht-get buffer-rings key)))
    (ht-set! buffer-rings
             key
             (cons bfr-ring
                   rings))))

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

(defun buffer-ring-add (ring-name)
  "buffer-ring-add RING-NAME

   Add the current buffer to a ring. It will prompt for the ring
   to add the buffer to.
  "
  (interactive "sAdd to ring ? ")
  (let* ((bfr-ring (bfr-torus-get-ring ring-name))
         (ring (bfr-ring-ring bfr-ring))
         (buffer (current-buffer)))
    (cond ((dyn-ring-contains-p ring buffer)
           (message "buffer %s is already in ring \"%s\"" (buffer-name)
                    ring-name)
           nil)
          (t (dyn-ring-insert ring buffer)
             (bfr-register-ring buffer bfr-ring)
             (add-hook 'kill-buffer-hook 'buffer-ring-drop-buffer t t)
             t))))

(defun buffer-ring-delete ()
  "buffer-ring-delete

   Delete the current buffer from the current ring.
   This modifies the ring, it does not kill the buffer.
  "
  (interactive)
  (let ((ring (bfr-ring-ring (bfr-current-ring)))
        (buffer (current-buffer)))
    (if (dyn-ring-delete ring buffer)
        (message "Deleted buffer from ring %s" (bfr-current-ring-name))
      (message "This buffer is not in the current ring"))))

(defun buffer-ring-drop-buffer ()
  "Drop buffer from all rings."
  (interactive)
  (remove-hook 'kill-buffer-hook 'buffer-ring-drop-buffer t))

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
  (let ((ring (bfr-ring-ring (bfr-current-ring))))
    (if (< (dyn-ring-size ring) 2)
        (message "There is only one buffer in the ring.")
      (progn
        (funcall direction ring)
        (switch-to-buffer (dyn-ring-value ring))))))

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

(defun bfr-ring-jump-to-buffer ()
  "If a buffer is visited directly without rotating
   to it, it should modify the ring structure so that
   recency is accounted for correctly."
  ;; if ∈ current ring, break-and-insert
  ;; elif ∈ some ring R, switch to one of them
  ;;   this should itself be a ring of rings, but just
  ;;   use a list for now
  ;; else do nothing - we retain our position in the
  ;; active buffer ring, and any buffer-ring operations
  ;; would assume the current buffer doesn't even exist
  ;; or rather, would assume that we are currently at head
  (let* ((buffer (current-buffer))
         (bfr-rings (bfr-get-rings buffer)))
    (cond ((dyn-ring-contains-p (bfr-current-ring)
                                buffer)
           (dyn-ring-break-insert (bfr-ring-ring (bfr-current-ring))
                                  (current-buffer)))
          (bfr-rings
           (bfr-torus-switch-to-ring
            (bfr-ring-name (car bfr-rings)))))))

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
   buffer-ring-default is updated. The buffer-ring is returned.
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
  (interactive)
  (let ((segment (dyn-ring-find-forwards buffer-ring-torus
                                         (lambda (r)
                                           (string= name
                                                    (bfr-ring-name r))))))
    (when segment
      (let ((ring (dyn-ring-segment-value segment)))
        (dyn-ring-break-insert buffer-ring-torus
                               ring)
        (switch-to-buffer
         (dyn-ring-value ring))))))

(defun bfr-current-ring-name ()
  (bfr-ring-name (dyn-ring-value buffer-ring-torus)))

(defun bfr-current-ring ()
  (dyn-ring-value buffer-ring-torus))

(defun bfr-torus--rotate ( direction )
  (if (< (dyn-ring-size buffer-ring-torus) 2)
    (message "There is only one buffer ring; ignoring the rotate global ring command")
    ;; rotate past any empties
    (if (dyn-ring-rotate-until buffer-ring-torus
                               direction
                               (lambda (ring)
                                 (not (dyn-ring-empty-p ring))))
      (progn
        (message "switching to ring %s" (bfr-current-ring-name))
        (switch-to-buffer
         (dyn-ring-value (bfr-current-ring))))
      (message "All of the buffer rings are empty. Keeping the current ring position")) ))

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
  (dyn-ring-delete buffer-ring-torus (bfr-current-ring)))

(provide 'buffer-ring)
;;; buffer-ring.el ends here
