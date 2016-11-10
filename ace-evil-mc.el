;;; ace-evil-mc.el --- Ace-mc with evil-mc support

;; Copyright (C) 2016 Liu233w
;;               2015 Josh Moller-Mara

;; Author: Liu233w <wwwlsmcom@outlook.com>
;; Version: 1.0
;; Package-Requires: ((ace-jump-mode "1.0") (evil-mc "0.0.2") (dash "2.10.0"))
;; Keywords: motion, location, cursor
;; URL: https://github.com/Liu233w/ace-evil-mc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ace-evil-mc.el is a package that allows you to quickly add and remove
;; evil-mc cursors using ace-jump.

;; This package adds two commands: `ace-evil-mc-add-multiple-cursors' and
;; `ace-evil-mc-add-single-cursor'.  Both commands act like
;; `ace-jump-mode', accepting simliar prefix arguments. However,
;; instead of jumping to the location, as with ace-jump, the command
;; will add a new multiple cursor mark. If one is already there, it
;; will be removed. This makes it easy to remove cursors you've added
;; accidentally.

;; `ace-evil-mc-add-multiple-cursors' will continue to keep prompting for
;; places to add or remove cursors until you hit Enter. The command
;; `ace-evil-mc-add-single-cursor' is a non-looping version.

;;; Code:
(require 'ace-jump-mode)
(require 'evil-mc)
(require 'dash)

(defvar ace-evil-mc-marking nil
  "Internal flag for detecting if currently marking.")

(defvar ace-evil-mc-keyboard-reset nil
  "See if we've quit out yet.")

(defvar ace-evil-mc-query-char nil
  "Char.")

(defvar ace-evil-mc-loop-marking nil
  "Keep adding until we quit.")

(defvar ace-evil-mc-saved-point nil
  "The position of our cursor before jumping around with ace-jump.")

(defvar ace-evil-mc-ace-mode-function nil
  "The function from `ace-jump-mode-submode-list' to use.")

(defun ace-evil-mc-maybe-jump-start ()
  "Push the mark when marking with `ace-jump-char-mode'."
  (when ace-evil-mc-marking
    (setq ace-evil-mc-saved-point (point)
          ace-evil-mc-keyboard-reset nil)))

(defun ace-evil-mc-maybe-jump-end ()
  "Add/remove cursor jumping with `ace-jump-char-mode'."
  (if (not ace-evil-mc-marking)
      (ace-evil-mc-reset)
    (let ((ace-evil-mc-fake-cursor-at-point
           (-filter (lambda (a)
                      (eql (evil-mc-get-cursor-start a) (point)))
                    evil-mc-cursor-list)))
      (if ace-evil-mc-fake-cursor-at-point
          (evil-mc-undo-cursor (car ace-evil-mc-fake-cursor-at-point))
        (unless (equal ace-evil-mc-saved-point (point))
          (evil-mc-make-cursor-here))))
    (when ace-evil-mc-saved-point
      (goto-char ace-evil-mc-saved-point))
    (if (and ace-evil-mc-loop-marking
             (not ace-evil-mc-keyboard-reset)
             ;; Pretty hacky, but we look at "candidate-list" defined
             ;; in ace-jump-do.  If it's defined without any cdr,
             ;; there's only one candidate, and we should stop
             ;; looping.
             (or (not (boundp 'candidate-list)) (cdr candidate-list)))
        (ace-evil-mc-add-char ace-evil-mc-query-char)
      (ace-evil-mc-reset))))

(add-hook 'ace-jump-mode-before-jump-hook #'ace-evil-mc-maybe-jump-start)

(add-hook 'ace-jump-mode-end-hook #'ace-evil-mc-maybe-jump-end)

(defun ace-evil-mc-reset ()
  "Reset the internal jumping flags."
  (setq ace-evil-mc-marking nil))

(defun ace-evil-mc-do-keyboard-reset ()
  "Reset when `ace-jump-mode' is cancelled.
Also called when chosen character isn't found while zapping."
  (interactive)
  (ace-evil-mc-reset)
  (ace-jump-done))

;;;###autoload
(defun ace-evil-mc-add-multiple-cursors (&optional prefix single-mode)
  "Use AceJump to add or remove multiple cursors.

ace-evil-mc-add-multiple-cursors will prompt your for locations to add
multiple cursors.  If a cursor already exists at that location,
it will be removed.  This process continues looping until you
exit, for example by pressing return or escape.

Without a \\[universal-argument] prefix argument, use the default
AceJump jumping mode as described in
`ace-jump-mode-submode-list'.  When called interactively with one
or more \\[universal-argument] prefix arguments PREFIX, use the
corresponding mode from `ace-jump-mode-submode-list'.  For
example, by default
   \\[ace-evil-mc-add-multiple-cursors] ==> ace-jump-word-mode
   \\[universal-argument] \\[ace-evil-mc-add-multiple-cursors] ==> ace-jump-char-mode
   \\[universal-argument] \\[universal-argument] \\[ace-evil-mc-add-multiple-cursors] ==> ace-jump-line-mode

If SINGLE-MODE is set to 't', don't loop.

When the region is active, prompt for AceJump matches based on matching strings."
  (interactive "pi")
  (let* ((index (/ (logb prefix) 2))
         (submode-list-length (length ace-jump-mode-submode-list)))
    (setq ace-evil-mc-loop-marking (not single-mode))
    (if (< index 0)
        (setq index 0))
    (if (>= index submode-list-length)
        (setq index (- submode-list-length 1)))
    (setq ace-evil-mc-ace-mode-function (if (use-region-p)
                                       'ace-evil-mc-regexp-mode
                                     (nth index ace-jump-mode-submode-list)))
    ;; Sometimes we want to go to different characters. Gets reset with movement.
    (if (use-region-p)
        (progn
          (backward-char)
          (when (> (point) (mark))
            (evil-mc-execute-for-all-cursors 'exchange-point-and-mark)
            )
          (deactivate-mark)
          (ace-evil-mc-add-char (buffer-substring-no-properties (mark) (point))))
      (ace-evil-mc-add-char (unless (eq ace-evil-mc-ace-mode-function 'ace-jump-line-mode)
                         (read-char "Query Char:"))))))

;;;###autoload
(defun ace-evil-mc-add-single-cursor (&optional prefix)
  "Add a single multiple cursor.

This is a wrapper for `ace-evil-mc-add-multiple-cursors', only adding
a single cursor.

PREFIX is passed to `ace-evil-mc-add-multiple-cursors', see the
documentation there."
  (interactive "p")
  (ace-evil-mc-add-multiple-cursors prefix t))

(defun ace-evil-mc-regexp-mode (regex)
  "Ace Jump Multiple Cursor with a REGEX."
  (ace-jump-do (regexp-quote regex)))

(defun ace-evil-mc-add-char (query-char)
  "Call `ace-jump-char-mode' with a character QUERY-CHAR and add a cursor at the point."
  (let ((ace-jump-mode-scope 'window))
    (setq ace-evil-mc-marking t
          ace-evil-mc-query-char query-char)
    (if query-char
        (funcall ace-evil-mc-ace-mode-function query-char)
      (funcall ace-evil-mc-ace-mode-function))
    (when overriding-local-map
      (define-key overriding-local-map [t] 'ace-evil-mc-do-keyboard-reset))))

;; Prevent keyboard-reset from being added to mc-list
;; mc/cmds-to-run-once
;; Use ace-jump-do when the region is active.

(mapc (lambda (el) (add-to-list 'evil-mc-known-commands el))
      '(ace-evil-mc-add-char
        ace-evil-mc-do-keyboard-reset
        ace-evil-mc-add-multiple-cursors
        ace-evil-mc-add-single-cursor
        ace-jump-move
        ))

(provide 'ace-evil-mc)
;;; ace-evil-mc.el ends here
