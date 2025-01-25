;;; mini-ontop.el --- Prevent windows from jumping on minibuffer activation -*- lexical-binding: t; -*-
;;
;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; URL: https://github.com/hkjels/mini-ontop.el
;; Version: 0.2
;; Package-Requires: ((emacs "26.1") cl-lib)
;; Keywords: convenience
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; mini-ontop-mode is a minor mode that prevents Emacs from scrolling the main
;; editing window when a multi-line minibuffer (e.g., icomplete-vertical)
;; appears. It automatically adjusts point just enough so that Emacs doesn't
;; force a jump in the visible buffer. Once the minibuffer session is complete,
;; point is restored if it hasn’t been moved manually by the user.
;;
;; Usage:
;;
;;   (require 'mini-ontop)
;;   (mini-ontop-mode 1)
;;
;; That’s it! With `mini-ontop-mode` enabled, your main buffer will no longer
;; jump when you press M-x or other commands that open multi-line minibuffers,
;; and there’s no extra configuration necessary.
;;
;; If you'd like to tweak its behavior, consult the code or customize the user
;; options. But for most people, the default settings “just work.”
;;
;; License:
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Code:

(require 'cl-lib)

(defgroup mini-ontop nil
  "Preempt scrolling by moving point up before the minibuffer is used."
  :group 'convenience)

(defcustom mini-ontop-ignore-predicates nil
  "List of predicates to prevent mini-ontop behavior.
If *any* of them returns non-nil, `mini-ontop-mode' will skip moving point up.

For example, you could add a predicate that checks if `ivy-yasnippet' is active,
or any custom condition you want to exclude from this behavior."
  :type '(repeat function)
  :group 'mini-ontop)

(defcustom mini-ontop-lines
  (let* ((fraction (if (floatp max-mini-window-height)
                       max-mini-window-height
                     (/ (float max-mini-window-height) (frame-height))))
         (approx-lines (round (* (frame-height) fraction))))
    (max 10 (+ 2 approx-lines)))
  "Number of lines to jump to make sure scroll does not occur.

This default attempts to estimate a good value by looking at
`max-mini-window-height`.  If you're using a specialized completion
framework (e.g. Ivy, Vertico, icomplete-vertical) that can
show many lines, you may need to set this higher.

Set this to a comfortably larger number than your typical
minibuffer height to avoid any jump/flicker."
  :type 'integer
  :group 'mini-ontop)

(defvar mini-ontop--saved-positions nil
  "List of saved positions for each window that had its point moved.
Each element is (WINDOW BUFFER POINT).")

(defun mini-ontop--any-predicate-p ()
  "Evaluate the predicates in `mini-ontop-ignore-predicates'."
  (cl-some (lambda (pred) (funcall pred)) mini-ontop-ignore-predicates))

(defun mini-ontop--distance-to-bottom (win)
  "Return how many buffer lines exist between point and WIN's bottom."
  (save-excursion
    (with-selected-window win
      (max 0 (- (floor (window-screen-lines))
            (count-screen-lines (window-start) (point)))))))

(defun mini-ontop--move-point-up-for-window (win)
  "Check WIN: if point is within `mini-ontop-lines' lines of bottom, move it up.
Return t if point was moved, nil otherwise."
  (with-selected-window win
    (let ((buf (window-buffer win)))
      (unless (or (region-active-p)
                  (minibufferp buf)
                  (mini-ontop--any-predicate-p)
                  (< (count-screen-lines (window-start) (window-end nil t) nil win) mini-ontop-lines))
        (let ((dist (mini-ontop--distance-to-bottom win)))
          (when (< dist mini-ontop-lines)
            (let ((needed (- mini-ontop-lines dist)))
              (push (list win buf (point)) mini-ontop--saved-positions)
              (previous-line needed)
              t)))))))

(defun mini-ontop--move-point-up-if-needed-everywhere ()
  "For each live window, move point up if needed."
  (setq mini-ontop--saved-positions nil)
  (dolist (win (window-list))
    (mini-ontop--move-point-up-for-window win)))

(defun mini-ontop--restore-point ()
  "Restore the original point in all windows that were adjusted."
  (dolist (entry mini-ontop--saved-positions)
    (cl-destructuring-bind (win buf pt) entry
      (when (and (window-live-p win)
                 (buffer-live-p buf))
        (with-selected-window win
          (with-current-buffer buf
            (goto-char pt))))))
  (setq mini-ontop--saved-positions nil))

(defun mini-ontop--exit-hook (&rest _args)
  "Hook run upon minibuffer exit; restore points if needed."
  (mini-ontop--restore-point)
  (remove-hook 'minibuffer-exit-hook #'mini-ontop--exit-hook))

(defun mini-ontop--teardown (&rest _args)
  "Run after `read-from-minibuffer`; remove the exit hook for cleanup."
  (remove-hook 'minibuffer-exit-hook #'mini-ontop--exit-hook))

(defun mini-ontop--setup (&rest _args)
  "Run before `read-from-minibuffer`; move point up in all windows if needed."
  (mini-ontop--move-point-up-if-needed-everywhere)
  (add-hook 'minibuffer-exit-hook #'mini-ontop--exit-hook))

;;;###autoload
(define-minor-mode mini-ontop-mode
  "Global minor mode to prevent windows from jumping on minibuffer activation."
  :global t
  (if mini-ontop-mode
      (progn
        (advice-add 'read-from-minibuffer :before #'mini-ontop--setup)
        (advice-add 'read-from-minibuffer :after  #'mini-ontop--teardown))
    (advice-remove 'read-from-minibuffer #'mini-ontop--setup)
    (advice-remove 'read-from-minibuffer #'mini-ontop--teardown)))

(provide 'mini-ontop)
;;; mini-ontop.el ends here
