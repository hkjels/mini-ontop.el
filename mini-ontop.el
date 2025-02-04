;;; mini-ontop.el --- Prevent windows from jumping on minibuffer activation -*- lexical-binding: t; -*-
;;
;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; URL: https://github.com/hkjels/mini-ontop.el
;; Version: 0.3
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

(defvar mini-ontop--saved-positions-stack nil
  "Stack of saved positions.
Each element is a list of entries for a minibuffer or which-key session.
An entry is a list of the form (WINDOW BUFFER POINT).")

(defun mini-ontop--any-predicate-p ()
  "Return non-nil if any predicate in `mini-ontop-ignore-predicates' returns non-nil."
  (cl-some (lambda (pred) (funcall pred)) mini-ontop-ignore-predicates))

(defun mini-ontop--distance-to-bottom (win)
  "Return the number of buffer lines between point and WIN's bottom."
  (with-selected-window win
    (save-excursion
      (let ((current (line-number-at-pos (point)))
            (bottom (progn
                      (goto-char (window-end win t))
                      (line-number-at-pos (point)))))
        (max 0 (- bottom current))))))

(defun mini-ontop--move-point-up-for-window (win)
  "If WIN’s point is too close to the bottom, move it up.
Return a cons cell (WIN . OLD-POINT) if moved, or nil otherwise."
  (with-selected-window win
    (let ((buf (window-buffer win)))
      (unless (or (region-active-p)
                  (minibufferp buf)
                  (mini-ontop--any-predicate-p)
                  (< (count-screen-lines (window-start) (window-end nil t) nil win)
                     mini-ontop-lines))
        (let ((dist (mini-ontop--distance-to-bottom win)))
          (when (< dist mini-ontop-lines)
            (let ((needed (- mini-ontop-lines dist))
                  (old (point)))
              (forward-line (- needed))
              (cons win old))))))))

(defun mini-ontop--move-point-up-for-all-windows ()
  "Move point up in all eligible windows and return a list of saved positions.
Each saved position is a list of the form (WINDOW BUFFER OLD-POINT)."
  (let (saved)
    (dolist (win (window-list))
      (let ((res (mini-ontop--move-point-up-for-window win)))
        (when res
          (push (list (car res) (window-buffer win) (cdr res)) saved))))
    saved))

(defun mini-ontop--restore-points (saved)
  "Restore points in windows using SAVED, a list of (WINDOW BUFFER POINT) entries."
  (dolist (entry saved)
    (cl-destructuring-bind (win buf pt) entry
      (when (and (window-live-p win) (buffer-live-p buf))
        (with-selected-window win
          (with-current-buffer buf
            (goto-char pt)))))))

(defun mini-ontop--push-and-adjust (&rest _args)
  "Push a new saved positions entry onto the stack and adjust windows."
  (push (mini-ontop--move-point-up-for-all-windows)
        mini-ontop--saved-positions-stack))

(defun mini-ontop--pop-and-restore ()
  "Pop the latest saved positions and restore window points."
  (when mini-ontop--saved-positions-stack
    (let ((saved (pop mini-ontop--saved-positions-stack)))
      (mini-ontop--restore-points saved))))

;;;###autoload
(define-minor-mode mini-ontop-mode
  "Global minor mode to prevent windows from jumping on minibuffer activation."
  :global t
  (if mini-ontop-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'mini-ontop--push-and-adjust)
        (add-hook 'minibuffer-exit-hook #'mini-ontop--pop-and-restore)
        (when (featurep 'which-key)
          (advice-add 'which-key--show-popup :before #'mini-ontop--push-and-adjust)
          (advice-add 'which-key--hide-popup :after #'mini-ontop--pop-and-restore)))
    (remove-hook 'minibuffer-setup-hook #'mini-ontop--push-and-adjust)
    (remove-hook 'minibuffer-exit-hook #'mini-ontop--pop-and-restore)
    (when (featurep 'which-key)
      (advice-remove 'which-key--show-popup #'mini-ontop--push-and-adjust)
      (advice-remove 'which-key--hide-popup #'mini-ontop--pop-and-restore))
    (setq mini-ontop--saved-positions-stack nil)))

(provide 'mini-ontop)
;;; mini-ontop.el ends here
