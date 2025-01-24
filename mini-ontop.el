;;; mini-ontop.el --- Prevent windows from jumping on minibuffer activation -*- lexical-binding: t; -*-
;;
;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; URL: https://github.com/hkjels/mini-ontop.el
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
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

(defgroup mini-ontop nil
  "Preempt scrolling by moving point up before the minibuffer is used."
  :group 'convenience)

(defcustom mini-ontop-lines 20
  "Number of lines to jump to make sure scroll does not occur.
If point is closer than this many lines to the bottom,
move it up by the difference."
  :type 'integer
  :group 'mini-ontop)

(defvar mini-ontop--saved-buffer nil
  "Buffer where point was adjusted, so we can restore it.")

(defvar mini-ontop--saved-point nil
  "Original position of point in that buffer, to restore on minibuffer exit.")

(defun mini-ontop--distance-to-bottom (win)
  "Return how many buffer lines exist between point and WIN's bottom."
  (save-excursion
    (let ((current-line (line-number-at-pos (point)))
          (bottom-line  (progn
                          (goto-char (window-end win t))
                          (line-number-at-pos (point)))))
      (max 0 (- bottom-line current-line)))))

(defun mini-ontop--move-point-up-if-needed ()
  "If point is within `mini-ontop-lines` lines of window bottom, move it up."
  (when (not (region-active-p))
    (let ((win (selected-window)))
      (unless (minibufferp (window-buffer win))
        (let ((dist (mini-ontop--distance-to-bottom win)))
          (when (< dist mini-ontop-lines)
            (let ((needed (- mini-ontop-lines dist)))
              (setq mini-ontop--saved-buffer (current-buffer)
                    mini-ontop--saved-point  (point))
              (forward-line (- needed)))))))))

(defun mini-ontop--restore-point ()
  "Restore the original point if we haven't moved too far."
  (when mini-ontop--saved-buffer
    (with-current-buffer mini-ontop--saved-buffer
      (let ((win (get-buffer-window mini-ontop--saved-buffer))
            (current-pos (point)))
        (when (window-live-p win)
          (select-window win)
          (when (<= (line-number-at-pos (abs (- current-pos mini-ontop--saved-point)))
                    (- (line-number-at-pos mini-ontop--saved-point) (+ mini-ontop-lines 1)))
            (goto-char mini-ontop--saved-point))))
      (setq mini-ontop--saved-buffer nil
            mini-ontop--saved-point  nil))))

(defun mini-ontop--exit-hook (&rest _args)
  "Hook run upon minibuffer exit, restore point if needed."
  (mini-ontop--restore-point)
  (remove-hook 'minibuffer-exit-hook #'mini-ontop--exit-hook))

(defun mini-ontop--teardown (&rest _args)
  "Run after `read-from-minibuffer`—remove the exit hook for cleanup."
  (remove-hook 'minibuffer-exit-hook #'mini-ontop--exit-hook))

(defun mini-ontop--setup (&rest _args)
  "Run before `read-from-minibuffer` to move point up if needed."
  (mini-ontop--move-point-up-if-needed)
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
