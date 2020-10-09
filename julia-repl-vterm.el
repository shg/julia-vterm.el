;;; julia-repl-vterm.el --- A mode for Julia REPL using vterm -*- lexical-binding: t -*-

;; Copyright (C) 2020 Shigeaki Nishina

;; Author: Shigeaki Nishina
;; Maintainer: Shigeaki Nishina
;; Created: March 11, 2020
;; URL: https://github.com/shg/julia-repl-vterm.el
;; Version: 0.8
;; Keywords: languages, julia

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Provides a major-mode for inferior Julia process that runs in vterm, and a
;; minor-mode that extends julia-mode to support interaction with the inferior
;; Julia process.

;;; Usage:

;; You must have julia-mode and vterm installed.
;; Install julia-repl-vterm.el manually using package.el
;;
;;   (package-install-file "/path-to-download-dir/julia-repl-vterm.el")
;;
;; Eval the following line. Add this line to your init file to enable this
;; mode in future sessions.
;;
;;   (add-hook 'julia-mode-hook #'julia-with-repl-vterm-mode)
;;
;; Now you can interact with an inferior Julia REPL from a Julia buffer.
;;
;; C-c C-z in a julia-mode buffer to open an inferior Julia REPL buffer.
;; C-c C-z in the REPL buffer to switch back to the script buffer.
;; C-<return> in the script buffer to send region or current line to REPL.
;;
;; See the code below for a few more key bidindings.

;;; Code:

(require 'vterm)


;;----------------------------------------------------------------------
(defgroup inferior-julia-repl-vterm nil
  "A major mode for inferior Julia REPL"
  :group 'julia)

(defvar inferior-julia-repl-vterm-buffer-name "*julia-repl*")

(defvar inferior-julia-repl-vterm-program "julia")

(defvar-local inferior-julia-repl-vterm-script-buffer nil)

(defvar inferior-julia-repl-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'inferior-julia-repl-vterm-switch-to-script-buffer)
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    (define-key map (kbd "M-k") #'inferior-julia-repl-vterm-clear-buffer)
    (define-key map (kbd "C-c C-t") #'inferior-julia-repl-vterm-copy-mode)
    map))

(define-derived-mode inferior-julia-repl-vterm-mode vterm-mode "Inf-Julia"
  "A major mode for inferior Julia REPL."
  :group 'inferior-julia-repl-vterm)

(defun inferior-julia-repl-vterm-buffer ()
  "Return the inferior Julia REPL buffer `*julia-repl*`.
If the buffer doesn't exist, create one and return it.  If there's already the
buffer and the inferior Julia REPL is running, return the buffer.  If the
buffer exists but the process is not running, kill the buffer and create a new
one."
  (if-let ((buffer (get-buffer inferior-julia-repl-vterm-buffer-name))
	   (proc (with-current-buffer buffer vterm--process)))
      buffer
    (if buffer (kill-buffer buffer))
    (let ((buffer (generate-new-buffer inferior-julia-repl-vterm-buffer-name))
	  (vterm-shell inferior-julia-repl-vterm-program))
      (with-current-buffer buffer
	(inferior-julia-repl-vterm-mode))
      buffer)))

(defun inferior-julia-repl-vterm ()
  "Create an inferior Julia REPL buffer `*julia-repl*` and open it.
If there's already one with the process alive, just open it."
  (interactive)
  (pop-to-buffer-same-window (inferior-julia-repl-vterm-buffer)))

(defun inferior-julia-repl-vterm-switch-to-script-buffer ()
  "Switch to the script buffer that opened this Julia REPL buffer."
  (interactive)
  (if (and (boundp 'inferior-julia-repl-vterm-script-buffer) (buffer-live-p inferior-julia-repl-vterm-script-buffer))
      (switch-to-buffer-other-window inferior-julia-repl-vterm-script-buffer)
    (message "The script buffer does not exist.")))

(defun inferior-julia-repl-vterm-clear-buffer ()
  "Clear the content of the Julia REPL buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (vterm-clear 1)))

(defvar inferior-julia-repl-vterm-copy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") #'inferior-julia-repl-vterm-copy-mode)
    (define-key map [return] #'inferior-julia-repl-vterm-copy-mode-done)
    (define-key map (kbd "RET") #'inferior-julia-repl-vterm-copy-mode-done)
    (define-key map (kbd "C-c C-r") #'vterm-reset-cursor-point)
    map))

(define-minor-mode inferior-julia-repl-vterm-copy-mode
  "Toggle copy mode."
  :group 'inferior-julia-repl-vterm
  :lighter " VTermCopy"
  :keymap inferior-julia-repl-vterm-copy-mode-map
  (when inferior-julia-repl-vterm-copy-mode
    (message "Start copy mode")
    (use-local-map nil)
    (vterm-send-stop)))

(defun inferior-julia-repl-vterm-copy-mode-done ()
  "Save the active region to the kill ring and exit copy mode."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (user-error "No active region"))
  (inferior-julia-repl-vterm-copy-mode -1)
  (vterm-reset-cursor-point)
  (use-local-map inferior-julia-repl-vterm-mode-map)
  (vterm-send-start)
  (message "End copy mode"))


;;----------------------------------------------------------------------
(defgroup julia-with-repl-vterm nil
  "A minor mode for a Julia script buffer that interacts with an inferior Julia
REPL."
  :group 'julia)

(defcustom julia-with-repl-vterm-hook nil
  "Hook to run after starting a Julia script buffer that interacts with an
inferior Julia REPL."
  :type 'hook
  :group 'julia-with-repl-vterm)

(defun julia-with-repl-vterm-switch-to-repl-buffer ()
  "Switch to the REPL buffer if one already exists, or open a new REPL buffer."
  (interactive)
  (let ((current-script-buffer (current-buffer))
	(inferior-buffer (inferior-julia-repl-vterm-buffer)))
    (with-current-buffer inferior-buffer
      (setq inferior-julia-repl-vterm-script-buffer current-script-buffer)
      (switch-to-buffer-other-window inferior-buffer))))

(defun julia-with-repl-vterm-send-return-key ()
  "Send a return key to the Julia REPL buffer."
  (with-current-buffer (inferior-julia-repl-vterm-buffer)
    (vterm-send-return)))

(defun julia-with-repl-vterm-paste-string (string)
  "Send STRING to the Julia REPL buffer using brackted paste mode."
  (with-current-buffer (inferior-julia-repl-vterm-buffer)
    (vterm-send-string string t)))

(defun julia-with-repl-vterm-send-current-line ()
  "Send the current line to the Julia REPL buffer, and move to the next line.
This sends a newline after the content of the current line even if there's no
newline at the end.  A newline is also inserted after the current line of the
script buffer."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((clmn (current-column))
	  (char (char-after))
	  (line (string-trim (thing-at-point 'line t))))
      (unless (and (zerop clmn) char)
	(when (/= 0 clmn)
	  (julia-with-repl-vterm-paste-string line)
	  (julia-with-repl-vterm-send-return-key)
	  (if (not char)
	      (newline))))))
  (forward-line))

(defun julia-with-repl-vterm-send-region-or-current-line ()
  "Send the content of region if region is active, or send the current line."
  (interactive)
  (if (use-region-p)
      (progn
	(julia-with-repl-vterm-paste-string
	 (buffer-substring-no-properties (region-beginning) (region-end)))
	(deactivate-mark))
    (julia-with-repl-vterm-send-current-line)))

(defun julia-with-repl-vterm-send-buffer ()
  "Send the whole content of the script buffer to the Julia REPL buffer."
  (interactive)
  (save-excursion
    (julia-with-repl-vterm-paste-string (buffer-string))))

;;;###autoload
(define-minor-mode julia-with-repl-vterm-mode
  "A minor mode for a Julia script buffer that interacts with an inferior Julia REPL."
  nil "⁂"
  `((,(kbd "C-c C-z") . julia-with-repl-vterm-switch-to-repl-buffer)
    (,(kbd "C-c C-b") . julia-with-repl-vterm-send-buffer)
    (,(kbd "C-<return>") . julia-with-repl-vterm-send-region-or-current-line)))

(provide 'julia-repl-vterm)

;;; julia-repl-vterm.el ends here
