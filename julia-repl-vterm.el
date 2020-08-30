;; julia-repl-vterm.el --- A mode for Julia REPL using vterm -*- lexical-binding: t -*-
;;
;; Copyright (C) 2020 Shigeaki Nishina
;; Author: Shigeaki Nishina
;; Maintainer: Shigeaki Nishina
;; URL: https://github.com/shg/julia-repl-vterm
;; Version: 0.2
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; License:
;;
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
;;
;;; Code:

(require 'julia-mode)
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
  "Return the inferior Julia REPL buffer `*julia-repl*`. If the buffer doesn't exist,
create one and return it. If there's already the buffer and the inferior Julia REPL
is running, return the buffer. If the buffer exists but the process is not running,
kill the buffer and create a new one."
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
  "Create an inferior Julia REPL buffer `*julia-repl*` and open it. If there's
already one with the process alive, just open it."
  (interactive)
  (pop-to-buffer-same-window (inferior-julia-repl-vterm-buffer)))

(defun inferior-julia-repl-vterm-switch-to-script-buffer ()
  (interactive)
  (if (and (boundp 'inferior-julia-repl-vterm-script-buffer) (buffer-live-p inferior-julia-repl-vterm-script-buffer))
      (switch-to-buffer-other-window inferior-julia-repl-vterm-script-buffer)
    (message "The script buffer does not exist.")))

(defun inferior-julia-repl-vterm-clear-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (vterm-clear)))

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
  "Save the active region to the kill ring and exit `inferior-julia-repl-vterm-copy-mode'."
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
  "A minor mode for a Julia script buffer that interacts with an inferior Julia REPL."
  :group 'julia)

(defcustom julia-with-repl-vterm-hook nil
  "Hook to run after starting a Julia script buffer that interacts with an inferior Julia REPL."
  :type 'hook
  :group 'julia-with-repl-vterm)

(defun julia-with-repl-vterm-switch-to-repl-buffer ()
  (interactive)
  (let ((current-script-buffer (current-buffer))
        (inferior-buffer (inferior-julia-repl-vterm-buffer)))
    (with-current-buffer inferior-buffer
      (setq inferior-julia-repl-vterm-script-buffer current-script-buffer)
      (switch-to-buffer-other-window inferior-buffer))))

(defun julia-with-repl-vterm-send-string (string)
  (with-current-buffer (inferior-julia-repl-vterm-buffer)
    (vterm-send-string string)))

(defun julia-with-repl-vterm-send-current-line ()
  (interactive)
  (let ((current-line (thing-at-point 'line t)))
    (julia-with-repl-vterm-send-string current-line))
  (forward-line))

(defun julia-with-repl-vterm-send-buffer ()
  (interactive)
  (save-excursion
    (let ((repl-buffer (inferior-julia-repl-vterm-buffer)))
      (julia-with-repl-vterm-send-string (buffer-string)))))

;;;##autoload
(define-minor-mode julia-with-repl-vterm-mode
  "A minor mode for a Julia script buffer that interacts with an inferior Julia REPL."
  nil "⁂"
  `((,(kbd "C-c C-z") . julia-with-repl-vterm-switch-to-repl-buffer)
    (,(kbd "C-c C-b") . julia-with-repl-vterm-send-buffer)
    (,(kbd "C-<return>") . julia-with-repl-vterm-send-current-line)))

(add-hook 'julia-mode-hook (lambda () (julia-with-repl-vterm-mode 1)))

(provide 'julia-repl-vterm)
