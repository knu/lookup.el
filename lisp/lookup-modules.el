;;; lookup-modules.el --- Lookup modules mode
;; Copyright (C) 2000 Keisuke Nishida <knishida@ring.gr.jp>

;; Author: Keisuke Nishida <knishida@ring.gr.jp>
;; Keywords: dictionary

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'lookup)

;;;###autoload
(defun lookup-list-modules ()
  (interactive)
  (with-current-buffer (lookup-get-buffer " *Module List*")
    (lookup-modules-mode)
    (lookup-modules-build-buffer)
    (setq buffer-undo-list nil)
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    (lookup-modules-goto-first)
    (lookup-pop-to-buffer (current-buffer))))

(defun lookup-modules-build-buffer ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Type `c' to create module, `v' to visit, "
	    "`q' to leave, `?' for help.\n")
    (insert "Current module : " (lookup-module-name (lookup-current-module)) "\n")
    (lookup-table-insert
     "%-8t %s\n"
     (append
      '(("Name" "Dictionaries")
	("----" "------------"))
      (mapcar (lambda (module)
		(let* ((dicts (lookup-module-dictionaries module))
		       (str (mapconcat 'lookup-dictionary-name dicts "/")))
		  (list
		   (lookup-module-name module)
		   (format "[%d] %s" (length dicts) str))))
	      lookup-module-list)))
    (lookup-modules-goto-first)
    (while (re-search-forward "^[^ ]+" nil t)
      (put-text-property (match-beginning 0) (match-end 0)
			 'module (lookup-get-module (match-string 0))))))

;;;
;;; Modules Mode
;;;


(defconst lookup-select-mode-help
  "Lookup Select mode:

`C-k'      - remove this module `C-xC-t' - transpose modules
`c'(reate) - create a module    `C-y'    - yank a removed module
`v'        - visit a module     `q' - leave")

(defconst lookup-modules-mode-help
  "Lookup Modules mode:")

(defvar lookup-modules-mode-map nil
  "*Keymap for Lookup Modules mode.")

(unless lookup-modules-mode-map
  (setq lookup-modules-mode-map (make-sparse-keymap))
  (set-keymap-parent lookup-modules-mode-map lookup-global-map)
  ;; general commands
  (define-key lookup-modules-mode-map " " 'next-line)
  (define-key lookup-modules-mode-map "n" 'next-line)
  (define-key lookup-modules-mode-map "p" 'previous-line)
  ;; module management
  (define-key lookup-modules-mode-map "c" 'lookup-modules-create-module)
  (define-key lookup-modules-mode-map "v" 'lookup-modules-visit-this-module)
  (define-key lookup-modules-mode-map "\ey" 'lookup-modules-wrap-command)
  (define-key lookup-modules-mode-map "\C-k" 'lookup-modules-wrap-command)
  (define-key lookup-modules-mode-map "\C-y" 'lookup-modules-wrap-command)
  (define-key lookup-modules-mode-map "\C-x\C-t" 'lookup-modules-wrap-command)
  (define-key lookup-modules-mode-map [?\C-/] 'lookup-modules-wrap-command)
  ;; general commands
  (define-key lookup-modules-mode-map "g" 'lookup-modules-update)
  (define-key lookup-modules-mode-map "q" 'lookup-leave)
  )

(defvar lookup-modules-mode-hook nil
  "*Hook for Lookup select mode.")

(defvar lookup-modules-kill-ring nil)

(defun lookup-modules-mode ()
  "\\{lookup-modules-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lookup-modules-mode)
  (setq mode-name "Modules")
;  (setq mode-line-buffer-identification
;	(list "Lookup:%12b <" (lookup-module-name (lookup-current-module)) ">"))
  (setq lookup-help-message lookup-modules-mode-help)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (use-local-map lookup-modules-mode-map)
  (run-hooks 'lookup-modules-mode-hook))

;;;
;;; Interactive Commands
;;;

(defun lookup-modules-create-module (name)
  (interactive "sModule name: ")
  (if (lookup-get-module name)
      (error "Module `%s' already exists" name))
  (let ((lookup-current-module (lookup-new-module name t)))
    (setq lookup-module-list
	  (cons lookup-current-module lookup-module-list))
    (lookup-select-dictionaries lookup-current-module))
  (princ name))

(defun lookup-modules-visit-this-module ()
  (interactive)
  (lookup-select-dictionaries (lookup-modules-this-module)))

(defun lookup-modules-wrap-command (arg)
  "Call the corresponding global command with keys and reset dictionaries.
This command should be binded for the same keys with the commands
`kill-line', `yank', `yank-pop',`transpose-lines', or `undo'.
When this command is called, the variable `lookup-modules-kill-ring'
will be used instead of the usual `kill-ring'."
  (interactive "P")
  (let ((kill-ring lookup-modules-kill-ring)
	(kill-whole-line t)
	(inhibit-read-only t))
    (beginning-of-line)
    (unwind-protect
	(progn
	  (use-local-map global-map)
	  (call-interactively (key-binding (this-command-keys))))
      (use-local-map lookup-modules-mode-map))
    (setq lookup-modules-kill-ring kill-ring)
    (lookup-modules-reset-modules)))

(defun lookup-modules-update ()
  (interactive)
  (let* ((module (lookup-current-module))
	 (message (format "Updating %s..." (lookup-module-name module))))
    (message message)
    ;; (dolist (dict (lookup-module-dictionaries module))
    ;;   (lookup-dictionary-setplist dict nil))
    (lookup-modules-update-buffer)
    (message (concat message "done"))))

;;;
;;; Internal functions
;;;

(defun lookup-modules-goto-first ()
  (goto-char (point-min))
  (forward-line 4))

(defun lookup-modules-this-module ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^[^ ]+")
	(lookup-get-module (match-string 0)))))

(defun lookup-modules-reset-modules ()
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (let (new-lookup-module-list)
	(lookup-modules-goto-first)
	(while (re-search-forward "^[^ ]+" nil t)
	  (let ((module (get-text-property (match-beginning 0) 'module)))
	    (if module
		(setq new-lookup-module-list
		      (cons module new-lookup-module-list)))))
	(if new-lookup-module-list
	    (setq lookup-module-list new-lookup-module-list))))))

(provide 'lookup-modules)

;;; lookup-modules.el ends here
