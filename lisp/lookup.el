;;; lookup.el --- Search interface to electronic dictionaries
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

(require 'cl)
(require 'lookup-utils)
(require 'lookup-vars)
(require 'lookup-types)

(defconst lookup-version "1.99.1"
  "The version numbers of Lookup.")


;;;
;;; Top-level
;;;

;;;###autoload
(defun lookup-version (arg)
  "Display the version string of Lookup.
With prefix argument, insert string at point."
  (interactive "P")
  (let ((version (concat "Lookup " lookup-version)))
    (if arg (insert version) (message version))))

;;;###autoload
(defun lookup ()
  "Start Lookup and display the list of your dictionaries.
If you have already started lookup, display the last status of buffers."
  (interactive)
  (let ((session (lookup-history-ref lookup-search-history)))
    (if session
	(lookup-session-display session)
      (lookup-select-dictionaries (lookup-default-module)))))

(defun lookup-kill ()
  "Force Lookup to be quiet when you exit Emacs.
This can be used when you cannot finish Emacs because of an error of Lookup."
  (interactive)
  (lookup-clear)
  (message "OK, you can exit Emacs"))

(defun lookup-debug ()
  "Toggle Lookup debug mode."
  (interactive)
  (setq lookup-enable-debug (not lookup-enable-debug))
  (setq debug-on-error lookup-enable-debug)
  (message (if lookup-enable-debug
	       "Lookup debug enabled"
	     "Lookup debug disabled")))


;;;
;;; Global commands
;;;

(defvar lookup-global-map nil)

(unless lookup-global-map
  (setq lookup-global-map (make-sparse-keymap))
  (define-key lookup-global-map "\en" 'lookup-next-history)
  (define-key lookup-global-map "\ep" 'lookup-previous-history)
  (define-key lookup-global-map "\ef" 'lookup-forward-module)
  (define-key lookup-global-map "\eb" 'lookup-backward-module)
  (define-key lookup-global-map "B" 'lookup-list-bookmarks)
  (define-key lookup-global-map "H" 'lookup-list-history)
  (define-key lookup-global-map "f" 'lookup-find-pattern)
  (define-key lookup-global-map "o" 'lookup-open-window)
  (define-key lookup-global-map "r" 'lookup-return)
  (define-key lookup-global-map "q" 'lookup-suspend)
  (define-key lookup-global-map "Q" 'lookup-exit)
  (define-key lookup-global-map "R" 'lookup-restart)
  (define-key lookup-global-map "?" 'lookup-help))

(defun lookup-nth-module (n &optional module)
  (let* ((len (length lookup-module-list))
	 (pos (if module
		  (- len (length (memq module lookup-module-list)))
		0)))
    (setq pos (% (+ pos n) len))
    (if (< pos 0) (setq pos (+ pos len)))
    (nth pos lookup-module-list)))

(defun lookup-forward-module (arg)
  (interactive "p")
  (let ((module (lookup-nth-module arg (lookup-current-module))))
    (if (eq major-mode 'lookup-select-mode)
	(lookup-select-dictionaries module)
      (let ((query (lookup-session-query (lookup-current-session))))
	(if (not (eq (lookup-query-method query) 'reference))
	    (lookup-search-query module query)
	  (error "Error"))))
    (princ (lookup-module-name module))))

(defun lookup-backward-module (arg)
  (interactive "p")
  (lookup-forward-module (- arg)))

(defun lookup-next-history (&optional arg)
  (interactive "p")
  (let ((session (lookup-history-move lookup-search-history (or arg 1))))
    (lookup-session-display session))
  (princ (lookup-history-position lookup-search-history)))

(defun lookup-previous-history (&optional arg)
  (interactive "p")
  (lookup-next-history (- (or arg 1))))

(defun lookup-find-pattern (pattern)
  (interactive
   (let* ((session (lookup-current-session))
	  (default (if session
		       (lookup-query-string (lookup-session-query session)))))
     (list (lookup-read-string "Look up" nil 'lookup-input-history default))))
  (lookup-search-pattern (lookup-current-module) pattern))

(defun lookup-list-bookmarks ()
  (interactive)
  (let ((entries (lookup-module-bookmarks (lookup-current-module))))
    (if entries
	(let ((query (lookup-new-query 'reference "Bookmarks")))
	  (lookup-display-entries (lookup-current-module) query entries))
      (error "This module has no bookmark"))))

(defun lookup-list-history ()
  (interactive)
  (lookup-history-display (lookup-current-module)))

(defun lookup-open-window ()
  (interactive)
  (if (window-live-p lookup-start-window)
      (delete-other-windows)
    (lookup-suspend))
  (lookup))

(defun lookup-return ()
  (interactive)
  (if (window-live-p lookup-start-window)
      (select-window lookup-start-window)
    (lookup-suspend)
    (let ((lookup-open-function 'lookup-other-window))
      (lookup))
    (select-window lookup-start-window)))

(defun lookup-suspend ()
  "Close all Lookup windows temporary.
The last states of windows will be recovered if the varialbe
`lookup-save-configuration' is non-nil.  Type `\\[lookup]'
to back to Lookup."
  (interactive)
  (if (lookup-exclusive-frame-p)
      (delete-frame)
    (mapc 'lookup-hide-buffer lookup-buffer-list)
    (when (and lookup-save-configuration lookup-window-configuration)
      (set-window-configuration lookup-window-configuration)
      (setq lookup-window-configuration nil))))

(defun lookup-exit ()
  "Exit Lookup and related processes."
  (interactive)
  (if (not lookup-last-session)
      (if (interactive-p) (error "Lookup is not started"))
    (when (or (not (interactive-p))
	      (y-or-n-p "Are you sure to exit Lookup? "))
      (lookup-with-message "Exitting Lookup"
	(if lookup-cache-file (lookup-dump-cache lookup-cache-file))
	(lookup-suspend)
	(mapc 'kill-buffer lookup-buffer-list)
	(mapc 'lookup-agent-clear lookup-agent-list)
	(setq lookup-buffer-list nil)
	(setq lookup-agent-list nil)
	(setq lookup-module-list nil)
	(setq lookup-dictionary-list nil)
	(setq lookup-entry-table nil)
	(setq lookup-current-session nil)
	(setq lookup-last-session nil)))))

(defun lookup-leave ()
  "Leave the current buffer.
If this is the first session, this is the same with \\[lookup-suspend].
Otherwise, this is the same with \\[lookup-previous-history]."
  (interactive)
  (if (> (lookup-history-position lookup-search-history) 1)
      (lookup-previous-history)
    (lookup-suspend)))

(defun lookup-restart ()
  "Exit Lookup, initialize it again, and restart."
  (interactive)
  (when (or (not (interactive-p))
	    (yes-or-no-p "Are you sure to restart Lookup? "))
    (lookup-exit)
    (lookup-initialize)
    (lookup)))

(defun lookup-help ()
  (interactive)
  (with-current-buffer (lookup-get-buffer "*Lookup Help*")
    (help-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert lookup-help-message))
    (goto-char (point-min))
    (if (window-live-p lookup-start-window)
	(set-window-buffer lookup-start-window (current-buffer))
      (display-buffer (current-buffer)))))


;;;
;;; Search commands
;;;

(defun lookup-pattern-input ()
  (let ((module (if current-prefix-arg (lookup-input-module)))
	(pattern (lookup-input-pattern)))
    (list pattern module)))

;;;###autoload
(defun lookup-pattern (pattern &optional module)
  "Search for the PATTERN."
  (interactive (lookup-pattern-input))
  (lookup-search-pattern (or module (lookup-default-module)) pattern))

;;;###autoload
(defun lookup-pattern-full-screen (pattern &optional module)
  "Search for the PATTERN in full screen.
See `lookup-pattern' for details."
  (interactive (lookup-pattern-input))
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-pattern pattern module)))

;;;###autoload
(defun lookup-pattern-other-frame (pattern &optional module)
  "Search for the PATTERN in another frame.
See `lookup-pattern' for details."
  (interactive (lookup-pattern-input))
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-pattern pattern module)))

(defun lookup-word-input ()
  (let ((module (if current-prefix-arg (lookup-input-module)))
	(word (lookup-current-word)))
    (list word module)))

;;;###autoload
(defun lookup-word (word &optional module)
  "Search for the word near the cursor."
  (interactive (lookup-word-input))
  (let ((lookup-search-method lookup-default-method))
    (lookup-pattern word module)))

;;;###autoload
(defun lookup-word-full-screen (word &optional module)
  "Search for the word near the cursor in full screen.
See `lookup-word' for details."
  (interactive (lookup-word-input))
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-word word module)))

;;;###autoload
(defun lookup-word-other-frame (word &optional module)
  "Search for the word near the cursor in another frame.
See `lookup-word' for details."
  (interactive (lookup-word-input))
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-word word module)))

(defun lookup-region-input ()
  (let ((module (if current-prefix-arg (lookup-input-module)))
	(start (mark)) (end (point)) tmp)
    (if (> start end) (setq tmp start start end end tmp))
    (list start end module)))

;;;###autoload
(defun lookup-region (start end &optional module)
  "Search for the region."
  (interactive (lookup-region-input))
  (lookup-word (buffer-substring-no-properties start end) module))

;;;###autoload
(defun lookup-region-full-screen (start end &optional module)
  "Search for the region in full screen.
See `lookup-region' for details."
  (interactive (lookup-region-input))
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-region start end module)))

;;;###autoload
(defun lookup-region-other-frame (start end &optional module)
  "Search for the region in another frame.
See `lookup-region' for details."
  (interactive (lookup-region-input))
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-region start end module)))

;;;###autoload
(defun lookup-selection (click)
  "Search for the mouse's selection."
  (interactive "e")
  (lookup-word (current-kill 0 t)))

;;;###autoload
(defun lookup-selection-full-screen (click)
  "Search for the mouse's selection in full screen.
See `lookup-selection' for details."
  (interactive "e")
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-selection click)))

;;;###autoload
(defun lookup-selection-other-frame (click)
  "Search for the mouse's selection in another frame.
See `lookup-selection' for details."
  (interactive "e")
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-selection click)))

;;;###autoload
(defun lookup-secondary (click)
  "Search for the mouse's secondary selection."
  (interactive "e")
  (call-interactively 'mouse-drag-secondary)
  (let ((start (overlay-start mouse-secondary-overlay))
	(end (overlay-end mouse-secondary-overlay)))
    (unless (eq start end)
      (with-current-buffer (window-buffer (posn-window (event-start click)))
	(unwind-protect
	    (lookup-word (buffer-substring-no-properties start end))
	  (delete-overlay mouse-secondary-overlay))))))

;;;###autoload
(defun lookup-secondary-full-screen (click)
  "Search for the mouse's secondary selection in full screen.
See `lookup-secondary' for details."
  (interactive "e")
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-secondary click)))

;;;###autoload
(defun lookup-secondary-other-frame (click)
  "Search for the mouse's secondary selection in another frame.
See `lookup-secondary' for details."
  (interactive "e")
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-secondary click)))


;;;
;;; Search functions
;;;

(defvar lookup-input-history nil
  "History of inputed search patterns.")

(defvar lookup-input-module-history nil
  "History of inputed module names.")

(defvar lookup-input-dictionary-history nil
  "History of inputed dictionary-IDs.")

(defun lookup-input-pattern ()
  (let ((default (lookup-current-word)))
    (if (string-equal default "") (setq default nil))
    (lookup-read-string "Look up" nil 'lookup-input-history default t)))

(defun lookup-input-module ()
  (let ((table (mapcar (lambda (module) (lookup-module-name module) module)
		       lookup-module-list)))
    (lookup-get-module
     (completing-read "Search module: " table nil t nil
		      'lookup-input-module-history))))

(defun lookup-input-dictionary ()
  (let ((table (mapcar (lambda (dict) (lookup-dictionary-id dict) dict)
		       lookup-dictionary-list)))
    (lookup-get-dictionary
     (completing-read "Dictionary: " table nil t nil
		      'lookup-input-dictionary-history))))

(defun lookup-search-pattern (module pattern)
  (cond ((> (length pattern) 80) (error "Too long query"))
	((string-match "\n" pattern) (error "Query should be one line")))
  (let ((query (if lookup-search-method
		   (lookup-new-query lookup-search-method pattern)
		 (lookup-parse-pattern pattern))))
    (when (or (not (eq (lookup-query-method query) 'text))
	      (y-or-n-p "Are you sure to search text? "))
      (lookup-search-query module query))))

(defun lookup-search-query (module query)
;  (if (and lookup-last-session
;	    (let ((last (lookup-session-query lookup-last-session)))
;	      (and (eq (lookup-query-method query)
;		       (lookup-query-method last))
;		   (string= (lookup-query-string query)
;			    (lookup-query-string last)))))
;      (lookup-session-display  (lookup-history-ref lookup-search-history))
    (lookup-with-message (format "Looking up `%s'" (lookup-query-pattern query))
      (let ((lookup-dynamic-display t)
	    found valid)
	(do ((mod module next-module)
	     (next-module nil (lookup-nth-module 1 mod)))
	    ((or found (eq next-module module)))
	  (dolist (dict (or lookup-search-dictionaries
			    (lookup-module-dictionaries mod)))
	    (when (and
		   ;; Check dictionary priority
		   (or lookup-search-dictionaries
		       (let ((p (lookup-module-dictionary-priority mod dict)))
			 (cond ((eq p t) t)
			       ((eq p 'secondary) (not found))
			       ((eq p 'supplement) found))))
		   ;; Check search method
		   (let ((method (lookup-query-method query)))
		     (or (eq method 'default)
			 (memq method (lookup-dictionary-methods dict)))))
	      (setq valid t)
	      (lookup-message (concat (lookup-dictionary-title dict) "..."))
	      (let ((entries (lookup-dictionary-search dict query)))
		(when entries
		  (if found
		      (lookup-summary-append entries)
		    (setq found t)
		    (let ((session (lookup-new-session mod query entries)))
		      (lookup-session-open session))))))))
	(cond
	 ((not valid) (error "No valid dictionary for method `%s'"
			     (lookup-query-method query)))
	 ((not found) (error "No entry for query: %s"
			     (lookup-query-pattern query)))))))

(defun lookup-display-entries (module query entries)
  (lookup-session-open (lookup-new-session module query entries)))


;;;
;;; Arrange & Adjust
;;;

(defun lookup-arrange-content (entry)
  (let ((funcs (lookup-dictionary-arranges (lookup-entry-dictionary entry))))
    (lookup-format-internal entry funcs "formatting")))

(defun lookup-adjust-content (entry)
  (let ((funcs '(lookup-adjust-show-gaijis
		 lookup-adjust-hide-examples
		 lookup-adjust-check-references)))
    (if (featurep 'xemacs)
	(mapcar-extents 'delete-extent)
      (let ((overlay (overlay-lists)))
	(mapc 'delete-overlay (car overlay))
	(mapc 'delete-overlay (cdr overlay))))
    (lookup-format-internal entry funcs nil)
    (goto-char (point-min))))

(defun lookup-format-internal (entry functions msg)
  (let ((n 1))
    (dolist (func functions)
      (when func
	(if msg
	    (lookup-message (concat msg (make-string (setq n (1+ n)) ?.))))
	(widen)
	(goto-char (point-min))
	(funcall func entry)))))

;; replace

(defun lookup-arrange-replaces (entry)
  (dolist (pair (lookup-dictionary-option (lookup-entry-dictionary entry)
					  :replace-alist))
    (goto-char (point-min))
    (while (re-search-forward (car pair) nil t)
      (replace-match (cdr pair)))))

;; structure

(defun lookup-arrange-structure (entry)
  (lookup-make-region-heading (point) (lookup-point-eol) 1))

(defun lookup-adjust-hide-examples (entry)
  (unless lookup-enable-example
    (lookup-map-over-property
     (point-min) (point-max) 'face
     (lambda (start end face)
       (when (eq face 'lookup-comment-face)
	 (if (eq (char-after (1- start)) ?\n)
	     (setq start (1- start)))
	 (let ((overlay (make-overlay start (1- end))))
	   (overlay-put overlay 'invisible t)
	   (overlay-put overlay 'evaporate t)
	   (overlay-put overlay 'before-string "...")))))))

;; reference

(defun lookup-arrange-references (entry)
  (let* ((dict (lookup-entry-dictionary entry))
	 (pattern (lookup-dictionary-reference-pattern dict)))
    (when pattern
      (when (functionp pattern)
	(setq pattern (funcall pattern entry)))
      (let ((case-fold-search nil)
	    (regexp (car pattern)) (link-item (nth 1 pattern))
	    (heading-item (nth 2 pattern)) (code-item (nth 3 pattern)))
	(while (re-search-forward regexp nil t)
	  (let* ((start (match-beginning 0))
		 (link (if (integerp link-item)
			   (match-string link-item)
			 (save-match-data (eval link-item))))
		 (heading (if (integerp heading-item)
			      (match-string heading-item)
			    (save-match-data (eval heading-item))))
		 (code (cond ((not code-item) heading)
			     ((integerp code-item) (match-string code-item))
			     (t code-item))))
	    (replace-match link t t)
	    (if (stringp code)
		(setq entry (lookup-new-entry 'regular dict code heading))
	      (setq entry (lookup-new-entry 'dynamic dict heading))
	      (lookup-put-property entry :dynamic code))
	    (lookup-set-link start (point) entry)))))))

(defun lookup-adjust-check-references (entry)
  (lookup-map-over-property
   (point-min) (point-max) 'lookup-reference
   (lambda (start end entry)
     (if (lookup-entry-refered-p entry)
	 (put-text-property start end 'face 'lookup-refered-face)
       (put-text-property start end 'face 'lookup-reference-face)))))

(defun lookup-dynamic-search (entry)
  (let ((query (lookup-new-query 'exact (lookup-entry-code entry))))
    (lookup-dictionary-search (lookup-entry-dictionary entry) query)))

;; gaiji

(defun lookup-arrange-gaijis (entry)
  (let ((case-fold-search nil)
	(dictionary (lookup-entry-dictionary entry))
	regexp start end gaiji)
    (when (setq regexp (lookup-dictionary-gaiji-regexp dictionary))
      (while (re-search-forward regexp nil t)
	(setq start (match-beginning 0) end (match-end 0))
	(setq gaiji (lookup-dictionary-gaiji dictionary (match-string 1)))
	(when gaiji
	  (delete-region start end)
	  (lookup-gaiji-insert gaiji))))))

(defun lookup-adjust-show-gaijis (entry)
  (when lookup-enable-gaiji
    (lookup-map-over-property
     (point-min) (point-max) 'lookup-gaiji
     (lambda (start end gaiji)
       (lookup-gaiji-glyph-paste start end (lookup-gaiji-glyph gaiji))))))

;; fill

(defun lookup-arrange-nofill (entry))

(defun lookup-arrange-fill-lines (entry)
  (text-mode)
  (let ((fill-column (if (integerp lookup-fill-column)
			 lookup-fill-column
		       (round (* (window-width) lookup-fill-column))))
	start)
    (while (not (eobp))
      (setq start (point))
      (end-of-line)
      (if (> (current-column) fill-column)
	  (fill-region start (point)))
      (forward-line))))

(defun lookup-arrange-fill-paragraphs (entry)
  (text-mode)
  (let ((fill-column (if (integerp lookup-fill-column)
			 lookup-fill-column
		       (round (* (window-width) lookup-fill-column)))))
    (fill-individual-paragraphs (point-min) (point-max))))

;; utils

(defun lookup-heading-face (level)
  (or (nth (1- level) '(lookup-heading-1-face
			lookup-heading-2-face lookup-heading-3-face
			lookup-heading-4-face lookup-heading-5-face))
      'lookup-comment-face))

(defun lookup-make-region-heading (start end level)
  (add-text-properties start end (list 'face (lookup-heading-face level)
				       'lookup-heading level)))

(defun lookup-set-link (start end reference)
  (add-text-properties start end (list 'mouse-face 'highlight
				       'lookup-reference reference)))

(defun lookup-get-link (position)
  (get-text-property position 'lookup-reference))

(defun lookup-goto-next-link ()
  (let ((p (point)))
    (and (setq p (next-single-property-change p 'lookup-reference))
	 (or (get-text-property p 'lookup-reference)
	     (setq p (next-single-property-change p 'lookup-reference)))
	 (goto-char p))))

(defun lookup-goto-previous-link ()
  (let ((p (point)))
    (and (setq p (previous-single-property-change p 'lookup-reference))
	 (or (get-text-property p 'lookup-reference)
	     (setq p (previous-single-property-change p 'lookup-reference)))
	 (goto-char p))))


;;;
;;; Object management
;;;

(defun lookup-current-session ()
  (or lookup-current-session
      (if (window-live-p lookup-main-window)
	  (with-current-buffer (window-buffer lookup-main-window)
	    lookup-current-session))))

(defun lookup-current-module ()
  (let ((session (lookup-current-session)))
    (if session (lookup-session-module session))))

(defun lookup-default-module ()
  (let ((name (or (lookup-assq-get lookup-mode-module-alist major-mode)
		  (lookup-assq-get lookup-mode-module-alist t))))
    (if name
	(or (lookup-get-module name)
	    (error "No such module: %s" name))
      (car lookup-module-list))))

(defun lookup-get-module (id)
  (car (member-if (lambda (module) (equal (lookup-module-id module) id))
		  lookup-module-list)))

(defun lookup-get-agent (id)
  (car (member-if (lambda (agent) (equal (lookup-agent-id agent) id))
		  lookup-agent-list)))

(defun lookup-get-dictionary (id)
  (car (member-if (lambda (dict) (equal (lookup-dictionary-id dict) id))
		  lookup-dictionary-list)))

(defun lookup-entry-list ()
  (let (entries)
    (when lookup-entry-table
      (mapatoms (lambda (symbol)
		  (setq entries (cons (symbol-value symbol) entries)))
		lookup-entry-table))
    entries))

(defun lookup-put-entry (entry)
  (unless lookup-entry-table
    (setq lookup-entry-table (make-vector 377 0)))
  (set (intern (lookup-entry-id entry) lookup-entry-table) entry))

(defun lookup-get-entry (id)
  (when lookup-entry-table
    (let ((symbol (intern id lookup-entry-table)))
      (if (boundp symbol) (symbol-value symbol)))))

(defun lookup-get-entry-create (id)
  (or (lookup-get-entry id)
      (when (string-match "#" id)
	(let ((dict (substring id 0 (match-beginning 0)))
	      (code (substring id (match-end 0))))
	  (lookup-new-entry 'regular (lookup-get-dictionary dict) code)))))

(defun lookup-gaiji-list ()
  (let (gaijis)
    (dolist (dict lookup-dictionary-list)
      (mapatoms (lambda (code)
		  (setq code (symbol-value code))
		  (if (lookup-gaiji-p code)
		      (setq gaijis (cons code gaijis))))
		(lookup-dictionary-gaiji-table dict)))
    gaijis))


;;;
;;; Buffer management
;;;

(defconst lookup-summary-buffer " *Summary*")
(defconst lookup-content-buffer " *Content*")

(defsubst lookup-summary-buffer () lookup-summary-buffer)
(defsubst lookup-content-buffer () lookup-content-buffer)

(defsubst lookup-temp-buffer ()
  (generate-new-buffer " *Lookup temp buffer*"))

(defsubst lookup-open-process-buffer (name)
  (if lookup-enable-debug (generate-new-buffer name)))

(defvar lookup-start-window nil)
(defvar lookup-main-window nil)
(defvar lookup-sub-window nil)

(defun lookup-get-buffer (name)
  (let ((buffer (get-buffer-create name)))
    (setq lookup-buffer-list (adjoin buffer lookup-buffer-list))
    buffer))

(defun lookup-pop-to-buffer (buffer)
  (if (window-live-p lookup-main-window)
      (progn
	(set-window-buffer (select-window lookup-main-window) buffer)
	(raise-frame (window-frame lookup-main-window)))
    (setq lookup-start-window (selected-window))
    (setq lookup-window-configuration (current-window-configuration))
    (funcall lookup-open-function buffer)
    (setq lookup-main-window (get-buffer-window buffer t)))
  (when (window-live-p lookup-sub-window)
    (delete-window lookup-sub-window)
    (setq lookup-sub-window nil))
  buffer)

(defun lookup-display-buffer (buffer)
  (if (window-live-p lookup-sub-window)
      (set-window-buffer lookup-sub-window buffer)
    (setq lookup-sub-window
	  (if (<= (window-height lookup-main-window) lookup-window-height)
	      (next-window)
	    (let ((height (if (integerp lookup-window-height)
			      lookup-window-height
			    (round (* (window-height) lookup-window-height)))))
	      (split-window lookup-main-window (1+ height)))))
    (set-window-buffer lookup-sub-window buffer))
  buffer)

(defun lookup-hide-buffer (buffer)
  (let ((window (get-buffer-window buffer)))
    (when window
      (cond ((eq window lookup-main-window)
	     (setq lookup-main-window nil))
	    ((eq window lookup-sub-window)
	     (if (window-live-p lookup-main-window)
		 (select-window lookup-main-window))
	     (setq lookup-sub-window nil)))
      (if (> (count-windows) 1)
	  (delete-window window)
	(switch-to-buffer (other-buffer)))))
  (bury-buffer buffer))

(defun lookup-full-screen (buffer)
  (delete-other-windows)
  (switch-to-buffer buffer))

(defun lookup-other-window (buffer)
  (let ((pop-up-windows t)
	(pop-up-frames nil))
    (pop-to-buffer buffer)))

(defun lookup-other-frame (buffer)
  (let ((pop-up-frames t)
	(default-frame-alist (cons '(name . "Lookup") lookup-frame-alist)))
    (pop-to-buffer buffer)))

(defun lookup-exclusive-frame-p ()
  (string= (frame-parameter (selected-frame) 'name) "Lookup"))


;;;;;;;;;;;;;;;;;;;;
;; Internal Functions
;;;;;;;;;;;;;;;;;;;;

(defvar lookup-message nil)

(put 'lookup-with-message 'lisp-indent-function 1)
(defmacro lookup-with-message (msg &rest body)
  `(let ((lookup-message ,msg))
     (message "%s..." lookup-message)
     (prog1 (progn ,@body)
       (message "%s...done" lookup-message))))

(defun lookup-message (msg)
  (message "%s... (%s)" lookup-message msg))


;;;;;;;;;;;;;;;;;;;;
;; Setup Functions
;;;;;;;;;;;;;;;;;;;;

(put 'lookup-set-agent-options 'lisp-indent-function 1)
;;;###autoload
(defun lookup-set-agent-options (id &rest options)
  (let ((plist (lookup-assoc-ref 'lookup-agent-option-alist id)))
    (while options
      (setq plist (plist-put plist (car options) (cadr options)))
      (setq options (cddr options)))
    (lookup-assoc-set 'lookup-agent-option-alist id plist)))

(put 'lookup-set-dictionary-options 'lisp-indent-function 1)
;;;###autoload
(defun lookup-set-dictionary-options (id &rest options)
  (let ((plist (lookup-assoc-ref 'lookup-dictionary-option-alist id)))
    (while options
      (setq plist (plist-put plist (car options) (cadr options)))
      (setq options (cddr options)))
    (lookup-assoc-set 'lookup-dictionary-option-alist id plist)))

;;;###autoload
(defun lookup-use-support (id file)
  (lookup-assoc-set 'lookup-support-alist id file))


;;;
;;; Initialize Lookup
;;;

(defun lookup-initialize ()
  (load lookup-init-file t)
  (when lookup-cache-file
    (require 'lookup-cache)
    (load lookup-cache-file t)) 
  (setq lookup-search-history (lookup-new-history))
  (setq lookup-agent-list
	(mapcar (lambda (spec) (apply 'lookup-new-agent spec))
		(or lookup-search-agents
		    (setq lookup-search-agents '((ndtut))))))
  (setq lookup-dictionary-list
	(apply 'append
	       (mapcar 'lookup-agent-dictionaries lookup-agent-list)))
  (setq lookup-module-list
	(mapcar (lambda (spec) (apply 'lookup-new-module spec))
		(or lookup-search-modules '(("default" t)))))
  (lookup-init-support-autoload)
  (run-hooks 'lookup-load-hook)
  (add-hook 'kill-emacs-hook 'lookup-exit))

(defun lookup-init-support-autoload ()
  (load "support-loaddef")
  (dolist (pair lookup-support-autoload-alist)
    (dolist (dict lookup-dictionary-list)
      (when (string-match (car pair) (lookup-dictionary-id dict))
	(lookup-assoc-set 'lookup-support-alist
			  (lookup-dictionary-id dict)
			  (cdr pair))
	(return)))))

(defun lookup-clear ()
  (remove-hook 'kill-emacs-hook 'lookup-exit))

(provide 'lookup)

(unless lookup-byte-compiling
  (lookup-initialize))

;;; lookup.el ends here
