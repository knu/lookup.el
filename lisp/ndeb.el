;;; ndeb.el --- Lookup eblook interface
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

;;;
;;; Customizable variables
;;;

(defgroup ndeb nil
  "Lookup eblook interface."
  :group 'lookup-search-agents)

(defcustom ndeb-program-name "eblook"
  "*Program name of eblook."
  :type 'string
  :group 'ndeb)

(defcustom ndeb-prompt-string "eblook> "
  "*Prompt string of eblook."
  :type 'string
  :group 'ndeb)

(defcustom ndeb-process-coding-system lookup-process-coding-system
  "*Coding system for eblook process."
  :type 'symbol
  :group 'ndeb)

(defvar ndeb-use-inline-image t)


;;;
;;; Internal varialbes
;;;

(defvar ndeb-current-agent nil)
(defvar ndeb-current-dictionary nil)
(defvar ndeb-current-process nil)

;;;
;;; types
;;;

(put 'ndeb :gaiji-regexp "<gaiji=\\([^>]*\\)>")
(put 'ndeb :reference-pattern
     (list (concat "<reference>\\($B"*(B?\\(\\([^<]\\|<gaiji=[^>]*>\\)+\\)\\)"
		   "</reference=\\([^>]*\\)>") 1 2 4))

;; ndeb agent:
;;
;;   (ndeb DIRECTORY :appendix APPENDIX)
;;
;; DIRECTORY - dictionary directory
;; APPENDIX  - appendix directory
;;
;; [property]
;; ndeb-process - eblook process related with agent
;; ndeb-dict    - last used dictionary
;; ndeb-method  - last used value of search-method
;; ndeb-stop    - last used value of stop-code

(defun ndeb-agent-directory (agent)
  (expand-file-name (lookup-agent-location agent)))

(defun ndeb-agent-appendix (agent)
  (let ((dir (lookup-agent-option agent :appendix)))
    (if dir (expand-file-name dir))))

(defun ndeb-agent-coding (agent)
  (or (lookup-agent-option agent :coding)
      ndeb-process-coding-system))

;; ndeb dictionary:
;;
;; NAME - given by eblook `list' command
;; 
;; [option]
;; :coding    - process coding system
;; :stop-code - stop-code used by eblook

(defun ndeb-dictionary-coding (dictionary)
  (or (lookup-dictionary-option dictionary :coding t)
      ndeb-process-coding-system))

(defun ndeb-dictionary-stopcode (dictionary)
  (lookup-dictionary-option dictionary :stop-code t))

;; ndeb entry:
;;
;; CODE    - entry specific code (e.g. "2c00:340") by eblook `search' command
;; HEADING - given by eblook `search' command

(defun ndeb-new-entry (type code &optional heading)
  (lookup-new-entry type ndeb-current-dictionary code heading))

;;;
;;; macros
;;;

(put 'ndeb-with-agent 'lisp-indent-function 1)
(defmacro ndeb-with-agent (agent &rest body)
  `(let ((ndeb-current-agent ,agent)
	 (ndeb-current-process (ndeb-agent-process ,agent)))
     ,@body))

(put 'ndeb-with-dictionary 'lisp-indent-function 1)
(defmacro ndeb-with-dictionary (dictionary &rest body)
  `(let ((dictionary ,dictionary))
     (ndeb-with-agent (lookup-dictionary-agent dictionary)
       (let ((ndeb-current-dictionary dictionary))
	 (unless (eq dictionary
		     (lookup-get-property ndeb-current-agent 'ndeb-dict))
	   (ndeb-process-require
	    (concat "select " (lookup-dictionary-name dictionary)))
	   (lookup-put-property ndeb-current-agent 'ndeb-dict
				dictionary)
	   (let ((code (ndeb-dictionary-coding dictionary)))
	     (when code
	       (set-process-coding-system ndeb-current-process code code))))
	 ,@body))))

(defun ndeb-agent-process (agent)
  (let ((process (lookup-get-property agent 'ndeb-process)))
    (unless (and process (eq (process-status process) 'run))
      (if process (lookup-process-kill process))
      (setq process (ndeb-process-open (ndeb-agent-directory agent)
				       (ndeb-agent-appendix agent)))
      (let ((coding (ndeb-agent-coding agent)))
	(when coding
	  (set-process-coding-system process coding coding)))
      (let ((ndeb-current-process process))
	(if lookup-max-hits (ndeb-require-set "max-hits" lookup-max-hits))
	(if lookup-max-text (ndeb-require-set "max-text" lookup-max-text)))
      (lookup-put-property agent 'ndeb-process process)
      (lookup-put-property agent 'ndeb-dict nil)
      (lookup-put-property agent 'ndeb-method nil)
      (lookup-put-property agent 'ndeb-stop nil))
    process))

(defun ndeb-agent-kill-process (agent)
  (let ((process (lookup-get-property agent 'ndeb-process)))
    (when process
      (if (eq (process-status process) 'run)
	  (process-send-string process "quit\n"))
      (lookup-process-kill process)
      (lookup-put-property agent 'ndeb-process nil))))


;;;
;;; Interface functions
;;;

(put 'ndeb :list 'ndeb-list)
(defun ndeb-list (agent)
  (ndeb-with-agent agent
    (ndeb-process-require "list"
      (lambda (process)
	(let (dicts)
	  (while (re-search-forward "^[^.]+\\. \\([^\t]+\\)" nil t)
	    (setq dicts (cons (lookup-new-dictionary ndeb-current-agent
						     (match-string 1)) dicts)))
	  (nreverse dicts))))))

(put 'ndeb :kill 'ndeb-agent-kill-process)

(put 'ndeb :title 'ndeb-dictionary-title)
(defun ndeb-dictionary-title (dictionary)
  (ndeb-dictionary-get-info dictionary "title"))

(put 'ndeb :methods 'ndeb-dictionary-methods)
(defun ndeb-dictionary-methods (dictionary)
  (let* ((string (or (ndeb-dictionary-get-info dictionary "search methods") ""))
	 (methods (if (string-match "menu" string) '(menu))))
    (if (string-match "endword" string)
	(setq methods (cons 'suffix methods)))
    (if (string-match "\\<word\\>" string)
	(setq methods (cons 'prefix methods)))
    (if (string-match "exactword" string)
	(setq methods (cons 'exact methods)))
    methods))

(defun ndeb-dictionary-get-info (dictionary key)
  (let ((alist (lookup-get-property dictionary 'ndeb-alist)))
    (unless alist
      (setq alist
	    (ndeb-with-dictionary dictionary
	      (ndeb-process-require "subinfo"
		(lambda (process)
		  (let (alist)
		    (while (re-search-forward "^ \\([^:]+\\): \\(.*\\)" nil t)
		      (setq alist
			    (acons (match-string 1) (match-string 2) alist)))
		    alist)))))
      (lookup-put-property dictionary 'ndeb-alist alist))
    (lookup-assoc-get alist key)))

(put 'ndeb :menu 'ndeb-dictionary-menu)
(defun ndeb-dictionary-menu (dictionary)
  (ndeb-with-dictionary dictionary
    (let ((menu (ndeb-new-entry 'regular "menu" "[menu]"))
	  (content (ndeb-process-require "menu")))
      (lookup-put-property menu 'ndeb-content content)
      menu)))

(defconst ndeb-method-table
  '((exact . "exact") (prefix . "word") (suffix . "endword")))

(put 'ndeb :search 'ndeb-dictionary-search)
(defun ndeb-dictionary-search (dictionary query &optional offset)
  (ndeb-with-dictionary dictionary
    (let ((method (lookup-query-method query))
	  (last (lookup-get-property ndeb-current-agent 'ndeb-method)))
      (unless (eq method last)
	(ndeb-require-set "search-method"
			  (lookup-assq-get ndeb-method-table method))
	(lookup-put-property ndeb-current-agent 'ndeb-method method)))
    (ndeb-process-require (format "search \"%s\" %d"
				  (lookup-query-string query) (or offset 0))
      (lambda (process)
	(let (code heading last-code last-heading entry entries)
	  (while (re-search-forward "^[^.]+\\. \\([^\t]+\\)\t\\(.*\\)" nil t)
	    (setq code (match-string 1) heading (match-string 2))
	    ;; remove duplicate entries
	    (when (or (not (string= code last-code))
		      (not (string= heading last-heading)))
	      (setq entries (cons (ndeb-new-entry 'regular code heading)
				  entries))
	      (setq last-code code last-heading heading)))
	  (when (re-search-forward "<more point=\\([0-9]*\\)>" nil t)
	    (setq entry (ndeb-new-entry 'dynamic "more"))
	    (lookup-put-property entry 'ndeb-query query)
	    (lookup-put-property entry 'ndeb-offset
				 (string-to-int (match-string 1)))
	    (setq entries (cons entry entries)))
	  (nreverse entries))))))

(put 'ndeb :dynamic 'ndeb-dynamic-search)
(defun ndeb-dynamic-search (entry)
  (let ((query (lookup-get-property entry 'ndeb-query))
	(offset (lookup-get-property entry 'ndeb-offset)))
    (ndeb-dictionary-search (lookup-entry-dictionary entry) query offset)))

(put 'ndeb :gaiji 'ndeb-dictionary-gaiji)
(defun ndeb-dictionary-gaiji (dictionary code)
  (list (ndeb-dictionary-font dictionary code)))

(put 'ndeb :font 'ndeb-dictionary-font)
(defun ndeb-dictionary-font (dictionary code)
  (ndeb-with-dictionary dictionary
    (let ((xbm (ndeb-process-require (concat "font " code))))
      (if (string-match "default_width" xbm)
	  (vector 'xbm xbm)))))

(put 'ndeb :content 'ndeb-entry-content)
(defun ndeb-entry-content (entry)
  (or (lookup-get-property entry 'ndeb-content)
      (ndeb-with-dictionary (lookup-entry-dictionary entry)
	(let ((stop (ndeb-dictionary-stopcode ndeb-current-dictionary))
	      (last (lookup-get-property ndeb-current-agent 'ndeb-stop)))
	  (unless (eq stop last)
	    (ndeb-require-set "stop-code" stop)
	    (lookup-put-property ndeb-current-agent 'ndeb-stop stop)))
	(ndeb-process-require (concat "content " (lookup-entry-code entry))))))


;;;
;;; eblook process
;;;

(defun ndeb-process-open (directory appendix)
  (let* ((args (cons "-q" (cons directory (if appendix (list appendix)))))
	 (buffer (lookup-open-process-buffer (concat " *ndeb+" directory "*")))
	 (process-connection-type nil)
	 (process (apply 'start-process "ndeb" buffer ndeb-program-name args)))
    (process-kill-without-query process)
    (accept-process-output process)
    (with-current-buffer (or buffer (lookup-temp-buffer))
      (save-excursion
	(goto-char (point-min))
	(if (search-forward "Warning: invalid book directory" nil t)
	    (error "Invalid dictionary directory: %s" directory))
	(goto-char (point-min))
	(if (search-forward "Warning: invalid appendix directory" nil t)
	    (error "Invalid appendix directory: %s" appendix))))
    process))

(put 'ndeb-process-require 'lisp-indent-function 1)
(defun ndeb-process-require (command &optional filter)
  (lookup-process-require ndeb-current-process (concat command "\n")
			  (concat "^" ndeb-prompt-string) filter))

(defun ndeb-require-set (var value)
  (if value
      (ndeb-process-require (format "set %s \"%s\"" var value))
    (ndeb-process-require (format "unset %s" var))))

(provide 'ndeb)


(defvar image-types nil)

(and
 ndeb-use-inline-image
 window-system
 (or
  (featurep 'xemacs)
  (memq 'xbm image-types))
 (require 'ndeb-image))

;;; ndeb.el ends here
