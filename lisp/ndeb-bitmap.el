;;; ndeb-bitmap.el --- inline image handler for ndeb backend
; -*- coding: iso-2022-7bit -*-
;; Copyright (C) 2002 yamagata@nwgpc.kek.jp
;; Author: yamagata@nwgpc.kek.jp
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

(message "Loading ndeb-bitmap...")

(and
 window-system
 (featurep 'bitmap)
 (put 'ndeb :arrange-table '((image . ndeb-arrange-xbm))))

(defun ndeb-arrange-xbm (entry)
  (let* ((dictionary (lookup-entry-dictionary entry)) width height pos)
    (while (re-search-forward "<img=mono:\\([0-9]+\\)x\\([0-9]+\\)>" nil t)
      (setq width (match-string 1)
	    height (match-string 2))
      (replace-match "〈")
      (re-search-forward "</img=\\([^<>]+\\)>" nil t)
      (setq pos (match-string 1))
      (replace-match "〉\n")
      (let ((xbm
	     (ndeb-with-dictionary dictionary
				   (ndeb-process-require
				    (format "xbm %s %s %s" pos width height)))))
	(if (string= "#define" (substring xbm 0 7))
	    (condition-case err
		(let ((content-buffer (current-buffer))
		      temp-buffer)
		  (with-temp-buffer
		    (set-buffer-file-coding-system 'raw-text)
		    (insert xbm)
		    (setq temp-buffer (current-buffer))
		    (with-current-buffer content-buffer
		      (bitmap-insert-xbm-buffer temp-buffer))))
	      (error (message "%s" err)))
	  (message xbm))))))

(provide 'ndeb-bitmap)
