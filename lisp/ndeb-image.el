;;; ndeb-image.el --- inline image handler for ndeb backend
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

(message "loading ndeb-image...")

(defcustom ndeb-image-cache-dir
  (expand-file-name "~/.lookup-image-cache")
  "*インラインイメージのテンポラリファイルをおくディレクトリ。"
  :type 'directory
  :group 'ndeb)

(defvar image-types nil)

(and
 window-system
 (cond
  ((memq 'xbm image-types) ;; emacs21
   (put 'ndeb :arrange-table '((image . ndeb-expand-images))))
  ((featurep 'xemacs)
   (put 'ndeb :adjust-table '((image . ndeb-expand-images))))))

(defun ndeb-expand-mono-images (entry)
  (goto-char (point-min))
  (let* ((dic (lookup-entry-dictionary entry)))
    (while (re-search-forward
	    "<img=mono:\\([0-9]+\\)x\\([0-9]+\\)>\\([^/]*\\)</img=\\([^>]+\\)>" nil t)
      (let ((pos (match-string 4))
	    (width (match-string 1))
	    (height (match-string 2))
	    (caption (format "〈%s〉\n" (match-string 3))))
	(replace-match caption)
	(let ((glyph
		(lookup-gaiji-glyph-compose
		 (vector
		  'xbm
		  (ndeb-with-dictionary
		      dic
		    (ndeb-process-require
			(format "xbm %s %s %s" pos width height)))))))
	  (lookup-gaiji-glyph-insert glyph))))))

(defun ndeb-expand-color-images (entry)
  (goto-char (point-min))
  (let* ((dic (lookup-entry-dictionary entry))
	 (cache-dir
	  (lookup-dictionary-get-property dic 'image-cache-dir)))
    (when (null cache-dir)
      (setq
       cache-dir
       (expand-file-name (lookup-dictionary-name dic) ndeb-image-cache-dir))
      (or (file-directory-p cache-dir)
	  (make-directory cache-dir t))
      (lookup-dictionary-put-property dic 'image-cache-dir cache-dir))
    ;; color
    (while (re-search-forward "<img=\\(bmp\\|jpeg\\)>" nil t)
      (let ((start (match-beginning 0))
	    (cmd (cdr (assoc (intern (match-string 1))
			     '((bmp . "bmp2tiff")
			       (jpeg . "jpeg"))))))
	(put-text-property start (match-end 0) 'invisible t)
	(if (re-search-forward "</img=\\([0-9a-z:]*\\)>" nil t)
	    (let* ((end (match-end 0))
		   (pos (match-string 1))
		   (cache-file (expand-file-name pos cache-dir)))
	      (put-text-property (match-beginning 0) end 'invisible t)
	      (goto-char end)
	      (insert "\n")
	      (or (file-exists-p cache-file)
		  (ndeb-with-dictionary dic
		    (ndeb-process-require
		     (format "%s %s %s" cmd pos cache-file))))
	      (lookup-image-file-insert cache-file)
	      (delete-file cache-file)))))))

(defun ndeb-expand-images (entry)
  (ndeb-expand-mono-images entry)
  (ndeb-expand-color-images entry))

(provide 'ndeb-image)
