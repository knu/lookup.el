;;; lookup-glyph.el --- glyph function definitions cut off from lookup-types.el
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

;; gaiji glyph

(defvar image-types nil) ;; for emacs-20.7 etc.

(cond
 ((xemacs-p)
  (defun lookup-gaiji-glyph-compose (spec)
    (cond
     ((stringp spec)
      (make-glyph (vector 'string :data spec)))
     ((eq (aref spec 0) 'compose)
      (make-glyph (vector 'string :data (aref spec 1))))
     ((eq (aref spec 0) 'xbm)
      (let ((xbm (aref spec 1))
	    width height data)
	(with-temp-buffer
	  (insert xbm)
	  (goto-char (point-min))
	  (if (re-search-forward "width[ \t]+\\([0-9]+\\)")
	      (setq width (string-to-int (match-string 1))))
	  (if (re-search-forward "height[ \t]+\\([0-9]+\\)")
	      (setq height (string-to-int (match-string 1))))
	  (while (re-search-forward "0x\\(..\\)" nil t)
	    (setq data (cons (string-to-int (match-string 1) 16) data)))
	  (setq data (concat (nreverse data))))
	(make-glyph (vector 'xbm :data (list width height data)))))
     (t (error "Invalid glyph spec: %S" spec))))

  (defun lookup-gaiji-glyph-paste (start end glyph)
    (cond
     ((eq (type-of glyph) 'glyph)
      (set-extent-property (extent-at start nil 'lookup-gaiji) 'invisible t)
      (let (extent extents)
	(while (setq extent (extent-at start nil nil extent 'at))
	  (if (eq (extent-start-position extent) (extent-end-position extent))
	      (setq extents (cons extent extents))))
	(while extents
	  (set-extent-endpoints (car extents) end end)
	  (setq extents (cdr extents)))
	(let ((extent (make-extent end end)))
	  (or
	   (extent-begin-glyph extent)
	   (set-extent-begin-glyph extent glyph)))))
     ((stringp glyph) nil)))

  ;; to handle inline-image

  (defun lookup-gaiji-glyph-insert (glyph)
    (set-extent-property (make-extent (point) (point))
			 'end-glyph glyph))

  (defun lookup-image-file-insert (file)
    (let ((format-alist
	   '((image/jpeg "JPEG" "\377\330"
			 image-decode-jpeg nil t image-mode)
	     (image/tiff "TIFF" "II\\*\000"
			 image-decode-tiff nil t image-mode)
	     (image/tiff "TIFF" "MM\000\\*"
			 image-decode-tiff nil t image-mode))))
      (insert-file-contents-internal file))))

 ((memq 'xbm image-types)
  (defun lookup-gaiji-glyph-compose (spec)
    (cond
     ((eq (aref spec 0) 'xbm)
      (let (width height data)
	(with-temp-buffer
	  (insert (aref spec 1))
	  (goto-char (point-min))
	  (if (re-search-forward "width[ \t]+\\([0-9]+\\)")
	      (setq width (string-to-int (match-string 1))))
	  (if (re-search-forward "height[ \t]+\\([0-9]+\\)")
	      (setq height (string-to-int (match-string 1))))
	  (while (re-search-forward "0x\\(..\\)" nil t)
	    (setq data (cons (string-to-int (match-string 1) 16) data)))
	  (setq data (concat (nreverse data))))
	(list 'image :type 'xbm :ascent 'center
	      :width width :height height :data data)))
     (t (error "Invalid glyph spec: %S" spec))))

  (defun lookup-gaiji-glyph-paste (start end glyph)
    (add-text-properties start end
			 (list 'display glyph
			       'intangible glyph
			       'rear-nonsticky (list 'display))))

  (defun lookup-gaiji-glyph-insert (glyph)
    (insert-image glyph))

  (defun lookup-image-file-insert (file)
    (let ((glyph
	   (with-temp-buffer
	     (insert-file-contents-literally file)
	     (string-make-unibyte
	      (buffer-substring-no-properties 
	       (point-min) (point-max))))))
      (insert-image
       (create-image glyph nil t :ascent 'center)))))

 ((featurep 'bitmap)
  (defun lookup-gaiji-glyph-compose (spec)
    (cond
     ((stringp spec) spec)
     ((eq (aref spec 0) 'compose)
      (apply 'compose-chars (string-to-list (aref spec 1))))
     ((eq (aref spec 0) 'xbm)
      (with-temp-buffer
        (insert (aref spec 1))
        (let ((cmp (bitmap-decode-xbm
                    (bitmap-read-xbm-buffer (current-buffer)))))
          (bitmap-compose (aref cmp 0)))))
     (t (error "Invalid glyph spec: %S" spec))))
  (defun lookup-gaiji-glyph-paste (start end glyph)
    (and
     (symbolp glyph)
     (vectorp (symbol-value glyph))
     (eq (type-of (aref (symbol-value glyph) 2)) 'string)
     (setq glyph (aref (symbol-value glyph) 2)))
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'after-string glyph))))

 (t
  (defun lookup-gaiji-glyph-compose (spec)
    (cond
     ((stringp spec) spec)
     ((eq (aref spec 0) 'compose)
      (apply 'compose-chars (string-to-list (aref spec 1))))
     ((eq (aref spec 0) 'xbm)
      lookup-gaiji-alternative)
     (t (error "Invalid glyph spec: %S" spec))))
  (defun lookup-gaiji-glyph-paste (start end glyph)
    (and
     (symbolp glyph)
     (vectorp (symbol-value glyph))
     (eq (type-of (aref (symbol-value glyph) 2)) 'string)
     (setq glyph (aref (symbol-value glyph) 2)))
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'after-string glyph)))))

(provide 'lookup-glyph)
