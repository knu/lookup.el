;;; srd.el --- support file for 『ランダムハウス英語辞典』
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'lookup)

(defun srd-arrange-structure (entry)
  (goto-char (point-min))
  (if (looking-at "□ ")
      (delete-region (match-beginning 0) (match-end 0)))
  (if (looking-at "[^\[\n]*")
      (lookup-make-region-heading (match-beginning 0) (match-end 0) 1))

  (goto-char (point-min))
  (while (re-search-forward "\\[[^]]*\\]" nil t)
    (put-text-property (match-beginning 0)
		       (match-end 0) 'face ipaface))

  (goto-char (point-min))
  (while (re-search-forward "^[ ■].*\n" nil t)
    (lookup-make-region-heading (match-beginning 0) (match-end 0)
				(if (eq (char-after (match-beginning 0)) ? )
				    6 2)))
  )

(setq lookup-support-options
      (list ':arrange-table '((structure . srd-arrange-structure))
	    ':gaiji-regexp "<\\(G[0-9]-[0-9a-f][0-9a-f]\\)>"
	    ':transformer 'lookup-stemming-search))

;;; srd.el ends here
