;;; mypaedia.el --- support file for 『マイペディア９７』
; -*- coding: iso-2022-7bit -*-
;; Copyright (C) 2000 Keisuke Nishida <knsihida@ring.gr.jp>
;; Copyright (C) 2000 Kazuhiko Shiozaki <kazuhiko@ring.gr.jp>
;; Copyright (C) 2000 Kazuyoshi KOREEDA <k_koreed@d2.dion.ne.jp>

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

(defvar mypaedia-data-directory "/mnt/cdrom/INDEX"
  "BH.DAT, CH.DAT, MAP.DAT, PICT.DAT のある場所。")
(defvar mypaedia-av-directory "/mnt/cdrom/DATA"
  "*.MID, *.WAV, *.AVI のある場所。")
(defvar mypaedia-play-midi-process "timidity"
  "MIDI を再生するプロセス名。nil なら再生しない。")
(defvar mypaedia-play-wav-process "soxplay"
  "WAV を再生するプロセス名。nil なら再生しない。")
(defvar mypaedia-play-avi-process "xanim"
  "AVI を再生するプロセス名。nil なら再生しない。")
(defvar mypaedia-display-image-process "display"
  "画像 を表示するプロセス名。nil なら表示しない。")
(defvar mypaedia-sound-without-notice nil
  "t なら検索と同時に音声を再生する。")
(defvar mypaedia-video-without-notice nil
  "t なら検索と同時に動画を再生する。")
(defvar mypaedia-image-inline t
  "nil なら (可能な場合でも) 画像をインライン表示しない。")
(defvar perl-process "perl"
  "perl のプロセス名。パスが通っていない場合はフルパスで記述すること。")

(defvar mypaedia-process-file-alist '())

(defun mypaedia-string-to-number (string)
  (let ((len (length string))
	(number 0) (i 0) c)
    (while (< i len)
      (setq number (* number 16))
      (setq c (aref string i))
      (cond
       ((and (<= ?0 c) (<= c ?9)) (setq number (+ number (- c ?0))))
       ((and (<= ?a c) (<= c ?f)) (setq number (+ number (- c ?a -10))))
       ((and (<= ?A c) (<= c ?F)) (setq number (+ number (- c ?A -10))))
       (t (setq i len)))
      (setq i (1+ i)))
    number))

(defun mypaedia-arrange-images (entry)
  (while (re-search-forward
	  "<image=\\([^:>]+\\):\\([^:]+\\):\\([^>]+\\)>" nil t)
    (let ((file (concat (match-string 1) ".DAT"))
	  (offset (match-string 2))
	  (length (match-string 3))
	  (start (match-beginning 0))
	  (end (match-end 0)))
	  ;; Find data file.
      (if (file-exists-p (expand-file-name file mypaedia-data-directory))
	  (setq file (expand-file-name file mypaedia-data-directory)))
      (if mypaedia-image-inline
	  (cond ((featurep 'xemacs)
		 (progn
		   (replace-match "\n")
		   (let ((offset (mypaedia-string-to-number offset))
			 (length (mypaedia-string-to-number length))
			 (format-alist
			  '((image/jpeg "JPEG image" "\377\330\377\340\000\020JFIF"
					image-decode-jpeg nil t image-mode)
			    (image/png "Portable Network Graphics" "\211PNG"
				       image-decode-png nil t image-mode))))
		     (insert-file-contents-internal file nil offset
						    (+ offset length)))))
		((featurep 'image)
		 (progn
		   (replace-match "\n")
		   (let ((tmp-img-file
			  (make-temp-name 
			   (expand-file-name "mp" temporary-file-directory))))
		     (call-process
		      "extract-mypaedia.pl" nil nil nil
		      file offset length tmp-img-file)
		     (let ((glyph
			    (with-temp-buffer
			      (insert-file-contents-literally
			       tmp-img-file nil 0 (string-to-int length 16))
			      (string-make-unibyte
			       (buffer-substring-no-properties 
				(point-min) (point-max))))))
		       (insert-image
			(create-image glyph nil t :ascent 'center)))
		       (delete-file tmp-img-file)
		       ))))
	(replace-match "→[画像]")
	(add-text-properties start 
			     (+ (length "→[画像]") start)
			     (list 'action 'mypaedia-display-image
				   'file  file
				   'offset offset
				   'mouse-face 'highlight
				   'face 'lookup-reference-face
				   'length   length))))))
;;
;; 外部プロセスを利用したイメージの表示
;;
(defun mypaedia-display-image (pos)
  (let* ((file (get-text-property pos 'file))
	 (offset (get-text-property pos 'offset))
	 (length (get-text-property pos 'length))
	 (tmp-img-file (make-temp-name 
			(expand-file-name "mp" temporary-file-directory))))
    (if mypaedia-display-image-process
	(progn
	  (call-process perl-process nil nil nil
			"extract-mypaedia.pl"
			file offset length tmp-img-file)
	  (mypaedia-start-process
	   mypaedia-display-image-process nil tmp-img-file t)))))
;;
;; 外部プロセスの呼出し
;; 
(defun mypaedia-start-process (program options file &optional delete-file)
    (message "Starting %s ..." program)
    (let ((pro (apply (function start-process)
		      (format "*mypaedia %s*" program)
		      nil
		      program
		      (append options (list file)))))
      	(message "Starting %s ... done" program)
	(set-process-sentinel pro 'mypaedia-start-process-sentinel)
	(setq mypaedia-process-file-alist 
		(cons (cons pro file) 
		      (if delete-file 
			  mypaedia-process-file-alist
			nil)))))
;;
;; プロセスの状態が変更されたときにファイルを削除する。
;;
(defun mypaedia-start-process-sentinel (process event)
  (let ((al (assoc process mypaedia-process-file-alist)))
    (and (cdr al) (delete-file (cdr al)))
    (setq mypaedia-process-file-alist 
	  (delete al mypaedia-process-file-alist))))

;;
;; 外部プロセスを利用した MIDI の再生
;;
(defun mypaedia-arrange-midi (entry)
  (while (re-search-forward "<sound=\\([^>]+mid\\)>" nil t)
    (let ((file (upcase (match-string 1)))
	  (start (match-beginning 0))
	  (end (match-end 0)))
      (if mypaedia-sound-without-notice
	  (mypaedia-play-midi-sub file t))
      (replace-match "→[音声]")
      (add-text-properties start 
			   (+ (length "→[音声]") start)
			   (list 'action 'mypaedia-play-midi
				 'file file
				 'mouse-face 'highlight
				 'face 'lookup-reference-face)))))

(defun mypaedia-play-midi-sub (file &optional bulk)
  (if mypaedia-play-midi-process
      (let ((midi-file (expand-file-name file mypaedia-av-directory)))
	(if (not (file-exists-p midi-file))
	    (or bulk (error "%s: not found." midi-file))
	  (if (get-process "lookup-play-midi")
	      (delete-process "lookup-play-midi")
	    (if (get-process "lookup-play-wav")
		(delete-process "lookup-play-wav")))
	  (message "Starting %s ..." mypaedia-play-midi-process)
	  (start-process "lookup-play-midi" nil
			 mypaedia-play-midi-process midi-file)
	  (message "Starting %s ... done" mypaedia-play-midi-process)))))

(defun mypaedia-play-midi (pos)
  (mypaedia-play-midi-sub (get-text-property pos 'file)))

;;
;; 外部プロセスを利用した WAV の再生
;;
(defun mypaedia-arrange-wav (entry)
  (while (re-search-forward "<sound=\\([^>]+wav\\)>" nil t)
    (let ((file (upcase (match-string 1)))
	  (start (match-beginning 0))
	  (end (match-end 0)))
      (if mypaedia-sound-without-notice
	  (mypaedia-play-wav-sub file t))
      (replace-match "→[音声]")
      (add-text-properties start 
			   (+ (length "→[音声]") start)
			   (list 'action 'mypaedia-play-wav
				 'file file
				 'mouse-face 'highlight
				 'face 'lookup-reference-face)))))

(defun mypaedia-play-wav-sub (file &optional bulk)
  (if mypaedia-play-wav-process
      (let ((wav-file (expand-file-name file mypaedia-av-directory)))
	(if (not (file-exists-p wav-file))
	    (or bulk (error "%s: not found." wav-file))
	  (if (get-process "lookup-play-midi")
	      (delete-process "lookup-play-midi")
	    (if (get-process "lookup-play-wav")
		(delete-process "lookup-play-wav")))
	  (message "Starting %s ..." mypaedia-play-wav-process)
	  (start-process "lookup-play-wav" nil
			 mypaedia-play-wav-process wav-file)
	  (message "Starting %s ... done" mypaedia-play-wav-process)))))

(defun mypaedia-play-wav (pos)
  (mypaedia-play-wav-sub (get-text-property pos 'file)))

;;
;; 外部プロセスを利用した AVI の再生
;;
(defun mypaedia-arrange-avi (entry)
  (while (re-search-forward "<video=\\([^>]+avi\\)>" nil t)
    (let ((file (upcase (match-string 1)))
	  (start (match-beginning 0))
	  (end (match-end 0)))
      (if mypaedia-video-without-notice
	  (mypaedia-play-avi-sub file t))
      (replace-match "→[動画]")
      (add-text-properties start 
			   (+ (length "→[動画]") start)
			   (list 'action 'mypaedia-play-avi
				 'file  file
				 'mouse-face 'highlight
				 'face 'lookup-reference-face)))))

(defun mypaedia-play-avi-sub (file &optional bulk)
  (if mypaedia-play-avi-process
      (let ((avi-file (expand-file-name file mypaedia-av-directory)))
	(if (not (file-exists-p avi-file))
	    (or bulk (error "%s: not found." avi-file))
	  (if (get-process "lookup-play-avi")
	      (delete-process "lookup-play-avi"))
	  (message "Starting %s ..." mypaedia-play-avi-process)
	  (start-process "lookup-play-avi" nil
			 mypaedia-play-avi-process avi-file)
	  (message "Starting %s ... done" mypaedia-play-avi-process)))))

(defun mypaedia-play-avi (pos)
  (mypaedia-play-avi-sub (get-text-property pos 'file)))

(defconst mypaedia-gaiji-table
  (lookup-new-gaiji-table
   '(("ha121" ",A4") ("ha122" ",0,(B") ("ha123" ",07(B") ("ha124" ",0#(B") ("ha125" ",0'(B")
     ("ha126" ",0I") ("ha127" ",0U") ("ha128" ",0M") ("ha129" ",0r(B") ("ha12a" "`")
     ("ha12b" nil "1/2") ("ha12c" nil "1/3") ("ha12d" ",AA") ("ha12e" ",Aa")
     ("ha12f" ",A@") ("ha130" ",A`") ("ha133" ",AB") ("ha134" ",Ab") ("ha136" ",AD")
     ("ha137" ",Ad") ("ha138" ",AC") ("ha139" ",Ac") ("ha13a" ",AE") ("ha13b" ",Ae")
     ("ha13c" ",AF") ("ha13d" ",Af") ("ha154" ",Aj") ("ha157" ",AK") ("ha158" ",Ak")
     ("ha161" ",Am") ("ha162" ",AL") ("ha168" ",AO") ("ha169" ",Ao") ("ha16a" ",0!(B")
     ("ha16b" ",0Z") ("ha16e" ",B#") ("ha17a" ",AS") ("ha17b" ",As") ("ha17c" ",AR")
     ("ha17d" ",Ar") ("ha222" ",AT") ("ha223" ",At") ("ha224" ",AV") ("ha225" ",Av")
     ("ha226" ",AU") ("ha227" ",Au") ("ha228" ",AX") ("ha229" ",Ax") ("ha22e" ",01(B")
     ("ha22f" ",00(B") ("ha242" ",Az") ("ha243" ",AY") ("ha244" ",Ay") ("ha247" ",A\")
     ("ha248" ",A|") ("ha25b" ",0H") ("ha25c" ",0L")
     ("za121" nil ",A4") ("za122" nil ",0,(B") ("za123" nil ",07(B") ("za124" nil ",0#(B")
     ("za125" nil ",0'(B") ("za126" nil ",0I") ("za127" nil ",0U") ("za128" nil ",0M")
     ("za129" nil ",0r(B") ("za12a" nil "`") ("za12b" nil "1/2")
     ("za12c" nil "1/3") ("za12d" nil ",AA") ("za12e" nil ",Aa") ("za12f" nil ",A@")
     ("za130" nil ",A`") ("za133" nil ",AB") ("za134" nil ",Ab") ("za136" nil ",AD")
     ("za137" nil ",Ad") ("za138" nil ",AC") ("za139" nil ",Ac") ("za13a" nil ",AE")
     ("za13b" nil ",Ae") ("za13c" nil ",AF") ("za13d" nil ",Af") ("za154" nil ",Aj")
     ("za157" nil ",AK") ("za158" nil ",Ak") ("za161" nil ",Am") ("za162" nil ",AL")
     ("za168" nil ",AO") ("za169" nil ",Ao") ("za16a" nil ",0!(B") ("za16b" nil ",0Z")
     ("za16e" nil ",B#") ("za17a" nil ",AS") ("za17b" nil ",As") ("za17c" nil ",AR")
     ("za17d" nil ",Ar") ("za222" nil ",AT") ("za223" nil ",At") ("za224" nil ",AV")
     ("za225" nil ",Av") ("za226" nil ",AU") ("za227" nil ",Au") ("za228" nil ",AX")
     ("za229" nil ",Ax") ("za22e" nil ",01(B") ("za22f" nil ",00(B") ("za242" nil ",Az")
     ("za243" nil ",AY") ("za244" nil ",Ay") ("za247" nil ",A\") ("za248" nil ",A|")
     ("za25b" nil ",0H") ("za25c" nil ",0L"))))

(defvar image-types nil)

(setq lookup-support-options
      (list ':gaiji-table mypaedia-gaiji-table
	    (cond
	     ((featurep 'xemacs)
	      ':adjust-table)
	     ((memq 'xbm image-types)
	      ':arrange-table))
	    '((image . mypaedia-arrange-images))))

;;; mypaedia.el ends here
