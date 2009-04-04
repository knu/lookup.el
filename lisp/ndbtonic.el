;;; ndbtonic.el --- Lookup BTONIC dictionary interface -*- coding: utf-8 -*-

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

;;; Documentation:

;; ndbtonic.el provides the suffix array index searching capability
;; for BTONIC dictionary by using `sary' program
;; (http://www.namazu.org/sary/).
;;
;; To use this agent, you must uncompress XXX.EXI format file to
;; XXXX.xml file, and then apply following ruby program and `mksary
;; -s' (or `mkary -so' if you like) command to create sufary index.

;; #!/usr/bin/env ruby -Ks
;; # Usage: ruby btonic.rb XXXX.xml
;; STDIN.reopen(ARGV[0], "r")
;; STDOUT.reopen(ARGV[0]+".ary", "w")
;; file = $stdin
;; $offset=0
;; file.each_line{|line|
;;   if (( line =~ /^(.*)(<key>)(.+)<\/key>/ ) ||
;;       ( line =~ /^(.*<headword[^>]*)(>)(.+)<\/headword>/))
;;     offs = $offset+$1.length
;;     print [offs].pack("N")
;;     offs = offs+$2.length
;;     chars=$3.split(//)
;;     chars.each {|char| 
;;       print [offs].pack("N")
;;       offs = offs+char.length
;;     }
;;   elsif (( line =~ /^(.*)<dic-item id="[0-9]+">/ ))
;;     offs = $offset+$1.length
;;     print [offs].pack("N")
;;   end
;;   $offset+=line.length
;; }

;;; Usage:

;;  Specify the directory with XXX.xml.ary files.  
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndbtonic "~/edicts/OnMusic")
;;           ....
;;           ))

;;; Code:

(require 'ndsary)


;;;
;;; Interface functions
;;;

(put 'ndbtonic :charsets '(ascii japanese-jisx0208))

(put 'ndbtonic :methods 'ndbtonic-dictionary-methods)
(defun ndbtonic-dictionary-methods (dictionary)
  '(exact prefix suffix substring))

(put 'ndbtonic :list 'ndbtonic-list)
(defun ndbtonic-list (agent)
  "Return list of dictionaries of btonic AGENT."
  (let* ((files (directory-files 
                 (expand-file-name (lookup-agent-location agent))
                 nil "\\.xml\\.ary\\'")))
    (mapcar (lambda (name) 
              (lookup-new-dictionary agent (file-name-sans-extension name)))
            files)))

(put 'ndbtonic :title 'ndbtonic-title)
(defun ndbtonic-title (dictionary)
  "Get title of DICTIONARY."
  (or (lookup-dictionary-option dictionary :title)
      (let ((name (lookup-dictionary-name dictionary)))
        (file-name-sans-extension name))))

(put 'ndbtonic :search 'ndbtonic-dictionary-search)
(defun ndbtonic-dictionary-search (dictionary query)
  "Return entry list of DICTIONARY for QUERY."
  (let ((entry-pairs '(("<key>" . "</key>")
                       (">" . "</headword>")))
        (string      (lookup-query-string query))
        (method      (lookup-query-method query))
        (regular     t)
        (file        (expand-file-name
                      (lookup-dictionary-name dictionary)
                      (lookup-agent-location
                       (lookup-dictionary-agent dictionary))))
        (header-func nil)
        (coding      'cp932-dos)
        (max-hits    (or lookup-max-hits 100)))
    (mapcar
     (lambda (x) (lookup-new-entry
                  'regular dictionary (car x)
                  (if header-func (funcall header-func x query)
                    (cdr x))))
     (apply 'nconc
            (mapcar (lambda (x)
                      (ndsary-file-search
                       file string method (car x) (cdr x) regular coding max-hits))
                    entry-pairs)))))

(put 'ndbtonic :content 'ndbtonic-entry-content)
(defun ndbtonic-entry-content (entry)
  "Return string content of ENTRY."
  (let* ((string         (lookup-entry-code entry))
         (dictionary     (lookup-entry-dictionary entry))
         (coding         'cp932-dos)
         (file           (expand-file-name
                          (lookup-dictionary-name dictionary)
                          (lookup-agent-location
                           (lookup-dictionary-agent dictionary))))
         (content-start "<dic-item")
         (content-end   "</dic-item>"))
    (if (string-match "^[0-9]+$" string)
        (setq string (concat "<dic-item id=\"" string)))
    (ndsary-file-content file string content-start content-end coding)))

(put 'ndbtonic :arrange-table
     '((replace   ndbtonic-arrange-replace
                  ndbtonic-arrange-image
                  ndbtonic-arrange-audio)
       (gaiji     lookup-arrange-gaijis
                  ndbtonic-arrange-gaiji
                  )
       (reference lookup-arrange-references)
       (structure ndbtonic-arrange-structure
                  lookup-arrange-structure
                  )
       (fill      lookup-arrange-fill-lines
                  ;; ndbtonic-arrange-snd-autoplay
                  )))

;; lookup content-arrangement functions and options
(put 'ndbtonic :gaiji-regexp  "<gi set=\"unicode\" name=\"\\(.+\\)\"/>")
(put 'ndbtonic :gaiji     #'ndbtonic-dictionary-gaiji)

(put 'ndbtonic :reference-pattern 
     '("<ref idref=\"\\([0-9]+\\)\">\\([^<]+\\)</ref>" 2 2 1))

;;;
;;; Main Program
;;;

(defun ndbtonic-arrange-replace (entry)
  (while (re-search-forward "\t+" nil t)
    (replace-match "")))

(defun ndbtonic-arrange-image (entry)
  (while (re-search-forward "<img resid=\"\\(.+?\\)\"/>" nil t)
    (replace-match "【画像】")))

(defun ndbtonic-arrange-audio (entry)
  (while (re-search-forward "<audio resid=\"\\(.+?\\)\"/>" nil t)
    (replace-match "【音声】")))

(defun ndbtonic-dictionary-gaiji (dictionary gaiji)
  (list (apply 'string (mapcar (lambda (x) (string-to-number x 16))
                           (split-string gaiji "|" t)))))

(defun ndbtonic-arrange-gaiji (entry)
  ;; for mojikyo characters, etc.
  )

(defun ndbtonic-arrange-structure (entry)
  (while (re-search-forward "<key type=\"ソート用かな\">.+?</key>" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "<\\(?:span\\|headword\\) type=\"\\(.+?\\)\".*?>\\(.+?\\)</.+?>" nil t)
    (replace-match "\\1:\\2" t))
  (goto-char (point-min))
  (while (re-search-forward "<\\(?:p\\|div\\) type=\"\\(.+?\\)\".*?>" nil t)
    (replace-match "\\1:" t))
  ;;(goto-char (point-min))
  ;;(while (re-search-forward "<\\(?:p\\|div\\).*?>" nil t)
  ;;  (replace-match "　　" t))
  (goto-char (point-min))
  (while (re-search-forward "<.+?>" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\n+" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (if (looking-at "\n") (delete-region (point-min) (1+ (point-min))))
  )

(provide 'ndbtonic)

;;; ndbtonic.el ends here