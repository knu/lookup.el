;;; oxford.el --- support file for "Oxford Dictionary/Thesaurus"
;; Copyright (C) 2000 Keisuke Nishida <knsihida@ring.gr.jp>

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
(require 'suppport-generic)

(defvar ipaface 'default)

(defconst oxford-gaiji-table
  (lookup-new-gaiji-table
    '(
      ("h0f01" "=")
      ("h0f02" "=")
      ("h0f03" "=")
      ("h0f04" "=")
      ("h0f05" "=")
      ("h0f06" "=")
      ("h0f07" "=")
      ("h0f08" "=")
      ("h0f09" "=")
      ("h0f0a" "=")
      ("h0f0b" "=")
      ("h0f0c" "=")
      ("h0f0d" "=")
      ("h0f0e" "=")
      ("h0f0f" "=")
      ("h0f10" "=")
      ("h0f11" "=")
      ("h0f12" "=")
      ("h0f13" "=")
      ("h0f14" "=")
      ("h0f15" "=")
      ("h0f16" "=")
      ("h0f17" "=")
      ("h0f18" "=")
      ("h0f19" "=")
      ("h0f1a" "=")
      ("h0f1b" "=")
      ("h0f1c" "=")
      ("h0f1d" "=")
      ("h0f1e" "=")
      ("h0f1f" "=")
      ("h0f20" "&#x00D8;")
      ("h0f21" "&#x00F8;")
      ("h0f22" "&#x25A0;")
      ("h0f23" "&#x00E1;")
      ("h0f24" "&#x0129;")
      ("h0f25" "&#x00E3;")
      ("h0f26" "<BOLD>&</BOLD>")
      ("h0f27" "&#x00F9;")
      ("h0f28" "&#x00F9;")
      ("h0f2a" "&#x00E5;")
      ("h0f2b" "&#x0153;")
      ("h0f2c" "&#x0153;")
      ("h0f30" "<HEAD2>0</HEAD2>")
      ("h0f31" "<HEAD2>1</HEAD2>")
      ("h0f32" "<HEAD2>2</HEAD2>")
      ("h0f33" "<HEAD2>3</HEAD2>")
      ("h0f34" "<HEAD2>4</HEAD2>")
      ("h0f35" "<HEAD2>5</HEAD2>")
      ("h0f36" "<HEAD2>6</HEAD2>")
      ("h0f37" "<HEAD2>7</HEAD2>")
      ("h0f38" "<HEAD2>8</HEAD2>")
      ("h0f39" "<HEAD2>9</HEAD2>")
      ("h0f3a" "<HEAD2>:</HEAD2>")
      ("h0f3b" "<HEAD2>;</HEAD2>")
      ("h0f3c" "&#x2070;")
      ("h0f3d" "&#x2078;")
      ("h0f3e" "&#x2079;")
      ("h0f3f" "&#x2080;")
      ("h0f40" "&#x221B;")
      ("h0f41" "<HEAD3>A</HEAD3>")
      ("h0f42" "<HEAD3>B</HEAD3>")
      ("h0f43" "<HEAD3>C</HEAD3>")
      ("h0f44" "<HEAD3>D</HEAD3>")
      ("h0f45" "<HEAD3>E</HEAD3>")
      ("h0f46" "<HEAD3>F</HEAD3>")
      ("h0f47" "<HEAD3>G</HEAD3>")
      ("h0f48" "<HEAD3>H</HEAD3>")
      ("h0f49" "<HEAD3>I</HEAD3>")
      ("h0f4a" "<HEAD3>J</HEAD3>")
      ("h0f4b" "<HEAD3>K</HEAD3>")
      ("h0f4c" "<HEAD3>L</HEAD3>")
      ("h0f4d" "<HEAD3>M</HEAD3>")
      ("h0f4e" "<HEAD3>N</HEAD3>")
      ("h0f4f" "<HEAD3>O</HEAD3>")
      ("h0f50" "<HEAD3>P</HEAD3>")
      ("h0f51" "<HEAD3>Q</HEAD3>")
      ("h0f52" "<HEAD3>R</HEAD3>")
      ("h0f53" "<HEAD3>S</HEAD3>")
      ("h0f54" "<HEAD3>T</HEAD3>")
      ("h0f55" "<HEAD3>U</HEAD3>")
      ("h0f56" "<HEAD3>V</HEAD3>")
      ("h0f57" "<HEAD3>W</HEAD3>")
      ("h0f58" "<HEAD3>X</HEAD3>")
      ("h0f59" "<HEAD3>Y</HEAD3>")
      ("h0f5a" "<HEAD3>Z</HEAD3>")
      ("h0f5b" "&#x221A;")
      ("h0f5c" "&#x00C9;")
      ("h0f5d" "&#x00A9;")
      ("h0f5e" "&#x00C5;")
      ("h0f5f" "&#x00E6;&#x0303;")
      ("h0f60" "&#x00F6;")
      ("h0f61" "<HEAD3>a</HEAD3>")
      ("h0f62" "<HEAD3>b</HEAD3>")
      ("h0f63" "<HEAD3>c</HEAD3>")
      ("h0f64" "<HEAD3>d</HEAD3>")
      ("h0f65" "<HEAD3>e</HEAD3>")
      ("h0f66" "<HEAD3>f</HEAD3>")
      ("h0f67" "<HEAD3>g</HEAD3>")
      ("h0f68" "<HEAD3>h</HEAD3>")
      ("h0f69" "<HEAD3>i</HEAD3>")
      ("h0f6a" "<HEAD3>j</HEAD3>")
      ("h0f6b" "<HEAD3>k</HEAD3>")
      ("h0f6c" "<HEAD3>l</HEAD3>")
      ("h0f6d" "<HEAD3>m</HEAD3>")
      ("h0f6e" "<HEAD3>n</HEAD3>")
      ("h0f6f" "<HEAD3>o</HEAD3>")
      ("h0f70" "<HEAD3>p</HEAD3>")
      ("h0f71" "<HEAD3>q</HEAD3>")
      ("h0f72" "<HEAD3>r</HEAD3>")
      ("h0f73" "<HEAD3>s</HEAD3>")
      ("h0f74" "<HEAD3>t</HEAD3>")
      ("h0f75" "<HEAD3>u</HEAD3>")
      ("h0f76" "<HEAD3>v</HEAD3>")
      ("h0f77" "<HEAD3>w</HEAD3>")
      ("h0f78" "<HEAD3>x</HEAD3>")
      ("h0f79" "<HEAD3>y</HEAD3>")
      ("h0f7a" "<HEAD3>z</HEAD3>")
      ("h0f7b" "&#x00FB;")
      ("h0f7c" "&#x0169;")
      ("h0f7d" "&#x2207;")
      ("h0f7e" "&#x25A1;")
      ("h0f7f" "&#x00F1;")
      ("h0f80" "&#x00FA;")
      ("h0f81" "&#x00FC;")
      ("h0f82" "&#x00E9;")
      ("h0f83" "&#x00E2;")
      ("h0f84" "&#x00E4;")
      ("h0f85" "&#x00E0;")
      ("h0f86" "&#x00EE;")
      ("h0f87" "&#x00E7;")
      ("h0f88" "&#x00EA;")
      ("h0f89" "&#x00F4;")
      ("h0f8a" "&#x00E8;")
      ("h0f8b" "&#x00EF;")
      ("h0f8c" "&#x00E5;")
      ("h0f8d" "&#x010C;")
      ("h0f8e" "&#x010D;")
      ("h0f8f" "&#x00E1;")
      ("h0f90" "&#x02c8;")
      ("h0f91" "&#x00b9;")
      ("h0f92" "&#x00b2;")
      ("h0f93" "&#x00b3;")
      ("h0f94" "&#x2074;")
      ("h0f95" "&#x2075;")
      ("h0f96" "&#x2076;")
      ("h0f97" "&#x0251;")
      ("h0f98" "&#x0251;&#x0303;")
      ("h0f99" "&#x0251;&#x0300;")
      ("h0f9a" "&#x00ED;")
      ("h0f9b" "&#x2261;")
      ("h0f9c" "&#x00AE;")
      ("h0f9f" "&#x00F6;")
      ("h0fa0" "&#x02cc;")
      ("h0fa1" "&#x2081;")
      ("h0fa2" "&#x2082;")
      ("h0fa3" "&#x2083;")
      ("h0fa4" "&#x2084;")
      ("h0fa5" "&#x2085;")
      ("h0fa6" "&#x2086;")
      ("h0fa7" "&#x2087;")
      ("h0fa8" "&#x2088;")
      ("h0fa9" "&#x2089;")
      ("h0faa" "&#x207A;")
      ("h0fab" "&#x207B;")
      ("h0fac" "&#x00EB;")
      ("h0fad" "&#x00C4;")
      ("h0fae" "&#x00E6;")
      ("h0faf" "&#x00F3;")
      ("h0fb0" "&#x0292;")
      ("h0fb1" "&#x0283;")
      ("h0fb2" "&#x014B;")
      ("h0fb3" "&#x03B8;")
      ("h0fb4" "&#x00F0;")
      ("h0fb5" "&#x00E6;")
      ("h0fb6" "&#x026a;")
      ("h0fb7" "&#x0259;")
      ("h0fb8" "&#x0250;")
      ("h0fb9" "&#x028a;")
      ("h0fba" "&#x028c;")
      ("h0fbb" "&#x025C;")
      ("h0fbc" "&#x0254;")
      ("h0fbd" "&#x0254;&#x0303;")
      ("h0fbe" "&#x00C5;")
      ("h0fbf" "&#x00D6;")
      ("h0fc0" "&#x00E0;")
      ("h0fc1" "<HEAD1>a</HEAD1>")
      ("h0fc2" "<HEAD1>b</HEAD1>")
      ("h0fc3" "<HEAD1>c</HEAD1>")
      ("h0fc4" "<HEAD1>d</HEAD1>")
      ("h0fc5" "<HEAD1>e</HEAD1>")
      ("h0fc6" "<HEAD1>f</HEAD1>")
      ("h0fc7" "<HEAD1>g</HEAD1>")
      ("h0fc8" "<HEAD1>h</HEAD1>")
      ("h0fc9" "<HEAD1>i</HEAD1>")
      ("h0fca" "<HEAD1>j</HEAD1>")
      ("h0fcb" "<HEAD1>k</HEAD1>")
      ("h0fcc" "<HEAD1>l</HEAD1>")
      ("h0fcd" "<HEAD1>m</HEAD1>")
      ("h0fce" "<HEAD1>n</HEAD1>")
      ("h0fcf" "<HEAD1>o</HEAD1>")
      ("h0fd0" "<HEAD1>p</HEAD1>")
      ("h0fd1" "<HEAD1>q</HEAD1>")
      ("h0fd2" "<HEAD1>r</HEAD1>")
      ("h0fd3" "<HEAD1>s</HEAD1>")
      ("h0fd4" "<HEAD1>t</HEAD1>")
      ("h0fd5" "<HEAD1>u</HEAD1>")
      ("h0fd6" "<HEAD1>v</HEAD1>")
      ("h0fd7" "<HEAD1>w</HEAD1>")
      ("h0fd8" "<HEAD1>x</HEAD1>")
      ("h0fd9" "<HEAD1>y</HEAD1>")
      ("h0fda" "<HEAD1>z</HEAD1>")
      ("h0fdb" "<HEAD1>&#x00E9;</HEAD1>")
      ("h0fdc" "<HEAD1>&#x00E7;</HEAD1>")
      ("h0fdd" "<HEAD1>&#x00E8;</HEAD1>")
      ("h0fde" "<HEAD1>&#x00EA;</HEAD1>")
      ("h0fdf" "<HEAD1>&#x00E2;</HEAD1>")
      ("h0fe0" "<HEAD1>&#x00E4;</HEAD1>")
      ("h0fe1" "<HEAD1>A</HEAD1>")
      ("h0fe2" "<HEAD1>B</HEAD1>")
      ("h0fe3" "<HEAD1>C</HEAD1>")
      ("h0fe4" "<HEAD1>D</HEAD1>")
      ("h0fe5" "<HEAD1>E</HEAD1>")
      ("h0fe6" "<HEAD1>F</HEAD1>")
      ("h0fe7" "<HEAD1>G</HEAD1>")
      ("h0fe8" "<HEAD1>H</HEAD1>")
      ("h0fe9" "<HEAD1>I</HEAD1>")
      ("h0fea" "<HEAD1>J</HEAD1>")
      ("h0feb" "<HEAD1>K</HEAD1>")
      ("h0fec" "<HEAD1>L</HEAD1>")
      ("h0fed" "<HEAD1>M</HEAD1>")
      ("h0fee" "<HEAD1>N</HEAD1>")
      ("h0fef" "<HEAD1>O</HEAD1>")
      ("h0ff0" "<HEAD1>P</HEAD1>")
      ("h0ff1" "<HEAD1>Q</HEAD1>")
      ("h0ff2" "<HEAD1>R</HEAD1>")
      ("h0ff3" "<HEAD1>S</HEAD1>")
      ("h0ff4" "<HEAD1>T</HEAD1>")
      ("h0ff5" "<HEAD1>U</HEAD1>")
      ("h0ff6" "<HEAD1>V</HEAD1>")
      ("h0ff7" "<HEAD1>W</HEAD1>")
      ("h0ff8" "<HEAD1>X</HEAD1>")
      ("h0ff9" "<HEAD1>Y</HEAD1>")
      ("h0ffa" "<HEAD1>Z</HEAD1>")
      ("h0ffb" "&#x00FC;")
      ("h0ffc" "&#x00EE;")
      ("h0ffd" "&#x00F4;")
      ("h0ffe" "&#x00F1;")
      ("h1001" "="))))

(defun oxford-arrange-structure (entry)
  (goto-char (point-min))
  (next-line 1)
  (and
   (search-forward ". -" nil t)
   (insert "\n "))

  (goto-char (point-min))
  (replace-string "</HEAD1><HEAD1>" "" nil)
  (goto-char (point-min))  
  (replace-string "</HEAD2><HEAD2>" "" nil)
  (goto-char (point-min))  
  (replace-string "</HEAD3><HEAD3>" "" nil)
  (goto-char (point-min))
  (replace-string "</HEAD3>-<HEAD3>" "-" nil)

  (goto-char (point-min))
  (while (re-search-forward "<HEAD1>\\([^<]*\\)</HEAD1>" nil t)
    (put-text-property (match-beginning 1)
		       (match-end 1)
		       'face 'lookup-heading-1-face))
  (goto-char (point-min))
  (while (re-search-forward "<HEAD2>\\([^<]*\\)</HEAD2>" nil t)
    (put-text-property (match-beginning 1)
		       (match-end 1)
		       'face 'lookup-heading-2-face))
  (goto-char (point-min))
  (while (re-search-forward "<HEAD3>\\([^<]*\\)</HEAD3>" nil t)
    (put-text-property (match-beginning 1)
		       (match-end 1)
		       'face 'lookup-heading-3-face))

  (goto-char (point-min))
  (while (re-search-forward "([^)]*\\(</?HEAD.>\\)[^)]*)" nil t)
    (delete-region (match-beginning 1)
		   (match-end 1))
    (goto-char (match-beginning 0)))

  (goto-char (point-min))
  (while (re-search-forward
	  "\\. <HEAD1>\\([nv]\\|adj\\)</HEAD1>\\. " nil t)
    (goto-char (match-end 0))
    (backward-char 1)
    (insert "\n")
    (delete-char 1)
    (goto-char (match-beginning 0))
    (forward-char 1)
    (insert "\n"))

  (goto-char (point-min))
  (and
   (search-forward (get-iso10646-str ?\x25A1) nil t)
   (let ((p (point)))
     (backward-char 1)
     (insert "\n")
     (forward-char 1)
     (insert "\n")
     (replace-string ". <HEAD3>" ".\n" nil)
     (goto-char p)
     (replace-string "<HEAD2>" "\n " nil)))

  (goto-char (point-min))
  (replace-string "</HEAD2> <HEAD3>" "" nil)


  (goto-char (point-min))
  (save-excursion
    (save-restriction
      (narrow-to-region
       (point-min)
       (if (search-forward (get-iso10646-str ?\x25A1) nil t)
	   (point)
	 (point-max)))

      (goto-char (point-min))
      (replace-string "\. <HEAD2>" "\n" nil)
      (goto-char (point-min))
      (replace-string "\) <HEAD2>" ")\n" nil)
      (goto-char (point-min))
      (replace-string "\. <HEAD3>" ".\n " nil)
      (goto-char (point-min))
      (replace-string "\) <HEAD3>" ")\n " nil)))

  (goto-char (point-min))
  (replace-string "<HEAD1>" "" nil)
  (goto-char (point-min))
  (replace-string "</HEAD1>" "" nil)
  (goto-char (point-min))
  (replace-string "<HEAD2>" "" nil)
  (goto-char (point-min))
  (replace-string "</HEAD2>" "" nil)
  (goto-char (point-min))
  (replace-string "<HEAD3>" "" nil)
  (goto-char (point-min))
  (replace-string "</HEAD3>" "" nil)

  (goto-char (point-min))
  (while (re-search-forward "/[^/]*/" nil t)
    (put-text-property (match-beginning 0)
		       (match-end 0)
		       'face ipaface))
  )

;; (defun oxford-arrange-structure (entry)
;;   (while (re-search-forward "\\( \\([nv]\\|adj\\)\\.\\)\\|\\[[0-9]\\]" nil t)
;;     (save-excursion
;;       (goto-char (match-beginning 0))
;;       (newline))))
;; 
(setq lookup-support-options
      (list ':title "Oxford Dictionary"
	    ':coding 'iso-8859-1
	    ':stop-code "0x1f090000"
	    ':gaiji-table oxford-gaiji-table
	    ':arrange-table '((structure . oxford-arrange-structure))
	    ':transformer 'lookup-stemming-search))

;;; oxford.el ends here
