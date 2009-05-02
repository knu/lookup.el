;;; support-plus.el --- support file for 『リーダーズ・プラス v2』
;; Copyright (C) 2000 KAWABATA, Taichi

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

(defconst readers-plus-v2-gaiji-table
  (lookup-new-gaiji-table
   '(
     ("ha127" . "◆")
     ("ha128" . "∼")
     ("ha129" . "∼́")
     ("ha12a" . "∼̀")
     ("ha12b" . "∼́")
     ("ha12c" . "∼")
     ("ha12d" . "-")
     ("ha12e" . "-́")
     ("ha12f" . "-̀")
     ("ha130" . "-́")
     ;; ("ha131" . "***") ; triple asterisk
     ("ha132" . "⁑") ; double asterisk
     ("ha133" . "*") ; single asterisk
     ("ha134" . "◇") ; diamond
     ("ha135" . "*")
     ("ha136" . "\"")
     ("ha137" . "ˊ")
     ("ha138" . "Á")
     ("ha139" . "É")
     ("ha13a" . "Í")
     ("ha13b" . "Ó")
     ("ha13c" . "Ú")
     ("ha13d" . "Ý")
     ("ha13e" . "á")
     ("ha13f" . "é")
     ("ha140" . "í")
     ("ha141" . "ó")
     ("ha142" . "ú")
     ("ha143" . "ý")
     ("ha144" . "Á")
     ("ha145" . "É")
     ("ha146" . "Í")
     ("ha147" . "Ó")
     ("ha148" . "Ú")
     ("ha149" . "Ý")
     ("ha14a" . "á")
     ("ha14b" . "é")
     ("ha14c" . "í")
     ("ha14d" . "ó")
     ("ha14e" . "ú")
     ("ha14f" . "ý")
     ("ha150" . "ʌ́")
     ("ha151" . "ǿ")
     ("ha152" . "ˊ")
     ("ha153" . "Á")
     ("ha154" . "É")
     ("ha155" . "Í")
     ("ha156" . "Ó")
     ("ha157" . "Ú")
     ("ha158" . "Ý")
     ("ha159" . "á")
     ("ha15a" . "é")
     ("ha15b" . "í")
     ("ha15c" . "ó")
     ("ha15d" . "ú")
     ("ha15e" . "ý")
     ("ha15f" . "ǿ")
     ("ha160" . "ʌ́")
     ("ha161" . "À")
     ("ha162" . "È")
     ("ha163" . "Ì")
     ("ha164" . "Ò")
     ("ha165" . "Ù")
     ("ha166" . "Ỳ")
     ("ha167" . "à")
     ("ha168" . "è")
     ("ha169" . "ì")
     ("ha16a" . "ò")
     ("ha16b" . "ù")
     ("ha16c" . "ỳ")
     ("ha16d" . "ˋ")
     ("ha16e" . "À")
     ("ha16f" . "È")
     ("ha170" . "Ì")
     ("ha171" . "Ò")
     ("ha172" . "Ù")
     ("ha173" . "Ỳ")
     ("ha174" . "à")
     ("ha175" . "è")
     ("ha176" . "ì")
     ("ha177" . "ò")
     ("ha178" . "ù")
     ("ha179" . "ỳ")
     ("ha17a" . "ø̀")
     ("ha17b" . "ʌ̀")
     ("ha17c" . "Ä")
     ("ha17d" . "Ë")
     ("ha17e" . "Ï")
     ("ha221" . "Ö")
     ("ha222" . "Ü")
     ("ha223" . "Ÿ")
     ("ha224" . "ä")
     ("ha225" . "ë")
     ("ha226" . "ï")
     ("ha227" . "ö")
     ("ha228" . "ü")
     ("ha229" . "ÿ")
     ("ha22a" . "ə")
     ("ha22b" . "Â")
     ("ha22c" . "Ê")
     ("ha22d" . "Î")
     ("ha22e" . "Ô")
     ("ha22f" . "Û")
     ("ha230" . "â")
     ("ha231" . "ê")
     ("ha232" . "î")
     ("ha233" . "ô")
     ("ha234" . "û")
     ("ha235" . "ā")
     ("ha236" . "ē")
     ("ha237" . "ī")
     ("ha238" . "ō")
     ("ha239" . "ū")
     ("ha23a" . "ȳ")
     ("ha23b" . "¸")
     ("ha23c" . "Ç")
     ("ha23d" . "ç")
     ("ha23e" . "ə́")
     ("ha23f" . "ɚ́")
     ("ha240" . "ɛ́")
     ("ha241" . "í")
     ("ha242" . "ɔ́")
     ("ha243" . "ʊ́")
     ("ha244" . "ɯ́")
     ("ha245" . "ʏ́")
     ("ha246" . "ɑ́")
     ("ha247" . "ə́")
     ("ha248" . "ɚ́")
     ("ha249" . "ɛ́")
     ("ha24a" . "í")
     ("ha24b" . "ɔ́")
     ("ha24c" . "ʊ́")
     ("ha24d" . "ɯ́")
     ("ha24e" . "ʏ́")
     ("ha24f" . "ɑ́")
     ("ha250" . "ə̀")
     ("ha251" . "ɚ̀")
     ("ha252" . "ɛ̀")
     ("ha253" . "ì")
     ("ha254" . "ɔ̀")
     ("ha255" . "ʊ̀")
     ("ha256" . "ɯ̀")
     ("ha257" . "ʏ̀")
     ("ha258" . "ɑ̀")
     ("ha259" . "~")
     ("ha25a" . "ɛ̃")
     ("ha25b" . "ɪ̃")
     ("ha25c" . "ɔ̃")
     ("ha25d" . "ɑ̃")
     ("ha25e" . "ã")
     ("ha25f" . "ñ")
     ("ha260" . "Ʌ̃")
     ("ha261" . "ɛ̃́")
     ("ha262" . "ɪ̃́")
     ("ha263" . "ɔ̃́")
     ("ha264" . "ɑ̃́")
     ("ha265" . "ã́")
     ("ha266" . "Ʌ̃́")
     ("ha267" . "ɔ̃̀")
     ("ha268" . "ɪ̃̀")
     ("ha269" . " ")
     ("ha26a" . "ɑ̃̀")
     ("ha26b" . "ã̀")
     ("ha26c" . "Ʌ̃̀")
     ("ha26d" . "Ʌ")
     ("ha26e" . "ø")
     ("ha26f" . "ə")
     ("ha270" . "ɚ")
     ("ha271" . "ɛ")
     ("ha272" . "ɪ")
     ("ha273" . "ɔ")
     ("ha274" . "ʊ")
     ("ha275" . "θ")
     ("ha276" . "ð")
     ("ha277" . "ʃ")
     ("ha278" . "ʒ")
     ("ha279" . "ŋ")
     ("ha27a" . "ː")
     ("ha27b" . "ɑ")
     ("ha27c" . "Ø")
     ("ha27d" . "ṭ")
;    ("ha27e" . "?")
;    ("ha321" . "?")
     ("ha322" . "\\")
     ("ha323" . "ā́")
     ("ha324" . "ḗ")
     ("ha325" . "ī́")
     ("ha326" . "ṓ")
     ("ha327" . " ̑")
     ("ha328" . "˘")
     ("ha329" . "Ă")
     ("ha32a" . "Ĕ")
     ("ha32b" . "Ĭ")
     ("ha32c" . "Ŏ")
     ("ha32d" . "Ŭ")
     ("ha32e" . "Y̆")
     ("ha32f" . "ă")
     ("ha330" . "ĕ")
     ("ha331" . "ğ")
     ("ha332" . "ĭ")
     ("ha333" . "ŏ")
     ("ha334" . "ŭ")
     ("ha335" . "y̆")
     ("ha337" . "ˇ")
     ("ha338" . "Č")
     ("ha339" . "Ǐ")
     ("ha33a" . "Ň")
     ("ha33b" . "Ř")
     ("ha33c" . "Š")
     ("ha33d" . "Ž")
     ("ha33e" . "ǎ")
     ("ha33f" . "č")
     ("ha340" . "ě")
     ("ha341" . "ǐ")
     ("ha342" . "ň")
     ("ha343" . "ř")
     ("ha344" . "š")
     ("ha345" . "ž")
     ("ha346" . "ḵ")
     ("ha347" . "ṯ")
;     ("" . "æ̀")
;     ("" . "æ̃́")
;     ("" . "æ̃̀")
;     ("" . "æ̃")
;     ("" . "ǣ")
;     ("" . "æ")
;     ("" . "Œ́")
;     ("" . "Œ̀")
;     ("" . "Œ̃")
;     ("" . "Œ")
;     ("" . "ɲ")
;     ("" . "ł")
;     ("" . "Å")
;     ("" . "å")
;     ("" . "ń")
;     ("" . "ó")
;     ("" . "ú")
;     ("" . "ý")
;     ("" . "Ǿ")
;     ("" . "ʌ")
;     ("" . "Ý")
;     ("" . "ẃ")
;     ("" . "Á")
;     ("" . "ć")
;     ("" . "ś")
;     ("" . "ỳ")
;     ("" . "ḿ")
;     ("" . "Ć")
;     ("" . "Í")
;     ("" . "Ś")
;     ("" . "Ú")
;     ("" . "Ý")
;     ("" . "Ź")
;     ("" . "ź")
;     ("" . "È")
;     ("" . "Ì")
;     ("" . "ø̀")
;     ("" . "Ä")
;     ("" . "Ë")
;     ("" . "Ï")
;     ("" . "ÿ")
;     ("" . "Â")
;     ("" . "Ê")
;     ("" . "Ô")
;     ("" . "Û")
;     ("" . "Ā")
;     ("" . "Ē")
;     ("" . "Ī")
;     ("" . "Ō")
;     ("" . "Ū")
;     ("" . "ã́")
;     ("" . "ẽ́")
;     ("" . "ĩ́")
;     ("" . "ã̀")
;     ("" . "ṍ")
;     ("" . "Ą")
;     ("" . "Ę")
;     ("" . "ą")
;     ("" . "ę")
;     ("" . "ǫ")
;     ("" . "Ş")
;     ("" . "Ţ")
;     ("" . "ţ")
;     ("" . "H̥")
;     ("" . "h̥")
;     ("" . "n̥")
;     ("" . "¸")
;     ("" . "Ç")
;     ("" . "Ý")
;     ("" . "ɚ")
;     ("" . "ʇ")
;     ("" . "Đ")
;     ("" . "đ")
;     ("" . "ı")
;     ("" . "Ł")
;     ("" . "¿")
;     ("" . "¡")
;     ("" . "~")
;     ("" . "Ã")
;     ("" . "Ñ")
;     ("" . "ẽ")
;     ("" . "ĩ")
;     ("" . "õ")
;     ("" . "ɨ")
;     ("" . "Ø")
;     ("" . "Ö")
;     ("" . "ö")
;     ("" . "ů")
;     ("" . "ẙ")
;     ("" . "ẘ")
;     ("ha348" . "Ḥ")
;     ("ha349" . "Ṇ")
;     ("ha34a" . "Ṣ")
;     ("ha34b" . "Ṭ")
;     ("ha34c" . "ḍ")
;     ("ha34d" . "ḥ")
;     ("ha34e" . "ṃ")
;     ("ha34f" . "ṇ")
;     ("ha350" . "ṛ")
;     ("ha351" . "ṣ")
;     ("ha352" . "ṭ")
;     ("ha353" . "ẓ")
;     ("ha354" . "˙")
;     ("ha355" . "İ")
;     ("ha356" . "Ż")
;     ("ha357" . "ġ")
;     ("ha358" . "ṁ")
;     ("ha359" . "ṅ")
;     ("ha35a" . "∇")
;     ("ha35d" . "Φ")
;     ("ha35f" . "ħ")
;     ("ha360" . "ʊ")
;     ("ha361" . "ʎ")
;     ("ha362" . "ú̥")
;     ("ha363" . "þ")
;     ("ha369" . "Ń")
     ("zb121" . "━")
     ("zb14f" . "æ̀")
     ("zb150" . "ǽ")
     ("zb155" . "æ")
     ("zb17b" . "𝄐")
     ("zb17c" . "𝄑")
     ("zb262" . "拼"))))

(setq lookup-support-options
      (list :gaiji-table readers-plus-v2-gaiji-table))

(require 'lookup)

;;; support-plusv2.el ends here
