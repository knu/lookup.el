(require 'texinfo)

(mapcar
 '(lambda (file)
    (find-file file)
    (set-buffer-file-coding-system 'iso-2022-7bit)
    (texinfo-every-node-update)
    (texinfo-format-buffer)
    (set-buffer-file-coding-system 'iso-2022-7bit)
    (save-buffer)
    (kill-buffer (current-buffer)))
 command-line-args-left)
