;;; lookup-compile.el

(defun lookup-bytecomp ()
  (setq lookup-byte-compiling t)
  (setq load-path (nconc '("." "agent") load-path))
  (mapcar 'load-file command-line-args-left)
  (mapcar 'byte-compile-file command-line-args-left))

(defun lookup-autoload ()
  (require 'autoload)
  (let ((generated-autoload-file "lookup-autoloads.el")
        (make-backup-files nil))
    (with-temp-buffer
      (setq buffer-file-name generated-autoload-file)
      (mapcar 'generate-file-autoloads command-line-args-left)
      (save-buffer))))

;;; lookup-compile.el ends here
