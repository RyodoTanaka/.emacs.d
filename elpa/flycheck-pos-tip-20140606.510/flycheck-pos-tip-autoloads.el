;;; flycheck-pos-tip-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (flycheck-pos-tip-error-messages) "flycheck-pos-tip"
;;;;;;  "flycheck-pos-tip.el" (21552 56127 847048 681000))
;;; Generated autoloads from flycheck-pos-tip.el

(autoload 'flycheck-pos-tip-error-messages "flycheck-pos-tip" "\
Display the tooltip that the messages of ERRORS.

Concatenate all non-nil messages of ERRORS separated by empty
lines, and display them with `pos-tip-show-no-propertize', which shows
 the messages in tooltip, depending on the number of lines.

\(fn ERRORS)" nil nil)

;;;***

;;;### (autoloads nil nil ("flycheck-pos-tip-pkg.el") (21552 56127
;;;;;;  866270 625000))

;;;***

(provide 'flycheck-pos-tip-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-pos-tip-autoloads.el ends here
