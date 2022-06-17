;;; cape-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cape" "cape.el" (0 0 0 0))
;;; Generated autoloads from cape.el

(autoload 'cape-file "cape" "\
Complete file name at point.
See the user option `cape-file-directory-must-exist'.
If INTERACTIVE is nil the function acts like a capf.

\(fn &optional INTERACTIVE)" t nil)

(autoload 'cape-symbol "cape" "\
Complete Elisp symbol at point.
If INTERACTIVE is nil the function acts like a capf.

\(fn &optional INTERACTIVE)" t nil)

(autoload 'cape-dabbrev "cape" "\
Complete with Dabbrev at point.
See the user options `cape-dabbrev-min-length' and
`cape-dabbrev-check-other-buffers'.
If INTERACTIVE is nil the function acts like a capf.

\(fn &optional INTERACTIVE)" t nil)

(autoload 'cape-ispell "cape" "\
Complete word at point with Ispell.
If INTERACTIVE is nil the function acts like a capf.

\(fn &optional INTERACTIVE)" t nil)

(autoload 'cape-dict "cape" "\
Complete word from dictionary at point.
See the custom option `cape-dict-file'.
If INTERACTIVE is nil the function acts like a capf.

\(fn &optional INTERACTIVE)" t nil)
 (autoload 'cape-tex "cape" nil t)
 (autoload 'cape-sgml "cape" nil t)
 (autoload 'cape-rfc1345 "cape" nil t)

(autoload 'cape-abbrev "cape" "\
Complete abbreviation at point.
If INTERACTIVE is nil the function acts like a capf.

\(fn &optional INTERACTIVE)" t nil)

(autoload 'cape-keyword "cape" "\
Complete programming language keyword at point.
See the variable `cape-keywords'.
If INTERACTIVE is nil the function acts like a capf.

\(fn &optional INTERACTIVE)" t nil)

(autoload 'cape-line "cape" "\
Complete current line from other lines in buffer.
If INTERACTIVE is nil the function acts like a capf.

\(fn &optional INTERACTIVE)" t nil)

(autoload 'cape-super-capf "cape" "\
Merge CAPFS and return new Capf which includes all candidates.

\(fn &rest CAPFS)" nil nil)

(autoload 'cape-company-to-capf "cape" "\
Convert Company BACKEND function to Capf.
VALID is the input comparator, see `cape--input-valid-p'.
This feature is experimental.

\(fn BACKEND &optional VALID)" nil nil)

(autoload 'cape-interactive-capf "cape" "\
Create interactive completion function from CAPF.

\(fn CAPF)" nil nil)

(autoload 'cape-wrap-buster "cape" "\
Call CAPF and return a completion table with cache busting.
The cache is busted when the input changes, where VALID is the input
comparator, see `cape--input-valid-p'.

\(fn CAPF &optional VALID)" nil nil)

(autoload 'cape-wrap-properties "cape" "\
Call CAPF and add additional completion PROPERTIES.
Completion properties include for example :exclusive, :annotation-function and
the various :company-* extensions. Furthermore a boolean :sort flag and a
completion :category symbol can be specified.

\(fn CAPF &rest PROPERTIES)" nil nil)

(autoload 'cape-wrap-predicate "cape" "\
Call CAPF and add an additional candidate PREDICATE.
The PREDICATE is passed the candidate symbol or string.

\(fn CAPF PREDICATE)" nil nil)

(autoload 'cape-wrap-silent "cape" "\
Call CAPF and silence it (no messages, no errors).

\(fn CAPF)" nil nil)

(autoload 'cape-wrap-case-fold "cape" "\
Call CAPF and return a case insenstive completion table.
If DONT-FOLD is non-nil return a case sensitive table instead.

\(fn CAPF &optional DONT-FOLD)" nil nil)

(autoload 'cape-wrap-noninterruptible "cape" "\
Call CAPF and return a non-interruptible completion table.

\(fn CAPF)" nil nil)

(autoload 'cape-wrap-purify "cape" "\
Call CAPF and ensure that it does not modify the buffer.

\(fn CAPF)" nil nil)
 (autoload 'cape-capf-noninterruptible "cape")
 (autoload 'cape-capf-case-fold "cape")
 (autoload 'cape-capf-silent "cape")
 (autoload 'cape-capf-predicate "cape")
 (autoload 'cape-capf-properties "cape")
 (autoload 'cape-capf-buster "cape")

(register-definition-prefixes "cape" '("cape-"))

;;;***

;;;### (autoloads nil nil ("cape-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cape-autoloads.el ends here
