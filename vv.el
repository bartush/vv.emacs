;;(require 'cl-lib)
;;(require 'cl)


(defun vv/what? (thing)
  "Prints type and object representation"
  (let ((typeof) (result))
    (progn
      (setq typeof (type-of thing))
      (setq result thing)
      (princ "what?")
      (princ "\n=================================\n")
      (princ "Type: ")
      (princ typeof)
      ;; symbol stuff
      (if (symbolp thing)
	  (progn
	    (princ "\nSymbol components:")
	    (princ "\n  name: ")
	    (princ (symbol-name thing))
	    (princ "\n  function: ")
	    (condition-case ex
		(princ (symbol-function thing))
	      (error (princ (format "!excepton: %s" ex))))
	    (princ "\n  property list: ")
	    (princ (symbol-plist  thing))
	    (princ "\n  value: ")
	    ;; (setq-local result (symbol-value thing))
	    (condition-case ex
		(princ (symbol-value thing))
	      (error (princ (format "!excepton: %s" ex))))
	    )
	(progn
	  (princ "\nvalue: ")
	  (princ thing)))
      (princ "\n=================================\n")
      result)))


(defvar vv/display-formfeed-as-line-flag nil
  "Stores state of formfeed ^L char display as line")

(defvar vv/display-formfeed-as-line-length 50
  "Length of formfeed ^L char line")

(defvar vv/display-formfeed-line-array
  (vconcat (make-list vv/display-formfeed-as-line-length (make-glyph-code ?─ 'font-lock-comment-face)))
  ;;(vconcat '(?↲ ?✂ ?☛)
  ;;[?☨]
  "Array of symbols for formfeed char display")

(defun vv/display-formfeed-line-decoration ()
  "Formfeed ^L char line display decoration"
  (aset vv/display-formfeed-line-array 0 (make-glyph-code ?✂ 'font-lock-comment-face)))

(defun vv/display-formfeed-toogle-as-line ()
  "Toggles dispaly of the formfeed ^L char as line."
  (interactive)
  (progn
    (vv/display-formfeed-line-decoration)
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (unless (aref buffer-display-table ?\^L)q
      (setq vv/display-formfeed-as-line-flag nil))
    (if vv/display-formfeed-as-line-flag
	(progn (aset buffer-display-table ?\^L nil)
	       (setq vv/display-formfeed-as-line-flag nil))
      (progn (aset buffer-display-table ?\^L
		   vv/display-formfeed-line-array)
	     (setq vv/display-formfeed-as-line-flag t)))
    (redraw-frame)))
