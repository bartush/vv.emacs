
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


(defun vv/find-file-with-coding-system (file coding-system)
  "Find (Open) FILE with CODING-SYSTEM."
  (interactive (list
                (read-file-name "File Name:")
                (read-coding-system "Coding System:")))
  (let (buf (coding-system-for-read coding-system))
    (if (setq buf
              (catch :exit
                (while (setq buf (find-buffer-visiting file))
                  (if (y-or-n-p (format "Kill buffer \"%s\" visiting file \"%s\"?" buf file))
                      (kill-buffer buf)
                    (throw :exit buf)))))
        (error "Buffer \"%s\" is still visiting \"%s\". In that case `find-file-with-coding-system' does not work as expected." buf file)
      (find-file file))))

(defun vv/find-file-with-windows1251 (file)
  "Find (Open) FILE with cp1251-dos coding system."
  (interactive (list (read-file-name "File Name:")))
  (let (buf (coding-system-for-read 'cp1251-dos))
    (if (setq buf
              (catch :exit
                (while (setq buf (find-buffer-visiting file))
                  (if (y-or-n-p (format "Kill buffer \"%s\" visiting file \"%s\"?" buf file))
                      (kill-buffer buf)
                    (throw :exit buf)))))
        (error "Buffer \"%s\" is still visiting \"%s\". In that case `find-file-with-coding-system' does not work as expected." buf file)
      (find-file file))))

(defun vv/translate-buffer-encoding (target-encoding &optional source-encoding)
  "Translate buffer encoding from SOURCE-ENCODING to TARGET-ENCODING."
  (interactive (list (read-non-nil-coding-system "Target encoding:")))
  (unless source-encoding
    (setq source-encoding buffer-file-coding-system))
  (save-restriction
    (widen)
    (encode-coding-region (point-min)
			  (point-max)
			  source-encoding)
    (decode-coding-region (point-min)
			  (point-max)
			  target-encoding)
    (set-buffer-file-coding-system target-encoding)))


(defvar vv/const/formfeed-as-line-length 40
  "Length of formfeed ^L char line")

(defvar vv/display-formfeed-line-array
  (vconcat (make-list vv/const/formfeed-as-line-length (make-glyph-code ?─ 'font-lock-comment-face)))
  ;;(vconcat '(?↲ ?✂ ?☛)
  ;;[?☨]
  "Array of symbols for formfeed char display")

(defun vv/display-formfeed-line-narrow-decoration ()
  "Formfeed ^L char line display standart decoration"
  (vv/display-formfeed-line-decoration ?✂))

(defun vv/display-formfeed-line-decoration (char)
  "Adds \\[char] at the beginnig and at the end of formfeed ^L char line display"
  (let ((decor-char char))
    (progn
      (aset vv/display-formfeed-line-array 0 (make-glyph-code decor-char 'font-lock-comment-face))
      (aset vv/display-formfeed-line-array
	    (1- (length vv/display-formfeed-line-array))
	    (make-glyph-code decor-char 'font-lock-comment-face)))))

(defun vv/display-formfeed-set-as-line ()
  "Set dispaly of the formfeed ^L char as line."
  (interactive)
  (progn
    (vv/display-formfeed-line-decoration ?─)
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
	  vv/display-formfeed-line-array)
    (redraw-frame)))

(defun vv/helper/formfeed-as-line-p ()
  (when (aref buffer-display-table ?\^L) t))

(defun vv/display-formfeed-toogle-as-line ()
  "Toggles dispaly of the formfeed ^L char as line."
  (interactive)
  (progn
    (vv/display-formfeed-line-decoration ?─)
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (unless (aref buffer-display-table ?\^L)
      (setq vv/display-formfeed-as-line-flag nil))
    (if (vv/helper/formfeed-as-line-p)
	(aset buffer-display-table ?\^L nil)
      (aset buffer-display-table ?\^L vv/display-formfeed-line-array))
    (redraw-frame)))

(defun vv/formfeed-line-narrowed-status ()
  "Changes formfeed ^L char as line decoration when narrowing."
  (if (buffer-narrowed-p)
      (vv/display-formfeed-line-narrow-decoration)
    (vv/display-formfeed-line-decoration ?─)))

(defun vv/narrow-to-formfeed-region ()
  "Toggle buffer narrowing to current region between formfeed ^L chars"
  (interactive)
  (add-hook 'post-command-hook 'vv/formfeed-line-narrowed-status)
  (if (not (buffer-narrowed-p))
      (let ((start) (end))
	(progn
	  (widen)
	  (save-excursion
	    (unless (= (point) (point-max))
	      (goto-char (1+ (point))))
	    (setq start (or (search-backward (char-to-string  ?\^L) nil t 1)
			    (point-min))))
	  (save-excursion
	    (unless (= (point) (point-max))
	      (goto-char (1+ (point))))
	    (setq end (or (search-forward (char-to-string ?\^L) nil t 1)
			  (point-max))))
	  ;; (unless (= start (point-max))
	  ;; 	(1+ start))
	  (unless (= end (point-max))
	    (1+ end))
	  (narrow-to-region start end)))
    (widen)))
