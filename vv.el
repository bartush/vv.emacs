
;; macros
(defmacro vv/until (test &rest body)
  `(while (not ,test)
    ,@body))

(defmacro vv/when-version< (ver &rest body)
  (declare (indent 1) (debug t))
  (list 'if (version< emacs-version ver) (cons 'progn body)))

(defmacro vv/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defmacro vv/measure-time-limit (limit &rest body)
  "Measure the time it takes to evaluate BODY and
   prints result if it more than `limit'"
  `(let ((time (current-time)))
     ,@body
     (let ((time-est (float-time (time-since time))))
       (unless (< time-est ,limit)
	 (message "%.06f" time-est)))))

(defmacro vv/defclosure (vars &rest defuns)
  (declare (indent 1) (debug t))
  `(let* ,vars
	,(cons 'progn defuns)))


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

(defun vv/which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))


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

(defun vv/find-file-with-dos866 (file)
  "Find (Open) FILE with cp1251-dos coding system."
  (interactive (list (read-file-name "File Name:")))
  (let (buf (coding-system-for-read 'cp866-dos))
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


(defun vv/get-leading-zeroes-index-string (num-digits index)
  "Function converts 'index' to string with leading
   zeroes fitting given number of 'num-digits'"
  (let* (str-index len-str-index)
    (setq str-index (number-to-string index))
    (setq len-str-index (length str-index))
    (cond ((< num-digits len-str-index) nil)
	  ((= num-digits len-str-index) str-index)
	  (t (concat (make-string (- num-digits len-str-index) ?0) str-index)))))

(defun vv/generate-available-file-name (path prefix num-digits extension)
  "Function generates available file name at given 'path'
   starting with 'prefix', 'num-digits' index and given 'extension'"
  (let* (file-name file-name-formater index)
    (setq index 0)
    (setq file-name-formater (lambda ()
			       (format "%s/%s%s%s"
				       path
				       prefix
				       (vv/get-leading-zeroes-index-string num-digits index)
				       extension)))
    (setq file-name (funcall file-name-formater))
    (while (file-exists-p file-name)
      (setq index (1+ index))
      (setq file-name (funcall file-name-formater)))
    file-name))

(defun vv/ipynb-export-image-at-point ()
  "Command to export Emacs IPython Notebook (EIN) generated image at selected point to '.png' file"
  (interactive)
  (let ((property-list (text-properties-at (point))))
    ;; (print (length property-list))
    (let (display-props)
      (setq display-props (plist-get property-list 'display))
      (let (image-plist image-data)
	(setq image-plist (when (eq 'image (car display-props))
			    (cdr display-props)))
	(when image-plist
	  (setq image-data (plist-get image-plist :data))
	  (with-temp-buffer
	    (insert image-data)
	    (let (file-name)
	      (setq file-name (vv/generate-available-file-name .emacs/image-export-path "img" 6 ".png"))
	      (write-file file-name))))))))



(defun vv/org-mode-image-url-toggle-overlays ()
  "Toggle image overlays on `image-url' link types
   `image-url' type must be registered using `org-add-link-type' function"
  (interactive)
  (dolist (image-url (org-element-map (org-element-parse-buffer) 'link
		       (lambda (link)
			 (when (string= "image-url" (org-element-property :type link))
			   link))))
    (let* ((link (org-element-property :path image-url))
	   (link (or link "http://cat-bounce.com/cb.png"))
	   (ovs (overlays-at (org-element-property :begin image-url)))
           (ov (if ovs nil (make-overlay (org-element-property :begin image-url)
					 (org-element-property :end image-url))))
	   (img (if ov (create-image
			(with-current-buffer ;;get image data from url and put it to temp buffer
			    (url-retrieve-synchronously link)
			  (goto-char (point-min))
			  (re-search-forward "\r?\n\r?\n")
			  (buffer-substring-no-properties (point) (point-max)))
			nil t :scale 0.5)
		  nil)))
      (if img (progn
		(overlay-put ov 'display img)
		(overlay-put ov 'face 'default)
		(overlay-put ov 'image-url t))
	(progn (dolist (curr-ov ovs) (delete-overlay curr-ov)))))))

(defun vv/insert-url-image (&optional link)
  (interactive)
  (let* ((link (or link "http://cat-bounce.com/cb.png"))
	 (buf (url-retrieve-synchronously link))
	 (point-curr (point))
	 (image-data
	  (with-current-buffer buf
	    (goto-char (point-min))
	    (re-search-forward "\r?\n\r?\n")
	    (buffer-substring-no-properties (point) (point-max)))))
    (put-text-property point-curr (1+ point-curr) 'display (create-image image-data nil t))))
