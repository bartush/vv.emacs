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
      result
      )
    )
  )
