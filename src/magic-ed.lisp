(defpackage :magic-ed
  (:use :cl)
  (:export :magic-ed)
  #+sbcl (:use :sb-alien)
  #+sbcl (:export :system :ed-editor))

(in-package :magic-ed)

(defun slurp-stream (stream)
  "Slurp stream fast as possible, but preallocating buffer first and reading everything in a single pass."
  (let* ((len (file-length stream))
		 (seq (make-string len)))
	(read-sequence seq stream)
	seq))

(defun slurp-file (path)
  "Slurp file from given path."
  (with-open-file (stream path)
    (slurp-stream stream)))

;; SBCL specific thing, stolen from: http://random-state.net/log/3453226588.html

#+sbcl
(defun namestring-for-vim (thing)
  (when thing
	(typecase thing
	  (pathname
	   (sb-ext:native-namestring (translate-logical-pathname thing) :as-file t))
	  (string
	   thing)
	  (t
	   (let* ((source   (sb-introspect:find-definition-source (fdefinition thing)))
			  (pathname (sb-introspect:definition-source-pathname source))
			  (offset   (or (sb-introspect:definition-source-character-offset source) 0)))
		 (unless pathname
		   (error "Don't know where the definition of ~S is, sorry." thing))
		 (format nil "-c \"goto ~A\" ~A"
				 offset
				 (namestring-for-vim pathname)))))))

#+sbcl
(unless sb-ext:*ed-functions*

  (defun ed-editor (thing)
	;; simple FFI for system() call
	(define-alien-routine system int (command c-string))

	(let* ((editor (sb-ext:posix-getenv "EDITOR"))
		   (editor (or editor "vi")))
	  (system
	    (format nil "~A~@[ ~A~]" editor (namestring-for-vim thing)))))

  ;; save it
  (push 'ed-editor sb-ext:*ed-functions*))

(defun magic-ed (&optional file &key (output :file) (eval t))
  "Call editor from REPL and depending on options, return to REPL or evaluate file content in repl.
This function will try to use implmentation specific (ed) function, which will in turn invoke editor set in
EDITOR environment variable. Some Common Lisp implementations uses different strategy for setting external
editor, so if unsure what they are, make sure to consult your implementation documentation first.

If called without file, this function will invoke editor without file argument.

Supported options are:

 :eval  (t or nil) - if :eval was set to nil, saved content will not be evaluated
 :output (:file or :string) - by default, content will be saved to file; if you want the content to
be returned as escaped string, set ':output :string'."

  ;; first assure we have valid parameters if we got them
  (unless (member output '(:file :string))
	(error "Received bad output type. For now only ':file' and ':string' are supported"))

  (let ((status (ed file)))
	(if (and file
			 status
			 (stringp file))
	  (ecase output
		(:file
		  ;; if user does not want to evaluate file
		  (if eval (load file)))
		(:string
		  (slurp-file file)))  
	  ;; invoked plain (ed), return it's status
	  status)))
