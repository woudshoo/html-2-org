;;; html-2-org.el -- Attempts to undo the html export of org

;;; Copyright (C) 2014, 2015 Willem Rein Oudshoorn

;;; Author: Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;;; Created: December 2014
;;; Version: 0.1
;;; Keywords: extensions

;; This file is not part of GNU Emacs
;; Standard GPL v3 or higher license applies

;;; Commentary:
;;
;; This converts a very limited subset of HTML to org source.
;;
;;  
;;; Code:
(require 'org-table)

(defvar h-2-o-conversion-handlers nil
  "Maps HTML tags to processing handlers.
The HTML tags are symbols whose name is equal to the the HTML tag
without attributes.  E.g. <BODY> corresponds to the symbol 'body.

Handlers are functions which take one argument, the parsed html
tag that is matched.  This format is described by the info documentation 
fo the function `libxml-parse-html-region'. 
Basically it is a list of the form:

  (tag attributes &rest) 

TAG is the html tag as a symbol, ATTRIBUTES is an alist of
attributes of the tag, and REST are the children of this tag.

The handler functions should insert the converted html into the
current buffer at point and move the point to the end of the
inserted content, so the next tag is appended.")

(setq h-2-o-conversion-handlers
  '((html . h-2-o-process-children)
	(body . h-2-o-process-children)
	(h1   . h-2-o-process-h1)
	(h2   . h-2-o-process-h2)
	(h3   . h-2-o-process-h3)
	(h4   . h-2-o-process-h4)
	(br   . h-2-o-process-br)
	(p    . h-2-o-process-p)
	(ol   . h-2-o-process-ol)
	(ul   . h-2-o-process-ul)
	(tr   . h-2-o-process-tr)
	(td   . h-2-o-process-td)
	(th   . h-2-o-process-td)
	(table . h-2-o-process-table)
	(thead . h-2-o-process-tbody)
	(tbody . h-2-o-process-tbody)
	(t    . h-2-o-process-children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Low level insert/support functions
;;
(defun h-2-o-string-is-all-the-same-char (string char)
  "Check if STRING is just a repetition of CHAR."
  (let ((index 0)
		(length (length string)))
	(while (and (< index length)
				(eql char (aref string index)))
	  (setq index (1+ index)))
	(eql index length)))

(defun h-2-o-string-trim-right (string)
  "Trims whitespace from the right hand side of STRING.

This function differs from the standard `string-trim-right'
function in what is considered whitespace.  
The standard function considers newlines whitespace, this function does not."
  (if (string-match "[ \t]+\\'" string)
	  (replace-match "" t t string)
	string))

(defun h-2-o-char-before-is (char count)
  "Return t if preceding to point CHAR occurs COUNT times.

This means that in the range [POINT - COUNT, POINT) only CHAR
occurs.  The part of the range that falls outside the buffer is
not considered.
So if POINT = (point-min) the range will be
empty and the function will always return t."
  (while (and (> count 0)
			  (eql char (char-after (- (point) count))))
	(setq count (1- count)))
  (or (eql 0 count) (<= (point) (point-min))))

(defun h-2-o-ensure-char-count (char count)
  "Insert CHAR if needed to ensure it appears COUNT times before point.

However note that the beginning of the buffer is treated special.
If between the beginning of the buffer and point only CHAR
occurs, no additional CHAR's are inserted, regardless of the
COUNT parameter.

The main use of this function is to insert a new line character.
And for this, we only are only interested in newlines between
text, not between the beginning of the buffer and the first
text."
  (while (not (h-2-o-char-before-is char count))
	(insert char)))

(defun h-2-o-ensure-newline ()
  "Insert New line if not at the beginning of a line."
  (h-2-o-ensure-char-count ?\n 1))

(defun h-2-o-ensure-new-paragraph ()
  "Make sure we are at the beginning of a paragraph.
This means that we are the beginning of a line which follows an empty line 
or is the first line in the buffer."
  (h-2-o-ensure-char-count ?\n 2))

(defun h-2-o-replace-in-region (regexp replacement start end)
  "Replace the REGEXP with REPLACEMENT in the region START to END.
This will leave the point at END."
  (goto-char start)
  (while (re-search-forward regexp end 'goto-max)
	(replace-match replacement)))

(defvar h-2-o-fill-string t
  "Controls the filling of strings inserted with `h-2-o-insert'.

If t, the strings that are inserted are filled as a paragraph by
`fill-region'.  If nil, the string is inserted without filling.")

(defun h-2-o-insert (string)
  "Insert STRING into current buffer.
The STRING is not literally included.  Some escape sequences are replaced:

  \\\\n              -> NEWLINE
  NON-BREAKING-SPACE -> SPACE

Also, if the string contains purely of whitespace and newlines,
it will ensure that the inserted string will not result in more
than 2 newlines before point.

Additionally if the variable h-2-o-fill-string is t, the inserted string
is formatted by `fill-regaion'."
  (let ((string (h-2-o-string-trim-right string)))
	(if (h-2-o-string-is-all-the-same-char string ?\n)
		(h-2-o-ensure-char-count ?\n (min 2 (length string)))
	  (let ((start (point)))
		(insert string)
		(h-2-o-replace-in-region "\\\\n" "\n" start (point-max))
		(h-2-o-replace-in-region " " " " start (point-max))
		(when h-2-o-fill-string
			(fill-region start (point)))))))

(defun h-2-o-process-children (parsed-html)
  "Basic handler function that iterates over the child nodes.
This handler will not produce any output for the tag of PARSED-HTML, 
however it will convert all children of the PARSED-HTML."
  (mapc 'h-2-o-html-2-org (cddr parsed-html)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; General Headers
;;
(defun h-2-o-process-h* (prefix parsed-html)
  "Create a paragraph with PREFIX from PARSED-HTML."
  (h-2-o-ensure-new-paragraph)
  (when prefix (insert prefix))
  (h-2-o-process-children parsed-html)
  (h-2-o-ensure-new-paragraph))

(defun h-2-o-process-h1 (parsed-html)  (h-2-o-process-h* "* " parsed-html))
(defun h-2-o-process-h2 (parsed-html)  (h-2-o-process-h* "** " parsed-html))
(defun h-2-o-process-h3 (parsed-html)  (h-2-o-process-h* "*** " parsed-html))
(defun h-2-o-process-h4 (parsed-html)  (h-2-o-process-h* "**** " parsed-html))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tables
;;
(defun h-2-o-process-tr (parsed-html)
  (h-2-o-ensure-newline)
  (insert "|")
  (h-2-o-process-children parsed-html))

(defun h-2-o-process-td (parsed-html)
  (h-2-o-process-children parsed-html)
  (insert "|"))

(defun h-2-o-process-tbody (parsed-html)
  (h-2-o-ensure-newline)
  (insert "|-")
  (h-2-o-process-children parsed-html))

(defun h-2-o-process-table (parsed-html)
  (let ((h-2-o-fill-string nil))
	(h-2-o-process-children parsed-html)
	(h-2-o-ensure-newline)
	(insert "|-")
	(h-2-o-ensure-newline)
	(org-table-align)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Breaks and paragraphs
;;
(defun h-2-o-process-br (parsed-html)
  "Convert the <br/> tag to a newline.
PARSED-HTML is ignored because the br tag should not have content."
  (insert "\n"))

(defun h-2-o-process-p (parsed-html)
  "Insert PARSED-HTML as a paragraph."
  (h-2-o-process-h* nil parsed-html))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Numbered and unnumbered lists
;;
(defvar h-2-o-counter 0
  "Counter used for numbered list items.")

(defun h-2-o-process-ul (parsed-html)
  "Setup the UL environment for PARSED-HTML.

This itself does not putput anything, but will setup the handlers
for the <LI> tags."
  (let ((h-2-o-conversion-handlers
		 (cons '(li . h-2-o-process-unnumbered-list) h-2-o-conversion-handlers)))
	(h-2-o-process-children parsed-html))
  (h-2-o-ensure-newline)) ;; might need ensure-new-paragraph instead

(defun h-2-o-process-unnumbered-list (parsed-html)
  "Handle the <LI> tags for the PARSED-HTML inside a <UL> block."
  (h-2-o-ensure-newline)
  (insert "- ")
  (h-2-o-process-children parsed-html))

;;; OL list
(defun h-2-o-process-ol (parsed-html)
  "Setup the OL environment for PARSED-HTML.

This will setup the handler for the <LI> items."
  (let ((h-2-o-counter 1)
		(h-2-o-conversion-handlers
		 (cons '(li . h-2-o-process-numbered-list) h-2-o-conversion-handlers)))
	(h-2-o-process-children parsed-html))
  (h-2-o-ensure-newline))

(defun h-2-o-process-numbered-list (parsed-html)
  "Handle the <LI> tags for PARSED-HTML inside a <OL> block."
  (h-2-o-ensure-newline)
  (insert (format  "%s. " h-2-o-counter))
  (setq h-2-o-counter (1+ h-2-o-counter))
  (h-2-o-process-children parsed-html))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support functions
;;
(defun h-2-o-handler-for-tag (tag)
  "Return the handler for TAG.
Or if TAG is not listed, try to get the handler for T."
  (or (assoc-default tag h-2-o-conversion-handlers)
	  (assoc-default t h-2-o-conversion-handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Main entry points
;;
(defun h-2-o-html-2-org (parsed-html)
  "Writes a ORG version of the PARSED-HTML into the current buffer.

The parsed-html should be in the format returned by `libxml-parse-html-region'.
See for the description of the format the documentation of that function 
in the elisp manual.

See also the function `h-2-o-insert-org-source-for-html'."
  (cond
   ((stringp parsed-html)
	(h-2-o-insert parsed-html))
   ((listp parsed-html)
	(let ((handler (h-2-o-handler-for-tag (car parsed-html))))
	  (if handler
		  (funcall handler parsed-html)
		(insert (format "%s" parsed-html)))))
   (t (error "Unknow form %s" parsed-html))))


(defun h-2-o-string-to-parsed-html (string)
  "Convert a STRING in html into the parsed form.

The parsed form can be used in `h-2-o-html-2-org'."
  (with-temp-buffer
	(insert string)
	(libxml-parse-html-region (point-min) (point-max))))

(defun h-2-o-insert-org-source-for-html (html-string)
  "Insert the org form of the html in HTML-STRING.

This will insert an approximation of the HTML-STRING content
in org format into the current buffer at point.

Point will be updated to point after the inserted content."
  (h-2-o-html-2-org (h-2-o-string-to-parsed-html html-string)))


(provide 'html-2-org)
;;; html-2-org.el ends here
