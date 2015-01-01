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
	(br   . h-2-o-process-br)
	(p    . h-2-o-process-p)
	(ol   . h-2-o-process-ol)
	(ul   . h-2-o-process-ul)))

(defvar h-2-o-counter 0)

(defun h-2-o-replace-in-region (regexp replacement start end)
  "Replace the REGEXP with REPLACEMENT in the region START to END.
This will leave the point at END."
  (goto-char start)
  (while (re-search-forward regexp end 'goto-max)
	(replace-match replacement)))

(defun h-2-o-insert (string)
  "Insert STRING into current buffer.
The STRING is not literally included.  Some escape sequences are rplaced:

  \\\\n              -> NEWLINE
  NON-BREAKING-SPACE -> SPACE

And the inserted string is filled with `fill-region'."
  (let ((start (point)))
	(insert string)
	(h-2-o-replace-in-region "\\\\n" "\n" start (point-max))
	(h-2-o-replace-in-region " " " " start (point-max))
	(fill-region start (point))))


(defun h-2-o-process-children (parsed-html)
  "Basic handler function that iterates over the child nodes.
This handler will not produce any output for the tag of PARSED-HTML, 
however it will convert all children of the PARSED-HTML."
  (mapc 'h-2-o-html-2-org (cddr parsed-html)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun h-2-o-process-br (parsed-html)
  "Convert the <br/> tag to a newline."
  (insert "\n"))


(defun h-2-o-process-p (parsed-html)
  "Wrap a <p> block between two newlines."
  (insert "\n")
  (h-2-o-process-children parsed-html)
  (insert "\n"))

;;; UL list

(defun h-2-o-process-ul (parsed-html)
  "Setup the UL environment so the child <LI> tags are formatted
correctly."
  (let ((h-2-o-conversion-handlers
		 (cons '(li . h-2-o-process-unnumbered-list) h-2-o-conversion-handlers)))
	(h-2-o-process-children parsed-html))
  (insert "\n"))

(defun h-2-o-process-unnumbered-list (parsed-html)
  "handles the <LI> tags inside a <UL> block."
  (insert "\n")
  (insert "- ")
  (h-2-o-process-children parsed-html))

;;; OL list
(defun h-2-o-process-ol (parsed-html)
  (let ((h-2-o-counter 1)
		(h-2-o-conversion-handlers
		 (cons '(li . h-2-o-process-numbered-list) h-2-o-conversion-handlers)))
	(h-2-o-process-children parsed-html))
  (insert "\n"))

(defun h-2-o-process-numbered-list (parsed-html)
  (insert "\n")
  (insert (format  "%s. " h-2-o-counter))
  (setq h-2-o-counter (1+ h-2-o-counter))
  (h-2-o-process-children parsed-html))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun h-2-o-html-2-org (parsed-html)
  "Writes a ORG version of the PARSED-HTML into the current buffer."
  (cond
   ((stringp parsed-html)
	(h-2-o-insert parsed-html))
   ((listp parsed-html)
	(let ((handler (assoc (car parsed-html) h-2-o-conversion-handlers)))
	  (if handler
		  (funcall (cdr handler) parsed-html)
		(insert (format "%s" parsed-html)))))
   (t (error "Unknow form %s" parsed-html))))



(defun h-2-o-string-to-parsed-html (string)
  (with-temp-buffer
	(insert string)
	(libxml-parse-html-region (point-min) (point-max))))

(defun h-2-o-insert-org-source-for-html (html-string)
  (h-2-o-html-2-org (h-2-o-string-to-parsed-html html-string)))


(provide 'html-2-org)
;;; html-2-org.el ends here
