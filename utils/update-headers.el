;;;
;;; $Id: //depot/adabroker/release-1/utils/update-headers.el#1 $
;;;
;;; This file contains the update-header command which can be used to
;;; update headers depending on the header.txt file in the current directory.
;;;
;;; XXXXX will be replaced by context-dependent information.
;;;

(defun update-header ()
  "Update headers according to header.txt."
  (interactive)
  (let (name spec)
    (goto-char (point-min))
    (if (re-search-forward "^--  $Id" nil t)
	(progn
	  (next-line 2)
	  (beginning-of-line)
	  (delete-region (point-min) (point))))
    (goto-char (point-min))
    (next-line 1)
    (if (re-search-forward "^----------" nil t)
	(progn
	  (next-line 1)
	  (beginning-of-line)
	  (delete-region (point-min) (point))))
    (goto-char (point-min))
    (if (re-search-forward "package body \\(.+\\) is" nil t)
	(setq name (buffer-substring (match-beginning 1) (match-end 1)))
      (goto-char (point-min))
      (if (re-search-forward "package \\(.+\\) \\(is\\|renames\\)" nil t)
	  (setq name (buffer-substring (match-beginning 1) (match-end 1))
		spec t)
	(goto-char (point-min))
	(if (re-search-forward "^procedure \\([^ ;]+\\)" nil t)
	    (setq name (buffer-substring (match-beginning 1) (match-end 1))
		  spec (string-match "ads" (buffer-name)))
	  (goto-char (point-min))
	  (if (re-search-forward "^function \\([^ ;]+\\)" nil t)
	      (setq name (buffer-substring (match-beginning 1) (match-end 1))
		    spec (string-match "ads" (buffer-name)))))))
    (goto-char (point-min))
    (insert-file "header.txt")
    (goto-char (point-min))
    (re-search-forward "^XXXXX" nil)
    (beginning-of-line)
    (let ((beg (point)))
      (next-line 1) (delete-region beg (point)))
    (insert (center-ada (upcase (expand-ada-name name))))
    (insert (center-ada ""))
    (insert (center-ada (if spec "S p e c" "B o d y")))
;    (insert (center-ada ""))
;    (insert (center-ada (concat "$" "Revision: 1.99 $") 'omit-terminator))
    (re-search-forward "----------")
    (next-line 1)
    (let ((beg (point)))
      (end-of-line)
      (if (not (equal (buffer-substring beg (point)) ""))
	  (progn
	    (beginning-of-line)
	    (insert "\n"))))))

(defun expand-ada-name (n)
  (if (<= (length n) 1) n
    (concat (substring n 0 1) " " (expand-ada-name (substring n 1)))))

(defun center-ada (l &optional omit-terminator)
  (let* ((tt 71)
	 (n (length l))
	 (s (/ (- tt n) 2)))
    (concat "-- " (spaces-ada s) l
	    (if omit-terminator ""
	      (concat (spaces-ada (- tt (+ s n))) "  --"))
	    "\n")))

(defun spaces-ada (n)
  (if (<= n 0) ""
    (concat " " (spaces-ada (- n 1)))))

(defun update-headers ()
  "Update headers of files given on the command line"
  (interactive)
  (let ((l (directory-files "." nil "\\.ad[bs]\\(\\.in\\|\\)$" nil t)))
    (while l
      (let ((current (car l)))
	(message "Updating %s..." current)
	(find-file current)
	(update-header)
	(write-file current)
	(message "Updating %s... done" current)
	(setq l (cdr l))))))

