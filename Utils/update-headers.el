------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
XXXXX
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GARLIC is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

;;;
;;; $Id$
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
    (next-line 1)
    (if (re-search-forward "^----------" nil t)
	(progn
	  (next-line 1)
	  (beginning-of-line)
	  (delete-region (point-min) (point))))
    (goto-char (point-min))
    (if (re-search-forward "^package body \\(.*\\) is" nil t)
	(setq name (buffer-substring (match-beginning 1) (match-end 1)))
      (goto-char (point-min))
      (re-search-forward "^package \\(.*\\) is")
      (setq name (buffer-substring (match-beginning 1) (match-end 1)))
      (setq spec t))
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
    (insert (center-ada ""))
    (insert (center-ada "$Revision$"))
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

(defun center-ada (l)
  (let* ((tt 71)
	 (n (length l))
	 (s (/ (- tt n) 2)))
    (concat "-- " (spaces-ada s) l (spaces-ada (- tt (+ s n))) "  --\n")))

(defun spaces-ada (n)
  (if (<= n 0) ""
    (concat " " (spaces-ada (- n 1)))))

