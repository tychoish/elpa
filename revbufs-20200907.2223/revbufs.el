;;; revbufs.el --- Reverts all out-of-date buffers safely

;; Author:     Neil Van Dyke <neil@neilvandyke.org>
;; Maintainer: Sam Kleinman <sam@tychoish.com>
;; Version:    1.2
;; Package-Version: 20200907.2223
;; Package-Commit: df3c02d3063951582c693ae12547993cec8256e2
;; URL:        http://www.neilvandyke.org/revbufs/
;; Keywords:   convenience buffers

;; Copyright (C) 1997-1999,2002,2007 Neil W. Van Dyke.  This is free software;
;; you can redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either version
;; 2, or (at your option) any later version.  This is distributed in the hope
;; that it will be useful, but without any warranty; without even the implied
;; warranty of merchantability or fitness for a particular purpose.  See the
;; GNU General Public License for more details.  You should have received a
;; copy of the GNU General Public License along with Emacs; see the file
;; `COPYING'.  If not, write to the Free Software Foundation, Inc., 59 Temple
;; Place, Suite 330, Boston, MA 02111-1307, USA.")

;;; Commentary:

;; `revbufs' reverts Emacs buffers that are visiting files that have been
;; modified outside Emacs' control.  This is useful for files generated by a
;; compiler, or log files.  `revbufs' won't revert a buffer that has been
;; modified (what it calls "conflicts"), and will tell you if any files
;; disappeared out from under your buffers ("orphans").

;;; Change Log:

;; [Version 1.3, 2020-09-07, sam@tychoish.com], New maintainer, minor
;; fixes, documentation, and added to MELPA.
;;
;; [Version 1.2, 2007-03-01, neil@neilvandyke.org] Added missing `provide'.
;;
;; [Version 1.1, 15-Oct-2002] Updated email address, URL, comments.
;;
;; [Version 1.0, 04-Sep-1999] Initial release.

;;; Code:
;;;###autoload
(defun revbufs ()
  "Examines all open buffers and reverts any that are out of date.
If there are orphans or out of date buffers, this operation opens
the `*revbufs*' buffer reporting the outcome."
  (interactive)
  (let ((conflicts  '())
	(orphans    '())
	(reverts    '())
	(report-buf (get-buffer-create "*revbufs*")))

    ;; Process the buffers.
    (mapc (function
	   (lambda (buf)
	     (let ((file-name (buffer-file-name buf)))
	       (cond
		;; If buf is the report buf, ignore it.
		((eq buf report-buf) nil)
		;; If buf is not a file buf, ignore it.
		((not file-name) nil)
		;; If buf file doesn't exist, buf is an orphan.
		((not (file-exists-p file-name))
		 (setq orphans (nconc orphans (list buf))))
		;; If file modified since buf visit, buf is either a conflict
		;; (if it's modified) or we should revert it.
		((not (verify-visited-file-modtime buf))
		 (if (buffer-modified-p buf)
		     (setq conflicts (nconc conflicts (list buf)))
		   (with-current-buffer buf
		     (revert-buffer t t))
		   (setq reverts (nconc reverts (list buf)))))))))
	  (copy-sequence (buffer-list)))

    ;; Prepare the report buffer.
    (with-current-buffer report-buf
      (let ((buffer-read-only nil))
	(delete-region (point-min) (point-max))
	(insert (revbufs-format-list conflicts "[ CONFLICTS ]")
		(revbufs-format-list orphans   "[ ORPHANS ]")
		(revbufs-format-list reverts   "[ REVERTS ]")))
      (revbufs-mode))

    ;; Print message in echo area.
    (if (or conflicts orphans)
	(progn
	  (pop-to-buffer report-buf)
	  (message
	   (concat
	    (format "Reverted %s with"
		    (revbufs-quantity (length reverts) "buffer"))
	    (if conflicts
		(format " %s%s"
			(revbufs-quantity (length conflicts) "conflict")
			(if orphans " and" "")))
	    (if orphans
		(format " %s"
			(revbufs-quantity (length orphans) "orphan"))))))
      (if reverts
	  (message "Reverted %s." (revbufs-quantity (length reverts) "buffer"))
	(message "No buffers need reverting.")))))

(defvar revbufs-marked '()  "List of marked buffers in the *revbufs* buffer.")

(define-derived-mode revbufs-mode text-mode "revbufs"
  "Major mode for the `revbufs' reversion report."
  (setq buffer-read-only t))

(defun revbufs-buffer-at-point ()
  "Used in revbufs report buffer to get the buffer in a report."
  (plist-get (text-properties-at (point)) 'revbufs-buffer))

(defun revbufs-find-other-window (&optional event)
  "Operation available within revbuf to view a listed buffer.
Optional EVENT argument is ignored."
  (interactive)
  (let ((buffer (revbufs-buffer-at-point)))
    (if buffer
	(pop-to-buffer buffer)
      (message "Must be on top of a *revbufs* line"))))

(defun revbufs-kill-buffer-at-point (&optional event)
  "Operation available within revbuf report buffer to kill a buffer.
Optional EVENT argument is ignored."
  (interactive)
  (let ((buffer (revbufs-buffer-at-point))
	(buffer-read-only nil))
    (cond (buffer
	   (kill-buffer buffer)
	   (kill-whole-line))
	  (t
	   (message "Must be on top of a *revbufs* line")))))

(defun revbufs-format-list (list label)
  "Produces the output for the LABEL given the LIST, used in the output buffer."
  (if list
      (concat label
	      (format ":\n")
	      (mapconcat
	       (function
		(lambda (buf)
		  (propertize
		   (format "  %s %s %s\n"
			   (propertize (buffer-name buf)
				       'mouse-face 'highlight
				       'help-echo "mouse-1, RET: find buffer in other window"
				       'keymap (let ((map (make-sparse-keymap)))
						 (define-key map (kbd "RET") 'revbufs-find-other-window)
						 (define-key map (kbd "C-k") 'revbufs-kill-buffer-at-point)
						 (define-key map [mouse-1] 'revbufs-find-other-window)
						 map))
			   (make-string (- 60 (length (buffer-name buf))) ? )
			   (buffer-file-name buf))
		   'revbufs-buffer buf)))
	       list
	       ""))
    ""))

(defun revbufs-quantity (num what)
  "Format the output, reporting the NUM of WHAT (buffer class) reverted."
  (format "%d %s%s" num what (if (= num 1) "" "s")))

(provide 'revbufs)
;;; revbufs.el ends here
