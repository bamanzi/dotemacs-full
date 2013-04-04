;;; comint-toggle.el -- switch for editing buffer to corresponding comint buffer & back

;; Copyright (C) 2013 Ba Manzi <bamanzi@gmail.com>

;; Author: Ba Manzi
;; Keywords: buffer comint 
;; URL: https://bitbucket.org/bamanzi/dotemacs-full/src/default/lisp/
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;;; Comment

;; This package provides a command to switch from editing buffer to
;; corresponding comint buffer (or other utility buffer), similar to
;; what`eshell-toggle' in esh-toggle.el does.

;;
;; How to use:
;; 1. bind `comint-toggle' to some key
;; 2. customize `comint-toggle-buffer-pairs' if needed
;; 3. invoke `comint-togle'
;;
;; Refer command `comint-toggle' for detail info.

;;; Code

(defcustom comint-toggle-buffer-pairs
       '((emacs-lisp-mode "*scratch*" recreate-scratch-buffer)
         (python-mode inferior-python-mode run-python))
       "Configurations used in `comint-toggle'.

It is used to find target comint buffer to editing buffer, or vice versa.

The format of each item in this list:
- MAJOR-MODE of edit buffer.
- MAJOR-MODE of buffer name of comint buffer (not necessarily a comint buffer,
  other utility buffer would be ok, e.g scratch)
- COMMAND to create new comint buffer if not existing found.")
       

(defvar comint-toggle-restore-data nil
  "Data used in `comint-toggle' when return from comint buffer to editing buffer.

The format: a list, in which each item is a list like this:
  (major-mode restore-data last-window-conf)
LAST-BUFFER is the buffer before switching to comint buffer.
LAST-WINDOW_CONF is the window configuration before switching to comint buffer.")


(defun comint-toggle-goto-comint (mode newcmd)
  (let ((comint-buffer  (if (symbolp mode)
                            (delq nil (mapcar #'(lambda (buffer)
                                                  (if (eq (with-current-buffer buffer
                                                            major-mode) mode)
                                                      buffer))
                                              (buffer-list)))
                          (list (get-buffer mode)))))
    (if comint-buffer
        (progn
          (message "Switching from %s to %s" major-mode mode)
          (switch-to-buffer-other-window (car comint-buffer)))
      (message "Create new comint buffer for %s" major-mode)
      (call-interactively newcmd))))


;;;###autoload
(defun comint-toggle ()
  "Switch to comint buffer from editing buffer or switch back.

If current buffer is an editing buffer (according to
`comint-toggle-buffer-pairs'), it would switch to corresponding
comint buffer (if no existing instance found, it would create a
new one.

If current buffer is a comint buffer, it would switch back to the
last editing buffer."
  (interactive)
  (let ((cfg (assq major-mode comint-toggle-buffer-pairs))
        (restore-data-entry (assq major-mode comint-toggle-restore-data))
        (restore-data (list (current-buffer) (current-window-configuration)))
        (this-major  major-mode)
        (this-buffer (current-buffer)))
    (if cfg
        (progn
          ;; from major buffer to comint buffer
          (apply 'comint-toggle-goto-comint (cdr cfg))
          (if restore-data-entry
              (setcdr restore-data-entry restore-data)
            (add-to-list 'comint-toggle-restore-data (cons this-major restore-data))))
      ;;check if this is a comint buffer (according to `comint-toggle-buffer-pairs')
      (let ((cfg
             (delq nil (mapcar #'(lambda (item)
                                   (let ((comint-buffer (cadr item)))
                                     (if (if (symbolp comint-buffer)
                                             (eq comint-buffer major-mode)
                                           (string= comint-buffer (buffer-name)))
                                         item)))
                               comint-toggle-buffer-pairs))))
        (when cfg
          ;; configuration exists, this is a suported comint buffer
          ;; switch to last major buffer
          (let* ((target-major-mode (caar cfg))
                 (restore-data-entry (assq target-major-mode comint-toggle-restore-data)))
            (message "Returning from %s to %s" major-mode target-major-mode)
            (if (and (eq last-command 'comint-toggle)
                     (> (count-windows) 1))
                  ;;double invoke would maximize the comint buffer window
                (delete-other-windows)
              (when restore-data-entry
                  ;;(cdr restore-data-entry) => (last-buffer window-conf)
                  ;; return from comint buffer to major-buffer
                  ;;(switch-to-buffer-other-window (cadr restore-data-entry)))))))))
                  (set-window-configuration (nth 2 restore-data-entry))))))))))

(define-key global-map (kbd "C-`") 'comint-toggle)

(provide 'comint-toggle)
