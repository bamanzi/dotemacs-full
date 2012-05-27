
;;** eproject (best? for file-based project)


;;** projectile (best for folder-based project)
(idle-require 'projectile)
(eval-after-load "projectile"
  `(progn
     (or (require 'anything-projectile nil t)  ;; https://github.com/bbatsov/projectile/commit/cdcf5afc65dc916a836010f0b41613f3b1e264fc
         (require 'helm-projectile nil t))
     
     ;;(projectile-global-mode t)
     
     (easy-menu-define projectile-mode-menu projectile-mode-map
       "Menu for Projectile mode"
       '("Projectile"
         ["Find file" (if (fboundp 'projectile-find-file)
                          (projectile-find-file)
                        (projectile-jump-to-project-file))]
         ["Switch buffer" projectile-switch-to-buffer]
         "--"
         ["Find in project" (if (fboundp 'projectile-grep)
                                (projectile-grep)
                              (projectile-grep-in-project))]
         ["Replace in project" (if (fboundp 'projectile-replace)
                                   (projectile-replace)
                                 (projectile-replace-in-project))]
         ["Multi-occur in project" projectile-multi-occur]
         "--"
         ["Invalidate cache" (if (fboundp 'projectile-invalidate-cache)
                                 (projectile-invalidate-cache)
                               (projectile-invalidate-project-cache))]
         ["Regenerate etags" projectile-regenerate-tags]
         ["Kill all buffers"  projectile-kill-buffers]
         "--"
         ["Eshell cd to project root" projectile-eshell-cd-root]
         ["Eshell cd to current folder" projectile-eshell-cd-current]
         "--"
         ["Show project info" projectile-show-project-info]))

     ))

;; (defun eepy-open-project (dir)
;;   (interactive "Dproject root dir:")
;;   (let ((prj-file (concat dir ".projectile")))
;; 	(if (file-exists-p prj-file)
;; 		(

(defun projectile-create-project (dir)
  "Create a projectile project (actually create an empty file named .projectile)."
  (interactive "Dproject root dir:")
  (let ((prj-file (concat dir ".projectile")))
    (unless (file-exists-p prj-file)
      (with-temp-buffer
        (insert ";; This file is meant for projectile project.")
        (write-file prj-file))))
  (if (string-prefix-p dir (buffer-file-name))
      (projectile-on)))

(defun projectile-show-project-info ()
  "Show project info of current project."
  (interactive)
  (if projectile-mode
      (message "Project: %s. Root dir: %s. Type: %s"
               (propertize (projectile-get-project-name) :bold t)
               (projectile-get-project-root)
               (loop for file in projectile-project-root-files
                     when (locate-dominating-file default-directory file)
                     do (return file)))
    (message "projectile-mode not turned on.")))


(defun projectile-eshell-cd (dir)
  "If there is an EShell buffer, cd to DIR in that buffer."
  (interactive "D")
  (let* ((eshell-buf-p (lambda (buf)
                         (with-current-buffer buf (eq major-mode 'eshell-mode))))
         (eshell-win-p (lambda (win)
                         (let ((buf (window-buffer win)))
                           (with-current-buffer buf (eq major-mode 'eshell-mode)))))
         (eshell-win (find-if eshell-win-p (window-list)))
         (eshell-buf (find-if eshell-buf-p (buffer-list))))
    (if eshell-win
        (setq eshell-buf (window-buffer eshell-win)))
    (unless eshell-buf
      (eshell)
      (setq eshell-buf (current-buffer)))
    (with-current-buffer eshell-buf
      (goto-char (point-max))
      (eshell/cd dir)
      (eshell-send-input nil t)
      eshell-buf ;; returns eshell-buf so you can focus
      ; the window if you want
      )
    (if eshell-win
        (select-window eshell-win)
      (switch-to-buffer eshell-buf))))

(defun projectile-eshell-cd-root ()
  (interactive)
  (projectile-eshell-cd (projectile-get-project-root)))

(defun projectile-eshell-cd-current ()
  (interactive)
  (projectile-eshell-cd default-directory))
