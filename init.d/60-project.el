
;;** eproject (best? for file-based project)


;;** projectile (best for folder-based project)
(autoload 'projectile-mode  "projectile"
  "Minor mode to assist project management and navigation." t)
(autoload 'projectile-global-mode  "projectile"
  "Toggle Projectile mode in every possible buffer." t)

(setq projectile-keymap-prefix (kbd "<M-f12>"))

(idle-require 'projectile-ext)


(eval-after-load "projectile"
  `(progn
     (require 'anything-projectile)

     (delete ".projectile" projectile-project-root-files)
     (add-to-list 'projectile-project-root-files ".projectile")     
;;     (require 'projectile-ext)
     (define-key projectile-mode-map (kbd "<M-f12> C-f") 'projectile-find-file-)
     (define-key projectile-mode-map (kbd "<M-f12> d") 'projectile-dired)     
     (define-key projectile-mode-map (kbd "<M-f12> E") 'projectile-eshell-cd-current)
     (define-key projectile-mode-map (kbd "<M-f12> E") 'projectile-eshell-cd-root)
     (define-key projectile-mode-map (kbd "<M-f12> G") 'projectile-grin)
     (define-key projectile-mode-map (kbd "<M-f12> a") 'projectile-ack)
     (define-key projectile-mode-map (kbd "<M-f12> A") 'projectile-ack-find-file)  
     ))
