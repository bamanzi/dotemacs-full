
;;** eproject (best? for file-based project)


;;** projectile (best for folder-based project)
(autoload 'projectile-mode  "projectile"
  "Minor mode to assist project management and navigation." t)
(autoload 'projectile-global-mode  "projectile"
  "Toggle Projectile mode in every possible buffer." t)

(idle-require 'projectile-ext)
