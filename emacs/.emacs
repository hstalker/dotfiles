;; simply defer actual config to XDG (newer versions of emacs don't need this)
(message "Loading actual configuration...")
(load-file 
  (expand-file-name 
    (substitute-env-vars 
      "${XDG_CONFIG_HOME}/emacs/init.el" 
      (lambda(_) (concat (file-name-as-directory (getenv "HOME")) ".config/emacs/init.el")))))

