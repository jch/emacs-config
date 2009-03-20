(require 'ruby-mode)

(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".gemspec$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rake$" . ruby-mode) auto-mode-alist))