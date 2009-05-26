;;; ### Plugin Initialization ###
(setq plugins-to-load '("harvey-navigation" "js2" "dsvn" "ruby"
                        "ido" "rhtml" "mozrepl"))

;; add to "~/.emacs.d/plugins/__plugins-to-load__ to load-path
(let* ((my-lisp-dir "~/.emacs.d/plugins")
       (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-to-load-path plugins-to-load))

;; run the init file for the plugin
(mapcar (lambda (plugin-name)
          (load (concat "load-" plugin-name ".el")))
        plugins-to-load)

;; ### General Setup ###
(menu-bar-mode -1)
(global-font-lock-mode t)
(set-background-color "black")
(set-foreground-color "white")

;; doing a replace followed by a search, uses the last search string
(setq query-replace-interactive t)

;; flymake colors
;;(set-face-background 'flymake-errline "red4")
;;(set-face-background 'flymake-warnline "dark slate blue")

;; default tab width to four spaces
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)

;; title bar shows name of current buffer ;;
(setq frame-title-format '("emacs: %*%+ %b"))

;; don't show the startup message every time, I know I'm using emacs
(setq inhibit-startup-message t)

;; Don't truncate, wrap, or fold lines of split windows
(setq truncate-partial-width-windows nil)
(setq truncate-lines nil)

;; use y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; default mode and fill
(setq default-major-mode 'text-mode)

;; http://thebogles.com/blog/2008/05/customizing-emacs-grep-find-to-ignore-subversion-files/
(custom-set-variables '(grep-find-command "find . -type f -not \
-name \"*.svn-base\" -and -not -name \"*.tmp\" -print0 | xargs -0 \
grep -n -s -F "))

;; http://www.emacsblog.org/2007/01/17/indent-whole-buffer/
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;; Aliases
(defalias 'qrr    'query-replace-regexp)
(defalias 'ta     'tags-apropos)
(defalias 'ts     'tags-search)
(defalias 'gf     'grep-find)

;;; ### Set up Keybindings ###
(global-set-key "\M-1" 'compile)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-i" 'indent-region)
(global-set-key "\M-s" 'isearch-forward-regexp)

;; Dired remove directories
(setq dired-recursive-deletes t)

;; (put 'upcase-region 'disabled nil)

;; fix annoying shell backspace problem
;; 0 for ibook, 1 for imac
(normal-erase-is-backspace-mode 1)

;; http://trey-jackson.blogspot.com/2008/01/emacs-tip-11-uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)   ; rename after killing
                                        ; uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special
                                        ; buffers

;; http://www.emacsblog.org/2007/02/08/quick-tip-dired-recursive-deletes/
(setq dired-recursive-deletes 'top)

;; http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/emacs/lisp/emacs-lisp/re-builder.el