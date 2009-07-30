;;; ### Plugin Initialization ###
(setq plugins-to-load '("harvey-navigation" "dsvn" "ruby"
                        "ido" "rhtml" "keyolution" "browse-kill-ring"))

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

;; http://curiousprogrammer.wordpress.com/2009/05/19/customiz-dabbrev/
(setq dabbrev-abbrev-skip-leading-regexp "[^ ]*[<>=*]")

;; flymake colors
;;(set-face-background 'flymake-errline "red4")
;;(set-face-background 'flymake-warnline "dark slate blue")

;; default tab width to two spaces
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
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
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(grep-find-command "find . -type f -not -name \"*.svn*\" -and -not -name \"all-wcprops\" -and -not -name \"*.tmp\" -and -not -name \"*~\" -and -not -name \"TAGS\" -print0 | xargs -0 grep -n -s -F ")
 '(gud-gdb-command-name "gdb --annotate=1")
 '(js2-auto-indent-flag nil)
 '(large-file-warning-threshold nil)
 '(tags-revert-without-query t))

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
(global-set-key "\M-c" 'comment-or-uncomment-region)
(global-set-key "\C-ck" 'browse-kill-ring)

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
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;; fuck-wordpress: start in index.html buffer, have 2nd buffer open that's from firefox
(fset 'fuck-wordpress
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217788 14 14 14 14 14 14 14 14 14 14 67108896 134217790 16 16 5 134217847 24 111 134217788 5 return 25 67108896 134217790 23] 0 "%d")) arg)))

(fset 'restart-passenger
   "\C-[!touch ~/projects/coupa_enterprise/trunk/tmp/restart.txt\C-m")
(global-set-key "\M-9" 'restart-passenger)

(defun coupa-svn-st ()
  "Run 'svn status' on coupa enterprise trunk"
  (interactive)
  (svn-check-running)
  (svn-status "~/projects/coupa_enterprise/trunk"))
(global-set-key "\M-8" 'coupa-svn-st)