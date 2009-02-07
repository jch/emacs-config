(add-to-list 'load-path "~/.emacs.d/plugins/nxml-mode-20041004")
(add-to-list 'load-path "~/.emacs.d/plugins/nxml-mode-20041004/util")

(add-to-list 'load-path "~/.emacs.d/plugins/rhtml")

(load (expand-file-name "~/.emacs.d/plugins/nxhtml/autostart.el"))

(setq
 nxhtml-global-minor-mode t
 mumamo-chunk-coloring 'submode-colored
 nxhtml-skip-welcome t
 indent-region-mode t
 rng-nxml-auto-validate-flag nil
 nxml-degraded t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo))


;; MuMaMo-Mode for rhtml files
(require 'mumamo-fun)
(setq mumamo-chunk-coloring 'submode-colored)
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))


;;; rhtml-mode
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))
