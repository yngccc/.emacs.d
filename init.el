(setq warning-minimum-level :emergency)

(require 'package)
(add-to-list 'package-archives (cons "melpa" "http://melpa.org/packages/") t)
(package-initialize)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)
(set-default 'truncate-lines t)
(setq-default tab-width 2)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
			backup-by-copying t
			version-control t
			delete-old-versions t
			kept-new-versions 20
			kept-old-versions 5
			)

(prefer-coding-system 'utf-8)

(setq create-lockfiles nil)
(global-auto-revert-mode)
(setq inhibit-startup-screen t)
(desktop-save-mode)
;; (show-paren-mode)
;; (global-hl-line-mode)

(cua-mode)
(setq cua-prefix-override-inhibit-delay 0.001)
(global-set-key (kbd "C-S-v") 'yank-pop)
(define-key cua-global-keymap (kbd "C-<return>") nil)

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(add-hook 'ido-setup-hook (lambda ()
	(define-key ido-completion-map (kbd "C-s") 'ido-next-match)
	(define-key ido-completion-map (kbd "C-w") 'ido-prev-match)
	(define-key ido-completion-map (kbd "C-v") 'yank)
	(define-key ido-completion-map (kbd "C-k") 'kill-line)
	(define-key ido-completion-map (kbd "C-d") 'forward-word)))
(global-set-key (kbd "C-t") 'ido-find-file)
(when (require 'flx-ido nil 'noerror)
	(flx-ido-mode 1)
	(setq ido-use-faces nil))
(when (require 'ido-vertical-mode nil 'noerror)
	(ido-vertical-mode))
(when (require 'idomenu nil 'noerror)
	(global-set-key (kbd "C-y") (lambda () (interactive) (imenu--menubar-select imenu--rescan-item) (idomenu))))

(defun flatten-imenu-index-alist-class-bucket (alist)
	(let ((class-str (car (car alist)))
				(class (cdr (car alist))))
		(if (and (stringp class-str)
						 (string= class-str "Class"))
				(append class (cdr alist))
			alist)))

(add-function :filter-return imenu-create-index-function 'flatten-imenu-index-alist-class-bucket)

(add-hook 'dired-load-hook (lambda ()		
	(define-key dired-mode-map (kbd "C-o") nil)		
	(define-key dired-mode-map (kbd "C-w") 'dired-previous-line)		
	(define-key dired-mode-map (kbd "C-s") 'dired-next-line)))
(global-set-key (kbd "C-x C-t") 'dired)
(put 'dired-find-alternate-file 'disabled nil)

(global-set-key (kbd "C-w") 'previous-line)
(global-set-key (kbd "C-s") 'next-line)
(global-set-key (kbd "C-a") 'backward-word)
(global-set-key (kbd "C-d") 'forward-word)
(global-set-key (kbd "C-S-w") (lambda () (interactive) (previous-line 6)))
(global-set-key (kbd "C-S-s") (lambda () (interactive) (next-line 6)))
(global-set-key (kbd "M-w") (lambda () (interactive) (previous-line 6)))
(global-set-key (kbd "M-s") (lambda () (interactive) (next-line 6)))
(global-set-key (kbd "C-S-a") 'backward-char)
(global-set-key (kbd "C-S-d") 'forward-char)
(global-set-key (kbd "M-a") 'backward-char)
(global-set-key (kbd "M-d") 'forward-char)

(global-set-key (kbd "C-q") (lambda () (interactive) (if (bolp) (back-to-indentation) (beginning-of-line))))
(global-set-key (kbd "C-e") 'end-of-line)
(global-set-key (kbd "C-S-k") (lambda () (interactive) (kill-line 0)))

(global-set-key (kbd "C-S-q") 'beginning-of-buffer)
(global-set-key (kbd "C-S-e") 'end-of-buffer)
(global-set-key (kbd "M-q") 'beginning-of-buffer)
(global-set-key (kbd "M-e") 'end-of-buffer)

(global-set-key (kbd "C-S-l") 'goto-line)

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-S-o") 'delete-window)
(global-set-key (kbd "C-8") 'split-window-vertically)
(global-set-key (kbd "C-9") 'split-window-horizontally)

(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "C-x C-b") 'list-buffers)
(global-set-key (kbd "C-S-b") (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key (kbd "C-;") 'switch-to-prev-buffer)
(global-set-key (kbd "C-'") 'switch-to-next-buffer)

(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-r") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-d") 'isearch-yank-word-or-char)
(define-key isearch-mode-map (kbd "C-a") 'isearch-del-char)
(define-key isearch-mode-map (kbd "C-e") 'isearch-yank-line)
(define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
(global-set-key (kbd "C-S-f") 'isearch-forward-regexp)
(global-set-key (kbd "C-S-r") 'isearch-backward-regexp)

(global-set-key (kbd "C-/") 'comment-dwim)
(global-set-key (kbd "C-<return>") 'dabbrev-expand)
(global-set-key (kbd "C-.") 'xref-find-definitions-other-window)
(global-set-key (kbd "C->") 'xref-find-definitions)
(global-set-key (kbd "C-x C-.") 'xref-find-references)
(global-set-key (kbd "C-,") 'xref-pop-marker-stack)
(add-hook 'xref--xref-buffer-mode-hook (lambda ()
	(define-key xref--xref-buffer-mode-map (kbd "C-o") 'other-window)
	(define-key xref--xref-buffer-mode-map (kbd "<tab>") 'xref-next-line)))

(global-set-key (kbd "C-n") 'save-buffer)
(global-set-key (kbd "C-1") 'eval-expression)
(global-set-key (kbd "C-!") 'shell-command)

(require 'shell)
(define-key shell-mode-map (kbd "C-:") 'comint-previous-input)
(define-key shell-mode-map (kbd "C-\"") 'comint-next-input)

(when (eq system-type 'windows-nt)
	(grep-compute-defaults)
	(grep-apply-setting 'grep-command "findstr /s /n /i ")
	(defun grep-w32-headers ()
		(interactive)
		(let ((default-directory "C:\\Program Files (x86)\\Windows Kits\\8.1\\Include\\um"))
			(call-interactively 'grep))))
(add-hook 'grep-mode-hook (lambda () (define-key grep-mode-map (kbd "C-o") 'other-window)))

(define-key minibuffer-local-map (kbd "C-w") 'previous-history-element)
(define-key minibuffer-local-map (kbd "C-s") 'next-history-element)
(define-key minibuffer-local-map (kbd "C-v") 'yank)
(define-key minibuffer-local-map (kbd "C-s") nil)
(define-key minibuffer-local-map (kbd "C-r") nil)
(define-key minibuffer-local-completion-map (kbd "C-v") 'yank)

(define-key Buffer-menu-mode-map (kbd "C-s") 'next-line)
(define-key Buffer-menu-mode-map (kbd "C-w") 'previous-line)
(define-key Buffer-menu-mode-map (kbd "C-o") 'other-window)

(global-set-key (kbd "C-P") 'compile)
(add-to-list 'compilation-error-regexp-alist 'clang-cl)
(add-to-list 'compilation-error-regexp-alist-alist '(clang-cl "\\(.+?\\)(\\([0-9]+\\),\\([0-9]+\\)):" 1 2 3))
(add-hook 'compilation-mode-hook (lambda ()
	(define-key compilation-mode-map (kbd "C-s") 'next-line)
	(define-key compilation-mode-map (kbd "C-w") 'previous-line)
	(define-key compilation-mode-map (kbd "C-p") nil)
	(define-key compilation-mode-map (kbd "C-o") nil)))

(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'html-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-j") 'hs-toggle-hiding)
(global-set-key (kbd "C-S-j") 'hs-hide-level)

(require 'cc-mode)
(define-key c++-mode-map (kbd "C-d") nil)
(setq-default c-basic-offset 2)
(c-set-offset 'brace-list-intro '+)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook
					(lambda ()
						(hs-minor-mode)
						(subword-mode)))

(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                     ;;   and with unclosed brace.
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0                           ; no additional indent
          ad-do-it)))                   ; default behavior

(setq js-indent-level 2)

(when (require 'company nil 'noerror)
	(global-company-mode)
	(define-key company-active-map (kbd "<tab>") 'company-complete)
	(define-key company-active-map (kbd "C-s") 'company-select-next)
	(define-key company-active-map (kbd "C-w") 'company-select-previous)
	(define-key company-active-map (kbd "C-S-s") 'company-select-next)
	(define-key company-active-map (kbd "C-S-w") 'company-select-previous)
	(define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
	(define-key company-active-map (kbd "C-f") 'company-search-candidates)
	(define-key company-search-map (kbd "C-f") 'company-search-repeat-forward)
	(define-key company-search-map (kbd "C-r") 'company-search-repeat-backward)
	(define-key company-search-map (kbd "C-g") 'company-search-abort))

(when (require 'multiple-cursors nil 'noerror)
	(global-set-key (kbd "C-=") 'mc/mark-all-like-this)
	(global-set-key (kbd "C--") 'mc/mark-next-like-this)
	(global-set-key (kbd "C-_") 'mc/skip-to-next-like-this))

(when (require 'yasnippet nil 'noerror)
	(yas-global-mode t)
	(global-set-key (kbd "C-<tab>") 'yas-expand))

(when (require 'magit nil 'noerror)
	(remove-hook 'server-switch-hook 'magit-commit-diff)
	(add-hook 'magit-mode-hook (lambda ()
		(define-key magit-mode-map  (kbd "C-w") 'previous-line)
		(define-key magit-mode-map  (kbd "C-s") 'next-line)
	  (define-key magit-status-mode-map (kbd "C-w") 'previous-line)
		(define-key magit-status-mode-map (kbd "C-s") 'next-line)
		(define-key magit-status-mode-map (kbd "C-SPC") 'set-mark-command)
		(define-key magit-log-mode-map (kbd "C-w") 'previous_line)
	  (define-key magit-log-mode-map (kbd "C-s") 'next-line))))

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook
					(lambda ()
						(hs-minor-mode)
						(subword-mode)
						(local-set-key (kbd "C-.") 'godef-jump)
						(local-set-key (kbd "C->") 'godef-jump-other-window)))

(when (require 'company-go nil 'noerror)
	(add-hook 'go-mode-hook
						(lambda ()
							(set (make-local-variable 'company-backends) '(company-go))
							(company-mode))))

; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)) (add-hook
; 'web-mode-hook (lambda () (setq web-mode-markup-indent-offset 2)
; (setq web-mode-css-indent-offset 2) (setq
; web-mode-code-indent-offset 2) (define-key web-mode-map (kbd "C-j")
; 'web-mode-fold-or-unfold)))

(when (require 'google-this nil 'noerror))

(when (require 'projectile nil 'noerror)
	(projectile-mode)
	(global-set-key (kbd "C-S-t") 'projectile-find-file))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "70cfdd2e7beaf492d84dfd5f1955ca358afb0a279df6bd03240c2ce74a578e9e" default))
 '(menu-bar-mode nil)
 '(package-selected-packages
	 '(rust-mode idomenu ido-vertical-mode flx-ido yasnippet multiple-cursors expand-region magit flycheck projectile company go-mode company-go google-this web-mode solarized-theme zenburn-theme))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 108 :width normal)))))
 
