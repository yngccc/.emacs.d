;; packages
(package-initialize)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, begin installation if it’s not"
  (mapcar (lambda (package)
	    (unless (package-installed-p package)
	      (package-install package)))
	  packages))

;; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir) (package-refresh-contents))

(ensure-package-installed 'company
			  'cmake-mode
			  'exec-path-from-shell
			  'flx 'flx-ido
			  'flycheck
			  'geiser
			  'magit
			  'google-this
			  'maxframe
			  'multiple-cursors
			  'pretty-lambdada
			  'projectile
			  'slime
			  'zenburn-theme
			  'solarized-theme
			  'color-theme-sanityinc-tomorrow
			  'yasnippet
			  'glsl-mode
			  'haskell-mode
			  'swift-mode
			  'go-mode
			  'company-go)
(package-initialize)
			  
;; company
(global-company-mode)
(setq company-selection-wrap-around t)

(define-key company-mode-map (kbd "M-m M-c") 'company-manual-begin)

(define-key company-active-map (kbd "<tab>") 'company-complete)
(define-key company-active-map (kbd "M-s") 'company-select-next)
(define-key company-active-map (kbd "M-w") 'company-select-previous)
(define-key company-active-map (kbd "M-S") 'company-select-next)
(define-key company-active-map (kbd "M-W") 'company-select-previous)

(define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)

(define-key company-active-map (kbd "M-f") 'company-search-candidates)
(define-key company-search-map (kbd "M-f") 'company-search-repeat-forward)
(define-key company-search-map (kbd "M-r") 'company-search-repeat-backward)
(define-key company-search-map (kbd "M-g") 'company-search-abort)

;; paths
(add-to-list 'exec-path "/usr/local/bin")
(exec-path-from-shell-initialize)

;; pretty-lambda
(defadvice pretty-lambda-mode (around enable-pretty-lambda-for)
  (when (memq major-mode
	      (list 'lisp-mode 'emacs-lisp-mode 'scheme-mode))
    ad-do-it))
(ad-activate 'pretty-lambda-mode)
(global-pretty-lambda-mode)

;; maximize frame
(maximize-frame)
(defadvice make-frame (around maximize-make-frame)
  ad-do-it
  (maximize-frame))
(ad-activate 'make-frame)

;; flex ido
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; projectile
(projectile-global-mode)
(define-key projectile-mode-map (kbd "A-t") 'projectile-find-file)
(define-key projectile-mode-map (kbd "M-h") 'projectile-find-other-file)
(define-key projectile-mode-map (kbd "A-h") 'projectile-find-other-file-other-window)

(push '("vs" . ("fs")) projectile-other-file-alist)
(push '("fs" . ("vs")) projectile-other-file-alist)

;; multiple cursors
(require 'multiple-cursors)
(define-key mc/keymap (kbd "M-g") 'mc/keyboard-quit)
(global-set-key (kbd "M-\\") 'mc/mark-all-like-this)
(global-set-key (kbd "A-\\") 'mc/edit-lines)

(global-set-key (kbd "M-\[") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-\]") 'mc/mark-next-like-this)

(global-set-key (kbd "M-\{") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "M-\}") 'mc/skip-to-next-like-this)

(global-set-key (kbd "A-\[") 'mc/unmark-previous-like-this)
(global-set-key (kbd "A-\]") 'mc/unmark-next-like-this)

;; yasnippet
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-global-mode t)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "A-<tab>") 'yas-expand)

(setq ac-source-yasnippet nil)

;; flycheck
(global-flycheck-mode t)
(global-set-key (kbd "A-f") 'flycheck-buffer)
(global-set-key (kbd "M-S") 'flycheck-next-error)
(global-set-key (kbd "M-W") 'flycheck-previous-error)

(add-hook 'c++-mode-hook (lambda ()
			   (setq flycheck-clang-language-standard "c++1y")
			   (setq flycheck-clang-include-path '("/usr/include/freetype2" "/usr/local/include/freetype2"))))

(add-hook 'c-mode-hook (lambda () 
			 (setq flycheck-clang-language-standard "c11")
			 (setq flycheck-clang-include-path '("/usr/include/freetype2" "/usr/local/include/freetype2"))))

;; magit
(global-set-key (kbd "A-i") 'magit-status)
(add-hook 'magit-mode-hook (lambda ()
	  (local-set-key (kbd "M-w") (kbd "C-p"))
	  (local-set-key (kbd "M-s") (kbd "C-n"))
	  (local-set-key (kbd "M-g") (kbd "C-g"))))

;; slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(add-hook 'slime-lisp-mode-hook
	  (lambda ()
	    (define-key slime-editing-map (kbd "M-m M-b") 'slime-eval-buffer)
	    (define-key slime-editing-map (kbd "M-m M-e") 'slime-eval-last-expression)
	    (define-key slime-editing-map (kbd "M-m M-r") 'slime-eval-region)
	    (define-key slime-editing-map (kbd "M-m M-d") 'slime-describe-symbol)))

;; geiser
(add-hook 'scheme-mode-hook
	  (lambda ()
	    (geiser-mode t)
	    (define-key geiser-mode-map (kbd "M-m M-e") 'geiser-eval-last-sexp)
	    (define-key geiser-mode-map (kbd "M-m M-b") 'geiser-eval-buffer)
	    (define-key geiser-mode-map (kbd "M-m M-r") 'geiser-eval-region)
	    (define-key geiser-mode-map (kbd "M-m M-d") 'geiser-doc-symbol-at-point)))

;; haskell
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (capitalized-words-mode)
	    (turn-on-haskell-doc)
	    (turn-on-haskell-indent)
	    (local-set-key (kbd "M-m M-l") 'inferior-haskell-load-file)
	    (local-set-key (kbd "M-m M-r") 'inferior-haskell-load-and-run)
	    (local-set-key (kbd "M-m M-i") 'inferior-haskell-info)
	    (local-set-key (kbd "M-m M-t") 'inferior-haskell-type)
	    (local-set-key (kbd "M-m M-d") 'inferior-haskell-find-definition)))

(add-hook 'inferior-haskell-mode-hook
	  (lambda ()
	    (turn-on-ghci-completion)
	    (local-set-key (kbd "M-r") 'isearch-backward)))

;; rtags
(add-to-list 'load-path "~/lib/rtags/src")
(require 'rtags)
(setq rtags-path "~/lib/rtags")
(setq rtags-jump-to-first-match nil)

(defun rtags-handle-results-buffer-other-window (&optional noautojump)
  (setq rtags-last-request-not-indexed nil)
  (rtags-reset-bookmarks)
  (cond ((= (point-min) (point-max))
         (message "RTags: No results") nil)
        ((= (count-lines (point-min) (point-max)) 1)
         (let ((string (buffer-string)))
           (if (rtags-not-indexed/connected-message-p string)
               (progn
                 (setq rtags-last-request-not-indexed t)
                 nil)
             (bury-buffer)
             (rtags-goto-location string nil t))))
        (t
         (switch-to-buffer-other-window rtags-buffer-name)
         (shrink-window-if-larger-than-buffer)
         (goto-char (point-max))
         (if (= (point-at-bol) (point-max))
             (delete-char -1))
         (rtags-init-bookmarks)
         (rtags-mode)
         (when (and rtags-jump-to-first-match (not noautojump))
           (rtags-select-other-window)))))

(defun rtags-find-symbol-at-point-other-window (&optional prefix)
  (interactive "P")
  (rtags-location-stack-push)
  (let ((arg (rtags-current-location))
        (fn (buffer-file-name)))
    (rtags-reparse-file-if-needed)
    (with-current-buffer (rtags-get-buffer)
      (rtags-call-rc :path fn :path-filter prefix "-f" arg)
      (rtags-handle-results-buffer-other-window))))

(define-key c-mode-map (kbd "M-.") 'rtags-find-symbol-at-point-other-window)
(define-key c-mode-map (kbd "M-,") 'rtags-find-references-at-point)
(define-key c-mode-map (kbd "M->") 'rtags-find-symbol)
(define-key c-mode-map (kbd "M-<") 'rtags-find-references)
(define-key c-mode-map (kbd "M-m M-r") 'rtags-rename-symbol)
(define-key c++-mode-map (kbd "M-.") 'rtags-find-symbol-at-point-other-window)
(define-key c++-mode-map (kbd "M-,") 'rtags-find-references-at-point)
(define-key c++-mode-map (kbd "M->") 'rtags-find-symbol)
(define-key c++-mode-map (kbd "M-<") 'rtags-find-references)
(define-key c++-mode-map (kbd "M-m M-r") 'rtags-rename-symbol)
(define-key rtags-mode-map (kbd "ENTER") 'rtags-select)
(define-key rtags-mode-map (kbd "RET") 'rtags-select)
(define-key rtags-mode-map (kbd "M-RET") 'rtags-select-other-window)
(define-key rtags-mode-map (kbd "M-o") 'other-window)
(rtags-start-process-maybe)

;; go mode
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 2)
            (setq indent-tabs-mode 1)))

(add-hook 'go-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-.") 'godef-jump)
	    (local-set-key (kbd "M->") 'godef-jump-other-window)
	    (local-set-key (kbd "M-m M-d") 'godef-describe)))

(add-hook 'go-mode-hook 
	  (lambda ()
	    (set (make-local-variable 'company-backends) '(company-go))))

;; glsl mode
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))

;; mu4e
; (add-to-list 'load-path "/usr/local/Cellar/mu/0.9.9.5/share/emacs/site-lisp/mu4e")
; (setq mu4e-mu-binary "/usr/local/Cellar/mu/0.9.9.5/bin/mu")
(require 'mu4e)
(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 300)

(setq mu4e-maildir "~/Maildir")

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

(setq mu4e-sent-messages-behavior 'delete)

(setq
 user-mail-address "yngccc@gmail.com"
 user-full-name  "JeremyC (陈漾)")

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "yngccc@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq message-kill-buffer-on-exit t)

