(let ((minver 23))
  (unless (>= emacs-major-version minver)
    (error "Emacs version 23 or higher required" minver)))

;; set meta/alt key on mac/linux
(cond ((eq system-type 'darwin)
       (progn (setq mac-command-modifier 'meta)
	      (setq mac-option-modifier 'alt)))
      ((eq system-type 'gnu/linux)
       (progn (setq x-super-keysym 'meta)
	      (setq x-meta-keysym 'alt)))
      ((eq system-type 'windows-nt)
       (error "fix me")))

(defun mac-swap-command-option-key ()
  (interactive)
  (rotatef mac-command-modifier mac-option-modifier))

(defun linux-swap-meta-super-key ()
  (interactive)
  (rotatef x-meta-keysym x-super-keysym))

;; Key Bindings
(global-unset-key (kbd "M-m"))
(global-unset-key (kbd "M-n"))

(global-set-key (kbd "M-1") 'shell-command)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-frame)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-8") 'split-window-horizontally)
(global-set-key (kbd "M-9") 'split-window-vertically)

(global-set-key (kbd "M-w") 'previous-line)
(global-set-key (kbd "M-s") 'next-line)
(global-set-key (kbd "M-a") 'backward-word)
(global-set-key (kbd "M-d") 'forward-word)

(global-set-key (kbd "A-w") (lambda () (interactive) (previous-line 6)))
(global-set-key (kbd "A-s") (lambda () (interactive) (next-line 6)))
(global-set-key (kbd "A-a") 'backward-char)
(global-set-key (kbd "A-d") 'forward-char)

(global-set-key (kbd "A-W") 'scroll-down)
(global-set-key (kbd "A-S") 'scroll-up)

(global-set-key (kbd "M-q") 'beginning-of-line)
(global-set-key (kbd "M-e") 'end-of-line)

(global-set-key (kbd "A-q") 'beginning-of-buffer)
(global-set-key (kbd "A-e") 'end-of-buffer)

(global-set-key (kbd "M-DEL") 'backward-kill-word)
(global-set-key (kbd "A-<backspace>") 'kill-this-buffer)

(setq recenter-positions '(top middle bottom))
(global-set-key (kbd "M-l") 'recenter-top-bottom)

(global-set-key (kbd "A-l") 'goto-line)
(global-set-key (kbd "M--") 'undo)
(global-set-key (kbd "M-z") 'save-buffer)

(global-set-key (kbd "C-c C-c") 'comment-region)

(global-set-key (kbd "M-m M-a") 'align)

;; prevent keyboard-escape-quit from closing other windows
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(global-set-key (kbd "M-g") 'keyboard-escape-quit)

(global-set-key (kbd "M-c") 'copy-region-as-kill)
(global-set-key (kbd "A-c") 'kill-region)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "A-v") 'yank-pop)

(global-set-key (kbd "M-_") 'undo)

(global-set-key (kbd "M-k") 'kill-line)
(global-set-key (kbd "A-k") (lambda () (interactive) (kill-line 0)))

(global-set-key (kbd "M-b") 'switch-to-buffer)
(global-set-key (kbd "A-b") 'list-buffers)
(global-set-key (kbd "M-;") 'switch-to-prev-buffer)
(global-set-key (kbd "M-'") 'switch-to-next-buffer)

(global-set-key (kbd "M-h") 'ff-find-other-file)
(global-set-key (kbd "A-h") (lambda () (interactive) (ff-find-other-file t)))

(global-set-key (kbd "A-p") 'rgrep)
(global-set-key (kbd "M-u") 'universal-argument)

(global-set-key (kbd "M-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-forward)
(global-set-key (kbd "M-r") 'isearch-backward)
(define-key isearch-mode-map (kbd "M-r") 'isearch-repeat-backward)

(global-set-key (kbd "M-F") 'isearch-forward-regexp)
(global-set-key (kbd "M-R") 'isearch-backward-regexp)

(global-set-key (kbd "M-y") 'query-replace)
(global-set-key (kbd "M-Y") 'query-replace-regexp)

(global-set-key (kbd "M-[") 'kmacro-start-macro)
(global-set-key (kbd "M-]") 'kmacro-end-macro)

(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "A-SPC") (kbd "C-u C-<SPC>"))

(global-set-key (kbd "A-g") 'google-search)

(global-set-key (kbd "A-R") 'point-to-register)
(global-set-key (kbd "A-r") 'jump-to-register)

(global-set-key (kbd "M-RET") 'completion-at-point)

;; backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; no bars/ring-bell
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq ring-bell-function 'ignore)

;; linum-mode
(defadvice linum-mode (around disable-linum-for)
  (unless (memq major-mode (list 'shell-mode 'eshell-mode 'compilation-mode 'eww-mode))
    ad-do-it))
(ad-activate 'linum-mode)
(global-linum-mode 1)

;; theme
(defadvice load-theme 
  (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

(setq solarized-high-contrast-mode-line t)

;; font
(defun set-face-height (height)
  (interactive "nheight: ")
  (set-face-attribute 'default nil :height height))
(set-face-height 110)

;; move thru camelCaseWords
(global-subword-mode 1)

;;
(setq x-select-enable-clipboard t)
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; alignment
(add-hook 'align-load-hook
	  (lambda ()
	    ;; make align ignore empty lines in region
	    (setq align-region-separate 'entire)
	    ;; fixed align-c++-mode variable declaration regex to allow nested template
	    (let ((rule (find-if (lambda (e) (eq (car e) 'c-variable-declaration)) 
				 align-rules-list)))
	      (when rule
		(setf (cdr (cadr rule))
		      (concat "[*&0-9A-Za-z_]>*[&*]*\\(\\s-+[*&]*\\)"
			      "[A-Za-z_][0-9A-Za-z:_]*\\s-*\\(\\()\\|"
			      "=[^=\n].*\\|(.*)\\|\\(\\[.*\\]\\)*\\)?"
			      "\\s-*[;,]\\|)\\s-*$\\)"))))))

;; minibuffer
(define-key minibuffer-local-map (kbd "M-s") nil)
(define-key minibuffer-local-map (kbd "M-r") nil)

;; auto revert
(global-auto-revert-mode t)

;; dired
(setq dired-listing-switches "-ahl --group-directories-first")
(global-set-key (kbd "M-i") 'dired-at-point)
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "M-h") 'wdired-change-to-wdired-mode)
	    (define-key dired-mode-map (kbd "M-w") (kbd "C-p"))
	    (define-key dired-mode-map (kbd "M-s") (kbd "C-n"))))

;; ido
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(global-set-key (kbd "M-t") 'ido-find-file)

(setq ido-decorations (quote ("\n-> " "" "\n   " "\n	.	.." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "M-s") 'ido-next-match)
  (define-key ido-completion-map (kbd "M-w") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;; shell
(add-hook 'shell-mode-hook
	  (lambda ()
	    (define-key shell-mode-map (kbd "M-r") 'isearch-backward)
	    (define-key shell-mode-map (kbd "M-p") 'compile)))

(defun shell-dir (name dir)
  (interactive "sShell name: \nDDirectory: ")
  (let ((default-directory dir))
    (shell name)))

;; eshell
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (define-key eshell-mode-map (kbd "M-n") nil)
	    (define-key eshell-mode-map (kbd "M-p") nil)
	    (define-key eshell-mode-map (kbd "M-r") nil)
	    (define-key eshell-mode-map (kbd "M-s") nil)))
			      
;; compilation mode
(global-set-key (kbd "M-p") 'compile)
(setq compile-command "ninja")

(defun compilation-hook ()
  (local-set-key (kbd "M-S") 'compilation-next-error)
  (local-set-key (kbd "M-W") 'compilation-previous-error))
(add-hook 'compilation-mode-hook 'compilation-hook)

;; OSX clang include path
(defun add-c++-include-dir ()
  (if (eq system-type 'darwin)
      (progn
	(add-to-list 'cc-search-directories "/usr/local/opt/llvm34/lib/llvm-3.4/include/c++/v1")
	(add-to-list 'cc-search-directories "/usr/local/include/freetype2"))))

(add-hook 'ff-pre-find-hook 'add-c++-include-dir)

;; c++ style
(c-add-style "my-cc-style"
	     '((fill-column . 100)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((innamespace 0)))))

(defun set-my-cc-style ()
  (c-set-style "my-cc-style"))
(add-hook 'c++-mode-hook 'set-my-cc-style)

;; lisp hook
(defun my-lisp-hook ()
  (show-paren-mode)
  (local-set-key (kbd "M-A") 'backward-sexp)
  (local-set-key (kbd "M-D") 'forward-sexp))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
(add-hook 'lisp-mode-hook 'my-lisp-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-hook)
(add-hook 'scheme-mode-hook 'my-lisp-hook)

;; elisp
(defun eval-last-sexp-and-print-result ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (insert (format " => %s" value))))
  
(define-key emacs-lisp-mode-map (kbd "M-m M-e") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "M-m M-E") 'eval-last-sexp-and-print-result)
(define-key emacs-lisp-mode-map (kbd "M-m M-b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "M-m M-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "M-m M-f") 'find-function)
(define-key emacs-lisp-mode-map (kbd "M-m M-v") 'find-variable)
(define-key emacs-lisp-mode-map (kbd "M-m M-F") 'describe-function)
(define-key emacs-lisp-mode-map (kbd "M-m M-V") 'describe-variable)

(setq initial-major-mode 'emacs-lisp-mode)

;; erc
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; doc-view
(setq doc-view-ghostscript-program "/usr/local/bin/gs")
(add-hook 'doc-view-mode-hook
	  (lambda ()
	    (linum-mode -1)
	    (local-set-key (kbd "M-w") 'doc-view-previous-page)
	    (local-set-key (kbd "M-s") 'doc-view-next-page)
	    (local-set-key (kbd "M-f") 'doc-view-fit-page-to-window)))

;; packages
(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/packages.el")))

;;
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
