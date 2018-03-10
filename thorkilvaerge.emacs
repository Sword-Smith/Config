;;; Package --- Thorkil  <thorkilk@gmail.com> Emacs Configuration
;;; Commentary:
;;;
;;; When in the course of human events it becomes necessary for one
;;; people to dissolve its political bands which have connected them
;;; with another and to assume among the powers of the earth, the
;;; separate and equal station to which the Laws of Nature and of
;;; Nature's God entitle them, a decent respect to the opinion of
;;; mankind requires that they should declare the causes which impel
;;; them to the seperation.
;;;
;;; Code:

;; These commands may need to be run manually:
;; ALT-x package-install -> ...
;; ALT-x package-install erlang
(setq inhibit-splash-screen t) ; Don't show the Emacs welcome screen

(ido-mode 1)                   ; Improved file hinting when opening/saving files
(electric-pair-mode 1)         ; Write parentheses in pairs
(show-paren-mode 1)            ; Show matching parentheses
(delete-selection-mode 1)      ; Delete selected text when typing
(global-font-lock-mode 1)      ; Syntax coloring by default
(column-number-mode 1)         ; Show cursor's column number
(setq-default fill-column 76)  ; Word-wrap at 76 characters
(setq tab-width 4)             ; tab is four spaces
(setq-default indent-tabs-mode nil) ; indent with spaces, not tabs
(prefer-coding-system 'utf-8)  ; Resolve encoding probs when loading packages

(defalias 'yes-or-no-p 'y-or-n-p) ; Answer yes/no questions with y/n keys

(setq confirm-kill-emacs 'yes-or-no-p) ; Require confirm on kill

(defun unhtml (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&lt;")
      (goto-char (point-min))
      (replace-string ">" "&gt;")
      )))

;; *** Install po-mode ***
(load "/usr/share/emacs/23.3/site-lisp/gettext/po-mode.elc")

;; *** CPERL CONFIG START ***
;; Use 4 space indents via cperl mode
(load "~/dev-utils/conf/emacs/jix-auto-insert.el")
(load "/home/ksm/.emacs.lib/cperl-mode.el")
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook 'n-cperl-mode-hook t)
(defun n-cperl-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 4)
  (setq cperl-indent-parens-as-block t)
  (setq cperl-close-paren-offset -4)
  )

(defun perltidy ()
  "Run perltidy on the current region or buffer."
  (interactive)
  (save-excursion
    (unless mark-active (mark-defun))
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))

;;Show tab-characters as a red block when in cperl-mode.
(add-hook 'cperl-mode-hook
	  (lambda ()  
	    (font-lock-add-keywords  
	     nil  
	     '(("\t" 0 'trailing-whitespace prepend)))))

;; Use 4 space indents via cperl mode
;; (custom-set-variables
;;   '(cperl-close-paren-offset -4)
;;   '(cperl-continued-statement-offset 4)
;;   '(cperl-indent-level 4)
;;   '(cperl-indent-parens-as-block t)
;;   '(cperl-tab-always-indent t)
;; )

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

; revert with conf
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer t t))

; recent file
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 10000)

;; disable backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

; Remove menubar, toolbar and scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; Ubuntu once broke dead keys in Emacs, so export XMODIFIERS='' in .zshrc.

;; Removes *Messages* buffer
(setq-default message-log-max nil)
(if (get-buffer "*Messages*")
    (kill-buffer "*Messages*"))

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Custom keybindings
(dolist (pair
  '(;("C-k" kill-whole-line)      ; including the end-of-line character(s)
    ("M-c" comment-region)       ; comment out the selected region
    ("M-u" uncomment-region)     ; uncomment the selected region
    ("C-n" dabbrev-expand)       ; auto-completion
    ("C-x C-g" goto-line)
    ("C-z" undo)
    ("C-<tab>" next-multiframe-window)
    ("C--" text-scale-decrease)
    ("C-+" text-scale-increase)
    ))
  (global-set-key (kbd (car pair)) (cadr pair)))

;; Package system
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("user42" . "http://download.tuxfamily.org/user42/elpa/packages/")))
(package-initialize)

;;; Prolog mode
;; (setq load-path (cons "~/.emacs.d/site-lisp/prolog/prolog.el" load-path))
;; (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
;; (autoload 'prolog-mode "prolog" "Major mode for editing prolog programs." t)
;; (setq prolog-system 'swi) ; prolog-system below for possible values
;; (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode))
;;                               auto-mode-alist))

;; Automatically install the following packages

; only do this every ten times
(if (> (random 10) 8)
    (package-refresh-contents))

(setq auto-install-packages
      '(color-theme bar-cursor htmlize flycheck flycheck-haskell
                    haskell-mode sml-mode rust-mode fsharp-mode nasm-mode go-mode
                    perl-mode web-mode )) ;;ffap-perl-module markdown-mode))
(dolist (pkg auto-install-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Font -- make sure it's installed
;(set-frame-font "Inconsolata-13")

;; Use a vertical bar as cursor instead of a block
(require 'bar-cursor)
(bar-cursor-mode 1)

;; Solarized theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))


;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert-alist
   (quote
    (((".pl" . "Perl script")
      . "perl.pl")
     (("/t/lib/JIX/Mojo/.*.pm" . "Mojo Test")
      .
      ["perlmojotest.pm" jix/expand-package-name])
     (("/lib/JIX/Mojo/.*.pm" . "Mojo controller")
      .
      ["perlmojo.pm" jix/expand-package-name])
     (("/t/lib/JIX/.*.pm" . "Test module")
      .
      ["perltest.pm" jix/expand-package-name])
     (("/lib/JIX/Service/Object/.*.pm" . "Service object")
      .
      ["perlservice.pm" jix/expand-package-name])
     (("/lib/JIX/View/.*.pm" . "View module")
      .
      ["perlview.pm" jix/expand-package-name])
     (("/lib/JIX/.*.pm" . "Perl module")
      .
      ["perl.pm" jix/expand-package-name]))))
 '(auto-insert-directory "~/dev-utils/conf/skeletons/")
 '(auto-insert-query nil)
 '(package-selected-packages
   (quote
    (less-css-mode ffap-perl-module autumn-light-theme white-sand-theme color-theme-modern color-theme-cobalt portage-navi perl-use-utf8-coding perlcritic multi-web-mode web-mode sml-mode rust-mode nasm-mode htmlize go-mode fsharp-mode flycheck-haskell color-theme bar-cursor))))

;;Prolog
;; (setq prolog-system 'swi) ; prolog-system below for possible values
;; (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode))
;;                               auto-mode-alist))
;; (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)

;; Erlang
;;(require 'erlang-mode)
;;(require 'erlang-start)
;;(add-to-list 'auto-mode-alist '("\\.erl" . erlang-mode))

;; Haskell
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

;; Rust
;(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

; UTF-8
(require 'perl-use-utf8-coding)
(modify-coding-system-alist 'file "\\.ep\\'" 'utf-8)

;; ElDoc -- show function hints in the echo area below
(require 'eldoc)
(add-hook 'haskell-mode-hook 'turn-on-eldoc-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;FFAP
;; (load "/home/thv/.emacs.lib/ffap-perl-module.el")
;; (ffap-bindings)
;; (setq ffap-require-prefix t)
;; (setq ffap-url-fetcher 'w3m-browse-url)
;; ;;Ignore / as a file.
;; (defadvice ffap-file-at-point (after ffap-file-at-point-after-advice ())
;;   (if (string= ad-return-value "/")
;;       (setq ad-return-value nil)))
;; (ad-activate 'ffap-file-at-point)


;; *** WEB-MODE CONFIG START ***
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.shtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ep\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  ;;(setq indent-tabs-mode nil) ; Undlad tabs
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  )
(add-hook 'web-mode-hook 'my-web-mode-hook)
