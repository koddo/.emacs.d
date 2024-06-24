;; -*- lexical-binding: t; -*-

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun m/dot-emacs-reload-init-file ()
  (interactive)
  ;; (ym-load-path)
  (load-file (expand-file-name (convert-standard-filename "init.el") user-emacs-directory)))

;; =========================================================

;; https://github.com/radian-software/straight.el#getting-started
;; see also ./early-init.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; https://github.com/radian-software/straight.el#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)   ; to avoid putting ":straight t" everywhere

(use-package blackout)     ; = diminish, see https://github.com/radian-software/blackout

;; =========================================================

(use-package no-littering
  :init   ; by default these are just etc/ and var/
  (setq no-littering-etc-directory (expand-file-name (convert-standard-filename "no-littering-etc/") user-emacs-directory))
  (setq no-littering-var-directory (expand-file-name (convert-standard-filename "no-littering-var/") user-emacs-directory)))

;; =========================================================

(setq server-name "new-server")      ;  usage: emacsclient -s $server-name
(server-start)

(setq frame-title-format "emacs")

(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message
      ";; scratch buffer\n\n")
(setq use-dialog-box nil)

(setq truncate-partial-width-windows nil)     ; I use visual-lines-mode instead

(setq-default truncate-lines t)    ;; In case if you're confused about the word-wrap variable, here's a clarification: Instead of setting this variable directly, most users should use Visual Line mode.
(global-visual-line-mode -1)   ;; Now toggle visual-line-mode per buffer via hydra.

(put 'downcase-region 'disabled nil)
(put   'upcase-region 'disabled nil)

(setq require-final-newline t)   ; t means when saving; other options are 'visit, 'visit-save, 'ask, nil
(setq next-line-add-newlines t)       ; TODO: make it add a new line so there's always a newline after cursor

(setq ediff-split-window-function 'split-window-horizontally)

;; =========================================================

(defun display-buffer-same-or-next-window (buffer alist)     ; slightly modified definition of display-buffer-same-window
  (if (not (or
            (cdr (assq 'inhibit-same-window alist))      ; some functions like occur-mode-display-occurrence signal the request to show occurences in some other window using inhibit-same-window, and we abide below by choosing next window
            ;; (window-dedicated-p)   ; TODO: make it skip all dedicated windows until it finds a free one
	        (window-minibuffer-p)))
      (window--display-buffer buffer (selected-window) 'reuse alist)
    (window--display-buffer buffer (next-window) 'reuse alist)
    ))

(setq display-buffer-alist
      '((".*" (display-buffer-same-or-next-window))))

;; for magit
(setq transient-display-buffer-action   
      '(display-buffer-same-or-next-window
        (inhibit-same-window . t)))

(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'prompt)    ; I'm not sure what this does, so I configured it to ask me.

;; =========================================================

(setq eldoc-echo-area-use-multiline-p nil)      ; don't let eldoc stretch the echo area vertically when displaying hints
(blackout 'eldoc-mode)

(setq debug-on-error t)
(setq visible-bell t)

(set-scroll-bar-mode 'right)

;; =========================================================

;; The rx Structured Regexp Notation
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Rx-Notation.html
(use-package rx)

;; there's also https://github.com/mattiase/xr --  Inverse of rx: convert Emacs string regexps to rx form

;; =========================================================

(require 'epa-file)   ; gnupg, gpg

;; not auto-saved, see the corresponding part of the configuration

;; =========================================================

;; disable the old and built-in auto-save-mode that creates a lot of junk #files#
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)
(setq auto-save-no-message t)   ; not sure if it has effect in other modes, but leaving it

;; Save Emacs buffers when they lose focus
(use-package super-save     ; https://github.com/bbatsov/super-save
  :blackout
  :config
  (setq auto-save-visited-interval 5)
  (auto-save-visited-mode 1)   ; it's a built-in mode, I found the super-save-auto-save-when-idle less usable, because it polluted messages
  ;; or, for fine-grained control: https://github.com/ChillarAnand/real-auto-save
  ;; it allows the following: (add-hook 'org-mode-hook 'real-auto-save-mode)
  ;; (setq super-save-idle-duration 5)    ; this is default, but it's not used anyway, see auto-save-visited-mode above

  (progn   ; do not auto-save .gpg
    (setq epa-file-inhibit-auto-save t)   ; it's on by default, but to be sure
    (setq auto-save-visited-predicate
          (lambda ()
            (not (string-suffix-p ".gpg" (buffer-file-name)))))
    (setq super-save-exclude '(".gpg")))
  
  (setq super-save-remote-files nil)

  (setq save-silently t)
  (super-save-mode +1)
  )

(setq make-backup-files nil)    ; foo~

;; editing files by different instances almost never happens in my case
(setq create-lockfiles nil)

;; =========================================================

;; isearch extension that shows number of matches and current match index
(use-package anzu
  :blackout
  :config
  (setq anzu-search-threshold 1000)    ; default: 1000
  (global-anzu-mode +1)
  )

;; =========================================================

(use-package dired+
  :config
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(dired-omit-extensions '() nil nil "Customized by me")
   '(dired-omit-files (rx (or
                           (seq bol "." eol) 
                           (seq bol "." (not (any "."))) 
                           ))
                      nil nil "Customized by me"))
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  (add-hook 'dired-mode-hook 'auto-revert-mode)   ; watch filesystem for changes

  (defun ym/dired-toggle-hide-and-omit ()
    (interactive)
    (if dired-omit-mode
        (progn (dired-omit-mode -1)
               (dired-hide-details-mode -1))
      (dired-omit-mode 1)
      (dired-hide-details-mode 1)))
  (define-key dired-mode-map "(" 'ym/dired-toggle-hide-and-omit)

  ;; used to be (setq dired-listing-switches "-la"), but, apparently, ls can't show dot files at the top
  (require 'ls-lisp)    ;; ignore case when listing directory
  (setq ls-lisp-ignore-case t)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-use-string-collate nil)

  )

(require 'wdired)
(setq wdired-confirm-overwrite t)

(require   'uniquify)  ; buffer names are uniquified with parts of directory name, for ex.: name|folder
(setq       uniquify-buffer-name-style 'reverse)

(setq delete-by-moving-to-trash t)

;; =========================================================

(recentf-mode 1)
(setq recentf-max-saved-items 50)   ; default: 20

(save-place-mode 1)
;; save-place-forget-unreadable-files is t by default, this make quitting emacs slow

(savehist-mode 1)
(setq history-length 25)    ; default: 100
;; history lengths can be adjusted separately, an example: (put 'minibuffer-history 'history-length 50)
;; see no-littering-var/savehist.el for the full list of settings
(setq history-delete-duplicates t)    ; default: nil, which is probably faster
(setq savehist-autosave 300)   ; default: 300
(add-to-list 'savehist-additional-variables 'search-ring)
(add-to-list 'savehist-additional-variables 'regexp-search-ring)
(add-to-list 'savehist-additional-variables 'query-replace-defaults)
(add-to-list 'savehist-additional-variables 'compile-history)
(setq savehist-additional-variables (remove 'kill-ring savehist-additional-variables))    ; Just in case. It tends to get huge and slow emacs.

;; =========================================================

;; (setq auto-revert-interval 1)    ; default: 5 -- not used because auto-revert-use-notify
(setq auto-revert-use-notify t)   ; default: t
(setq auto-revert-avoid-polling t)
(setq auto-revert-remote-files nil)   ; this is for tramp, remote files; default: nil -- because can be slow
;; (setq auto-revert-verbose t)
;; (setq auto-revert-debug t)
(setq auto-revert-notify-exclude-dir-regexp "")    ; somehow the default regexp excluded files in ~/.emacs.d
(global-auto-revert-mode 1)

;; =========================================================

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(require 'whitespace)
(setq whitespace-style
      ;; used to be '(tabs tab-mark space-mark)
      '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof))
;; toggle with M-x global-whitespace-mode	

;; =========================================================

(defun dont-kill-emacs ()   ;; disable C-x C-c
  (interactive)
  (message (substitute-command-keys "To exit emacs: \\[save-buffers-kill-emacs]")))
(global-set-key "\C-x\C-c" 'dont-kill-emacs)

;; =========================================================

;; with this all keybindings work even with non-English layout
(use-package reverse-im
  :demand
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

;; =========================================================

(setq calendar-week-start-day 1)     ;; week starts from Monday

;; agenda

;; =========================================================

(defvar ym-keys-minor-mode-map (make-keymap) "ym-keys-minor-mode keymap")
(define-minor-mode ym-keys-minor-mode
  "A minor mode for my global keybindings."
  :init-value t :lighter "" :keymap 'ym-keys-minor-mode-map)
(ym-keys-minor-mode 1)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'ym-keys-minor-mode))
      (let ((mykeys (assq 'ym-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'ym-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(defun ym-define-key (key func)   ; Not sure if we really need to define keys in both maps, but just in case.
  (global-set-key key func)
  (define-key ym-keys-minor-mode-map key func))

(defun ym-undefined-key-message () (interactive) (message "undefined keybinding yet, see ym-define-key"))

;; -----------------------------------------------

;; Unbind all keybindings with:
;; super
;; shift-super
;; meta
;; super-meta
;; shift-meta

(mapcar
 (lambda (x)
   (ym-define-key (kbd (concat "s-" (list x)))
                  nil))
 (concat
  "abcdefghijklmnopqrstuvwyxz"
  "ABCDEFGHIJKLMNOPQRSTUVWYXZ"
  "1234567890-=[];\\,./`"
  "!@#$%^&*()_+{}:|<>?~"
  "'\""
  ))

;; We leave M-x working.
;; There was M-s prefix for sorting, but I've never used it. It's easier to remember the names of the functions.
(mapcar
 (lambda (x)
   (ym-define-key (kbd (concat "M-" (list x)))
                  nil))
 (concat
  "abcdefghijklmnopqrstuvwy"
                                        ; "x"     ; <- the M-x prefix stays
  "z"
  "1234567890-=[];\\,./`"
  "'"
  ))

;; Mac option+keys are translated to these weird characters.
(mapcar
 (lambda (x)
   (ym-define-key (kbd (concat "M-s-" (list x)))
                  nil))
 (concat
  "œ∑´®†¥¨ˆøπ“‘åß∂ƒ©˙∆˚¬…æ«Ω≈ç√∫˜µ≤≥÷"
  "¡™£¢∞§¶•ªº–≠"
  "`"
  ))

;; We have to bind shift-meta-* to anything, otherwise these keybindings get translated to ones without shift.
;; https://unix.stackexchange.com/questions/25649/is-it-possible-to-stop-emacs-from-down-translating-my-key-chords/25719#25719
(mapcar
 (lambda (x)
   (ym-define-key (kbd (concat "M-" (list x)))
                  #'ym-undefined-key-message))
 (concat
  "ABCDEFGHIJKLMNOPQRSTUVWYXZ"    ; M-S-a doesn't work for some reason, but M-A does
  "!@#$%^&*()_+{}:|<>?~"
  "\""
  ))

(ym-define-key (kbd "C-z") nil)    ; I constantly hit this unintentionally.

;; https://emacs.stackexchange.com/questions/14755/how-to-remove-bindings-to-the-esc-prefix-key/14759#14759
;; Let the escape key do its thing. Yeah, I feel the judgemental stare.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))   ; = quit

;; -----------------------------------------------

;; Also clear combinations involving return, space, backspace, tab, and arrow keys:

(ym-define-key (kbd "<s-return>") nil)
(ym-define-key (kbd "<M-s-return>") nil)
(ym-define-key (kbd "<S-s-return>") nil)
(ym-define-key (kbd "<M-return>") nil)
(ym-define-key (kbd "<M-S-return>") nil)
(ym-define-key (kbd "<S-return>") nil)

(ym-define-key (kbd "<s-SPC>") nil)
(ym-define-key (kbd "M-s-SPC") nil)   ; if this doesn't work, try (kbd "M-s- ")
(ym-define-key (kbd "<S-s-SPC>") nil)
(ym-define-key (kbd "<M-SPC>") nil)
(ym-define-key (kbd "<M-S-SPC>") nil)
(ym-define-key (kbd "<S-SPC>") nil)

(ym-define-key (kbd "<s-backspace>") nil)
(ym-define-key (kbd "<M-s-backspace>") nil)
(ym-define-key (kbd "<S-s-backspace>") nil)
(ym-define-key (kbd "<M-backspace>") nil)
(ym-define-key (kbd "<M-S-backspace>") nil)
(ym-define-key (kbd "<S-backspace>") nil)

(ym-define-key (kbd "<s-up>") nil)
(ym-define-key (kbd "<s-down>") nil)
(ym-define-key (kbd "<s-left>") nil)
(ym-define-key (kbd "<s-right>") nil)
(ym-define-key (kbd "<M-up>") nil)
(ym-define-key (kbd "<M-down>") nil)
(ym-define-key (kbd "<M-left>") nil)
(ym-define-key (kbd "<M-right>") nil)
(ym-define-key (kbd "<M-s-up>") nil)
(ym-define-key (kbd "<M-s-down>") nil)
(ym-define-key (kbd "<M-s-left>") nil)
(ym-define-key (kbd "<M-s-right>") nil)
(ym-define-key (kbd "<S-s-up>") nil)
(ym-define-key (kbd "<S-s-down>") nil)
(ym-define-key (kbd "<S-s-left>") nil)
(ym-define-key (kbd "<S-s-right>") nil)
(ym-define-key (kbd "<S-M-up>") nil)
(ym-define-key (kbd "<S-M-down>") nil)
(ym-define-key (kbd "<S-M-left>") nil)
(ym-define-key (kbd "<S-M-right>") nil)

;; (ym-define-key (kbd "<S-up>") nil)     ; these four are used by org-mode
;; (ym-define-key (kbd "<S-down>") nil)
;; (ym-define-key (kbd "<S-left>") nil)
;; (ym-define-key (kbd "<S-right>") nil)

(ym-define-key (kbd "<M-tab>") nil)
;; <s-tab> is cmd-tab, binding is useless
;; <M-s-tab>, <S-s-tab>, <M-S-tab> are not convenient

;; (ym-define-key (kbd "<backtab>") nil)   ; = <S-tab>, it is probably used by org-mode.

;; M-S-- doesn't work, because alt+shift toggles layout.
;; (ym-define-key (kbd "M-_") (lambda () (interactive) (insert "—")))

;; =========================================================

;; use shift for selection
;; this is usual behaviour pretty much everywhere
(transient-mark-mode 1)   
(setq mark-even-if-inactive nil)     ; this is important, otherwise we can cut and paste something accidentally
(setq shift-select-mode 1)   ; shifted motion keys activate the mark momentarily
(delete-selection-mode 1)    ; typed text replaces the selection if the selection is active

;; I want unshifted movements to always deactivate region
;; without this fix it doesn't get deactivated even though the transient-mark-mode is on
(defun ym/advice-handle-shift-selection (orig-fun &optional args)
  (if (not this-command-keys-shift-translated)
      (deactivate-mark)
    (apply orig-fun args)))
(advice-add 'handle-shift-selection :around 'ym/advice-handle-shift-selection)
;; (advice-remove 'handle-shift-selection 'ym/advice-handle-shift-selection)

(ym-define-key (kbd "s-i") 'previous-line)
(ym-define-key (kbd "s-k") 'next-line)
(ym-define-key (kbd "s-j") 'backward-char)
(ym-define-key (kbd "s-l") 'forward-char)

;; mwim = move where I mean
;; https://github.com/alezost/mwim.el
(use-package mwim
  :config
  (ym-define-key (kbd "s-u") #'mwim-beginning-of-code-or-line)    ; I used to have custom functions for this, see git history
  (ym-define-key (kbd "s-o") #'mwim-end))

(ym-define-key (kbd "s-s") (lambda () (interactive) (ignore-error 'user-error (windmove-left))))
(ym-define-key (kbd "s-d") (lambda () (interactive) (ignore-error 'user-error (windmove-down))))
(ym-define-key (kbd "s-f") (lambda () (interactive) (ignore-error 'user-error (windmove-right))))
(ym-define-key (kbd "s-e") (lambda () (interactive) (ignore-error 'user-error (windmove-up))))

(use-package buffer-move)
(ym-define-key (kbd "s-S") (lambda () (interactive) (ignore-error 'error (buf-move-left))))
(ym-define-key (kbd "s-D") (lambda () (interactive) (ignore-error 'error (buf-move-down))))
(ym-define-key (kbd "s-F") (lambda () (interactive) (ignore-error 'error (buf-move-right))))
(ym-define-key (kbd "s-E") (lambda () (interactive) (ignore-error 'error (buf-move-up))))

(ym-define-key (kbd "s-w") (lambda () (interactive) (ignore-error 'user-error (tab-bar-switch-to-prev-tab))))
(ym-define-key (kbd "s-r") (lambda () (interactive) (ignore-error 'user-error (tab-bar-switch-to-next-tab))))

(ym-define-key (kbd "s-W") (lambda () (interactive) (tab-bar-move-tab -1)))
(ym-define-key (kbd "s-R") (lambda () (interactive) (tab-bar-move-tab 1)))

(defvar ym/toggle-single-window-last-wc nil)
(defun ym/toggle-single-window ()
  (interactive)
  (if (not (one-window-p))            ; used to be (> (count-windows) 1)
      (progn
        (setq ym/toggle-single-window-last-wc (current-window-configuration))
        (delete-other-windows))
    (when (window-configuration-p ym/toggle-single-window-last-wc)
      (set-window-configuration ym/toggle-single-window-last-wc)
      (setq ym/toggle-single-window-last-wc nil))))

(ym-define-key (kbd "s-!") (lambda () (interactive) (ignore-error 'error (delete-window))))
(ym-define-key (kbd "s-1") 'ym/toggle-single-window)
(ym-define-key (kbd "s-@") (lambda () (interactive) (ignore-error 'error (split-window-below))))
(ym-define-key (kbd "s-#") (lambda () (interactive) (ignore-error 'error (split-window-right))))
(ym-define-key (kbd "s-2") 'tab-bar-history-back)
(ym-define-key (kbd "s-3") 'tab-bar-history-forward)

(ym-define-key (kbd "s-z") #'undo)
(ym-define-key (kbd "s-Z") #'undo-tree-visualize)
(ym-define-key (kbd "s-x")
               (lambda (beg end)
                 (interactive "r")
                 (prog1
                     (kill-region beg end)
                   (setq deactivate-mark nil))))   ; leave the region highlighted after the cut
(ym-define-key (kbd "s-c")
               (lambda (beg end)
                 (interactive "r")
                 (prog1
                     (kill-ring-save beg end)
                   (setq deactivate-mark nil))))   ; leave the region highlighted after the copy
(ym-define-key (kbd "s-v") #'yank)

;; =========================================================

(defun ym-delete-current-line-or-region ()
  (interactive)
  (let* ((beg (save-excursion (if (and mark-active (> (point) (mark))) (exchange-point-and-mark))
			                  (move-beginning-of-line 1)
			                  (point)))
	     (end (save-excursion (if (and mark-active (< (point) (mark))) (exchange-point-and-mark))
			                  (if (and mark-active (= (point) (line-beginning-position))) (forward-line -1))
			                  (move-beginning-of-line 2) (point)))
	     (orig-col(current-column))
	     )
    (delete-region beg end)
    (move-to-column orig-col)
    ))

(defun ym-kill-current-line-or-region ()
  (interactive)
  (let* ((beg (save-excursion (if (and mark-active (> (point) (mark))) (exchange-point-and-mark))
			                  (move-beginning-of-line 1)
			                  (point)))
	     (end (save-excursion (if (and mark-active (< (point) (mark))) (exchange-point-and-mark))
			                  (if (and mark-active (= (point) (line-beginning-position))) (forward-line -1))
			                  (move-beginning-of-line 2) (point)))
	     (orig-col(current-column))
	     )
    (kill-region beg end)
    (move-to-column orig-col)
    ))

(defun ym-copy-current-line-or-region ()
  (interactive)
  (let* ((beg (save-excursion (if (and mark-active (> (point) (mark))) (exchange-point-and-mark))
			                  (move-beginning-of-line 1)
			                  (point)))
	     (end (save-excursion (if (and mark-active (< (point) (mark))) (exchange-point-and-mark))
			                  (if (and mark-active (= (point) (line-beginning-position))) (forward-line -1))
			                  (move-beginning-of-line 2) (point)))
	     (orig-col(current-column))
	     )
    (kill-ring-save beg end)
    (move-to-column orig-col)
    ))

(defun ym/comment-or-uncomment-line-or-region ()
  "Like comment-or-uncomment-region, but comment current line if there is no mark."
  (interactive)
  (if (not mark-active)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (if (< (point) (mark))
        (comment-or-uncomment-region (save-excursion (beginning-of-line) (point))
                                     (save-excursion (goto-char (mark))
                                                     (if (<= (point) (progn (back-to-indentation) (point))) (forward-line -1))
                                                     (line-end-position)))
      (comment-or-uncomment-region (save-excursion (goto-char (mark)) (line-beginning-position))
                                   (save-excursion (if (<= (point)
                                                           (progn (back-to-indentation) (point)))
                                                       (forward-line -1))
                                                   (end-of-line) (point))))))

(defun ym-duplicate-current-line-or-region (arg)   ; took it from here: http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")          ; also see if any problems: http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (if (and mark-active (= (point) (line-beginning-position)))
        (forward-line -1))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun ym-duplicate-and-comment-current-line-or-region (arg)   ; took it from here: http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")          ; also see if any problems: http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (if (and mark-active (= (point) (line-beginning-position)))
        (forward-line -1))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (comment-or-uncomment-region beg end)
      (setq end (line-end-position))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      ;; (goto-char (+ origin (* (length region) arg) arg))
      ;; goto-char is commented out for the time being
      ;; maybe fix this: we don't actually know the length of the resulting commented region, so it jumps to an arbitrary position
      ;; but on the other hand, it's not really important
      )))

(ym-define-key (kbd "s-S-<backspace>") #'ym-delete-current-line-or-region)
(ym-define-key (kbd "s-/") #'ym/comment-or-uncomment-line-or-region)
;; (ym-define-key (kbd "C-S-<backspace>") #'ym-copy-current-line-or-region)
;; (ym-define-key (kbd "M-S-<backspace>") #'ym-kill-current-line-or-region)      ; Doesn't work at the moment, because alt+shift toggles layout.
;; (ym-define-key (kbd "s-d") #'ym-duplicate-current-line-or-region)
;; (ym-define-key (kbd "s-D") #'ym-duplicate-and-comment-current-line-or-region)

(defun ym-backward-kill-word ()
  "Similar to backward-kill-word, but treats newlines and whitespace sequences as a words."
  (interactive)
  (if (bolp)
      (backward-delete-char 1)
    (delete-region (point)
                   (max (save-excursion (beginning-of-line) (point))
                        (save-excursion (backward-word) (point))
                        (save-excursion (let ((distance (skip-chars-backward "[:blank:]")))
                                          (if (or
                                               (= distance 0)
                                               (= distance -1)
                                               ;; (= distance -2)
                                               )
                                              0
                                            (point))
                                          ))   ; comment this block if removing whitespaces only is annoying
                        ))))
(ym-define-key (kbd "<M-backspace>") #'ym-backward-kill-word)
(ym-define-key (kbd "<s-backspace>") #'ym-backward-kill-word)

;; =========================================================

(use-package topspace
  :config
  (setq-default indicate-empty-lines t)
  (setq topspace-autocenter-buffers nil)
  (global-topspace-mode 1)
  )

;; see https://www.emacswiki.org/emacs/SmoothScrolling
;; and https://stackoverflow.com/questions/18386824/emacs-how-do-you-disable-auto-recentering
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)   ; 
(setq mouse-wheel-scroll-amount '(1))

(setq scroll-preserve-screen-position t)
(setq scroll-error-top-bottom nil)     ; from the docs: Move point to top/bottom of buffer before signaling a scrolling error.
(defvar my/scroll-command---virtual-cur-line nil)
(defvar my/scroll-command---n-lines-from-top nil)
(defvar my/scroll-command---column-before-scrolling nil)
(make-variable-buffer-local 'my/scroll-command---virtual-cur-line)
(make-variable-buffer-local 'my/scroll-command---n-lines-from-top)
(make-variable-buffer-local 'my/scroll-command---column-before-scrolling)

(defun my/scroll-command (n-lines)
  (interactive)
  (unless
      (or (eq last-command #'my/scroll-down-command)
          (eq last-command #'my/scroll-up-command))
    (setq my/scroll-command---virtual-cur-line
          (line-number-at-pos (point)))
    (setq my/scroll-command---n-lines-from-top
          (- my/scroll-command---virtual-cur-line (line-number-at-pos (window-start))))
    (setq my/scroll-command---column-before-scrolling (current-column)))
  (let* ((cur-line my/scroll-command---virtual-cur-line)
         (next-screen-line (+ cur-line n-lines))
         ;; (n-lines-from-top (- cur-line (line-number-at-pos (window-start))))
         )
    (unless (or (< next-screen-line (line-number-at-pos (beginning-of-buffer)))
                (> next-screen-line (line-number-at-pos (end-of-buffer))))
      (goto-line next-screen-line)
      (recenter my/scroll-command---n-lines-from-top)
      (setq my/scroll-command---virtual-cur-line next-screen-line)
      (move-to-column my/scroll-command---column-before-scrolling))))
(defun my/scroll-down-command () (interactive) (my/scroll-command (- (window-body-height))))
(defun my/scroll-up-command () (interactive) (my/scroll-command (+ (window-body-height))))

;; Please be aware, that this can under- or overscroll when there are images or tall lines in buffer.
;; The trade-off here is that this is more predictable to me, because I can get back to the exact location from where I started scrolling.
;; Works perfectly in code files though.
(ym-define-key (kbd "s-m") #'my/scroll-down-command)        ; page up
(ym-define-key (kbd "s-n") #'my/scroll-up-command)          ; page down

(ym-define-key (kbd "s-,") (lambda () (interactive "^") (scroll-up-command 3)))     ; the built-in scroll command is fine for this
(ym-define-key (kbd "s-.") (lambda () (interactive "^") (scroll-down-command 3)))

;; =========================================================

(setq isearch-repeat-on-direction-change nil)   ; I like it to go into search mode first before searching
(defun ym-search-selection-or-isearch (forward)
  (interactive)
  (if (not mark-active)
      (if forward (isearch-forward) (isearch-backward))
    (if forward
	    (if (< (point) (mark)) (exchange-point-and-mark))
      (if (> (point) (mark)) (exchange-point-and-mark)))
    (let* ((beg (point))
	       (end (mark))
	       (selection (buffer-substring-no-properties beg end)))
      (deactivate-mark)
      (isearch-mode forward nil nil nil)
      (isearch-yank-string selection)
      )))
(defun ym-search-selection-or-isearch-forward ()   (interactive) (ym-search-selection-or-isearch t))
(defun ym-search-selection-or-isearch-backward ()  (interactive) (ym-search-selection-or-isearch nil))
;; (ym-define-key (kbd "s-s") 'ym-search-selection-or-isearch-forward)
;; (ym-define-key (kbd "s-r") 'ym-search-selection-or-isearch-backward)
;; (define-key isearch-mode-map (kbd "s-s") 'isearch-repeat-forward)
;; (define-key isearch-mode-map (kbd "s-r") 'isearch-repeat-backward)

;; =========================================================

(use-package avy
  :config
  (defun avy-goto-parens ()
    (interactive)
    (let ((avy-command this-command))   ; for look up in avy-orders-alist
      (avy-jump "[][{}()]+")   ; any sequence of parens, brackets, curly braces        ; was (avy-jump "\\[+\\|\\]+\\|(+\\|)+\\|{+\\|}+")
      ))
  (add-to-list 'avy-orders-alist '(avy-goto-parens . avy-order-closest))
  (setq avy-keys
	    (list
         ?j ?k ?l
	     ?s ?d ?f     ; ?a -- looks too similar to ?d
         ?r ?e ?w          ; ?q -- I often can't distinguish q from g
	     ?v ?c ?x ?z       ; ?b -- o
	     ?m ;; ?n
                                        ; ?1 ?2 ?3 ?4
                                        ; ?5 ?6
                                        ; ?7 ?8 ?9 ?0
                                        ; ?u -- it's fine, but hard to reach after the semicolon         ; ?i=?l  ; ?o -- similar to a   ;; ?p -- vertical line is not visible enough
                                        ; ?h -- hard to reach          ; ?g -- similar to a
                                        ; ?y ?t -- hard to reach
                                        ; ?, ?. -- these don't work for some reason
	     ))
  (setq avy-background nil)      ; (set-face-attribute 'avy-background-face nil :foreground "grey90" :background "grey98") -- doesn't work with my highlighting of active window
  (setq avy-highlight-first nil)
  (setq avy-all-windows t) ; 'all-frames
  (setq avy-style 'at-full)       ; (setq avy-style 'de-bruijn)
  (setq avy-single-candidate-jump nil)
  ;; (setq avy-timeout-seconds 0.3)
  (add-to-list 'avy-orders-alist '(avy-goto-char-2 . avy-order-closest))
  ;; (add-to-list 'avy-orders-alist '(avy-goto-word-1 . avy-order-closest))

  ;; https://github.com/abo-abo/avy/issues/268
  ;; TODO:this should be a pull request with adding an option for excluding current point
  (defun avy-jump-advice-exclude-current-point (orig-func &rest args)
    (let* ((between-inclusive (lambda (val low high) (and (<= low val) (<= val high))))
	       (current-point (point))
	       (oldpred (plist-get :pred args))
	       (pred (lambda ()
		           (and
		            (let ((candidate (point))) (or (< candidate current-point) (> candidate (+ 3 current-point))))   ; +3 should be enough: with avy-goto-word-1 it's +1, with avy-goto-char-2 it's +2
		            (or (null oldpred) (funcall oldpred))))))
      (apply orig-func (append args (list :pred pred)))))
  (advice-add 'avy-jump :around #'avy-jump-advice-exclude-current-point)

  ;; https://emacs.stackexchange.com/questions/74840/hide-cursor-and-marks-in-all-windows-while-using-avy-or-ace-window/76656#76656
  ;; This snippet works under the assumption the cursor type is same everywhere. It uses setq-default for buffer-local variables cursor-in-non-selected-windows and cursor-in-non-selected-windows.
  ;; When the inactive cursor is on one of the avy targets,
  ;; the overlay and cursor colors mix and the avy sequence gets hard to read.
  (defun avy-jump-advice--hide-cursor-temporarily (orig-func &rest args)
    (let ((ct  cursor-type)
	      (ctn cursor-in-non-selected-windows))
      (setq-default cursor-in-non-selected-windows nil)
      (setq-default cursor-type nil)
      (apply orig-func args)
      (setq-default cursor-type ct)
      (setq-default cursor-in-non-selected-windows ctn)))
  (advice-add 'avy-jump :around #'avy-jump-advice--hide-cursor-temporarily)
  ;; just in case: (advice-remove 'avy-jump #'avy-jump-advice--hide-cursor-temporarily)

  (define-key isearch-mode-map (kbd "s-;") 'avy-isearch)
  (ym-define-key (kbd "s-;") #'avy-goto-word-1)
  ;; (ym-define-key (kbd "s-;") #'avy-goto-word-2)
  ;; (ym-define-key (kbd "s-;") #'avy-goto-char-timer)
  ;; (ym-define-key (kbd "s-^") #'avy-goto-parens)   ; "S-s-;" -- this is not a usual ^, it's a unicode character

  :custom-face
  (avy-lead-face   ((t (:foreground "white" :background "#dc9656"))))
  (avy-lead-face-0 ((t (:foreground "white" :background "#dc9656"))))
  (avy-lead-face-1 ((t (:foreground "white" :background "#dc9656"))))
  (avy-lead-face-2 ((t (:foreground "white" :background "#dc9656"))))
  )

;; =========================================================

(use-package ido
  :config
  (setq
   ido-use-virtual-buffers t   ; keep a list of closed buffers
   ido-enable-flex-matching t
   ;; ido-use-faces nil   ; turned off for flx
   ido-default-buffer-method 'selected-window    ; do not switch frames if a buffer is opened -- http://ergoemacs.org/misc/emacs_ido_switch_window.html
   ido-auto-merge-work-directories-length -1     ; disable search for a file in other recent used directories -- https://stackoverflow.com/questions/17986194/emacs-disable-automatic-file-search-in-ido-mode
   )
  (ido-mode 1)
  (ido-everywhere 1)
  ;; I don't use flx-ido, because it doesn't take recency into account. C-space or toggling regex matching works great for me.

  ;; we turn off the keybindings-only minor mode in minibuffer
  ;; but the very same keybindings still work in global keymap
  ;; ido takes precedxence in minibuffer
  (add-hook 'minibuffer-setup-hook (lambda () (ym-keys-minor-mode 0)))

  (mapcar (lambda (map)
	        (define-key map (kbd "s-j") #'ido-prev-match)
	        (define-key map (kbd "s-l") #'ido-next-match))
          (list
	       ido-buffer-completion-map
	       ido-common-completion-map
	       ido-file-completion-map
	       ido-file-dir-completion-map
	       ))
  )
(use-package ido-grid-mode
  :config
  ;; (setq ido-grid-mode-start-collapsed t)
  )

;; C-h f, while Amx is active, runs describe-function on the currently selected command.
;; M-. jumps to the definition of the selected command.
;; C-h w shows the key bindings for the selected command. (Via where-is.)
;; there is also amx-major-mode-commands
(require 'warnings)      ; for warning-suppress-types
(use-package amx   ; smex successor, for M-x
  :config
  (setq
   amx-ignored-command-matchers nil
   ido-cr+-max-items 50000   ; default: 30000
   amx-show-key-bindings nil
   ;; and set amx-save-file to do-not-litter
   )
  (amx-mode 1)
  (add-to-list 'warning-suppress-types '(amx))
  ;;; I used to have the following to update smex cache
  ;; (defun ym-smex-update-after-load-file (unused)
  ;;   (when (boundp 'smex-cache)
  ;;     (smex-update)))
  ;; (add-hook 'after-load-functions 'ym-smex-update-after-load-file)   ; see init_keybindings.el
  )

(use-package ido-completing-read+   ; enhanced ido-everywhere, https://github.com/DarwinAwardWinner/ido-completing-read-plus
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-yes-or-no
  :config
  (ido-yes-or-no-mode 1))

;;; Tried ido-vertical-mode, but I prefer classic ido.
;; (use-package ido-vertical-mode
;;   :config
;;   (ido-vertical-mode 0))

;; (add-to-list 'ido-ignore-buffers "\\` ")
(add-to-list 'ido-ignore-buffers "\\*Ido Completions\\*")
(add-to-list 'ido-ignore-buffers "\\*Ibuffer\\*")
(add-to-list 'ido-ignore-buffers "\\*Ibuffer\\*")
(add-to-list 'ido-ignore-buffers "\\*Messages\\*")
(add-to-list 'ido-ignore-buffers "\\*Completions\\*")
(add-to-list 'ido-ignore-buffers "\\*Scratch\\*")
(add-to-list 'ido-ignore-buffers "\\*Help\\*")
(setq ido-use-filename-at-point 'guess)   ;; nil, guess and t for literal filename
(setq ido-use-url-at-point t)
(setq ido-file-extensions-order '(".org" ".md"))
;; (setq ido-enter-matching-directory nil)
(setq ido-show-dot-for-dired t)
;; (setq ido-enable-tramp-completion nil)
;; (setq ido-max-prospects 7)

(use-package ido-sort-mtime
  :config
  (setq
   ido-sort-mtime-limit 2000
   ;; ido-sort-mtime-tramp-files-at-end nil
   ))

(ym-define-key (kbd "s-b") #'ido-switch-buffer)
(ym-define-key (kbd "s-B") #'ibuffer)
(global-set-key [remap list-buffers] 'ibuffer)

;; =========================================================

;; Show em-dash as "---"
;; from https://emacs.stackexchange.com/questions/9623/tell-a-dash-an-en-dash-and-an-emdash-apart
(defun my-shortcut/enable-showing-mdash-in-buffer-as-triple-minus ()
  (interactive)
  (let ((glyph-en-dash (make-glyph-code ?\u002D 'font-lock-keyword-face))
        (glyph-em-dash (make-glyph-code ?\u002D 'font-lock-function-name-face)) )
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table 8211 `[,glyph-en-dash ,glyph-en-dash])
    (aset buffer-display-table 8212 `[,glyph-em-dash ,glyph-em-dash ,glyph-em-dash])))

;; =========================================================

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
	    (list (concat
	           (file-name-as-directory user-emacs-directory)
	           "yasnippets")))
  (yas-reload-all)
  ;; I use yas-insert-snippet manually, glbal mode is disabled: (yas-global-mode -1)

  ;; unbind tab -- I use yas-insert-snippet, not expand
  ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)

  ;; there's also grep-edit.el, which could be used to modify snippets in its buffer, instead of doing C-x C-f at their paths here, but the difference is not worth it at the moment
  (defun ym-list-yasnippets (bufname dirs)
    (switch-to-buffer (if bufname (get-buffer-create bufname) (get-buffer-create "*yasnippets*")))
    (erase-buffer)
    (dolist (dir dirs)
      (dolist (filename (directory-files-recursively dir "" nil))
	    (insert "---------------------------------------------------------")
	    (newline)
	    (insert filename)
	    (newline)
	    (insert-file-contents filename)
	    (end-of-buffer)   ; any better way to jump after the insertion?
	    (newline)
	    (newline)
	    (newline)
	    (newline)
	    ))
    (beginning-of-buffer)
    (view-mode))
  (defun ym-list-all-yasnippets-official ()
    (interactive)
    (ym-list-all-yasnippets "*yasnippets-official*"
                            (list (expand-file-name (convert-standard-filename "straight/repos/yasnippet-snippets/snippets/") user-emacs-directory))))
  (defun ym-list-all-yasnippets-my ()
    (interactive)
    (ym-list-all-yasnippets "*yasnippets-my*"
                            (list (expand-file-name (convert-standard-filename "my-yasnippets") user-emacs-directory))))
  )

(use-package yasnippet-snippets)

;; =========================================================

(use-package markdown-mode)    ; https://jblevins.org/projects/markdown-mode/
(use-package web-mode)    ; https://web-mode.org
(use-package nix-mode)    ; https://github.com/NixOS/nix-mode
(use-package olivetti)    ; https://github.com/rnkn/olivetti
(use-package manage-minor-mode-table)    ; turn on/off minor modes, https://github.com/jcs-elpa/manage-minor-mode-table
(use-package beacon)    ; it highlights the cursor for a second when you scroll, usefull for presentations
(use-package highlight)
(use-package smartscan)     ; for searching symbols with a single keybinding, https://github.com/mickeynp/smart-scan
(use-package hide-lines)    ; hide lines that match regexp, https://github.com/vapniks/hide-lines
(use-package copy-as-format)    ; https://github.com/sshaw/copy-as-format
(use-package isend-mode)    ; equivalent of eval-region for shell buffers and any repls
(use-package yaml-pro)    ; https://github.com/zkry/yaml-pro
(use-package minimap    ; https://github.com/dengste/minimap
  :config
  (setq minimap-recenter-type 'middle))

(use-package which-key    ; https://github.com/justbur/emacs-which-key
  :demand
  :blackout
  :config
  (which-key-mode))

;; Displays the log of pressed keybindings.
;; For showing off emacs during online meetings.
(use-package command-log-mode)    ; https://github.com/lewang/command-log-mode
(comment
 (global-command-log-mode 1)    ; and switch to the *command-log* buffer
 (global-command-log-mode -1))

(use-package jsonian)    ; for jsonian-path; https://github.com/iwahbe/jsonian
;; see also https://github.com/DamienCassou/json-navigator
;; tried json-mode, but jsons-print-path doesn't work

;; I used to use undo-tree, but completely replaces the undo system, it's memory hungry and slow.
(use-package vundo)    ; https://github.com/casouri/vundo

;; =========================================================

(use-package cider
  :config
  ;; This file is project local. Apparently, you don't need to set it in .dir-locals.el
  (setq cider-repl-history-file ".cider-repl-history")
  (setq cider-eldoc-display-context-dependent-info t)
  )

;; =========================================================

(use-package org
  ;; :straight (:type built-in)     ; otherwise I get "Org version mismatch", because org is now shipped with emacs, and having both built-in and dev version at the same time breaks things
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib

  :init
  (require 'face-remap)
  (setq ym-org-latex-preview-scale 1.0)   ; depends on the font used in emacs or just on user preference
  (defun org-latex-preview-advice (orig-func &rest args)
    (let ((old-val (copy-tree org-format-latex-options)))     ; plist-put is maybe-destructive, weird. So, we have to restore old value ourselves
      (setq org-format-latex-options (plist-put org-format-latex-options
                                                :scale
                                                (* ym-org-latex-preview-scale (expt text-scale-mode-step text-scale-mode-amount))))
      (apply orig-func args)
      (setq org-format-latex-options old-val)))
  (advice-add 'org-latex-preview :around #'org-latex-preview-advice)

  :config
  (setq-default org-adapt-indentation nil)
  (setq org-src-tab-acts-natively t)    ; default: t
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)   ; usage: org-id, org-store-link, org-insert-link, org-id-update-id-locations
  (setq org-babel-min-lines-for-block-output 9999)   ;; this forces indenting results with colons, because I don't like how #+end_example is inserted at the beginning of line, not indented at all
  (setq org-preview-latex-image-directory ".ltximg/")
  (setq org-startup-folded nil)   ; nil means only fold the :PROPERTIES: drawer
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-agenda-window-setup 'only-window)
  (setq org-agenda-sticky t)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (setq org-startup-with-inline-images t)
  (setq org-hide-leading-stars t)   ; customize the org-hide face for this, set it to light gray
  (setq org-src-preserve-indentation t)  ; from https://emacs.stackexchange.com/questions/18877/how-to-indent-without-the-two-extra-spaces-at-the-beginning-of-code-blocks-in-or
  (progn
    (setq org-confirm-babel-evaluate nil)
    (setq org-link-elisp-skip-confirm-regexp ".*"))
  (setq org-link-descriptive nil)   ; shows links as is, e.g., [[http://example.com][example]], doesn't collapse to just example
  (setq org-cycle-separator-lines 0)    ; number of blank lines between trees when folded, default: 2; set it to -1 to preserve all whitespace; mine is set to 0 to have more content on screen
  ;; (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))    ; (setq org-refile-targets '(("~/werk" :maxlevel . 3)))

  ;; strike-though text color in org-mode
  (progn
    ;; wrapping lines with +- and -+
    ;; Spaces at the beginning of lines are common in code, and a space after the plus breaks the strike-through.
    ;; Hence minuses.
    (setq org-emphasis-alist         ; from https://stackoverflow.com/questions/13190195/org-mode-strike-through-color
          (cons '("+" '(:strike-through t :foreground "gray"))
                (cl-delete "+" org-emphasis-alist :key 'car :test 'equal)))
    (defun m/strike-through-org-region (beg end)
      (interactive "r")
      (when mark-active
        (save-match-data
          (save-excursion
            (replace-regexp "^\\(.+\\)$" "+-\\1-+" nil beg end))))))
  )

;; why can't I do (require 'org-checklist) without this?
(use-package org-contrib)

(with-eval-after-load 'base16-theme
  (with-eval-after-load 'org
    ;; to customize further, first do M-x describe-text-properties, then the following
    (set-face-attribute 'org-special-keyword nil :foreground (plist-get ym-base16-colors-darker :base03))    ; base03 is for comments
    (set-face-attribute 'org-drawer nil :foreground (plist-get ym-base16-colors-darker :base03))
    (set-face-attribute 'org-date   nil :foreground (plist-get ym-base16-colors-darker :base03))
    (set-face-attribute 'org-hide   nil :foreground "grey80")
    (let ((f "#5c69cc"))
      (set-face-attribute 'org-level-1 nil :height 5.0 :foreground f)  ; "#ae1200"
      (set-face-attribute 'org-level-2 nil :height 3.0 :foreground f)
      (set-face-attribute 'org-level-3 nil :height 1.5 :foreground f)
      (set-face-attribute 'org-level-4 nil :height 1.0 :foreground f)
      (set-face-attribute 'org-level-5 nil :height 1.0 :foreground f)
      (set-face-attribute 'org-level-6 nil :height 1.0 :foreground f)
      (set-face-attribute 'org-level-7 nil :height 1.0 :foreground f)
      (set-face-attribute 'org-level-8 nil :height 1.0 :foreground f))
    ))

;; =========================================================
;; https://github.com/abo-abo/org-download
;; usage: org-download-screenshot, org-download-image, org-download-edit, org-download-delete
(use-package org-download
  :after org
  :config
  (setq-default org-download-heading-lvl nil)   ; don't take header text into account, just put everything into the specified folder
  (setq org-download-annotate-function (lambda (link)   ; don't annotate screenshots, but annotate other images
                                         (if (equal link org-download-screenshot-file)   ; see the org-download source code
                                             ""
                                           (format "#+DOWNLOADED: %s @ %s\n" link (format-time-string "%Y-%m-%d %H:%M:%S")))))
  (progn
    (defun m/org-screenshot-to-cwd ()
      (interactive)
      (let ((org-download-image-dir "."))
        (org-download-screenshot)))
    (defun m/org-screenshot-to-repo-dot-images ()
      (interactive)
      (let ((org-download-image-dir
             (expand-file-name ".images" (vc-root-dir))))
        (org-download-screenshot))))

  (setq org-download-screenshot-method "xfce4-screenshooter --region --save %s"   ; "gnome-screenshot -a -f %s" ; for macos: "screencapture -i %s"
        org-download-edit-cmd "open -a Krita %s"   ; TODO: move to preinit
        org-download-backend "wget \"%s\" -O \"%s\"")
  )

;; =========================================================

(use-package org-drill
  :after org
  ;; :commands (org-drill)
  :init
  (setq
   org-drill-spaced-repetition-algorithm  'sm2
   org-drill-learn-fraction                0.4
   ;; https://orgmode.org/worg/org-contrib/org-drill.html#text-org42a64b5
   ;; 0.4 means 5 times in the first month, and 10 times in five months

   org-drill--help-key ?`   ; the key to the left of the 1 key
   org-drill-leech-method             'warn
   org-drill-leech-failure-threshold   5
   org-drill-maximum-duration                     nil
   org-drill-maximum-items-per-session            nil
   org-drill-add-random-noise-to-intervals-p      t
   org-drill-save-buffers-after-drill-sessions-p  nil
   org-drill-hide-item-headings-p                 nil
   org-drill-overdue-interval-factor              1.2
   org-drill-days-before-old                      10
   ;; org-drill-days-before-old 7
   ;; org-drill-failure-quality                      2
   ;; org-drill-adjust-intervals-for-early-and-late-repetitions-p t    ; doesn't have any effect on sm2
   org-drill-scope 'file-no-restriction     ; https://orgmode.org/worg/org-contrib/org-drill.html#orgf1d69c8
   ;; TODO: write functions: ym-drill-file, ym-drill-math, ym-drill-eng, ym-drill-list
   ;; or within each file, set org-drill-scope to 'directory'
   )
  :config
  (defun org-drill-entry-empty-p () nil)   ; don't ignore "empty" cards -- https://emacs.stackexchange.com/questions/38440/make-org-drill-allow-reviewing-empty-cards/58568#58568
  )

;; =========================================================

(use-package org-modern    ; https://github.com/minad/org-modern
  :after org
  :config
  (setq org-modern-block-fringe 4)

  (setq org-modern-block-name nil)
  (setq org-modern-checkbox nil)
  (setq org-modern-fold-stars nil)
  (setq org-modern-footnote nil)
  (setq org-modern-hide-stars nil)
  (setq org-modern-horizontal-rule nil)
  (setq org-modern-internal-target nil)
  (setq org-modern-keyword nil)
  (setq org-modern-label-border nil)
  (setq org-modern-list nil)
  (setq org-modern-priority nil)
  (setq org-modern-priority-faces nil)
  (setq org-modern-progress nil)
  (setq org-modern-radio-target nil)
  (setq org-modern-replace-stars nil)
  (setq org-modern-star 'replace)
  (setq org-modern-table nil)
  (setq org-modern-table-horizontal nil)
  (setq org-modern-table-vertical nil)
  (setq org-modern-tag nil)
  (setq org-modern-tag-faces nil)
  (setq org-modern-timestamp nil)
  (setq org-modern-todo nil)
  (setq org-modern-todo-faces nil)

  (global-org-modern-mode)
  )

;; =========================================================

(defun ym/find-last-heading-with-tag (tag)
  "Find the last org-mode heading with a specific TAG and go to the last line of it."
  (interactive "sEnter the tag: ")
  (let ((positions '()))
    (org-map-entries
     (lambda ()
       (when (member tag (org-get-tags))
         (setq positions (cons (point) positions))))
     (format "+%s" tag))
    (when positions
      (goto-char (car positions))
      (org-end-of-subtree t)
      ;; (beginning-of-line)
      ;; (newline)
      (forward-line)
      (recenter)
      )))

(defun ym/find-and-run-last-file (regexp directory function)
  "Find the last file in DIRECTORY matching REGEXP, open it, and run FUNCTION in its buffer."
  (let* ((files (directory-files directory t regexp))
         (last-file (car (last (sort files #'string<)))))
    (when last-file
      (find-file last-file)
      (funcall function))))

(comment
 (ym/find-and-run-last-file
  "^Notes-.*\\.org$"
  "~/werk"
  (lambda () (interactive) (ym/find-last-heading-with-tag "nnnnn")))
 )

;; =========================================================

(set-face-attribute 'default nil :family "Monaco" :height 75)

(defun m/what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Colorize color names in buffers, like #0000ff or blue
(use-package rainbow-mode)
(comment
 (rainbow-mode 1)
 )

;; http://chriskempson.com/projects/base16/
;; https://github.com/belak/base16-emacs
;; https://github.com/belak/base16-emacs/blob/master/base16-theme.el
(use-package base16-theme
  :demand
  :config
  (progn
    (deftheme ym-base16-theme)
    (setq ym-base16-colors (list
			                :base00 "grey99"     ; Default Background
			                :base01 "grey88"     ; Lighter Background (Used for status bars)
			                :base02 "#d8d8d8"    ; Selection Background
			                :base03 "grey70"     ; Comments, Invisibles, Line Highlighting
			                :base04 "#585858"    ; Dark Foreground (Used for status bars)
			                :base05 "grey20"     ; Default Foreground, Caret, Delimiters, Operators
			                :base06 "#282828"    ; Light Foreground (Not often used)
			                :base07 "#181818"    ; Light Background (Not often used)
			                :base08 "#ab4642"    ; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
			                :base09 "#dc9656"    ; Integers, Boolean, Constants, XML Attributes, Markup Link Url
			                :base0A "#cca770"    ; Classes, Markup Bold, Search Text Background
			                :base0B "#a1b56c"    ; Strings, Inherited Class, Markup Code, Diff Inserted
			                :base0C "#86c1b9"    ; Support, Regular Expressions, Escape Characters, Markup Quotes
			                :base0D "#7cafc2"    ; Functions, Methods, Attribute IDs, Headings
			                :base0E "#ba8baf"    ; Keywords, Storage, Selector, Markup Italic, Diff Changed
			                :base0F "#a16946"    ; Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
			                ))
    (setq ym-base16-colors-darker    ; from :base09 to :base0E
          (let ((percent-darker 33))
            (-map-indexed (lambda (ii cc)
                            (if (and (> ii 18) (<= ii 30)
                                     (cl-oddp ii))
                                (apply 'color-rgb-to-hex `(,@(color-name-to-rgb (color-darken-name cc percent-darker)) 2))
                              cc))
                          ym-base16-colors)))
    (base16-theme-define 'ym-base16-theme ym-base16-colors-darker)
    (enable-theme 'ym-base16-theme)
    )
  )

(defun m/toggle-color-of-comments ()
  (interactive)
  (let* ((comments-colors-togglable '("grey90" "grey70" "grey30"))
         (current-comments-color (face-attribute 'font-lock-comment-face :foreground))
         (next-color-in-list (cadr (member current-comments-color comments-colors-togglable)))
         (new-comments-color (if next-color-in-list
                                 next-color-in-list
                               (car comments-colors-togglable)))
         )
    (set-face-attribute 'font-lock-comment-face nil :foreground new-comments-color)
    ;; temporarily fixing this: https://github.com/belak/base16-emacs/issues/114
    ;; font-lock-comment-delimiter-face should be base03, not base02
    (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground new-comments-color)
    ))

;; =========================================================

(set-face-attribute 'mode-line-buffer-id nil
                    :foreground "black"
                    :distant-foreground "white"
                    ;; :background "grey"
                    )
(set-face-attribute 'mode-line nil
                    :foreground "grey60"
                    :box (list
                          :color "grey22"
                          :line-width '(0 . 2))       ; used to be '(0 . 9)
                    
                    :background "grey22")
(set-face-attribute 'mode-line-inactive nil
                    :foreground "grey60"
                    :box (list
                          :color "grey22"
                          :line-width '(1 . 2))      ; used to be '(1 . 2)
                    :background "grey90")

(defun ym/align-mode-line (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

;; if performance is an issue: https://stackoverflow.com/questions/71261843/how-can-i-hide-all-major-minor-modes-from-the-mode-line-without-a-package
;; https://emacs.stackexchange.com/questions/59219/how-to-measure-the-performance-of-the-mode-line
(setq-default
 mode-line-format
 (list
  (propertize "\u200b" 'display '((raise -0.4) (height 1.6)))          ; a zero-width character in mode-line in order to make it wider vertically
  '(:eval
    (ym/align-mode-line
     ;; Left.
     '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification)

     ;; Right.
     '("   "
       (:eval (when ym-mode-line-show-line-column-position mode-line-position))         ; or maybe "%l:%c"
       ;; (vc-mode vc-mode)
       "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
     ))))
;; see also https://stackoverflow.com/questions/6672251/easily-display-useful-information-in-custom-emacs-minor-mode-mode-line-woes

;; highlight minibuffer prompt, because large monitor
(set-face-attribute 'minibuffer-prompt nil :background "light green" :foreground "black")

;; usage: M-x goto-line
(progn
  ;; We configure what's shown.
  ;; But do not show it unless toggled.
  (line-number-mode 1)
  (column-number-mode 1)
  (size-indication-mode 1)   ; like Top or 70%
  (defvar ym-mode-line-show-line-column-position nil))
(defun ym-shortcut/toggle-mode-line-show-line-column-position ()
  (interactive)
  (if ym-mode-line-show-line-column-position
      (progn (setq ym-mode-line-show-line-column-position nil)
             (force-mode-line-update))
    (progn (setq ym-mode-line-show-line-column-position t)
           (force-mode-line-update))))

;; =========================================================

;; this is the new winner-mode
(tab-bar-mode)
(tab-bar-history-mode 1)
(setq tab-bar-history-limit 100)

(set-face-attribute 'tab-bar nil
                    :foreground "red"
                    :box nil
                    ;; :box (list
                    ;;       :color "grey22"
                    ;;       :line-width '(0 . 9)
                    ;;       )
                    :background "grey80"
                    )

(set-face-attribute 'tab-bar-tab nil
                    :foreground "white"
                    :box (list
                          :color "grey55"
                          :line-width '(10 . 8)
                          )
                    :background "grey55"                    
                    )

(set-face-attribute 'tab-bar-tab-inactive nil
                    :foreground "grey36"
                    :box (list
                          :color "grey72"
                          :line-width '(10 . 0)
                          )
                    :background "grey72"                    
                    )
;; tab-bar-tab-group-current
;; tab-bar-tab-group-inactive

;; tab-bar behaviour and appearance: https://github.com/daviwil/emacs-from-scratch/blob/82f03806d90eb356b815cf514d10b6d863a2cbdc/show-notes/Emacs-Tips-06.org
;; tab-bar menu, and other arbitrary info in tab-bar: https://karthinks.com/software/a-tab-bar-menu-in-emacs/
;; https://lambdaland.org/posts/2022-07-20_adding_a_clock_to_emacs/

;; =========================================================

;; Douglas Crockford once suggested syntax highlighting based on scope, this is the closest thing so far
(use-package prism       ; there's also https://github.com/Fanael/rainbow-delimiters, but it doesn't work for python
  :config
  (setq prism-comments nil)
  (setq prism-parens t)
  (setq prism-strings t)
  (prism-set-colors            ; used to be :num 1
    :desaturations '(40)                  ; used to be (cl-loop for i from 0 below 16 collect 60)
    :lightens '(0)
    :colors (list
             (color-darken-name "red" 20)
             (color-darken-name "green" 30)
             (color-lighten-name "purple" 10)
             (color-darken-name "blue" 10))
    :parens-fn (lambda (color) (color-lighten-name (color-saturate-name color 1000) 10))
    :strings-fn #'identity)
  (add-hook 'emacs-lisp-mode-hook 'prism-mode)
  (add-hook 'python-mode-hook 'prism-whitespace-mode)
  (add-hook 'clojure-mode-hook 'prism-mode)
  ;; found a comment in on a forum: nothing prevents you from doing: :hook ((markdown-mode . visual-line-mode) (markdown-mode flyspell-mode)) although I'd do the reverse, as in (use-package flyspell-mode :hook markdown-mode)
  )

;; In case prism is slow we have this.
(use-package rainbow-delimiters
  :config
  ;; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (setq rainbow-delimiters-max-face-count 6)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(rainbow-delimiters-depth-1-face ((t (:foreground "firebrick3"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "dodger blue"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "green3"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "peru"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "grey50"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "black"))))
   )
  )

;; =========================================================

(setq global-hl-line-sticky-flag nil)   ; only appear in one window
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows t)   ;;  displays a cursor related to the usual cursor type
(setq ym-hl-line-color-normal-mode-color "#e6eef7")
(set-face-background 'hl-line ym-hl-line-color-normal-mode-color)
(set-cursor-color "#ab4642")

;; =========================================================

;; disable version control enabled by default, it slows down emacs, and i don't use it
;; this probably breaks some functions like vc-annotate and vc-diff
;; also read the faq, disabling the built-in version control is no longer recommended
;; https://magit.vc/manual/magit/Should-I-disable-VC_003f.html
(setq vc-handled-backends nil)

(use-package f)   ; https://github.com/rejeep/f.el -- Modern API for working with files and directories in Emacs

(use-package magit
  :config
  ;; (setq magit-git-debug nil)   ; useful for checking out the actual commands behind the views

  ;; https://magit.vc/manual/magit/Action-Confirmation.html
  (setq magit-no-confirm '(
                           stage-all-changes
                           unstage-all-changes
                           repolist-all             ; Fetch all repos, when none is selected in the magit-list-repositories.
                           ;; set-and-push      ; When pushing to the upstream or the push-remote and that isn’t actually configured yet, then the user can first set the target.
                           ))

  (defun ym/magit-repolist-column--date-last-touched (_)
    (string-trim-right (shell-command-to-string            ; test using '%TY-%Tm-%Td %p\n'
                        ;; another option is to use -path ./.git, but -name '.git' also works for submodules
                        "THE_FIND_COMMAND=$(command -v gfind || echo find) && $THE_FIND_COMMAND . -name '.git' -prune -o -type f -printf '%TY-%Tm-%Td\n' | sort -r | head -1"
                        )))
  (setq magit-repository-directories
        (let* ((dirs (list
                      "~/.setuplets"
                      "~/werk"
                      "~/workspaces"
                      ))
               (w-subdirs (append
                           (f-directories "~/" (lambda (dir) (s-matches? "\/\.emacs\.d.*$" dir)))
                           (f-directories "~/workspaces" (lambda (dir) (not (f-hidden-p dir 'last))))
                           (f-directories "~/wurkspaces" (lambda (dir) (not (f-hidden-p dir 'last))))))
               (w-subdirs-relative-to-home (mapcar (lambda (d) (f-short d))    ; I'd like to see the ~/ in front of dirs in the list
                                                   w-subdirs))
               (my-dirs (append dirs w-subdirs-relative-to-home)))
          (mapcar (lambda (d) (cons d 0))       ; magit-repository-directories is a list of cons-cells, where the cdr is depth, 0 means only the dir itself
                  my-dirs)))

  (setq magit-repolist-columns      ; usage: M-x tabulated-list-sort
        '(
          ("mtime" 11 ym/magit-repolist-column--date-last-touched)   ; can't figure out how to use (:sort <)
          ("version" 30 magit-repolist-column-version)           ; (:sort magit-repolist-version<)
          ("upstream" 12 magit-repolist-column-upstream)
          ("B<U" 3 magit-repolist-column-unpulled-from-upstream ((:right-align t)))      ; (:sort <)
          ("B>U" 3 magit-repolist-column-unpushed-to-upstream ((:right-align t)))        ; (:sort <)
          ("B<P" 3 magit-repolist-column-unpulled-from-pushremote ((:right-align t)))
          ("B>P" 3 magit-repolist-column-unpushed-to-pushremote ((:right-align t)))
          ;; ("#b" 3 magit-repolist-column-branches)
          (" " 3 magit-repolist-column-flags)   ; N, U, and S mean: uNtracked, Unstaged, Staged
          ("#s" 3 magit-repolist-column-stashes)
          ("branch" 30 magit-repolist-column-branch ((:right-align t)))
          ("path" 300 magit-repolist-column-path)
          ))
  )

(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo"))
  )

(comment 
 (use-package github-review)
 ;; This is a cool package for leaving comments in PRs,
 ;; Maybe later.
 )

;; =========================================================

;; also try https://github.com/dgutov/diff-hl/

(use-package git-gutter
  ;; :config
  ;; (add-hook 'ruby-mode-hook 'git-gutter-mode)

  :custom
  (git-gutter:update-interval 2)         ; default: 0, which means update on file save
  (git-gutter:lighter " gg")   ; modeline
  (git-gutter:ask-p nil)    ; revert hunks without confirmation, this is safe, as there's undo

  ;; (custom-set-variables
  ;;  '(git-gutter:modified-sign "  ") ;; two space
  ;;  '(git-gutter:added-sign "++")    ;; multiple character is OK
  ;;  '(git-gutter:deleted-sign "--"))
  ;; (set-face-background 'git-gutter:modified "purple") ;; background color
  ;; (set-face-foreground 'git-gutter:added "green")
  ;; (set-face-foreground 'git-gutter:deleted "red")
  )

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [#b00111100] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [#b01100110] nil nil '(center repeated))    ; can also be [#b01100110 #b00000000]
  (define-fringe-bitmap 'git-gutter-fr:deleted [#b00111100] nil nil '(center repeated))
  (set-face-foreground 'git-gutter-fr:added    "green")
  (set-face-foreground 'git-gutter-fr:modified "purple")
  (set-face-foreground 'git-gutter-fr:deleted  "red")

  ;; bitmaps can be drawn this way:
  ;; (fringe-helper-define 'git-gutter-fr:added nil
  ;;                     "...XX..."
  ;;                     "...XX..."
  ;;                     "...XX..."
  ;;                     "XXXXXXXX"
  ;;                     "XXXXXXXX"
  ;;                     "...XX..."
  ;;                     "...XX..."
  ;;                     "...XX...")
  )

;; =========================================================

(use-package ob-nix
  :after org)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (sqlite . t)
   (clojure . t)
   (java . t)
   (nix . t)
   ;; (javascript . t)
   ;; (lisp . t)
   ;; (haskell . t)
   ;; (R . t)
   ;; (sql . t)
   ;; (typescript . t)     ; (use-package ob-typescript)
   ;; (mongo . t)     ; (use-package ob-mongo)
   ;; (jupyter . t)
   ;; (http . t)    ; it's better to use curl in org blocks
   ))

;; =========================================================

(use-package projectile
  :config
  (projectile-mode +1)

  (setq projectile-switch-project-action #'projectile-dired)

  ;; projectile-add-known-project
  )

;; =========================================================

;; (use-package ag)
;; (use-package projectile-ripgrep)
;; (use-package deadgrep)
;; (use-package emacs-wgrep)   ;; for editing grep buffer; deadgrep support -- https://github.com/mhayashi1120/Emacs-wgrep/pull/58

(use-package rg
  :config
  (rg-enable-menu)
  ;; https://rgel.readthedocs.io/en/latest/configuration.html
  ;; https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md#configuration-file
  )

;; =========================================================

(use-package eldoc
  :straight nil
  :blackout
  :config
  ;; (eldoc-mode -1)
  )

;; =========================================================

(ym-define-key (kbd "M-<tab>") #'completion-at-point)      ; corfu is integrated

(setq tab-always-indent t)
(setq c-tab-always-indent t)

(use-package dabbrev
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; tab completion is disabled, direct usage: M-x completion-at-point
(use-package corfu    ; https://github.com/minad/corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (
                              ;; corfu-info
                              corfu-popupinfo
                              ))     ; this is the way to add these packages, see https://www.reddit.com/r/emacs/comments/z6sk1f/how_to_update_corfudoc_to_the_new_corfuinfo/
  :custom
  (corfu-auto nil)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 4)
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  
  ;; :bind
  ;; (:map corfu-map
  ;;       ("SPC" . corfu-insert-separator))      ; no need for this, when using flex
  )

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless flex)   ; '(orderless flex) lets you input multiple parts of words out of order, separated by space; flex lets you avoid space, this is faster to type
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package cape
  ;; :bind
  ;; (("C-c p p" . completion-at-point) ;; capf
  ;;  ("C-c p t" . complete-tag)        ;; etags
  ;;  ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;;  ("C-c p h" . cape-history)
  ;;  ("C-c p f" . cape-file)
  ;;  ("C-c p k" . cape-keyword)
  ;;  ("C-c p s" . cape-elisp-symbol)
  ;;  ("C-c p e" . cape-elisp-block)
  ;;  ("C-c p a" . cape-abbrev)
  ;;  ("C-c p l" . cape-line)
  ;;  ("C-c p w" . cape-dict)
  ;;  ("C-c p :" . cape-emoji)
  ;;  ("C-c p \\" . cape-tex)
  ;;  ("C-c p _" . cape-tex)
  ;;  ("C-c p ^" . cape-tex)
  ;;  ("C-c p &" . cape-sgml)
  ;;  ("C-c p r" . cape-rfc1345))
  
  :init
  (setq cape-dabbrev '(    ; The order of the functions matters, the first function returning a result wins.
                       ;; Note that the list of buffer-local completion functions takes precedence over the global list.
                       cape-dabbrev
                       cape-file
                       cape-keyword
                       cape-elisp-symbol
                       cape-elisp-block
                       tags-completion-at-point-function
                       ;; cape-history
                       ;; cape-tex
                       ;; cape-sgml
                       ;; cape-rfc1345
                       ;; cape-abbrev
                       ;; cape-dict
                       ;; cape-line
                       ))
  )

(use-package dumb-jump
  :config
  (add-to-list 'xref-backend-functions 'dumb-jump-xref-activate t)   ; to the end of list, which means fall back to dumb-jump when there are no better options
  ;; (setq dumb-jump-force-searcher 'rg)   ; tries searches in this order: git-grep, ag, rg, grep
  ;; (dumb-jump-debug t)   ; try to jump and see *messages*
  )

;; (setq path-to-ctags "/opt/local/bin/ctags")        https://gist.github.com/kborling/13f2300e60ae4878d5d96f5f4d041664#file-init-el-L414

;; =========================================================

(use-package embark
  :demand
  :bind
  (("C-M-." . embark-act)
   ;; ("C-M-;" . embark-dwim)
   ;; ("C-h B" . embark-bindings)
   )

  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; =========================================================

;; https://emacs.stackexchange.com/questions/3564/auto-insert-a-skeleton-and-run-a-function-when-creating-a-new-file
;; https://www.gnu.org/software/emacs/manual/html_node/autotype/Autoinserting.html
;; https://www.emacswiki.org/emacs/AutoInsertMode
(use-package autoinsert
  :config
  (setq auto-insert-query nil)
  ;; (setq auto-insert-directory (locate-user-emacs-file "templates"))
  ;; (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  (define-auto-insert
    '("\\.sh\\'" . "Bash skeleton")
    '("Description"
      "#!/usr/bin/env bash\n\n"
      ))
  )

(add-hook
 'after-save-hook
 'executable-make-buffer-file-executable-if-script-p)   ;; chmod u+x current file when there's shebang

;; =========================================================

(load-file (expand-file-name (convert-standard-filename "init-hydra.el") user-emacs-directory))

;; =========================================================

