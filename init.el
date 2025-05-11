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

(progn
 (setq-default truncate-lines t)    ;; In case if you're confused about the word-wrap variable, here's a clarification: Instead of setting this variable directly, most users should use Visual Line mode.

 (global-visual-line-mode -1)   ;; Now toggle visual-line-mode per buffer via hydra.
 (with-eval-after-load 'org
   (add-hook 'org-mode-hook #'visual-line-mode))
 )

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
(setq recentf-max-saved-items 100)   ; default: 20

(save-place-mode 1)
;; save-place-forget-unreadable-files is t by default, this make quitting emacs slow
;; to disable it in a file: ;; -*- eval: (save-place-local-mode -1); -*-

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

(defun ym/define-key (key func)   ; Not sure if we really need to define keys in both maps, but just in case.
  (global-set-key key func)
  (define-key ym-keys-minor-mode-map key func))

(defun ym-undefined-key-message () (interactive) (message "undefined keybinding yet, see ym/define-key"))

;; -----------------------------------------------

;; Unbind all keybindings with:
;; super
;; shift-super
;; meta
;; super-meta
;; shift-meta

(mapcar
 (lambda (x)
   (ym/define-key (kbd (concat "s-" (list x)))
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
   (ym/define-key (kbd (concat "M-" (list x)))
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
   (ym/define-key (kbd (concat "M-s-" (list x)))
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
   (ym/define-key (kbd (concat "M-" (list x)))
                  #'ym-undefined-key-message))
 (concat
  "ABCDEFGHIJKLMNOPQRSTUVWYXZ"    ; M-S-a doesn't work for some reason, but M-A does
  "!@#$%^&*()_+{}:|<>?~"
  "\""
  ))

(ym/define-key (kbd "C-z") nil)    ; I constantly hit this unintentionally.

;; https://emacs.stackexchange.com/questions/14755/how-to-remove-bindings-to-the-esc-prefix-key/14759#14759
;; Let the escape key do its thing. Yeah, I feel the judgemental stare.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))   ; = quit

;; -----------------------------------------------

;; Also clear combinations involving return, space, backspace, tab, and arrow keys:

(ym/define-key (kbd "<s-return>") nil)
(ym/define-key (kbd "<M-s-return>") nil)
(ym/define-key (kbd "<S-s-return>") nil)
(ym/define-key (kbd "<M-return>") nil)
(ym/define-key (kbd "<M-S-return>") nil)
(ym/define-key (kbd "<S-return>") nil)

(ym/define-key (kbd "<s-SPC>") nil)
(ym/define-key (kbd "M-s-SPC") nil)   ; if this doesn't work, try (kbd "M-s- ")
(ym/define-key (kbd "<S-s-SPC>") nil)
(ym/define-key (kbd "<M-SPC>") nil)
(ym/define-key (kbd "<M-S-SPC>") nil)
(ym/define-key (kbd "<S-SPC>") nil)

(ym/define-key (kbd "<s-backspace>") nil)
(ym/define-key (kbd "<M-s-backspace>") nil)
(ym/define-key (kbd "<S-s-backspace>") nil)
(ym/define-key (kbd "<M-backspace>") nil)
(ym/define-key (kbd "<M-S-backspace>") nil)
(ym/define-key (kbd "<S-backspace>") nil)

(ym/define-key (kbd "<s-up>") nil)
(ym/define-key (kbd "<s-down>") nil)
(ym/define-key (kbd "<s-left>") nil)
(ym/define-key (kbd "<s-right>") nil)
(ym/define-key (kbd "<M-up>") nil)
(ym/define-key (kbd "<M-down>") nil)
(ym/define-key (kbd "<M-left>") nil)
(ym/define-key (kbd "<M-right>") nil)
(ym/define-key (kbd "<M-s-up>") nil)
(ym/define-key (kbd "<M-s-down>") nil)
(ym/define-key (kbd "<M-s-left>") nil)
(ym/define-key (kbd "<M-s-right>") nil)
(ym/define-key (kbd "<S-s-up>") nil)
(ym/define-key (kbd "<S-s-down>") nil)
(ym/define-key (kbd "<S-s-left>") nil)
(ym/define-key (kbd "<S-s-right>") nil)
(ym/define-key (kbd "<S-M-up>") nil)
(ym/define-key (kbd "<S-M-down>") nil)
(ym/define-key (kbd "<S-M-left>") nil)
(ym/define-key (kbd "<S-M-right>") nil)

;; (ym/define-key (kbd "<S-up>") nil)     ; these four are used by org-mode
;; (ym/define-key (kbd "<S-down>") nil)
;; (ym/define-key (kbd "<S-left>") nil)
;; (ym/define-key (kbd "<S-right>") nil)

(ym/define-key (kbd "<M-tab>") nil)
(ym/define-key (kbd "<C-tab>") nil)
(ym/define-key (kbd "<C-M-tab>") nil)
(ym/define-key (kbd "<M-s-tab>") nil)
(progn
  (ym/define-key (kbd "<s-tab>") nil)   ; = cmd-tab, binding is useless
  (ym/define-key (kbd "<S-s-tab>") nil))   ; = cmd-tab, binding is useless
(progn
  (ym/define-key (kbd "C-S-<tab>") nil)     ; these two should be the same keybinding
  (ym/define-key (kbd "C-S-<iso-lefttab>") nil))
(progn
  (ym/define-key (kbd "<M-S-tab>") nil)
  (ym/define-key (kbd "<M-S-iso-lefttab>") nil))

;; (ym/define-key (kbd "<backtab>") nil)   ; = <S-tab>, it is probably used by org-mode.

;; M-S-- doesn't work, because alt+shift toggles layout.
;; (ym/define-key (kbd "M-_") (lambda () (interactive) (insert "—")))

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

(ym/define-key (kbd "s-z") #'undo)
(ym/define-key (kbd "s-x")
               (lambda (beg end)
                 (interactive "r")
                 (prog1
                     (kill-region beg end)
                   (setq deactivate-mark nil))))   ; leave the region highlighted after the cut
(ym/define-key (kbd "s-c")
               (lambda (beg end)
                 (interactive "r")
                 (prog1
                     (kill-ring-save beg end)
                   (setq deactivate-mark nil))))   ; leave the region highlighted after the copy

;;;; Here are two functions:
;;;; yank-and-indent --- always indents, and this is undesirable, because I'd like to have the original text pasted untouched
;;;; yank-for-indent --- doesn't indent, but leaves a mark so that it can be activated, e.g., using exchange-point-and-mark and the text indented
;; (defun yank-and-indent ()
;;   "Yank and then indent the newly formed region according to mode."
;;   (interactive)
;;   (let ((point-before (point)))
;;     (yank)
;;     (indent-region point-before (point))
;;     ))
(defun yank-for-indent ()
  "Yank and mark the initial position so that the mark can be activated and the text indented."
  (interactive)
  (if mark-active
      (let* ((beg (save-excursion (if (> (point) (mark)) (exchange-point-and-mark))
			                      (point)))
	         (end (save-excursion (if (<= (point) (mark)) (exchange-point-and-mark))
			                      (point))))
        (delete-region beg end)
        ))
  (let ((point-before (point)))
    (yank)
    ;; (set-mark point-before)
    (push-mark point-before t)
    ))
(ym/define-key (kbd "s-v") #'yank-for-indent)
(ym/define-key (kbd "S-<insert>") #'yank-for-indent)    ; clipboard managers do this

;; =========================================================

(ym/define-key (kbd "s-i") 'previous-line)
(ym/define-key (kbd "s-k") 'next-line)
(ym/define-key (kbd "s-j") 'backward-char)
(ym/define-key (kbd "s-l") 'forward-char)

;; mwim = move where I mean
;; https://github.com/alezost/mwim.el
(use-package mwim
  :config
  (ym/define-key (kbd "s-u") #'mwim-beginning-of-code-or-line)    ; I used to have custom functions for this, see git history
  (ym/define-key (kbd "s-o") #'mwim-end))

(ym/define-key (kbd "s-s") (lambda () (interactive) (ignore-error 'user-error (windmove-left))))
(ym/define-key (kbd "s-d") (lambda () (interactive) (ignore-error 'user-error (windmove-down))))
(ym/define-key (kbd "s-f") (lambda () (interactive) (ignore-error 'user-error (windmove-right))))
(ym/define-key (kbd "s-e") (lambda () (interactive) (ignore-error 'user-error (windmove-up))))

(use-package buffer-move)
(ym/define-key (kbd "s-S") (lambda () (interactive) (ignore-error 'error (buf-move-left))))
(ym/define-key (kbd "s-D") (lambda () (interactive) (ignore-error 'error (buf-move-down))))
(ym/define-key (kbd "s-F") (lambda () (interactive) (ignore-error 'error (buf-move-right))))
(ym/define-key (kbd "s-E") (lambda () (interactive) (ignore-error 'error (buf-move-up))))

(ym/define-key (kbd "s-w") (lambda () (interactive) (ignore-error 'user-error (tab-bar-switch-to-prev-tab))))
(ym/define-key (kbd "s-r") (lambda () (interactive) (ignore-error 'user-error (tab-bar-switch-to-next-tab))))

(ym/define-key (kbd "s-W") (lambda () (interactive) (tab-bar-move-tab -1)))
(ym/define-key (kbd "s-R") (lambda () (interactive) (tab-bar-move-tab 1)))

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

(ym/define-key (kbd "s-!") (lambda () (interactive) (ignore-error 'error (delete-window))))
(ym/define-key (kbd "s-1") 'ym/toggle-single-window)
(ym/define-key (kbd "s-@") (lambda () (interactive) (ignore-error 'error (split-window-below))))
(ym/define-key (kbd "s-#") (lambda () (interactive) (ignore-error 'error (split-window-right))))
(ym/define-key (kbd "s-2") 'tab-bar-history-back)
(ym/define-key (kbd "s-3") 'tab-bar-history-forward)

;; =========================================================

(use-package hydra)
(use-package pretty-hydra)
(use-package major-mode-hydra)     ;; https://github.com/jerrypnz/major-mode-hydra.el

;; =========================================================

;; completion for hydras

;; this is my slightly modified version of Sacha Chua's code
;; made completions look like "f: follow mode"

;; https://sachachua.com/blog/2021/04/emacs-hydra-allow-completion-when-i-can-t-remember-the-command-name/
;; https://sachachua.com/dotemacs/index.html#hydra-completion
;; https://www.reddit.com/r/emacs/comments/123l17j/completions_of_functions_in_hydra_when_you_forget/

(defun my/hydra-format-head (h)
  (let ((key-binding (elt h 0))
        (hint (elt h 2))
        (cmd (and (elt h 1) (prin1-to-string (elt h 1)))))
    (format "%s: %s" key-binding hint)
    ;; (if cmd
    ;;     (format "%s (%s) - %s" hint key-binding cmd)
    ;;   (format "%s (%s)" hint key-binding))
    ))

(defun my/hydra-current-heads-as-candidates ()
  (let* ((base (replace-regexp-in-string "/body$" "" (symbol-name hydra-curr-body-fn)))
         (heads-plist (symbol-value (intern (concat base "/heads-plist"))))
         (heads-plist-values (cl-loop for (key value) on heads-plist by 'cddr collect value))
         (heads (apply #'append heads-plist-values)))
    (mapcar (lambda (h)
              (cons (my/hydra-format-head h) (hydra--head-name h (intern base))))
            heads)))      ; fixed: used to be (symbol-value (intern (concat base "/heads"))), but instead of /heads-plisp it somehow doesn't contain hints, they all are nil

(defun my/hydra-execute-extended (prefixarg &optional command-name typed)
  (declare (interactive-only command-execute))
  (interactive (let ((execute-extended-command--last-typed nil)
                     (candidates (my/hydra-current-heads-as-candidates)))
                 (hydra-keyboard-quit)
                 (list current-prefix-arg
                       (completing-read "Cmd: " candidates)
                       execute-extended-command--last-typed)))
  (let* ((candidates (my/hydra-current-heads-as-candidates))
         (bind (assoc-default command-name candidates 'string=)))
    (cond
     ((null bind) nil)
     ((hydra--callablep bind) (call-interactively bind)))))

(with-eval-after-load 'hydra
  (define-key hydra-base-map (kbd "<tab>") #'my/hydra-execute-extended))

;; =========================================================

;; jump to definition of hydra

(defun ym/go-to-definition-of-hydra ()
  (interactive)
  (hydra-keyboard-quit)
  (find-function hydra-curr-body-fn)
  )

(with-eval-after-load 'hydra
  (define-key hydra-base-map (kbd "M-s-u") #'ym/go-to-definition-of-hydra))

;; =========================================================

;; from https://github.com/abo-abo/hydra/issues/268
;; delay showing of hydra while when we do something like navigating windows or moving buffers
;; without this code the hydra is shown after the very first keypress

;; usage: ("j" (csb-wrap-ignore-error 'user-error (windmove-left)) "windmove-left")

(defun timer-reset (timer-sym secs fun)
  (let ((timer (and (boundp timer-sym) (symbol-value timer-sym))))
    (if (timerp timer)
        (cancel-timer timer)
      (setq timer (set timer-sym (timer-create))))
    (timer-set-time
     timer
     (timer-relative-time (current-time) secs))
    (timer-set-function timer fun)
    (timer-activate timer)))

(defun csb-hide ()
  ;; (hydra-disable)
  (hydra-set-property 'hydra-window :verbosity 0)
  ;; (hydra-window/body)
  )

(defun csb-show ()
  (hydra-set-property 'hydra-window :verbosity t)      ; 1 is for terminal emacs, otherwise t; see the hydra-show-hint function definition
  (let ((hydra-active (eq hydra-curr-map hydra-window/keymap))
        (f (timer--function hydra-message-timer))
        (a (timer--args hydra-message-timer))
        )
    (when hydra-active
      (hydra-window/body)
      (progn   ; show hydra immediately, without the idle delay, because we already waited for the moment to show
       (cancel-timer hydra-message-timer)
       (apply f a))
      )))

(defmacro csb-wrap (&rest body)
  `(progn
     ,@body
     (csb-hide)
     (timer-reset 'csb-timer 0.7 'csb-show)
     ))

(defmacro csb-wrap-ignore-error (condition &rest body)
  `(progn
     (ignore-error ,condition ,@body)
     (csb-hide)
     (timer-reset 'csb-timer 0.7 'csb-show)))

(defmacro csb-wrap-ignore-all-errors (&rest body)
  `(progn
     (ignore-errors ,@body)
     (csb-hide)
     (timer-reset 'csb-timer 0.7 'csb-show)))

;; =========================================================

(defun ym/delete-current-line-or-region ()
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

(defun ym/kill-current-line-or-region ()
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

(defun ym/copy-current-line-or-region ()
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

(defun ym/duplicate-current-line-or-region (arg)   ; took it from here: http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
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

(defun ym/duplicate-and-comment-current-line-or-region (arg)   ; took it from here: http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
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

(defun ym/backward-kill-word ()
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

(ym/define-key (kbd "s-S-<backspace>") #'ym/delete-current-line-or-region)
(ym/define-key (kbd "s-M-x") #'ym/kill-current-line-or-region)
(ym/define-key (kbd "s-M-c") #'ym/copy-current-line-or-region)
(ym/define-key (kbd "s-/") #'ym/comment-or-uncomment-line-or-region)
(ym/define-key (kbd "M-s-/") #'ym/duplicate-and-comment-current-line-or-region)
(ym/define-key (kbd "M-d") #'ym/duplicate-current-line-or-region)
(ym/define-key (kbd "<M-backspace>") #'ym/backward-kill-word)
(ym/define-key (kbd "<s-backspace>") #'ym/backward-kill-word)

(use-package drag-stuff
  :config
  ;; (drag-stuff-mode t)    ; I use its functions directly
  ;; drag-stuff-before-drag-hook
  ;; drag-stuff-after-drag-hook

  ;; hidden in a hydra
  )

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
(defvar my/scroll-command---point-before-scrolling nil)
(make-variable-buffer-local 'my/scroll-command---virtual-cur-line)
(make-variable-buffer-local 'my/scroll-command---n-lines-from-top)
(make-variable-buffer-local 'my/scroll-command---column-before-scrolling)
(make-variable-buffer-local 'my/scroll-command---point-before-scrolling)

;; Please be aware, that this can under- or overscroll when there are images or tall lines in buffer.
;; The trade-off here is that this is more predictable to me, because I can get back to the exact location from where I started scrolling.
;; Works perfectly in code files though.
(defun my/scroll-page-command (n-lines)
  (interactive)
  (unless
      (or (eq last-command #'my/scroll-page-up)
          (eq last-command #'my/scroll-page-down))
    (setq my/scroll-command---virtual-cur-line (line-number-at-pos (point)))
    (setq my/scroll-command---n-lines-from-top (- my/scroll-command---virtual-cur-line (line-number-at-pos (window-start))))
    (setq my/scroll-command---column-before-scrolling (current-column)))
  (let* ((cur-line my/scroll-command---virtual-cur-line)
         (next-screen-line (+ cur-line n-lines)))
    (unless (or (< next-screen-line (line-number-at-pos (beginning-of-buffer)))
                (> next-screen-line (line-number-at-pos (end-of-buffer))))
      (goto-line next-screen-line)
      (recenter my/scroll-command---n-lines-from-top)
      (setq my/scroll-command---virtual-cur-line next-screen-line)
      (move-to-column my/scroll-command---column-before-scrolling))))

(defun my/scroll-page-up ()   (interactive) (my/scroll-page-command (- (window-body-height))))
(defun my/scroll-page-down () (interactive) (my/scroll-page-command (+ (window-body-height))))
(ym/define-key (kbd "s-m") #'my/scroll-page-up)
(ym/define-key (kbd "s-n") #'my/scroll-page-down)

(defun my/scroll-a-little-command (n-lines)
  (interactive)
  (let ((we-are-scrolling-already (or (eq last-command #'my/scroll-a-little-up)
                                      (eq last-command #'my/scroll-a-little-down))))
    (unless we-are-scrolling-already
      (setq my/scroll-command---virtual-cur-line (line-number-at-pos (point)))
      (setq my/scroll-command---n-lines-from-top (- my/scroll-command---virtual-cur-line (line-number-at-pos (window-start))))
      (setq my/scroll-command---column-before-scrolling (current-column))
      (setq my/scroll-command---point-before-scrolling (point))))

  (let* ((next-screen-line (+ my/scroll-command---virtual-cur-line n-lines))
         (next-screen-line-is-out-of-range (or (< next-screen-line (line-number-at-pos (point-min)))
                                               (> next-screen-line (line-number-at-pos (point-max))))))


    ;;; we have to reconcile the following with setting this var after this block
    ;; (when next-screen-line-is-out-of-range
    ;;   (setq my/scroll-command---n-lines-from-top (- my/scroll-command---n-lines-from-top n-lines)))


    (unless next-screen-line-is-out-of-range
      (goto-line next-screen-line)
      (setq my/scroll-command---virtual-cur-line next-screen-line)
      (move-to-column my/scroll-command---column-before-scrolling)
      ;; (message "my/scroll-command---n-lines-from-top : %s" my/scroll-command---n-lines-from-top)
      )

    (recenter my/scroll-command---n-lines-from-top)
     
    )
  
  ;; )
  
  (let ((point-before-scrolling-is-visible-after-recenter
         (and (<= (line-number-at-pos (window-start)) (line-number-at-pos my/scroll-command---point-before-scrolling))
              (<= (line-number-at-pos my/scroll-command---point-before-scrolling) (line-number-at-pos (window-end nil t))))))
    (when point-before-scrolling-is-visible-after-recenter 
      (goto-char my/scroll-command---point-before-scrolling)
      (setq my/scroll-command---virtual-cur-line (line-number-at-pos my/scroll-command---point-before-scrolling))
      (setq my/scroll-command---n-lines-from-top (- my/scroll-command---virtual-cur-line (line-number-at-pos (window-start)))))))





(defun my/scroll-a-little-up ()   (interactive) (my/scroll-a-little-command (- (/ (window-body-height) 5))))
(defun my/scroll-a-little-down () (interactive) (my/scroll-a-little-command (+ (/ (window-body-height) 5))))
(ym/define-key (kbd "s-.") #'my/scroll-a-little-up)    ; used to be just 3, independently of window-body-height
(ym/define-key (kbd "s-,") #'my/scroll-a-little-down)

;; =========================================================

(setq text-scale-mode-step 1.1)
(ym/define-key (kbd "s--") #'text-scale-decrease)
(ym/define-key (kbd "s-=") #'text-scale-increase)
(ym/define-key (kbd "s-0") (lambda () (interactive) (text-scale-adjust 0)))

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
;; (ym/define-key (kbd "s-s") 'ym-search-selection-or-isearch-forward)
;; (ym/define-key (kbd "s-r") 'ym-search-selection-or-isearch-backward)
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
  (ym/define-key (kbd "s-;") #'avy-goto-word-1)
  ;; (ym/define-key (kbd "s-;") #'avy-goto-word-2)
  ;; (ym/define-key (kbd "s-;") #'avy-goto-char-timer)
  ;; (ym/define-key (kbd "s-^") #'avy-goto-parens)   ; "S-s-;" -- this is not a usual ^, it's a unicode character

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
   ido-use-faces t
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

;; ido virtual buffers now look like "[recentf] init.el ~/.emacs.d/"
;; they look the same in my short mode-line, see its configuration
(defun ido-virtual-buffers-names-advice (orig-func &rest args)
  (let ((orig-file-name-nondirectory (symbol-function 'file-name-nondirectory)))
   (cl-letf (((symbol-function 'file-name-nondirectory)
              (lambda (path)
                (concat "[recentf] "
                        (funcall orig-file-name-nondirectory path)
                        " "
                        (abbreviate-file-name (file-name-as-directory (file-name-directory path)))
                        ))
             ))
    (apply orig-func args)
    )))
(advice-add 'ido-add-virtual-buffers-to-list :around #'ido-virtual-buffers-names-advice)
;; (advice-remove 'ido-add-virtual-buffers-to-list #'ido-virtual-buffers-names-advice)

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

(ym/define-key (kbd "s-b") #'ido-switch-buffer)
(ym/define-key (kbd "s-B") #'ibuffer)
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

;; =========================================================

(setq undo-limit (* 100 1024)) ; 100KB
(setq undo-strong-limit (* 150 1024)) ; 150KB
(setq undo-outer-limit (* 50 1024 1024)) ; 50MB

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
  (setq org-startup-with-inline-images t)
  (setq org-hide-leading-stars t)   ; customize the org-hide face for this, set it to light gray
  (setq org-src-preserve-indentation t)  ; from https://emacs.stackexchange.com/questions/18877/how-to-indent-without-the-two-extra-spaces-at-the-beginning-of-code-blocks-in-or
  (progn
    (setq org-confirm-babel-evaluate nil)
    (setq org-link-elisp-skip-confirm-regexp ".*"))
  (setq org-link-descriptive nil)   ; shows links as is, e.g., [[http://example.com][example]], doesn't collapse to just example
  (setq org-cycle-separator-lines 0)    ; number of blank lines between trees when folded, default: 2; set it to -1 to preserve all whitespace; mine is set to 0 to have more content on screen
  (setq org-fontify-quote-and-verse-blocks t)   ; otherwise they are not highlighted
  (setq org-tags-column 40)

  (setq org-log-into-drawer t      ; log both into :LOGBOOK:
      org-clock-into-drawer t
      org-log-repeat nil          ; disable :LAST_REPEAT:
      )

  ;; (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))    ; (setq org-refile-targets '(("~/werk" :maxlevel . 3)))

  (setq org-agenda-files '("~/werk"))
  
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

  (require 'org-checklist)   ; for the reset_check_boxes property of repeated tasks
  )

;; why can't I do (require 'org-checklist) without this?
(use-package org-contrib)

(use-package org-ql)
(use-package org-super-agenda)


;; =========================================================

;; usage: org-download-screenshot, org-download-image, org-download-edit, org-download-delete
(use-package org-download    ; https://github.com/abo-abo/org-download
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

  (setq org-download-screenshot-method
        "sleep 1 && gnome-screenshot -a -f %s"       ; sleep is a hack here, since screenshot apps used to let me do alt-tab, but after an update this is broken
        ;; "gnome-screenshot -a -f %s"
        ;; "xfce4-screenshooter --region --save %s"
        ;; "screencapture -i %s"   ; macos
        )
  (setq org-download-edit-cmd "open -a Krita %s")   ; TODO: move to preinit
  (setq org-download-backend "wget \"%s\" -O \"%s\"")
  )

;; =========================================================

(use-package isend-mode)    ; equivalent of eval-region for shell buffers and any repls

(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
;; Usage: M-x compile-goto-error or simply the enter key
;; init-hydras.el:30:3

;; =========================================================

(use-package find-file-in-project    ; https://github.com/redguardtoo/find-file-in-project
  :config
  (setq ffip-prefer-ido-mode t)

  ;; find-file-in-project-at-point
  ;; find-file-in-project-by-selected     ; in combination with er/expand-region, because early-init.el:3 works, but early-init.el:3:3 doesn't
  ;; find-file-with-similar-name
  ;; find-directory-in-project-by-selected
  ;; find-file-in-current-directory
  ;; find-file-in-current-directory-by-selected
  ;; etc
  )

;; alternatives:
;; I like this one: https://github.com/emacsmirror/emacswiki.org/blob/master/find-file-with-line-number
;; https://emacs.stackexchange.com/questions/38651/quick-jump-to-a-specific-line-of-a-file
;; https://unix.stackexchange.com/questions/691444/how-do-i-open-a-file-at-specific-line-in-a-running-emacs
;; https://stackoverflow.com/questions/3139970/open-a-file-at-line-with-filenameline-syntax
;; $ emacsclient +4:3 FILE

;; =========================================================

;; (require 'org-protocol)

;; I don't use org-protocol, I use the following bookmarklets instead:

;; ;; Copylet
;; javascript:(function() {
;;     var title = document.title;
;;     var url = location.href;
;;     var selectedText = window.getSelection().toString();
;;     var tmpTextArea = document.createElement('textarea');
;;     tmpTextArea.value = 'title: ' + title + '\nurl: ' + url + '\n';
;;     document.body.appendChild(tmpTextArea);
;;     tmpTextArea.select();
;;     document.execCommand('copy');
;;     document.body.removeChild(tmpTextArea);
;;     })();

;; ;; Copylet with quote
;; javascript:(function() {
;;     var title = document.title;
;;     var url = location.href;
;;     var selectedText = window.getSelection().toString();
;;     var tmpTextArea = document.createElement('textarea');
;;     tmpTextArea.value = 'title: ' + title + '\nurl: ' + url + '\n#+begin_quote\n' + selectedText + '\n#+end_quote\n';
;;     document.body.appendChild(tmpTextArea);
;;     tmpTextArea.select();
;;     document.execCommand('copy');
;;     document.body.removeChild(tmpTextArea);
;;     })();

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

  ;; Manually adjust an interval after a repetition, when it feels like it it too long.
  (defun m/org-drill-tree/change-interval ()
   (interactive)
   (let* ((org-log-reschedule nil)
          (cur-interval (floor (string-to-number (org-entry-get nil "DRILL_LAST_INTERVAL"))))
          (new-interval (read-number "New interval: " cur-interval)))
     (org-schedule nil (concat "+" (number-to-string new-interval) "d"))
     (org-entry-put nil "DRILL_LAST_INTERVAL" (number-to-string new-interval))))
  )

;; =========================================================

(use-package org-modern    ; https://github.com/minad/org-modern
  :after org
  :config
  (setq org-modern-block-fringe 3)

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

(require 'org-habit)

(setq org-habit-show-all-today t)
(setq org-habit-show-habits t)
(setq org-habit-show-habits-only-for-today nil)

(setq org-habit-graph-column 17)
(setq org-habit-preceding-days 48)
(setq org-habit-following-days 3)

;; (setq ym-timer-list-to-show-habits   ; (dolist (x ym-timer-list-to-show-habits) (cancel-timer x))
;;       (list
;;        (run-at-time "07:00pm" (* 60 60 24)
;;                    (lambda () (setq org-habit-show-habits t)))
;;        (run-at-time "07:00am" (* 60 60 24)
;;                    (lambda () (setq org-habit-show-habits t)))))

(defun org-habit-get-priority (habit &optional moment) 1000)   ; this disables sorting by scheduled time, shows in the same order as in org file

;; (setq org-habit-today-glyph ?╋)
(setq org-habit-today-glyph ?⬛)
(setq org-habit-completed-glyph ?●)
(setq org-habit-missed-glyph ?○)    ; there's no such var originally, hence the monkey patch
(load-file (expand-file-name (convert-standard-filename "org-habits-monkey-patch.el") user-emacs-directory))

(defun my-org-todo-with-date (&optional arg date-str)
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
             (my-current-time (if date-str
				                  (org-read-date t t date-str)
				                (org-read-date t t nil "when:")
				                ))
             ((symbol-function #'current-time)
              #'(lambda () my-current-time))
             ((symbol-function #'org-today)
              #'(lambda () (time-to-days my-current-time)))
             ((symbol-function #'org-current-effective-time)
              #'(lambda () my-current-time)))
    (if (eq major-mode 'org-agenda-mode)
	    (org-agenda-todo arg)
      (org-todo arg))
    ))


(setq org-agenda-bulk-custom-functions
      '(
	    (?9 (lambda () (interactive) (my-org-todo-with-date "HABIT SKIPPED" "now")))
	    (?0 (lambda () (interactive) (my-org-todo-with-date "DONE" "now")))
	    (?1 (lambda () (interactive) (my-org-todo-with-date "DONE" "-1d 23:59")))
	    (?2 (lambda () (interactive) (my-org-todo-with-date "DONE" "-2d 23:59")))
	    (?3 (lambda () (interactive) (my-org-todo-with-date "DONE" "-3d 23:59")))
	    (?4 (lambda () (interactive) (my-org-todo-with-date "DONE" "-4d 23:59")))
	    (?5 (lambda () (interactive) (my-org-todo-with-date "DONE" "-5d 23:59")))
	    (?6 (lambda () (interactive) (my-org-todo-with-date "DONE" "-6d 23:59")))
	    (?7 (lambda () (interactive) (my-org-todo-with-date "DONE" "-7d 23:59")))
	    (?8 (lambda () (interactive) (my-org-todo-with-date "DONE" "-8d 23:59")))
	    ))

;; maybe
;; run the following after midnight in the Habits.org
;; https://www.emacswiki.org/emacs/MidnightMode

(defun ym-org-fix-habits-skipped ()
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-buffer)
      (while (search-forward "- State \"HABIT SKIPPED\"" nil t)
	    (let* ((date-str-on-the-line (save-match-data
				                       (re-search-forward "\\[\\(.*\\)\\]" (line-end-position))
				                       (match-string 1)))
	           (today-str (org-read-date nil nil "today"))
	           (number-of-days-since-the-date (- (org-time-string-to-absolute today-str)
						                         (org-time-string-to-absolute date-str-on-the-line)))
	           )
	      (when (>= number-of-days-since-the-date 1)
	        ;; I have no idea why, but the `save-match-data` above doesn't work, I get `args-out-of-range`
	        ;; but I don't care, it's a hack anyway, so I search for the substring once again
	        (save-match-data
	          (beginning-of-line)
	          (search-forward "- State \"HABIT SKIPPED\"" nil t)
	          (replace-match "- State \"HABIT\"")))
	      )))))

;; =========================================================

(use-package request)

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

(with-eval-after-load 'org
  (with-eval-after-load 'base16-theme
    ;; to customize further, first do M-x describe-text-properties, then the following
    (set-face-attribute 'org-special-keyword nil :foreground (plist-get ym-base16-colors-darker :base03))    ; base03 is for comments
    (set-face-attribute 'org-drawer nil :foreground (plist-get ym-base16-colors-darker :base03))
    (set-face-attribute 'org-date   nil :foreground (plist-get ym-base16-colors-darker :base03))
    (set-face-attribute 'org-hide   nil :foreground "grey80")
    (set-face-attribute 'org-block-begin-line  nil :foreground "grey50" :background "grey95")    ; org-block-end-line inherits this
    (set-face-attribute 'org-block             nil :foreground "grey50" :background "grey95")
    (let ((f "#5c69cc"))
      (set-face-attribute 'org-level-1 nil :height 5.0 :foreground f)  ; "#ae1200"
      (set-face-attribute 'org-level-2 nil :height 3.0 :foreground f)
      (set-face-attribute 'org-level-3 nil :height 1.5 :foreground f)
      (set-face-attribute 'org-level-4 nil :height 1.0 :foreground f)
      (set-face-attribute 'org-level-5 nil :height 1.0 :foreground f)
      (set-face-attribute 'org-level-6 nil :height 1.0 :foreground f)
      (set-face-attribute 'org-level-7 nil :height 1.0 :foreground f)
      (set-face-attribute 'org-level-8 nil :height 1.0 :foreground f)
      )))

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

;; My own short mode-line.
;; For performance reasons mostly, because I use a lot of modes, and regular mode-line becomes a bottleneck, it evaluates a lot of things constantly.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/_0025_002dConstructs.html
(defvar ym-mode-line-short-enabled t)
(defun m/mode-line-short (&optional enable)
  (interactive)
  (cond
   ((null enable) (if ym-mode-line-short-enabled
                      (setq ym-mode-line-short-enabled nil)
                    (setq ym-mode-line-short-enabled t)))
   ((> enable 0) (setq ym-mode-line-short-enabled t))
   ((< enable 0) (setq ym-mode-line-short-enabled nil)))
  (if ym-mode-line-short-enabled
      (setq-default mode-line-format
                    (list
                     (propertize "\u200b" 'display '((raise -0.4) (height 1.6)))          ; a zero-width character in mode-line in order to make it wider vertically
                     '(:eval
                       (ym/align-mode-line
                          ;;;; ----- Left.
                        '(
                          " "
                          (:eval (when buffer-read-only "%%")
                                 (when (buffer-narrowed-p) "n");       = "%n"
                                 (when (file-remote-p default-directory) "@")   ; = "%@"
                                 )
                          " "

                          ;;;; mode-line-buffer-identification
                          ;;;; this highlights the filename and dims the path
                          ;; (:eval (when buffer-file-name      ; see also the m/prepend-home-dir-to-buffer-name function
                          ;; (propertize
                          ;;  (file-name-nondirectory buffer-file-name)    ; or "%b", but it can show "init.el\.emacs.d"
                          ;;  'face 'mode-line-buffer-id)))
                          ;; " "
                          ;; (:eval (if buffer-file-name
                          ;;            (abbreviate-file-name (file-name-as-directory (file-name-directory buffer-file-name)))
                          ;;          "%b"))

                          ;; "%b"   ; instead of the commented block above
                          (:eval (propertize "%b" 'face 'mode-line-buffer-id))
                          )
                          ;;;; ----- Right.
                        '("   "
                          mode-line-end-spaces
                          )))))
    (setq-default mode-line-format
                  (list
                   (propertize "\u200b" 'display '((raise -0.4) (height 1.6)))          ; a zero-width character in mode-line in order to make it wider vertically
                   '(:eval
                     (ym/align-mode-line
                      ;; Left.
                      '(
                        "%e"    ; indication of nearly out of memory for Lisp objects
                        mode-line-front-space
                        mode-line-mule-info
                        mode-line-client
                        mode-line-modified
                        mode-line-remote
                        mode-line-frame-identification
                        mode-line-buffer-identification
                        )

                      ;; Right.
                      '("   "
                        (:eval (when ym-mode-line-show-line-column-position mode-line-position))         ; or maybe "%l:%c"
                        ;; (vc-mode vc-mode)
                        "  "
                        mode-line-modes
                        mode-line-misc-info
                        mode-line-end-spaces)
                      )))))
  (force-mode-line-update t))
(m/mode-line-short 1)
;; see also https://stackoverflow.com/questions/6672251/easily-display-useful-information-in-custom-emacs-minor-mode-mode-line-woes

;; highlight minibuffer prompt, because large monitor
(set-face-attribute 'minibuffer-prompt nil :background "light green" :foreground "black")

;; use this instead: M-x goto-line
(progn
  ;; We configure what's shown.
  ;; But do not show it unless toggled.
  (line-number-mode 1)
  (column-number-mode 1)
  (size-indication-mode 1)   ; like Top or 70%
  (defvar ym-mode-line-show-line-column-position t))
(defun ym-shortcut/toggle-mode-line-show-line-column-position ()
  (interactive)
  (if ym-mode-line-show-line-column-position
      (setq ym-mode-line-show-line-column-position nil)
    (setq ym-mode-line-show-line-column-position t))
  (force-mode-line-update))

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

;; Highlith the active window. Or the other way around, dim all inactive windows.

;; When you have one buffer in multiple windows, these windows are not dimmed.
;; In this case use indirect buffers: clone-indirect-buffer-other-window

;; https://emacs.stackexchange.com/questions/24630/is-there-a-way-to-change-color-of-active-windows-fringe
;; https://stackoverflow.com/questions/47456134/emacs-lisp-hooks-for-detecting-change-of-active-buffer
(defun ym/highlight-selected-window ()
  "Highlight selected window with a different background color."
  (let ((hydra-window lv-wnd))        ; this is the hydra echo area, see https://github.com/abo-abo/hydra/blob/master/lv.el
    (walk-windows (lambda (w)
                    (cond 
                     ((eq (window-buffer w) (window-buffer (selected-window)))         ; if you want to dim same buffers, use (eq w (selected-window))
                      (buffer-face-set '(:background "white")))
                     ((eq (window-buffer w) (window-buffer hydra-window))         ; saved for history: (eq w hydra-window)
                      (with-current-buffer (window-buffer w)
                        (buffer-face-set '(:background "grey"))))
                     (t
                      (with-current-buffer (window-buffer w)
                        (buffer-face-set '(:background "grey90"))))
                     )))))
(add-hook 'buffer-list-update-hook #'ym/highlight-selected-window)
(add-hook 'window-configuration-change-hook #'ym/highlight-selected-window)
;; (remove-hook 'buffer-list-update-hook #'ym/highlight-selected-window)
;; (remove-hook 'window-configuration-change-hook #'ym/highlight-selected-window)
  
;; see an alternative:
;; a package that apparently does the same: https://github.com/mina86/auto-dim-other-buffers.el

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


  (defun ym/advice-magit-list-repositories (orig-fun &optional args)
    (setq magit-repository-directories
          (let* ((dirs (list
                        "~/.password-store"
                        "~/.setuplets"
                        "~/werk"
                        "~/workspaces"
                        ))
                 (w-subdirs (append
                             (ignore-errors (f-directories "~/"
                                                           (lambda (dir)
                                                             (s-matches? (rx (seq "/.emacs.d."))    ; the last dot excludes ~/.emacs.d, which is for chemacs2
                                                                         dir))
                                                           ))
                             (ignore-errors (f-directories "~/wo" (lambda (dir) (not (f-hidden-p dir 'last)))))
                             (ignore-errors (f-directories "~/wu" (lambda (dir) (not (f-hidden-p dir 'last)))))
                             ))
                 (w-subdirs-relative-to-home (mapcar (lambda (d) (f-short d))    ; I'd like to see the ~/ in front of dirs in the list
                                                     w-subdirs))
                 (my-dirs (append dirs w-subdirs-relative-to-home)))
            (mapcar (lambda (d) (cons d 0))       ; magit-repository-directories is a list of cons-cells, where the cdr is depth, 0 means only the dir itself
                    my-dirs)))
    (apply orig-fun args))
  (advice-add 'magit-list-repositories :around 'ym/advice-magit-list-repositories)
  ;; (advice-remove 'magit-list-repositories 'ym/advice-magit-list-repositories)


  (setq magit-repolist-columns      ; usage: M-x tabulated-list-sort
        '(
          ("mtime" 11 ym/magit-repolist-column--date-last-touched)   ; can't figure out how to use (:sort <)
          ("version" 30 magit-repolist-column-version)           ; (:sort magit-repolist-version<)
          ("upstream" 20 magit-repolist-column-upstream)
          ("B<U" 3 magit-repolist-column-unpulled-from-upstream ((:right-align t)))      ; (:sort <)
          ("B>U" 3 magit-repolist-column-unpushed-to-upstream ((:right-align t)))        ; (:sort <)
          ("B<P" 3 magit-repolist-column-unpulled-from-pushremote ((:right-align t)))
          ("B>P" 3 magit-repolist-column-unpushed-to-pushremote ((:right-align t)))
          ;; ("#b" 3 magit-repolist-column-branches)
          (" " 3 magit-repolist-column-flags)   ; NUS -- N, U, and S mean: uNtracked, Unstaged, Staged
          ("#s" 3 magit-repolist-column-stashes)
          ("branch" 30 magit-repolist-column-branch ((:right-align t)))
          ("path" 300 magit-repolist-column-path (:sort <))
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

;; from https://emacs.stackexchange.com/questions/28537/a-way-to-insert-a-predefined-text-into-magits-commit-message-window
;; (defun my-git-commit-setup ()
;;   (insert "Ctrl-S"))
;; (add-hook 'git-commit-setup-hook 'my-git-commit-setup)
;; or
;; from https://emacs.stackexchange.com/questions/46244/how-to-perform-an-automatic-commit-with-predefined-message-using-magit
;; after staging:
;; (magit-call-git "commit" "-m" "the message")
;; (magit-refresh)


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

(use-package ob-mermaid)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (sqlite . t)
   (clojure . t)
   (java . t)
   (nix . t)
   (mermaid . t)
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

  ;; (setq projectile-project-root-files ?)       ; otherwise tramp is slow, because projectile looks for too many files
  )

;; TODO: hydra

;; =========================================================

(use-package envrc
  :hook (after-init . envrc-global-mode))

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

;; TODO: hydra

;; =========================================================

(use-package eldoc
  :straight nil
  :blackout
  :config
  ;; (eldoc-mode -1)
  )

;; =========================================================

(setq tab-always-indent t)
(setq c-tab-always-indent t)

(use-package dabbrev
  :config

  ;;;; this is in the corfu docs, but I don't understand what this means
  ;; (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)
  )


(use-package corfu    ;; https://github.com/minad/corfu
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
  
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator))      ; no need for this, when using flex

  ;; There are two main options for me for completion-styles in orderless configuration below:
  ;; 1. '(orderless basic) -- lets you input multiple parts of words out of order, separated by space.
  ;;    The keybinding of a separator is configured in corfu-map above.
  ;; 2. '(orderless flex) -- lets you avoid space, this is faster to type.
  ;;     No need for keybinding.
  )

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)   ; '(orderless basic) lets you input multiple parts of words out of order, separated by space; flex lets you avoid space, this is faster to type
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package cape    ; https://github.com/minad/cape
  :bind
  (
   ("C-c p p" . completion-at-point) ;; capf
   ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
   ("C-c p f" . cape-file)
   ("C-c p k" . cape-keyword)
   ("C-c p l" . cape-line)
   ;;  ("C-c p t" . complete-tag)        ;; etags
   ;;  ("C-c p s" . cape-elisp-symbol)
   ;;  ("C-c p e" . cape-elisp-block)
   ;;  ("C-c p a" . cape-abbrev)
   ;;  ("C-c p w" . cape-dict)
   ;;  ("C-c p :" . cape-emoji)
   ;;  ("C-c p \\" . cape-tex)
   ;;  ("C-c p _" . cape-tex)
   ;;  ("C-c p ^" . cape-tex)
   ;;  ("C-c p &" . cape-sgml)
   ;;  ("C-c p r" . cape-rfc1345)
   )

  :init
  ;; (setq cape-dabbrev '(    ; The order of the functions matters, the first function returning a result wins.
  ;;                      ;; Note that the list of buffer-local completion functions takes precedence over the global list.
  ;;                      cape-dabbrev
  ;;                      tags-completion-at-point-function
  ;;                      elisp-completion-at-point
  ;;                      cape-keyword
  ;;                      cape-elisp-symbol
  ;;                      ;; cape-file  
  ;;                      ;; cape-elisp-block
  ;;                      ;; cape-history
  ;;                      ;; cape-tex
  ;;                      ;; cape-sgml
  ;;                      ;; cape-rfc1345
  ;;                      ;; cape-abbrev
  ;;                      ;; cape-dict
  ;;                      ;; cape-line
  ;;                      ))

  ;; disabled cape-dabbrev above in favor of super-capf
  (defun my-cape-super-capf ()
    (cape-wrap-super
     #'cape-dabbrev
     #'cape-keyword
     #'cape-elisp-symbol
     #'elisp-completion-at-point
     #'tags-completion-at-point-function
     #'cape-line
     ))
  (setq-default completion-at-point-functions (list #'my-cape-super-capf))

  ;; Shell buffers redefine completion-at-point-functions with '(comint-completion-at-point).
  ;; This way regular shell completions work in shell buffers when pressing tab.
  ;; And with this little workaround my M-tab also works.
  (defun my/completion-at-point-with-corfu ()
    (interactive)
    (let ((completion-at-point-functions (list #'my-cape-super-capf)))
      (completion-at-point)))
  (ym/define-key (kbd "M-<tab>") #'my/completion-at-point-with-corfu))      ; corfu is integrated

;; =========================================================

(use-package dumb-jump    ; https://github.com/jacktasia/dumb-jump
  :config
  (add-to-list 'xref-backend-functions 'dumb-jump-xref-activate
               t)       ; to the end of list, which means fall back to dumb-jump when there are no better options
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  ;; (setq dumb-jump-force-searcher 'rg)   ; tries searches in this order: git-grep, ag, rg, grep
  ;; (setq dumb-jump-prefer-searcher 'rg)
  ;; (setq dumb-jump-git-grep-search-args "")
  ;; (setq dumb-jump-rg-search-args "")
  ;; (setq dumb-jump-quiet t)
  ;; (dumb-jump-debug t)   ; try to jump and see *messages*
  )

;; (setq path-to-ctags "/opt/local/bin/ctags")        https://gist.github.com/kborling/13f2300e60ae4878d5d96f5f4d041664#file-init-el-L414

(setq xref-auto-jump-to-first-xref nil)    ; default: nil
(setq xref-prompt-for-identifier t)

;; also try https://github.com/universal-ctags/citre

;; TODO: hydra

;; =========================================================

(use-package bm
         :init
         (setq bm-restore-repository-on-load t)

         :config
         (setq bm-highlight-style 'bm-highlight-only-fringe)
         (setq bm-marker 'bm-marker-left)
         (setq bm-in-lifo-order t)

         ;; (setq temporary-bookmark-p t)   ; default: nil --- remove bookmark after jump to it by bm-next or bm-previous
         ;; or (bm-bookmark-add nil nil t)
         
         (setq bm-cycle-all-buffers t)
         (add-hook 'bm-after-goto-hook 'org-bookmark-jump-unhide)
         ;; (setq bm-repository-file "~/.emacs.d/bm-repository")    ; no-littering takes care of this
         (setq-default bm-buffer-persistence t)

         (setq bm-electric-show nil)    ; default: t --- but this didn't work for me for some reason

         (add-hook 'after-init-hook 'bm-repository-load)
         (add-hook 'kill-buffer-hook #'bm-buffer-save)
         (add-hook 'kill-emacs-hook #'(lambda nil
                                          (bm-buffer-save-all)
                                          (bm-repository-save)))

         ;; The `after-save-hook' is not necessary to use to achieve persistence,
         ;; but it makes the bookmark data in repository more in sync with the file
         ;; state.
         (add-hook 'after-save-hook #'bm-buffer-save)

         (add-hook 'find-file-hooks   #'bm-buffer-restore)
         (add-hook 'after-revert-hook #'bm-buffer-restore)

         ;; The `after-revert-hook' is not necessary to use to achieve persistence,
         ;; but it makes the bookmark data in repository more in sync with the file
         ;; state. This hook might cause trouble when using packages
         ;; that automatically reverts the buffer (like vc after a check-in).
         ;; This can easily be avoided if the package provides a hook that is
         ;; called before the buffer is reverted (like `vc-before-checkin-hook').
         ;; Then new bookmarks can be saved before the buffer is reverted.
         ;; Make sure bookmarks is saved before check-in (and revert-buffer)
         (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

         ;; :bind (("<f2>" . bm-next)
         ;;        ("S-<f2>" . bm-previous)
         ;;        ("C-<f2>" . bm-toggle))

         ;; usage: M-x bm-show, bm-show-all
         )

;; TODO: hydra

;; =========================================================

(use-package embark
  :demand
  :bind
  (
   ("C-M-." . embark-become)
   ;; ("C-M-." . embark-act)
   ;; ("C-M-;" . embark-dwim)
   ;; ("C-h B" . embark-bindings)
   )

  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  )

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

(defun my/prepend-home-dir-to-buffer-name ()
  (interactive)
  "Prepend the user's home directory to the current buffer's file name."
  (let ((original-filename (buffer-file-name))
        ;; (home-directory (expand-file-name "~/"))
        (home-directory (abbreviate-file-name (file-name-as-directory (file-name-directory buffer-file-name))))
        )
    (when original-filename
      (rename-buffer
       (concat (file-name-nondirectory original-filename) " " home-directory )
       t)
      ))
  )
(add-hook 'find-file-hook #'my/prepend-home-dir-to-buffer-name)
;; (remove-hook 'find-file-hook #'my/prepend-home-dir-to-buffer-name)

;; =========================================================

;; Make help buffers stick by cloning and renaming them.
;; A two step process: first we wait for the buffer to be populated, then read its contents, then clone and rename accordingly.

(defun ym/help-buf-clone-and-rename (&optional buffer)    ; from https://emacs.stackexchange.com/questions/33156/how-can-i-have-multiple-help-buffers-with-different-content
  (let ((buf (if buffer
                 buffer
               (current-buffer))))
    (with-current-buffer buf
      (rename-buffer (generate-new-buffer-name
                      (concat (buffer-name) " -- "                 ; create name from old name and
                              (save-excursion                   ; use first word in buffer for new name
                                (goto-char 0)
                                (thing-at-point 'symbol t))))
                     t))))               ; show cloned buffer now

;; For some reason cloning directly in the hook doesn't work, we have to wait until the help buffer is populated to read it.
(defun ym/help-buffer-rename-when-populated ()
  (run-at-time "100 millisecond"     ; a lot, but still not noticeable
               nil     ; no repeat
               #'ym/help-buf-clone-and-rename (current-buffer)))
(add-hook 'help-mode-hook #'ym/help-buffer-rename-when-populated)
;; (remove-hook 'help-mode-hook #'m/rename-help-buffer-when-it-is-populated)

;; maybe write the same thing for occur, grep, rg, etc: https://dawranliou.com/blog/xref-with-eglot-and-project/

;; =========================================================

;; electric-pair-mode is enough most of the time
;; I don't use strict mode and soft deletion from puni and smartparens
(electric-pair-mode t)    ; insert () together and wrap into parens

(use-package smartparens
  ;; :demand t
  ;; :diminish smartparens-mode smartparens-global-mode show-smartparens-mode show-smartparens-global-mode
  :config
  (require 'smartparens-config)   ; default configuration
  (setq sp-navigate-reindent-after-up-in-string nil)
  (setq sp-navigate-reindent-after-up nil)

  ;;;; I now use electric-pair-mode
  ;; (smartparens-global-mode 1)     ; used to be (smartparens-global-strict-mode 1), but I don't need it to be that strict
  ;; (show-smartparens-global-mode 1)

  ;;;; customize sp-show-pair-match-content-face if you want to highlight not only parens but also the content of the s-exp
  ;; '(sp-show-pair-enclosing ((t (:inherit show-paren-match))))
  )


;; highlight matching parenthesis
(require 'paren)   ; I prefer stock show-paren-mode over show-smartparen-mode because it's ultra-fast
;; (setq show-paren-delay 0)
;; (setq show-paren-delay 0.1)
;; (setq show-paren-delay 0.05)
(setq show-paren-delay 0.01)
(show-paren-mode 1)
;; (show-paren-mode -1)
(setq show-paren-style 'parenthesis)
(setq show-paren-context-when-offscreen 'overlay)
(copy-face 'default 'show-paren-match)
(set-face-attribute 'show-paren-match nil
                    :weight 'bold
                    :foreground "black"
                    :background "grey"         ; was ym-hl-line-color-normal-mode-color
                    )    ; inherited by show-paren-match-expression

;; No configuration here. I just directly use functions from these packages without activating them.
(use-package lispy)
(use-package symex)
(use-package puni)

;; =========================================================

(use-package expand-region)

(ym/define-key (kbd "M-e") #'er/expand-region)
(ym/define-key (kbd "M-r") #'er/contract-region)

(ym/define-key (kbd "M-w") #'exchange-point-and-mark)

;; =========================================================

;; https://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
;; https://emacs.stackexchange.com/questions/12180/why-use-indirect-buffers/12185#12185
(defun m/narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end)
      ;; (python-mode)
      )
    (switch-to-buffer buf)
    (font-lock-fontify-buffer)   ; without this the colors get lost
    ))

;; =========================================================

(pretty-hydra-define hydra-1 ()
  (
   ""
   (
    ("M-i" drag-stuff-up :exit nil)
    ("M-k" drag-stuff-down :exit nil)
    ("M-l" drag-stuff-right :exit nil)
    ("M-j" drag-stuff-left :exit nil)

    ("m" m/mode-line-short :exit t)
    ("n" m/narrow-to-region-indirect :exit t)


    )
   ))

(ym/define-key (kbd "M-a") #'hydra-1/body)

(pretty-hydra-define hydra-2 ()
  (
   ""
   (
    (" " bm-next :exit nil)
    (" " bm-previous :exit nil)
    ("b" bm-toggle :exit t)
    ("v" bm-show :exit t)
    ("V" bm-show-all :exit t)

    ("s-f" org-ctrl-c-ctrl-c :exit t)      ; Which is C-a C-f. Could be C-a C-a, but just a precaution.

    ("c" m/toggle-color-of-comments :exit nil)
    )
   ))

(ym/define-key (kbd "s-a") #'hydra-2/body)


(pretty-hydra-define hydra-f1 (:exit t)
  (
   "ccc" (("e" (lambda () (interactive)
		         (org-ql-search "~/werk/English.org" '(tags "drill") :sort 'date)
		         (delete-other-windows)
		         )
	       "english")
	      ("8" (lambda () (interactive)
		         (org-ql-search "~/werk/Spanish.org" '(tags "drill") :sort 'date)
		         (delete-other-windows)
		         )
	       "spanish")
	      ("9" (lambda () (interactive)
		         (org-ql-search (org-agenda-files) '(and (tags "clojure") (tags "drill" "drilltodo")) :sort 'date)
		         (delete-other-windows)
		         )
	       "clojure")
	      ("u" (lambda () (interactive)
		         (org-ql-search (org-agenda-files) '(and (tags "drill" "drilltodo") (not (tags "english" "spanish" "humor"))) :sort '(date))
		         (delete-other-windows)
		         )
	       "drill and drilltodo")
	      ("i" (lambda () (interactive)
		         (org-ql-search (org-agenda-files) '(and (tags "drilltodo") (not (tags "english" "spanish" "humor"))) :sort '(date))
		         (delete-other-windows)
		         )
	       "drilltodo only")
	      ("o" (lambda () (interactive)
		         (let ((tt (read-string "drill tag: "))
                       (org-agenda-files '("~/werk")))
                   (org-ql-search (org-agenda-files) `(and (tags "drill") (tags ,tt) (not (tags "english" "spanish" "humor"))) :sort '(date))
		           (delete-other-windows)))
	       "drill only")
          )
   
   "eee" (("1" (lambda () (interactive ) 
                 (ym/org-ql-search--projects)
                 (delete-other-windows))
	       "projects")
          ("2" (lambda () (interactive ) 
                 (ym/org-ql-search--todos)
                 (delete-other-windows))
	       "todos")
          ("3" (lambda () (interactive)
                 (org-id-goto "7329a7e5-d444-43c1-8f61-be928613acad")
		         (delete-other-windows))
	       "checklist")
          ("4" (lambda () (interactive)
                 (org-agenda nil "x1")
		         (delete-other-windows))
	       "habits")
          ("5" (lambda () (interactive)
                 (let ((org-agenda-files '("~/werk")))
                   (org-ql-search (org-agenda-files)
                     `(and
                       (or  ; basically, list all todos
                        (not (done))
                        (todo ,@ym-org-todo-keywords-done---with-no-shortcuts)
                        )
                       (not (habit))
                       (tags "occasionally")
                       ;; (ts)
                       ;; (or
                       ;;  (deadline auto)
                       ;;  (scheduled :to today)
                       ;;  (ts-active :to today)
                       ;;  (ts-inactive :to today))
                       )
                     ;; :super-groups '((:auto-ts))
                     ))
		         (delete-other-windows))
	       "occasionally")
          ("6" (lambda () (interactive)
                 (org-open-link-from-string "[[id:79c89c1c-76ce-428f-ade6-053f56369e25][movies and documentaries]]")
                 (delete-other-windows)
                 )
	       "movies and documentaries")
          ("7" (lambda () (interactive)
                 (org-open-link-from-string "[[id:dcf78ab5-5ee9-4f12-938e-cbcfaf6e429f][trampolines park]]")
                 (delete-other-windows)
                 )
	       "trampolines park")
          ("8" (lambda () (interactive)
                 (org-open-link-from-string "[[id:8377d402-f9d4-4bd5-8228-6032af4336c5][muay thai]]")
                 (delete-other-windows)
                 )
	       "muay thai")
          )
   ))
(ym/define-key (kbd "<f1>") #'hydra-f1/body)


(setq org-super-agenda-date-format "=== %Y-%m-%d %a ===")

(defun ym/org-agenda-goto-timestamp ()
  (interactive)
  (progn (org-agenda-goto)
         (next-line)
         (end-of-line))
  (let ((timestamp-beginning-pos (re-search-backward org-ts-regexp-both (line-beginning-position) t)))
	(if timestamp-beginning-pos
        (progn
	      (forward-char 7)      ; to the month pos, [2022-01-31 Mon]
          (let ((prompt-for-new-timestamp-active-or-inactive (lambda ()
                                                           (let* ((context (org-element-context))
                                                                  (ts-type (when (memq (org-element-type context) '(timestamp timestamp-range))
                                                                             (org-element-property :type context)))   ; = 'active or 'inactive
                                                                  (scheduled (when (eq (org-element-type context) 'planning)
                                                                               (org-element-property :scheduled context))))
                                                             (cond
                                                              ((eq ts-type 'active) (org-timestamp nil))
                                                              ((eq ts-type 'inactive) (org-timestamp-inactive nil))
                                                              (scheduled (org-schedule nil))
                                                              (t (message "No timestamp at point"))
                                                              )))))
            (split-window-vertically)
            (let ((w (selected-window)))
              (other-window 1)
              (unwind-protect
                  (funcall prompt-for-new-timestamp-active-or-inactive)
                (progn (delete-window)
                       (select-window w))))
	        ))
      (progn
        (beginning-of-line)
        (split-window-vertically)
            (let ((w (selected-window)))
              (other-window 1)
              (unwind-protect
                  (progn (org-timestamp-inactive nil)
                         (newline))
                (progn (delete-window)
                       (select-window w))))))))
(defun ym/agenda-setup ()
  (local-set-key (kbd "t") 'ym/org-agenda-goto-timestamp))
(add-hook 'org-agenda-mode-hook #'ym/agenda-setup)

(comment

 (defun org-ql-ts-property<-fn (property)
   "Return a comparator function comparing the element PROPERTY as a timestamp."
   (lambda (a b)
     (let ((a-ts (org-entry-get a property))
           (b-ts (org-entry-get b property)))
       (cond
        ((and a-ts b-ts)
         (string< a-ts b-ts))
        (a-ts t)
        (b-ts nil)
        ;; (a-ts nil)
        ;; (b-ts t)
        ))))
 ;; :sort (org-ql-ts-property<-fn "TIMESTAMP_IA");; :sort (org-ql-ts-property<-fn "TIMESTAMP_IA")
 
   )



;; =========================================================

;; (load-file (expand-file-name (convert-standard-filename "init-hydras.el") user-emacs-directory))

;; =========================================================

;; (ignore-errors
;;   (use-package bubbles
;;     :straight (:host nil :local-repo "~/wu/emacs-bubbles-wmm")
;;     )
;;   )








;; =========================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-omit-extensions 'nil nil nil "Customized by me")
 '(dired-omit-files (rx (or (seq bol "." eol) (seq bol "." (not (any "."))))) nil nil "Customized by me"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-virtual ((t (:foreground "black"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "firebrick3"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dodger blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "green3"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "peru"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "grey50"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "black")))))





;; =========================================================

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (electric-indent-mode 0)
              ))
  )

;; =========================================================

(setq org-todo-repeat-to-state t)   ; habit -> done -> habit, not todo

;; t vs. T
;; "PAUSED(p!)"
;; "WORKING(w!)"
(setq ym-org-todo-keywords-working '("TODAY(T!)" "NOW(n!)"))
(setq ym-org-todo-keywords-undone
      `("TODO(t!)" "NEXT(n!)"
	,@ym-org-todo-keywords-working
	"POSTPONED(P!)" "WAITING(W!)" "IN PROGRESS(i!)"
	"HABIT(h/@)" "REGULARLY(r!)" "SOMEDAY(S!)" "MAYBE(M!)"
	))
(setq ym-org-todo-keywords-done
      '("DONE(d!)"
	"CANCELED(c@)"
	"HABIT SKIPPED(!)"
	"REDIRECTED(R@)" "DELEGATED(D@)"
	"MERGED(m@)" "JIRA(j@)"))
(setq ym-org-todo-keywords-done---with-no-shortcuts
      (mapcar (lambda (string)
                (let ((index (string-match "(" string)))
                  (if index
                      (substring string 0 index)
                    string)))
              ym-org-todo-keywords-done
              ))
(setq ym-org-todo-state-string-in-log "State:     (")
(setq org-todo-keywords
      `((sequence ,@ym-org-todo-keywords-undone "|" ,@ym-org-todo-keywords-done)))

(setq org-agenda-custom-commands
	  `(
        ("x1" "habits"
	     (
	      (todo "" (
			        (org-agenda-files nil)
			        (org-agenda-overriding-header
			         (let* ((habits-top-path "~/werk/Habits-top.org")
				            (habits-top (if (f-exists-p habits-top-path) (string-trim (f-read-text habits-top-path)) "file missing")))
			           (concat
			            (unless (string-empty-p habits-top) (concat habits-top-path ": \n\n" habits-top "\n\n"))
			            ))
			         )))
	      (agenda "" (
			          (org-agenda-files '("~/werk/Habits.org"))
			          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":MY_HABITS_GROUP: 1"))
			          (org-agenda-span 1)
			          (org-agenda-overriding-header "")
			          (org-agenda-todo-keyword-format "")
			          (org-agenda-prefix-format "")
			          ))
	      (agenda "" (
			          (org-agenda-files '("~/werk/Habits.org"))
			          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":MY_HABITS_GROUP: 2"))
			          (org-agenda-span 1)
			          (org-agenda-overriding-header "")
			          (org-agenda-todo-keyword-format "")
			          (org-agenda-prefix-format "")
			          ))
	      (agenda "" (
			          (org-agenda-files '("~/werk/Habits.org"))
			          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":MY_HABITS_GROUP: 3"))
			          (org-agenda-span 1)
			          (org-agenda-overriding-header "")
			          (org-agenda-todo-keyword-format "")
			          (org-agenda-prefix-format "")
			          ))
	      ))

        

	    ))


(defun ym/org-ql-search--projects () (interactive)
	   (let ((org-agenda-files `("~/werk"
                                 ,@(directory-files-recursively "~/wo/veson.dev/content/blog" "\\.org$")
                                 ,@(directory-files-recursively "~/.setuplets" "\\.org$")
                                 )))
         (org-ql-search (org-agenda-files) '(and (not (todo))
                                                 (tags "try" "read" "watch" "listen" "blog" "todo")
                                                 ;; (tags "git")
                                                 (not
                                                  (tags "done" "canceled")
                                                  )
                                                 ;; (tags "zoom_out")
                                                 ;; (ts-inactive)
                                                 )
           :super-groups '((:auto-ts))))
	   ;; (delete-other-windows)
	   )

(defun ym/org-ql-search--todos () (interactive)
	   (let ((org-agenda-files '("~/werk")))
         (org-ql-search (org-agenda-files)
           '(and
             (todo)
             (not (done))
             (not (habit))
             ;; (ts)
             ;; (or
             ;;  (deadline auto)
             ;;  (scheduled :to today)
             ;;  (ts-active :to today)
             ;;  (ts-inactive :to today))
             )
           :super-groups '((:auto-ts))
           ))
       ;; (delete-other-windows)
       )

(progn
  (delete-other-windows)
  (org-id-goto "7329a7e5-d444-43c1-8f61-be928613acad")
  
  (split-window-horizontally)
  (ym/org-ql-search--projects)
  
  (split-window-horizontally)
  (ym/org-ql-search--todos)
  
  (split-window-horizontally)
  (find-file "~/werk/Notes-2.org")
  (end-of-buffer)

  (split-window-vertically)
  (find-file "~/werk/mobile_org/Agenda-mobile.org")
  
  (split-window-vertically)
  (find-file "~/werk/mobile_org/Notes-mobile.org")
  
  (split-window-vertically)
  (find-file "~/werk/mobile_org/Random-coffee-mobile.org")
  
  ;; random coffee
  ;; 

  (balance-windows)

  )





