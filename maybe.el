;; -*- lexical-binding: t; -*-

(use-package crux    ; useful functions, https://github.com/bbatsov/crux
  )

;; this package shows org-links like clickable links in other modes
;; [[http://ya.ru][yandex]]
(use-package orglink)

(defun ignore-error-wrapper (fn)
  "This makes a wrapper that ignores errors of the wrapped function."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

;; spanish 
;; https://emacs.stackexchange.com/questions/66629/how-to-make-c-x-9-the-same-as-c-x-8
(defun ym-ctl-x-8-single-quote ()
  "Simulate typing: C-x 8 \'"
  (interactive)
  (dolist (event (nreverse (list ?\C-x ?8 ?\')))
    (push (cons t event) unread-command-events)))

;; https://www.emacswiki.org/emacs/CleanBufferList
;; https://www.emacswiki.org/emacs/MidnightMode
(require 'midnight)

(use-package drag-stuff)    ; https://github.com/rejeep/drag-stuff.el

(use-package multiple-cursors)

(use-package eldoc-box)    ; https://github.com/casouri/eldoc-box

(use-package bm
  :demand t
  :config
  ;; https://readingworldmagazine.com/emacs/2019-10-20-emacs-bookmarks/
  ;; https://github.com/joodland/bm
  )

;; (use-package direnv    ; https://github.com/wbolster/emacs-direnv
;;   :config
;;   (direnv-mode))
;; try https://github.com/purcell/envrc

(use-package ignoramus)    ; https://github.com/rolandwalker/ignoramus/blob/master/ignoramus.el
(ignoramus-setup)
;; (ignoramus-setup '(pcomplete shell ido))


;; =========================================================

;; https://alhassy.github.io/org-special-block-extras/

;; =========================================================

;; https://github.com/eqmacs-lsp/lsp-mode



;; =========================================================

(defvar ym/highlight-active-window-enabled nil)
(defun m/highlight-active-window (&optional enable)
  (interactive)
  (cond
   ((null enable) (if ym/highlight-active-window-enabled
                      (setq ym/highlight-active-window-enabled nil)
                    (setq ym/highlight-active-window-enabled t)))
   ((> enable 0) (setq ym/highlight-active-window-enabled t))
   ((< enable 0) (setq ym/highlight-active-window-enabled nil)))
  (if ym/highlight-active-window-enabled
      (progn      (add-hook 'buffer-list-update-hook #'ym/highlight-selected-window)
                  (add-hook 'window-configuration-change-hook #'ym/highlight-selected-window))
    (progn
      (remove-hook 'buffer-list-update-hook #'ym/highlight-selected-window)
      (remove-hook 'window-configuration-change-hook #'ym/highlight-selected-window)
      )))

(m/highlight-active-window 1)

