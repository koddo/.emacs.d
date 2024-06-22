;; -*- lexical-binding: t; -*-

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

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

;; =========================================================

(use-package no-littering
  :init   ; by default these are just etc/ and var/
  (setq no-littering-etc-directory (expand-file-name (convert-standard-filename "no-littering-etc/") user-emacs-directory))
  (setq no-littering-var-directory (expand-file-name (convert-standard-filename "no-littering-var/") user-emacs-directory)))
