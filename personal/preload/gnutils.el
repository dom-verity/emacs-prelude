;;; gnutils.el --- Dom's Prelude Config: Enable use of gnu utils by dired.
;;
;; Copyright © 2022 Dom Verity
;;
;; Author: Dom Verity <dom.verity@gmail.com>
;; URL: https://github.com/dom-verity/prelude

;; This file is not part of GNU Emacs.

;; Commentary

;; On MacOS configure dired to use GNU versions of the standard
;; utilities ls, df, chmod etc.

;; Get these by installing the homebrew `coreutils` package.

(when (eq system-type 'darwin)
  (setq-default insert-directory-program "gls")
  (setq-default directory-free-space-program "gdf")
  (setq-default dired-chmod-program "gchmod")
  (setq-default dired-chown-program "gchown"))
