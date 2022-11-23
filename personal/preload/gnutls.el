;;; gnutls.el --- Dom's Prelude Config: TLS trust checking.
;;
;; Copyright © 2022 Dom Verity
;;
;; Author: Dom Verity <dom.verity@gmail.com>
;; URL: https://github.com/dom-verity/prelude

;; This file is not part of GNU Emacs.

;; Commentary

;; Setup use of gnutls to do trust checking.

;; Install the python3 `certifi` package to install and locate
;; trust files.

;; Enable TLS trust checking
(let ((trustfile
      (replace-regexp-in-string
       "\\\\" "/"
       (replace-regexp-in-string
        "\n" ""
        (shell-command-to-string "python3 -m certifi")))))
  (setq gnutls-trustfiles (list trustfile))
  (setq gnutls-verify-error t))
