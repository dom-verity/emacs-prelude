;;; local-agda.el --- Dom's Prelude Config: local agda mode setup.
;;
;; Copyright © 2023 Dom Verity
;;
;; Author: Dom Verity <dom.verity@gmail.com>
;; URL: https://github.com/dom-verity/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'prelude-programming)

(unless (fboundp 'agda2-mode)
  (load-file
   (let ((coding-system-for-read 'utf-8))
     (shell-command-to-string "agda-mode locate")))
  )

(with-eval-after-load 'agda2-mode
  (assq-delete-all 'background agda2-highlight-faces)

  (defun doms-agda2-mode-defaults ()
    (turn-off-auto-fill)
    (setq fill-column 80)
    (yas-minor-mode 1)
    )

  (add-hook 'agda2-mode-hook 'doms-agda2-mode-defaults t)
)



;;; local-adga.el ends here
