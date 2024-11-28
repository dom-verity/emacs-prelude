;;; local-swi-prolog.el --- Dom's Prelude Config: local SWI Prolog setup.
;;
;; Copyright © 2024 Dom Verity
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

(prelude-require-packages '(sweeprolog))

(with-eval-after-load 'sweeprolog
  (push "--stack-limit=512m" sweeprolog-init-args)

  (defun doms-sweeprolog-mode-defaults ()
    ;; Enable automatic whitespace adjustment
    (sweeprolog-electric-layout-mode)
    ;; Automatically insert standard skeleton into new prolog file
    (auto-insert-mode))

  (add-hook 'sweeprolog-mode-hook #'doms-sweeprolog-mode-defaults)
)

(add-to-list 'auto-mode-alist '("\\.plt?\\'"  . sweeprolog-mode))

;;; local-swi-prolog.el ends here
