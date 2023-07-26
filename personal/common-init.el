;;; common-init.el --- Dom's Prelude Config: Settings common to all modules.
;;
;; Copyright © 2022 Dom Verity
;;
;; Author: Dom Verity <dom.verity@gmail.com>
;; URL: https://github.com/dom-verity/prelude

;; This file is not part of GNU Emacs.

;;; Commentary

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

;; Add to the list of locals saved by Emacs desktop
(add-to-list 'desktop-locals-to-save 'whitespace-style)

;; Reload YASnippets.
(yas-reload-all)

;; Toggle whitespace mode locally rather than globally
(global-whitespace-mode 0)

;; Turn off guru mode to prevent messages about arrow keys
(setq prelude-guru nil)

;; Use ivy to do flyspell correction
(define-key flyspell-mode-map (kbd "C-c $") 'flyspell-correct-wrapper)

;;; common-init.el ends here
