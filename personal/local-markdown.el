;;; local-markdown.el --- Dom's Prelude Config: local markdown mode setup.
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

(defun doms-markdown-mode-defaults ()
  (turn-off-auto-fill)
  (visual-line-fill-column-mode 1)
  (setq fill-column 80)
  (flyspell-mode 1)
  (whitespace-toggle-options 'lines-tail)
  (yas-minor-mode 1))

(add-hook 'gfm-mode-hook 'doms-markdown-mode-defaults t)

;;; local-markdown.el ends here
