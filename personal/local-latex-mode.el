;;; local-latex-mode.el --- Dom's Prelude Config: Custom latex mode config.
;;
;; Copyright © 2022 Dom Verity
;;
;; Author: Dom Verity <dom.verity@gmail.com>
;; URL: https://github.com/dom-verity/prelude

;; This file is not part of GNU Emacs.

;;; Commentary

;; Customised configuration of the AUCTeX based LaTeX module.

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

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(setq TeX-auto-untabify t)
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)

;; Configure viewer.
(when (eq system-type 'darwin)
  (add-to-list
   'TeX-view-program-list
   '("skimmer"
     "displayline -r -b %n %o %b"
     "displayline"))
  (setq-default
   TeX-view-program-selection
   '((output-dvi "skimmer")
     (output-pdf "skimmer")
     (output-html "open"))))

;; User hook
(add-hook 'prelude-latex-mode-hook
          #'(lambda ()
              (visual-line-mode 1)
              (flyspell-mode 1)
              (LaTeX-math-mode 1)
              (reftex-mode 1)
              (yas-minor-mode 1)
              (setq reftex-plug-into-AUCTeX t)
              (whitespace-mode 1)
              (whitespace-toggle-options 'lines-tail)
              (auto-fill-mode -1)) t)

;;; local-latex-mode.el ends here
