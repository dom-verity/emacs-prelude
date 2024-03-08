;;; local-latex.el --- Dom's Prelude Config: Custom latex mode config.
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

;; Enable building using LatexMk
(auctex-latexmk-setup)

;; Configure view to use Skim.
(when (eq system-type 'darwin)
  (add-to-list 'TeX-view-program-list
               '("Skimmer" "displayline -r -b %n %o %b" "displayline"))
  (setq TeX-view-program-selection
   '((output-dvi "Skimmer")
     (output-pdf "Skimmer")
     (output-html "HTML Viewer"))))

;; Configure reftex for theorem-like environments

;; Inserting references
(setq reftex-label-alist
      '(("thm" ?h "thm:" "~\\ref{%s}" t ("theorem" "thm."))
        ("lem" ?l "lem:" "~\\ref{%s}" t ("lemma" "lem."))
        ("prop" ?p "prop:" "~\\ref{%s}" t ("proposition" "prop."))
        ("cor" ?c "cor:" "~\\ref{%s}" t ("corollary" "cor."))
        ("obs" ?o "obs:" "~\\ref{%s}" t ("observation" "obs."))
        ("dig" ?g "dig:" "~\\ref{%s}" t ("digression" "dig."))
        ("rec" ?r "rec:" "~\\ref{%s}" t ("recollection" "rec."))
        ("rmk" ?m "rmk:" "~\\ref{%s}" t ("remark" "rmk."))
        ("defn" ?d "defn:" "~\\ref{%s}" t ("definition" "defn."))
        ("ntn" ?a "ntn:" "~\\ref{%s}" t ("notation" "ntn."))
        ("ex" ?x "ex:" "~\\ref{%descs}" t ("example" "ex."))
        ))

;; Inserting bespoke environements
(setq doms-LaTeX-environments
      '(("thm" LaTeX-env-label)
        ("lem" LaTeX-env-label)
        ("prop" LaTeX-env-label)
        ("cor" LaTeX-env-label)
        ("obs" LaTeX-env-label)
        ("dig" LaTeX-env-label)
        ("rec" LaTeX-env-label)
        ("rmk" LaTeX-env-label)
        ("defn" LaTeX-env-label)
        ("ntn" LaTeX-env-label)
        ("ex" LaTeX-env-label)))

;; User hook
(defun doms-latex-mode-hook ()
  (turn-off-auto-fill)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (LaTeX-math-mode 1)
  (flycheck-mode 0)
  (reftex-mode 1)
  (yas-minor-mode 1)
  (setq reftex-plug-into-AUCTeX t)
  (mapcar #'LaTeX-add-environments doms-LaTeX-environments)
  (whitespace-mode 1)
  (whitespace-toggle-options 'lines-tail)
  (setq LaTeX-indent-level 4)
  (setq LaTeX-item-indent -4)
  (setq LaTeX-left-right-indent-level 4)
  (setq TeX-brace-indent-level 4))

(add-hook 'prelude-latex-mode-hook 'doms-latex-mode-hook t)

;;; local-latex-mode.el ends here
