;;; common-init.el --- Dom's Prelude Config: Settings common to all modules.
;;
;; Copyright © 2022 Dom Verity
;;
;; Author: Dom Verity <dom.verity@gmail.com>
;; URL: https://github.com/dom-verity/prelude

;; This file is not part of GNU Emacs.

;; Commentary

;; Add to the list of locals saved by Emacs desktop
(add-to-list 'desktop-locals-to-save 'whitespace-style)

;; Reload YASnippets.
(yas-reload-all)
