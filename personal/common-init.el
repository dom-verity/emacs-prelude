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

;; Setup tree-sitter available languages alist
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (elpi "https://github.com/dom-verity/tree-sitter-elpi")
     (fish "https://github.com/ram02z/tree-sitter-fish")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Modify C-x C-c so that without a prefix it closes the current
;; emacsclient and with a prefix it kills emacs.

(defun dom/emacsclient-c-x-c-c (&optional arg)
  "If running in emacsclient, make C-x C-c exit frame, and C-u C-x C-c exit Emacs."
  (interactive "P") ; prefix arg in raw form
  (if arg
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

(if (daemonp)
    (global-set-key (kbd "C-x C-c") #'dom/emacsclient-c-x-c-c))

;; Always follow symbolic links to the actual file
(setq vc-follow-symlinks t)

;;; common-init.el ends here
