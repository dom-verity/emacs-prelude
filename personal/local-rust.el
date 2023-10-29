;;; local-rust.el --- Dom's Prelude Config: local rust settings.
;;
;; Copyright © 2022 Dom Verity
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

(prelude-require-packages '(rustic
                            lsp-mode
                            yasnippet
                            ron-mode))

(with-eval-after-load 'rustic

  (defun my/rustic-hook-func ()
    ;; enable tree-sitter for nicer syntax highlighting
    (tree-sitter-mode 1)
    (tree-sitter-hl-mode 1)

    ;; Enable yasnippet
    (yas-minor-mode 1)

    ;; Linting via clippy
    ; (setq lsp-rust-analyzer-cargo-watch-command "clippy")

    ;; Configure lsp-mode
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-show-with-cursor t)
    (setq lsp-ui-doc-show-with-mouse nil)
    ; (setq lsp-ui-doc-use-childframe nil)
    ; (setq lsp-eldoc-render-all t)
    ; (setq lsp-signature-render-documentation nil)
    (setq lsp-ui-sideline-enable nil)

    ;; Eldoc configuration
    ; (setq eldoc-echo-area-prefer-doc-buffer t)
    ; (setq eldoc-echo-area-use-multiline-p 1)
    )

  (add-hook 'rustic-mode-hook 'my/rustic-hook-func)
  )

;;; local-rust.el ends here
