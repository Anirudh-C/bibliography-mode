;;; bibliography.el --- Annotated Bibliography minor-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Anirudh Chandramouli

;; Author: Anirudh Chandramouli <anirudhvan@gmail.com>
;; Homepage: https://github.com/Anirudh-C/bibliography-mode
;; Keywords: convenience
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Code:

;; Emacs Core
(require 'ol-bibtex)

;; Create a customisation group
(defgroup bibliography nil
  "Annotated bibliography"
  :group 'applications)

(defcustom bibliography-path ""
  "Path to the annotated bibliography"
  :type 'string
  :group 'bibliography)

(defun bibliography-headline-format (entry)
  "Function returning the headline text to use for new entry. Replaces extra curly braces in title"

  (format "%s"
          (string-replace
           "}"
           ""
           (string-replace
            "{"
            ""
            (cdr (assq :title entry))))))

(defun add-bib-entries ()
  "Interactively create new entries with bibtex input"
  (interactive)
  (let ((input
         (let ((keymap (copy-keymap minibuffer-local-map)))
           (define-key keymap (kbd "RET") 'newline)
           (read-from-minibuffer "Enter bibtex (C-j to finish): " nil keymap))))
    (let ((entries
           (with-temp-buffer
             (insert input)
             (goto-char (point-min))
             (org-bibtex-read-buffer (current-buffer)))))
      (dotimes (_ entries)
        (save-excursion (org-bibtex-write))
        (re-search-forward org-property-end-re)
        (open-line 1) (forward-char 1)))))

(defun bibliography-activate ()
  "Activates bibliography mode."

  ;; Set org-bibtex headline format
  (setq org-bibtex-headline-format-function #'bibliography-headline-format)

  (with-current-buffer (current-buffer)
    (let ((map (current-local-map)))
      (define-key map (kbd "C-c b a") 'add-bib-entry))))

(defun bibliography-deactivate ()
  "De-activates bibliography mode"

  ;; Reset default org-bibtex headline format
  (setq org-bibtex-headline-format-function #'(lambda (entry) (cdr (assq :title entry)))))

;; The minor mode
(define-minor-mode bibliography-mode
  "Minor mode for my annotated bibliography"

  :initial-value t
  :global nil
  :lighter "bibliography"

  (if bibliography-mode
      (bibliography-activate)
    (bibliography-deactivate)))

(provide 'bibliography)
;;; bibliography.el ends here
