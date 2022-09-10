;;; bibliography.el --- Annotated Bibliography minor-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Anirudh Chandramouli

;; Author: Anirudh Chandramouli <anirudhvan@gmail.com>
;; Homepage: https://github.com/Anirudh-C/bibliography-mode
;; Keywords: convenience
;; Version: 0.3

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

;;; News

;; Version 0.3
;; - Bug fixes for org-bibtex-read
;; - Modified some keybindings
;;
;; Version 0.2
;; - Added previews and utilities
;; - Documentation added
;;
;; Version 0.1
;; - Capability to add entries

;;; Code:

;; Emacs Core
(require 'org-indent)
(require 'org-element)
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
  (setq buffer-read-only nil)
  (let ((input
         (let ((keymap (copy-keymap minibuffer-local-map)))
           (define-key keymap (kbd "RET") 'newline)
           (read-from-minibuffer "Enter bibtex (C-j to finish): " nil keymap))))
    (let ((entries
           (with-temp-buffer
             (insert input)
             (goto-char (point-min))
             (bibtex-mode)
             (bibtex-set-dialect 'biblatex t)
             (org-bibtex-read-buffer (current-buffer)))))
      (end-of-buffer)
      (dotimes (_ entries)
        (save-excursion (org-bibtex-write))
        (end-of-buffer))))
  (save-buffer)
  (setq buffer-read-only t))

(defun tag-entry ()
  "Interactively tag entry at current point"

  (interactive)
  (setq buffer-read-only nil)
  (org-set-tags-command)
  (save-buffer)
  (setq buffer-read-only t))

(defun preview ()
  "Build a preview of the entry at point."

  (let* ((author (org-entry-get (point) "AUTHOR"))
         (title (org-entry-get (point) "ITEM"))
         (doi (org-entry-get (point) "DOI"))
         (year (org-entry-get (point) "YEAR"))
         (pdf (org-entry-get (point) "PDF"))
         (filename (buffer-file-name (current-buffer)))
         (custom-id (org-entry-get (point) "CUSTOM_ID"))
         (journal (or (org-entry-get (point) "JOURNAL")
                      (org-entry-get (point) "CONFERENCE")
                      (if (string= (org-entry-get (point) "BTYPE") "book") "BOOK")
                      (if (member "PREPRINT" (org-get-tags)) "PREPRINT")
                      "ONLINE"))
         (notes (save-excursion
                  (org-goto-first-child)
                  (org-goto-sibling)
                  (let* ((element (org-element-at-point))
                         (beg (org-element-property :robust-begin element))
                         (end (org-element-property :robust-end element)))
                    (if (and beg end)
                        (buffer-substring-no-properties beg (+ end 1))))))
         (notes (if notes
                    (string-trim notes)
                  "None.")))

    (concat
     (propertize (format "[[file:%s::#%s][%s (%s)]]\n"
                         filename  custom-id (upcase journal) year)
                 'face '(:inherit (nano-popout nano-strong) :height 0.85))
     (propertize (format "*%s*\n" title))
     (propertize (format "/%s/\n\n" author) 'face '(:inherit default :height 0.85))
     (concat
      (propertize "*Notes.* " 'face 'bold)
      (if notes
          (propertize notes 'face '(:inherit nano-default))
        "")
      "\n\n")
     (propertize (format "[[%s]]" pdf)))))

(defun hide-preview ()
  "Delete preview window"

  (interactive)
  (delete-other-windows)
  (when (get-buffer "*bib-preview*")
    (kill-buffer "*bib-preview*")))

(defun show-pdf ()
  "Show PDF in dedicated buffer"

  (interactive)
  (hide-preview)
  (when (org-entry-get (point) "PDF")
    (find-file (org-entry-get (point) "PDF"))
    (setq-local header-line-format nil)))

(defun show-preview ()
  "View preview of current item in a dedicated preview buffer."

  (interactive)
  (hide-preview)
  (let ((preview))
    (save-excursion
      (if (and (eq 1 (org-element-property :level (org-element-at-point)))
               (org-goto-first-child))
          (let ((sibling t))
            (while sibling
              (setq preview (concat preview (preview)))
              (setq sibling (org-goto-sibling))))
        (setq preview (preview))))

    (split-window-right)
    (let ((buf (current-buffer)))
      (switch-to-buffer-other-window (get-buffer-create "*bib-preview*"))
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (insert preview)
        (visual-line-mode)
        (hl-line-mode)
        (org-mode)
        (setq-local cursor-type nil)
        (setq org-image-actual-width (list (window-width nil t)))
        (setq-local org-return-follows-link t)
        (setq buffer-read-only t)
        (setq header-line-format nil)
        (goto-char (point-min))
        (switch-to-buffer-other-window buf)))))

(defun import-bib-entries (file)
  "Import bib entries from file"

  (interactive "fFile: ")
  (setq buffer-read-only nil)
  (end-of-buffer)
  (dotimes (_
            (with-temp-buffer
              (insert-file-contents file)
              (bibtex-mode)
              (bibtex-set-dialect 'biblatex t)
              (org-bibtex-read-buffer (current-buffer))))
    (save-excursion (org-bibtex-write))
    (end-of-buffer))
  (save-buffer)
  (setq buffer-read-only t))

(defun bibliography-activate ()
  "Activates bibliography mode."

  ;; Set org-bibtex headline format
  (setq org-bibtex-headline-format-function #'bibliography-headline-format)

  ;; Set read-write permissions
  (setq buffer-read-only t))

(defun bibliography-deactivate ()
  "De-activates bibliography mode"

  (interactive)
  ;; Reset default org-bibtex headline format
  (setq org-bibtex-headline-format-function #'(lambda (entry) (cdr (assq :title entry))))

  ;; Restore read-write permissions
  (setq buffer-read-only nil)

  ;; Hide any open previews
  (hide-preview)

  ;; Kill the bibliography buffer
  (kill-buffer (current-buffer)))

;; The minor mode
(define-minor-mode bibliography-mode
  "Minor mode for my annotated bibliography"

  :initial-value t
  :global nil
  :lighter "bibliography"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "a") 'add-bib-entries)
            (define-key map (kbd "i") 'import-bib-entries)
            (define-key map (kbd "e") 'org-bibtex)
            (define-key map (kbd "t") 'tag-entry)
            (define-key map (kbd "RET") 'show-preview)
            (define-key map (kbd "DEL") 'hide-preview)
            (define-key map (kbd "p") 'show-pdf)
            (define-key map (kbd "s") 'org-tags-view)
            (define-key map (kbd "q") 'bibliography-deactivate)
            map)

  (if bibliography-mode
      (bibliography-activate)
    (bibliography-deactivate)))

(provide 'bibliography)
;;; bibliography.el ends here
