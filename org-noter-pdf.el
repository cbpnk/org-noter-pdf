;;; org-noter-pdf.el --- Modules for PDF-Tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  c1-g

;; Author: c1-g <char1iegordon@protonmail.com>
;; Homepage: https://github.com/cbpnk/org-noter-pdf
;; Keywords: org-noter pdf
;; Package-Requires: ((org-noter-core "1.5.0") (pdf-tools "1.0"))
;; Version: 1.5.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'org-noter-core)
(require 'pdf-tools)

(defcustom org-noter-pdf-arrow-delay 0.2
  "Number of seconds from when the command was invoked until the tooltip arrow appears.

When set to a negative number, the arrow tooltip is disabled.
This is needed in order to keep Emacs from hanging when doing many syncs."
  :group 'org-noter-pdf
  :type 'number)


(defvar org-noter-pdf-view--arrow-location nil
  "A vector [TIMER WINDOW TOP LEFT] that shows where the arrow should appear, when idling.")

(defun org-noter-pdf-view--mode-supported (major-mode)
  (eq major-mode 'pdf-view-mode))

(add-hook 'org-noter--mode-supported-hook #'org-noter-pdf-view--mode-supported)

(defun org-noter-pdf-view--doc-approx-location (major-mode &optional precise-info _force-new-ref)
  (when (org-noter-pdf-view--mode-supported major-mode)
    (cons (image-mode-window-get 'page)
          (if (and (listp precise-info)
                   (numberp (car precise-info))
                   (numberp (cadr precise-info)))
              precise-info 0))))

(add-hook 'org-noter--doc-approx-location-hook #'org-noter-pdf-view--doc-approx-location)

(defun org-noter-pdf-view--set-up-document (major-mode)
  (when (org-noter-pdf-view--mode-supported major-mode)
    (pdf-view-mode)
    (add-hook 'pdf-view-after-change-page-hook 'org-noter--doc-location-change-handler nil t)
    t))

(add-hook 'org-noter--set-up-document-hook #'org-noter-pdf-view--set-up-document)


(defun org-noter-pdf-view--pretty-print-location (location)
  (org-noter--with-valid-session
   (when (org-noter-pdf-view--mode-supported (org-noter--session-doc-mode session))
     (format "%s" (if (or (not (org-noter--get-location-top location))
                          (<= (org-noter--get-location-top location) 0))
                      (car location)
                    location)))))

(add-hook 'org-noter--pretty-print-location-hook #'org-noter-pdf-view--pretty-print-location)

(defun org-noter-pdf-view--get-precise-info (major-mode)
  (when (org-noter-pdf-view--mode-supported major-mode)
    (if (pdf-view-active-region-p)
        (let ((edges (pdf-view-active-region)))
          (car edges))

      (let (event)
        (while (not (and (eq 'mouse-1 (car event))
                         (eq (selected-window) (posn-window (event-start event)))))
          (setq event (read-event "Click where you want the start of the note to be!")))
        (let ((col-row (posn-col-row (event-start event))))
          (org-noter--conv-page-scroll-percentage (+ (window-vscroll) (cdr col-row))
                                                  (+ (window-hscroll) (car col-row))))))))

(add-hook 'org-noter--get-precise-info-hook #'org-noter-pdf-view--get-precise-info)


(defun org-noter-pdf-view--show-arrow ()
  (when (and org-noter-pdf-view--arrow-location
             (window-live-p (aref org-noter-pdf-view--arrow-location 1)))
    (with-selected-window (aref org-noter-pdf-view--arrow-location 1)
      ;; From `pdf-util-tooltip-arrow'.
      (pdf-util-assert-pdf-window)
      (let* (x-gtk-use-system-tooltips
             (image-top (if (floatp (aref org-noter-pdf-view--arrow-location 2))
                            (round (* (aref org-noter-pdf-view--arrow-location 2)
                                      (cdr (pdf-view-image-size))))))
             (image-left (if (floatp (aref org-noter-pdf-view--arrow-location 3))
                             (round (* (aref org-noter-pdf-view--arrow-location 3) (car (pdf-view-image-size))))))

	     (dx (or image-left
                     (+ (or (car (window-margins)) 0)
                        (car (window-fringes)))))
             (dy (or image-top 0))
             (pos (list dx dy dx (+ dy (* 2 (frame-char-height)))))
             (vscroll (pdf-util-required-vscroll pos))
             (tooltip-frame-parameters
              `((border-width . 0)
                (internal-border-width . 0)
                ,@tooltip-frame-parameters))
             (tooltip-hide-delay 3))

        (when vscroll
          (image-set-window-vscroll vscroll))
        (setq dy (max 0 (- dy
                           (cdr (pdf-view-image-offset))
                           (window-vscroll nil t)
                           (frame-char-height))))
        (when (overlay-get (pdf-view-current-overlay) 'before-string)
          (let* ((e (window-inside-pixel-edges))
                 (xw (pdf-util-with-edges (e) e-width)))
            (cl-incf dx (/ (- xw (car (pdf-view-image-size t))) 2))))
        (pdf-util-tooltip-in-window
         (propertize
          " " 'display (propertize
                        "\u2192" ;; right arrow
                        'display '(height 2)
                        'face `(:foreground
                                "orange red"
                                :background
                                ,(if (bound-and-true-p pdf-view-midnight-minor-mode)
                                     (cdr pdf-view-midnight-colors)
                                   "white"))))
         dx dy))
      (setq org-noter-pdf-view--arrow-location nil))))

(defun org-noter-pdf-view--doc-goto-location (mode location)
  (when (eq mode 'pdf-view-mode)
    (let ((top (org-noter--get-location-top location))
          (left (org-noter--get-location-left location)))

      (pdf-view-goto-page (org-noter--get-location-page location))
      ;; NOTE(nox): This timer is needed because the tooltip may introduce a delay,
      ;; so syncing multiple pages was slow
      (when (>= org-noter-pdf-arrow-delay 0)
        (when org-noter-pdf-view--arrow-location (cancel-timer (aref org-noter-pdf-view--arrow-location 0)))
        (setq org-noter-pdf-view--arrow-location
              (vector (run-with-idle-timer org-noter-pdf-arrow-delay nil 'org-noter-pdf-view--show-arrow)
                      (selected-window)
                      top
                      left)))
      (image-scroll-up (- (org-noter--conv-page-percentage-scroll top)
                          (window-vscroll))))))

(add-hook 'org-noter--doc-goto-location-hook #'org-noter-pdf-view--doc-goto-location)

(defun org-noter-pdf-view--get-current-view (mode)
  (when (org-noter-pdf-view--mode-supported mode)
    (vector 'paged (car (org-noter-pdf-view--doc-approx-location mode)))))

(add-hook 'org-noter--get-current-view-hook #'org-noter-pdf-view--get-current-view)

(defun org-noter-pdf-view--get-selected-text (mode)
  (when (and (org-noter-pdf-view--mode-supported mode)
             (pdf-view-active-region-p))
    (mapconcat 'identity (pdf-view-active-region-text) ? )))

(add-hook 'org-noter-get-selected-text-hook #'org-noter-pdf-view--get-selected-text)

;; NOTE(nox): From machc/pdf-tools-org
(defun org-noter--pdf-tools-edges-to-region (edges)
  "Get 4-entry region (LEFT TOP RIGHT BOTTOM) from several EDGES."
  (when edges
    (let ((left0 (nth 0 (car edges)))
          (top0 (nth 1 (car edges)))
          (bottom0 (nth 3 (car edges)))
          (top1 (nth 1 (car (last edges))))
          (right1 (nth 2 (car (last edges))))
          (bottom1 (nth 3 (car (last edges)))))
      (list left0
            (+ top0 (/ (- bottom0 top0) 3))
            right1
            (- bottom1 (/ (- bottom1 top1) 3))))))

(defun org-noter-pdf-view--create-skeleton (mode)
  "Create notes skeleton with the PDF outline or annotations."
  (when (org-noter-pdf-view--mode-supported mode)
    (org-noter--with-valid-session
     (let* ((ast (org-noter--parse-root))
            (top-level (or (org-element-property :level ast) 0))
            (options '(("Outline" . (outline))
                       ("Annotations" . (annots))
                       ("Both" . (outline annots))))
            answer output-data)
       (with-current-buffer (org-noter--session-doc-buffer session)
         (setq answer (assoc (completing-read "What do you want to import? " options nil t) options))

         (when (memq 'outline answer)
           (dolist (item (pdf-info-outline))
             (let ((type (alist-get 'type item))
                   (page (alist-get 'page item))
                   (depth (alist-get 'depth item))
                   (title (alist-get 'title item))
                   (top (alist-get 'top item)))
               (when (and (eq type 'goto-dest) (> page 0))
                 (push (vector title (cons page top) (1+ depth) nil) output-data)))))

         (when (memq 'annots answer)
           (let ((possible-annots (list '("Highlights" . highlight)
                                        '("Underlines" . underline)
                                        '("Squigglies" . squiggly)
                                        '("Text notes" . text)
                                        '("Strikeouts" . strike-out)
                                        '("Links" . link)
                                        '("ALL" . all)))
                 chosen-annots insert-contents pages-with-links)
             (while (> (length possible-annots) 1)
               (let* ((chosen-string (completing-read "Which types of annotations do you want? "
                                                      possible-annots nil t))
                      (chosen-pair (assoc chosen-string possible-annots)))
                 (cond ((eq (cdr chosen-pair) 'all)
                        (dolist (annot possible-annots)
                          (when (and (cdr annot) (not (eq (cdr annot) 'all)))
                            (push (cdr annot) chosen-annots)))
                        (setq possible-annots nil))
                       ((cdr chosen-pair)
                        (push (cdr chosen-pair) chosen-annots)
                        (setq possible-annots (delq chosen-pair possible-annots))
                        (when (= 1 (length chosen-annots)) (push '("DONE") possible-annots)))
                       (t
                        (setq possible-annots nil)))))

             (setq insert-contents (y-or-n-p "Should we insert the annotations contents? "))

             (dolist (item (pdf-info-getannots))
               (let* ((type (alist-get 'type item))
                      (page (alist-get 'page item))
                      (edges (or (org-noter--pdf-tools-edges-to-region (alist-get 'markup-edges item))
                                 (alist-get 'edges item)))
                      (top (nth 1 edges))
                      (item-subject (alist-get 'subject item))
                      (item-contents (alist-get 'contents item))
                      name contents)
                 (when (and (memq type chosen-annots) (> page 0))
                   (if (eq type 'link)
                       (cl-pushnew page pages-with-links)
                     (setq name (cond ((eq type 'highlight) "Highlight")
                                      ((eq type 'underline) "Underline")
                                      ((eq type 'squiggly) "Squiggly")
                                      ((eq type 'text) "Text note")
                                      ((eq type 'strike-out) "Strikeout")))

                     (when insert-contents
                       (setq contents (cons (pdf-info-gettext page edges)
                                            (and (or (and item-subject (> (length item-subject) 0))
                                                     (and item-contents (> (length item-contents) 0)))
                                                 (concat (or item-subject "")
                                                         (if (and item-subject item-contents) "\n" "")
                                                         (or item-contents ""))))))

                     (push (vector (format "%s on page %d" name page) (cons page top) 'inside contents)
                           output-data)))))

             (dolist (page pages-with-links)
               (let ((links (pdf-info-pagelinks page))
                     type)
                 (dolist (link links)
                   (setq type (alist-get 'type link))
                   (unless (eq type 'goto-dest) ;; NOTE(nox): Ignore internal links
                     (let* ((edges (alist-get 'edges link))
                            (title (alist-get 'title link))
                            (top (nth 1 edges))
                            (target-page (alist-get 'page link))
                            target heading-text)

                       (unless (and title (> (length title) 0)) (setq title (pdf-info-gettext page edges)))

                       (cond
                        ((eq type 'uri)
                         (setq target (alist-get 'uri link)
                               heading-text (format "Link on page %d: [[%s][%s]]" page target title)))

                        ((eq type 'goto-remote)
                         (setq target (concat "file:" (alist-get 'filename link))
                               heading-text (format "Link to document on page %d: [[%s][%s]]" page target title))
                         (when target-page
                           (setq heading-text (concat heading-text (format " (target page: %d)" target-page)))))

                        (t (error "Unexpected link type")))

                       (push (vector heading-text (cons page top) 'inside nil) output-data))))))))


         (when output-data
           (if (memq 'annots answer)
               (setq output-data
                     (sort output-data
                           (lambda (e1 e2)
                             (or (not (aref e1 1))
                                 (and (aref e2 1)
                                      (org-noter--compare-locations '< (aref e1 1) (aref e2 1)))))))
             (setq output-data (nreverse output-data)))

           (push (vector "Skeleton" nil 1 nil) output-data)))

       (with-current-buffer (org-noter--session-notes-buffer session)
         ;; NOTE(nox): org-with-wide-buffer can't be used because we want to reset the
         ;; narrow region to include the new headings
         (widen)
         (save-excursion
           (goto-char (org-element-property :end ast))

           (let (last-absolute-level
                 title location relative-level contents
                 level)
             (dolist (data output-data)
               (setq title (aref data 0)
                     location (aref data 1)
                     relative-level (aref data 2)
                     contents (aref data 3))

               (if (symbolp relative-level)
                   (setq level (1+ last-absolute-level))
                 (setq last-absolute-level (+ top-level relative-level)
                       level last-absolute-level))

               (org-noter--insert-heading level title)

               (when location
                 (org-entry-put nil org-noter-property-note-location (org-noter--pretty-print-location location)))

               (when org-noter-doc-property-in-notes
                 (org-entry-put nil org-noter-property-doc-file (org-noter--session-property-text session))
                 (org-entry-put nil org-noter--property-auto-save-last-location "nil"))

               (when (car contents)
                 (org-noter--insert-heading (1+ level) "Contents")
                 (insert (car contents)))
               (when (cdr contents)
                 (org-noter--insert-heading (1+ level) "Comment")
                 (insert (cdr contents)))))

           (setq ast (org-noter--parse-root))
           (org-noter--narrow-to-root ast)
           (goto-char (org-element-property :begin ast))
           (outline-hide-subtree)
           (org-show-children 2)))
       output-data))))

(add-hook 'org-noter-create-skeleton-functions #'org-noter-pdf-view--create-skeleton)

(provide 'org-noter-pdf)
;;; org-noter-pdf.el ends here
