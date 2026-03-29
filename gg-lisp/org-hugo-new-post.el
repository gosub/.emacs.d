;;; org-hugo-new-post.el --- Insert a new Hugo post heading -*- lexical-binding: t; -*-

;;; Code:

(require 'org)
(declare-function org-hugo-slug "ox-hugo")

;;;###autoload
(defun org-hugo-new-post (title slug tags)
  "Create a new Hugo post under '* posts' in current Org buffer."
  (interactive
   (let* ((title (read-string "Post title: "))
          (date (format-time-string "%Y-%m-%d"))
          (base-slug (org-hugo-slug title))
          (default-slug (format "%s-%s" date base-slug))
          (slug (read-string (format "Slug (default %s): " default-slug)
                             nil nil default-slug))
          (tags (read-string "Tags (space-separated): ")))
     (list title slug tags)))

  (let* ((datetime (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
         (export-file-name slug)
         (tag-list (split-string tags "[ ,]+" t))
         new-heading-pos)

    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^\\* posts" nil t)
        (error "No '* posts' heading found"))

      (forward-line 1)
      (beginning-of-line)

      ;; Insert a level-2 TODO heading directly to avoid org-insert-heading
      ;; adding unwanted blank lines or inheriting the wrong level
      (insert "** TODO " title "\n")
      (forward-line -1)
      (setq new-heading-pos (point))

      (when tag-list
        (org-set-tags tag-list))

      (org-set-property "EXPORT_FILE_NAME" export-file-name)
      (org-set-property "EXPORT_DATE" datetime)

      ;; Leave one blank line after :END: for content
      (org-end-of-meta-data t)
      (insert "\n"))

    ;; Collapse everything, then reveal only the new heading
    (org-overview)
    (goto-char new-heading-pos)
    (org-fold-show-subtree)))

(provide 'org-hugo-new-post)
;;; org-hugo-new-post.el ends here
