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
         (tag-list (split-string tags "[ ,]+" t)))

    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^\\* posts" nil t)
        (error "No '* posts' heading found"))

      (org-fold-show-subtree)
      (forward-line 1)
      (beginning-of-line)

      ;; Insert a level-2 heading directly to avoid org-insert-heading
      ;; adding unwanted blank lines or inheriting the wrong level
      (insert "** " title "\n")
      (forward-line -1)

      (when tag-list
        (org-set-tags tag-list))

      (org-set-property "EXPORT_FILE_NAME" export-file-name)
      (org-set-property "EXPORT_DATE" datetime)

      ;; Leave point after :END: with one blank line for content
      (org-end-of-meta-data t)
      (insert "\n"))))

(provide 'org-hugo-new-post)
;;; org-hugo-new-post.el ends here
