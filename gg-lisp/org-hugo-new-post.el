
(defun org-hugo-new-post (title slug tags)
  "Create a new Hugo post under '* posts' in current Org buffer."
  (interactive
   (let* ((title (read-string "Post title: "))
          (date (format-time-string "%Y-%m-%d"))
          (base-slug (org-hugo-slug title))
          ;; Default slug includes date
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

      ;; Go to first child position
      (org-show-subtree)
      (forward-line 1)

      ;; Insert heading
      (org-insert-heading)
      (org-do-demote)
      (insert title)

      ;; Tags (org-native)
      (when tag-list
        (org-set-tags tag-list))

      ;; Properties
      (org-set-property "EXPORT_FILE_NAME" export-file-name)
      (org-set-property "EXPORT_DATE" datetime)

      (end-of-line)
      (insert "\n\n"))))
