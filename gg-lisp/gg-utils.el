;;; gg-utils.el --- General utility functions -*- lexical-binding: t; -*-

(defun gg/get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun gg/linux-computer-model ()
  "Get computer model from Linux DMI info."
  (interactive)
  (let ((dmi-product-family-file "/sys/devices/virtual/dmi/id/product_family"))
    (if (file-readable-p dmi-product-family-file)
	(string-trim (gg/get-string-from-file dmi-product-family-file))
      "unknown")))

(defun gg/is-computer-model? (model)
  "Check if current computer model matches MODEL (Linux only)."
  (string= model (gg/linux-computer-model)))

(defun gg/count-universal-arguments (arg)
  "Count how many times C-u was pressed to produce ARG."
  (if (and (integerp arg)
           (> arg 0)
           (= 0 (mod arg 4)))
      (round (log arg 4))
    0))

(provide 'gg-utils)
;;; gg-utils.el ends here
