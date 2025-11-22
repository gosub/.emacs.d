;; Example usage:
;; (gg/rot128-file "/path/to/input.bin")
;; (gg/rot128-file "/path/to/input.bin" "/path/to/output.bin")

(defun gg/rot128-file (input-file &optional output-file)
  "Load a binary INPUT-FILE into a unibyte temporary buffer, apply ROT128 transformation,
and save to OUTPUT-FILE. If OUTPUT-FILE is nil, append '.rot128' to INPUT-FILE."
  (let ((output (or output-file (concat input-file ".rot128"))))
    (with-temp-buffer
      (set-buffer-multibyte nil)  ; Make buffer unibyte

      ;; Read the binary file literally
      (insert-file-contents-literally input-file)

      ;; Apply ROT128 transformation
      (goto-char (point-min))
      (while (not (eobp))
        (let ((byte (char-after)))
          (delete-char 1)
          (insert-char (logand (+ byte 128) 255))))

      ;; Write the transformed content to the output file
      (write-region (point-min) (point-max) output nil 'silent))))


(provide 'gg-rot128)
