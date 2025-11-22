;;; Additional Unicode ISO translation (C-x 8 map)

; additional superscript and subscript
; unicode characters with C-x 8


(defun gg/more-ctl-x-8-superscript ()
  "Add more unicode superscript characters to C-x 8."
  (define-key 'iso-transl-ctl-x-8-map "^=" [#x207C])
  (define-key 'iso-transl-ctl-x-8-map "^(" [#x207D])
  (define-key 'iso-transl-ctl-x-8-map "^)" [#x207E])
  (define-key 'iso-transl-ctl-x-8-map "^n" [#x207F])
  (define-key 'iso-transl-ctl-x-8-map "^i" [#x2071]))


(defun gg/more-ctl-x-8-subscript ()
  "Add more unicode subscript characters to C-x 8."
  (define-key 'iso-transl-ctl-x-8-map "_=" [#x208C])
  (define-key 'iso-transl-ctl-x-8-map "_(" [#x208D])
  (define-key 'iso-transl-ctl-x-8-map "_)" [#x208E])
  (define-key 'iso-transl-ctl-x-8-map "_n" [#x2099]))


(provide 'gg-more-iso-transl)
