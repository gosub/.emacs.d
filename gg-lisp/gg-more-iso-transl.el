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


(defun more-ctl-x-8-greek ()
  "Add more greek unicode characters to C-x 8"
  (let ((greek-alist '(("ga" . "α") ("gA" . "Α")
                       ("gb" . "β") ("gB" . "Β")
                       ("gc" . "χ") ("gC" . "Χ")
                       ("gd" . "δ") ("gD" . "Δ")
                       ("ge" . "ε") ("gE" . "Ε")
                       ("gf" . "φ") ("gF" . "Φ")
                       ("gg" . "γ") ("gG" . "Γ")
                       ("gh" . "η") ("gH" . "Η")
                       ("gi" . "ι") ("gI" . "Ι")
                       ("gk" . "κ") ("gK" . "Κ")
                       ("gl" . "λ") ("gL" . "Λ")
                       ("gm" . "μ") ("gM" . "Μ")
                       ("gn" . "ν") ("gN" . "Ν")
                       ("go" . "ο") ("gO" . "Ο")
                       ("gp" . "π") ("gP" . "Π")
                       ("gq" . "θ") ("gQ" . "Θ")
                       ("gr" . "ρ") ("gR" . "Ρ")
                       ("gs" . "σ") ("gS" . "Σ")
                       ("gt" . "τ") ("gT" . "Τ")
                       ("gu" . "υ") ("gU" . "Υ")
                       ("gv" . "ς") ("gV" . "Σ")
                       ("gw" . "ω") ("gW" . "Ω")
                       ("gx" . "ξ") ("gX" . "Ξ")
                       ("gy" . "ψ") ("gY" . "Ψ")
                       ("gz" . "ζ") ("gZ" . "Ζ"))))
    (dolist (cell greek-alist)
      (define-key 'iso-transl-ctl-x-8-map
                  (car cell) (cdr cell)))))


(provide 'gg-more-iso-transl)
