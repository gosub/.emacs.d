;;; crop-gg.el --- Keyboard-driven image cropping -*- lexical-binding: t; -*-

(defvar-local crop--path    nil)
(defvar-local crop--img-w   nil)
(defvar-local crop--img-h   nil)
(defvar-local crop--scale   1.0)
(defvar-local crop--x1      0)
(defvar-local crop--y1      0)
(defvar-local crop--x2      nil)
(defvar-local crop--y2      nil)
(defvar-local crop--handle  'tl)   ; active corner: 'tl or 'br
(defvar-local crop--step    100)   ; current step size: 100, 10, or 1
(defvar-local crop--img-uri nil)   ; data URI, built once at setup

(defun crop--mime-type (path)
  (let ((ext (downcase (or (file-name-extension path) ""))))
    (cond ((string= ext "png")             "image/png")
          ((member ext '("jpg" "jpeg"))    "image/jpeg")
          ((string= ext "gif")             "image/gif")
          ((string= ext "webp")            "image/webp")
          ((string= ext "bmp")             "image/bmp")
          ((member ext '("tif" "tiff"))    "image/tiff")
          (t                               "image/png"))))

(defun crop--render ()
  (let* ((dw  (round (* crop--img-w crop--scale)))
         (dh  (round (* crop--img-h crop--scale)))
         (dx1 (round (* crop--x1 crop--scale)))
         (dy1 (round (* crop--y1 crop--scale)))
         (dx2 (round (* crop--x2 crop--scale)))
         (dy2 (round (* crop--y2 crop--scale)))
         (hx  (if (eq crop--handle 'tl) dx1 dx2))
         (hy  (if (eq crop--handle 'tl) dy1 dy2))
         (lbl (format "%dx%d+%d+%d [%s step:%d]"
                      (- crop--x2 crop--x1) (- crop--y2 crop--y1)
                      crop--x1 crop--y1
                      (if (eq crop--handle 'tl) "TL" "BR")
                      crop--step))
         (svg (concat
               (format (concat "<svg xmlns='http://www.w3.org/2000/svg'"
                               " xmlns:xlink='http://www.w3.org/1999/xlink'"
                               " width='%d' height='%d'>") dw dh)
               (format "<image xlink:href='%s' x='0' y='0' width='%d' height='%d'/>"
                       crop--img-uri dw dh)
               ;; dark overlay: top, bottom, left, right strips
               (format "<rect x='0' y='0' width='%d' height='%d' fill='rgba(0,0,0,0.55)'/>"
                       dw dy1)
               (format "<rect x='0' y='%d' width='%d' height='%d' fill='rgba(0,0,0,0.55)'/>"
                       dy2 dw (- dh dy2))
               (format "<rect x='0' y='%d' width='%d' height='%d' fill='rgba(0,0,0,0.55)'/>"
                       dy1 dx1 (- dy2 dy1))
               (format "<rect x='%d' y='%d' width='%d' height='%d' fill='rgba(0,0,0,0.55)'/>"
                       dx2 dy1 (- dw dx2) (- dy2 dy1))
               ;; crop border
               (format "<rect x='%d' y='%d' width='%d' height='%d' fill='none' stroke='yellow' stroke-width='2' stroke-dasharray='6 3'/>"
                       dx1 dy1 (- dx2 dx1) (- dy2 dy1))
               ;; active handle marker
               (format "<circle cx='%d' cy='%d' r='6' fill='yellow' opacity='0.9'/>" hx hy)
               ;; geometry label
               (format "<text x='%d' y='%d' fill='yellow' font-size='12' font-family='monospace'>%s</text>"
                       (+ dx1 4) (- dy2 4) lbl)
               "</svg>")))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-image (create-image svg 'svg t)))))

(defun crop--clamp ()
  (setq crop--x2 (min crop--img-w (max (1+ crop--x1) crop--x2))
        crop--y2 (min crop--img-h (max (1+ crop--y1) crop--y2))
        crop--x1 (max 0           (min (1- crop--x2) crop--x1))
        crop--y1 (max 0           (min (1- crop--y2) crop--y1))))

(defun crop--redraw ()
  (crop--clamp)
  (crop--render))

(defun crop--setup ()
  (let* ((path crop--path)
         (img  (create-image path))
         (size (image-size img t))
         (mime (crop--mime-type path))
         (b64  (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally path)
                 (base64-encode-string (buffer-string)))))
    (setq crop--img-w   (car size)
          crop--img-h   (cdr size)
          crop--scale   (min 1.0
                             (/ (float (window-body-width  nil t)) (car size))
                             (/ (float (window-body-height nil t)) (cdr size)))
          crop--x2      (car size)
          crop--y2      (cdr size)
          crop--img-uri (format "data:%s;base64,%s" mime b64)))
  (crop--redraw))

(defun crop--move (dx dy)
  (if (eq crop--handle 'tl)
      (setq crop--x1 (+ crop--x1 dx)
            crop--y1 (+ crop--y1 dy))
    (setq crop--x2 (+ crop--x2 dx)
          crop--y2 (+ crop--y2 dy)))
  (crop--redraw))

(defun crop-toggle-handle-gg ()
  "Toggle active handle between top-left (TL) and bottom-right (BR)."
  (interactive)
  (setq crop--handle (if (eq crop--handle 'tl) 'br 'tl))
  (crop--redraw))

(defun crop-cycle-step-gg ()
  "Cycle step size through 100, 10, 1."
  (interactive)
  (setq crop--step (pcase crop--step (100 10) (10 1) (_ 100)))
  (crop--redraw))

(defun crop-move-right-gg () (interactive) (crop--move  crop--step 0))
(defun crop-move-left-gg  () (interactive) (crop--move (- crop--step) 0))
(defun crop-move-down-gg  () (interactive) (crop--move 0  crop--step))
(defun crop-move-up-gg    () (interactive) (crop--move 0 (- crop--step)))

(defun crop-help-gg ()
  "Show keybinding help in the echo area."
  (interactive)
  (message "arrows: move  s: step(%d)  TAB: handle(%s)  r: reset  RET: crop  q/ESC: quit"
           crop--step
           (if (eq crop--handle 'tl) "TL" "BR")))

(defun crop-reset-gg ()
  "Reset crop region to the full image."
  (interactive)
  (setq crop--x1 0 crop--y1 0
        crop--x2 crop--img-w crop--y2 crop--img-h)
  (crop--redraw))

(defun crop-execute-gg ()
  "Perform the crop using ImageMagick and write to file."
  (interactive)
  (let* ((geom (format "%dx%d+%d+%d"
                       (- crop--x2 crop--x1) (- crop--y2 crop--y1)
                       crop--x1 crop--y1))
         (src  crop--path)
         (ext  (file-name-extension src t))
         (dest (expand-file-name
                (read-file-name
                 "Output file: "
                 (file-name-directory src)
                 nil nil
                 (concat (file-name-nondirectory
                          (file-name-sans-extension src))
                         "-crop" ext)))))
    (with-temp-buffer
      (unless (zerop (call-process "magick" nil t nil src "-crop" geom dest))
        (error "ImageMagick convert failed: %s" (buffer-string))))
    (message "Cropped -> %s" dest)
    (kill-buffer)))

(defvar crop-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB")     #'crop-toggle-handle-gg)
    (define-key map (kbd "s")       #'crop-cycle-step-gg)
    (define-key map (kbd "<right>") #'crop-move-right-gg)
    (define-key map (kbd "<left>")  #'crop-move-left-gg)
    (define-key map (kbd "<down>")  #'crop-move-down-gg)
    (define-key map (kbd "<up>")    #'crop-move-up-gg)
    (define-key map (kbd "RET")     #'crop-execute-gg)
    (define-key map (kbd "r")       #'crop-reset-gg)
    (define-key map (kbd "h")       #'crop-help-gg)
    (define-key map (kbd "?")       #'crop-help-gg)
    (define-key map (kbd "q")       #'kill-buffer)
    (define-key map (kbd "<escape>") #'kill-buffer)
    map))

(define-derived-mode crop-mode special-mode "Crop"
  "Major mode for keyboard-driven image cropping.
TAB toggles the active corner handle (TL/BR).
Arrow keys move the active handle by the current step size.
s cycles the step size through 100, 10, and 1 pixels.
RET performs the crop via ImageMagick."
  (setq-local cursor-type nil)
  (use-local-map crop-mode-map))

(defun crop-image-gg (path)
  "Open an interactive crop buffer for the image at PATH.
If the current buffer visits a supported image file, it is used as default."
  (interactive
   (list (if (and buffer-file-name
                  (member (downcase (or (file-name-extension buffer-file-name) ""))
                          '("png" "jpg" "jpeg" "gif" "webp" "bmp" "tif" "tiff")))
             buffer-file-name
           (read-file-name "Image: " nil nil t))))
  (let ((buf (get-buffer-create
              (format "*crop: %s*" (file-name-nondirectory path)))))
    (with-current-buffer buf
      (crop-mode)
      (setq crop--path (expand-file-name path)))
    (switch-to-buffer buf)
    (crop--setup)))

(provide 'crop-gg)
;;; crop-gg.el ends here
