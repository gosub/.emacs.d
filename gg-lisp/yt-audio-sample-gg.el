;;; yt-audio-sample-gg.el --- Save audio samples from YouTube -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; `yt-audio-sample-gg' asks for a YouTube link (or bare video id), an
;; output folder, an optional start and end timestamp, and an optional
;; audio format, then downloads just that slice of the audio track.
;;
;; The download is always done by yt-dlp.  Cutting is done by yt-dlp's
;; own `--download-sections' when it is available (much faster: only the
;; requested range is fetched), otherwise the full audio is downloaded
;; and ffmpeg trims it.  Format conversion is done by whichever tool is
;; doing the cut.  See `yt-audio-sample-gg-cut-method'.

;;; Code:

(require 'subr-x)

(defgroup yt-audio-sample-gg nil
  "Save audio samples from YouTube videos."
  :group 'multimedia
  :prefix "yt-audio-sample-gg-")

(defcustom yt-audio-sample-gg-yt-dlp-program "yt-dlp"
  "Program used to download audio from YouTube."
  :type 'string
  :group 'yt-audio-sample-gg)

(defcustom yt-audio-sample-gg-ffmpeg-program "ffmpeg"
  "Program used to cut and convert audio."
  :type 'string
  :group 'yt-audio-sample-gg)

(defcustom yt-audio-sample-gg-output-directory "~/"
  "Default folder where samples are written.
Used when the output folder prompt is left empty."
  :type 'directory
  :group 'yt-audio-sample-gg)

(defcustom yt-audio-sample-gg-formats
  '("mp3" "m4a" "opus" "flac" "wav" "aac" "vorbis" "alac")
  "Candidate audio formats offered at the format prompt."
  :type '(repeat string)
  :group 'yt-audio-sample-gg)

(defcustom yt-audio-sample-gg-default-format nil
  "Audio format proposed at the format prompt.
nil means keep the format native to the video."
  :type '(choice (const :tag "Native to the video" nil) string)
  :group 'yt-audio-sample-gg)

(defcustom yt-audio-sample-gg-cut-method 'auto
  "How to cut the requested time range out of the audio.

`auto'   use yt-dlp's `--download-sections' if that option exists,
         fall back to downloading everything and trimming with ffmpeg.
`yt-dlp' always let yt-dlp cut, only the requested range is downloaded.
`ffmpeg' always download the whole audio and trim it with ffmpeg,
         which gives a sample-exact cut at the cost of bandwidth."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "Always yt-dlp" yt-dlp)
                 (const :tag "Always ffmpeg" ffmpeg))
  :group 'yt-audio-sample-gg)

(defcustom yt-audio-sample-gg-extra-yt-dlp-args nil
  "Extra command line arguments passed to yt-dlp."
  :type '(repeat string)
  :group 'yt-audio-sample-gg)

(defcustom yt-audio-sample-gg-buffer-name "*yt-audio-sample*"
  "Name of the buffer logging the yt-dlp and ffmpeg runs."
  :type 'string
  :group 'yt-audio-sample-gg)

(defvar yt-audio-sample-gg--sections-support 'unknown
  "Cached result of probing yt-dlp for `--download-sections'.")


;;; Parsing helpers

(defun yt-audio-sample-gg--url (input)
  "Turn INPUT, a YouTube link or a bare video id, into a watch URL."
  (let ((s (string-trim (or input ""))))
    (cond
     ((string-empty-p s)
      (user-error "No video given"))
     ((string-match-p "\\`https?://" s) s)
     ((string-match-p "\\`\\(www\\.\\|m\\.\\)?\\(youtube\\.com\\|youtu\\.be\\)/" s)
      (concat "https://" s))
     ((string-match-p "\\`[A-Za-z0-9_-]\\{11\\}\\'" s)
      (concat "https://www.youtube.com/watch?v=" s))
     (t
      (user-error "Not a YouTube link or video id: %s" s)))))

(defun yt-audio-sample-gg--parse-timestamp (input)
  "Parse INPUT into a number of seconds, or nil when INPUT is empty.
Accepted forms are SS, MM:SS and HH:MM:SS, with optional decimals."
  (let ((s (string-trim (or input ""))))
    (if (string-empty-p s)
        nil
      (let ((parts (split-string s ":" t)))
        (unless (and (<= (length parts) 3)
                     (seq-every-p
                      (lambda (p) (string-match-p "\\`[0-9]+\\(\\.[0-9]+\\)?\\'" p))
                      parts))
          (user-error "Invalid timestamp: %s" s))
        (seq-reduce (lambda (acc p) (+ (* acc 60) (string-to-number p)))
                    parts 0)))))

(defun yt-audio-sample-gg--normalize-format (input)
  "Return INPUT as an audio format string, or nil for the native format."
  (let ((s (downcase (string-trim (or input "")))))
    (unless (member s '("" "native"))
      s)))

(defun yt-audio-sample-gg--slug (secs)
  "Format SECS as a compact filename-friendly timestamp."
  (let* ((total (floor secs))
         (h (/ total 3600))
         (m (/ (% total 3600) 60))
         (s (% total 60)))
    (if (> h 0)
        (format "%dh%02dm%02ds" h m s)
      (format "%02dm%02ds" m s))))

(defun yt-audio-sample-gg--suffix (start end)
  "Return the filename suffix describing the range START to END."
  (if (or start end)
      (format " [%s-%s]"
              (if start (yt-audio-sample-gg--slug start) "start")
              (if end (yt-audio-sample-gg--slug end) "end"))
    ""))


;;; Process plumbing

(defun yt-audio-sample-gg--check-program (program)
  "Signal a user error unless PROGRAM is on `exec-path'."
  (unless (executable-find program)
    (user-error "Program not found: %s" program)))

(defun yt-audio-sample-gg--yt-dlp-can-cut-p ()
  "Return non-nil when the configured yt-dlp knows `--download-sections'."
  (when (eq yt-audio-sample-gg--sections-support 'unknown)
    (setq yt-audio-sample-gg--sections-support
          (with-temp-buffer
            (ignore-errors
              (call-process yt-audio-sample-gg-yt-dlp-program nil t nil "--help"))
            (goto-char (point-min))
            (and (search-forward "--download-sections" nil t) t))))
  yt-audio-sample-gg--sections-support)

(defun yt-audio-sample-gg--use-ffmpeg-cut-p ()
  "Return non-nil when ffmpeg, not yt-dlp, should perform the cut."
  (pcase yt-audio-sample-gg-cut-method
    ('ffmpeg t)
    ('yt-dlp nil)
    (_ (not (yt-audio-sample-gg--yt-dlp-can-cut-p)))))

(defun yt-audio-sample-gg--run (label program args on-success &optional on-failure)
  "Run PROGRAM with ARGS asynchronously, logging the run.
LABEL names the step in messages.  ON-SUCCESS is called with no
arguments when the process exits with status zero, ON-FAILURE when
it does not."
  (let ((buf (get-buffer-create yt-audio-sample-gg-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n$ %s %s\n" program
                      (mapconcat #'shell-quote-argument args " "))))
    (message "yt-audio-sample: %s running..." label)
    (make-process
     :name (format "yt-audio-sample-%s" label)
     :buffer buf
     :command (cons program args)
     :noquery t
     :sentinel
     (lambda (proc event)
       (when (memq (process-status proc) '(exit signal))
         (if (and (eq (process-status proc) 'exit)
                  (zerop (process-exit-status proc)))
             (funcall on-success)
           (when on-failure (funcall on-failure))
           (message "yt-audio-sample: %s failed (%s)" label (string-trim event))
           (display-buffer buf)))))))


;;; The two download strategies

(defun yt-audio-sample-gg--download-section (url dir start end fmt suffix)
  "Let yt-dlp fetch and cut URL between START and END into DIR.
FMT is an audio format or nil for native, SUFFIX names the range."
  (let* ((default-directory dir)
         (args (append
                (list "--no-playlist" "--extract-audio")
                (when fmt (list "--audio-format" fmt))
                (when (or start end)
                  (list "--download-sections"
                        (format "*%s-%s"
                                (if start (number-to-string start) "0")
                                (if end (number-to-string end) "inf"))))
                (list "-P" (directory-file-name dir)
                      "-o" (concat "%(title)s [%(id)s]" suffix ".%(ext)s"))
                yt-audio-sample-gg-extra-yt-dlp-args
                (list url))))
    (yt-audio-sample-gg--run
     "yt-dlp" yt-audio-sample-gg-yt-dlp-program args
     (lambda () (message "yt-audio-sample: saved into %s" dir)))))

(defun yt-audio-sample-gg--download-then-cut (url dir start end fmt suffix)
  "Download the whole audio of URL, then have ffmpeg cut it into DIR.
START, END, FMT and SUFFIX are as in `yt-audio-sample-gg--download-section'."
  (let* ((tmp (file-name-as-directory (make-temp-file "yt-audio-sample-" t)))
         (default-directory tmp)
         (args (append
                (list "--no-playlist" "--extract-audio"
                      "-P" (directory-file-name tmp)
                      "-o" "%(title)s [%(id)s].%(ext)s")
                yt-audio-sample-gg-extra-yt-dlp-args
                (list url))))
    (yt-audio-sample-gg--run
     "yt-dlp" yt-audio-sample-gg-yt-dlp-program args
     (lambda () (yt-audio-sample-gg--cut tmp dir start end fmt suffix))
     (lambda () (delete-directory tmp t)))))

(defun yt-audio-sample-gg--cut (tmp dir start end fmt suffix)
  "Cut the file downloaded into TMP and write the result under DIR.
START and END delimit the range, FMT is an audio format or nil to
copy the stream untouched, SUFFIX names the range in the filename."
  (let ((src (car (directory-files tmp t directory-files-no-dot-files-regexp))))
    (unless src
      (delete-directory tmp t)
      (error "yt-audio-sample: yt-dlp produced no file"))
    (let* ((default-directory dir)
           (ext (or fmt (file-name-extension src)))
           (dest (expand-file-name
                  (concat (file-name-base src) suffix "." ext) dir))
           (args (append
                  (list "-hide_banner" "-loglevel" "warning" "-y")
                  (when start (list "-ss" (number-to-string start)))
                  (list "-i" src)
                  (when end (list "-t" (number-to-string (- end (or start 0)))))
                  (list "-vn")
                  (unless fmt (list "-c" "copy"))
                  (list dest))))
      (yt-audio-sample-gg--run
       "ffmpeg" yt-audio-sample-gg-ffmpeg-program args
       (lambda ()
         (delete-directory tmp t)
         (message "yt-audio-sample: saved %s" dest))
       (lambda () (delete-directory tmp t))))))


;;; Entry point

;;;###autoload
(defun yt-audio-sample-gg (video dir start end fmt)
  "Save the audio of VIDEO between START and END as a file under DIR.

VIDEO is a YouTube link or a bare 11 character video id.

DIR is the output folder; leaving the prompt empty uses
`yt-audio-sample-gg-output-directory'.

START and END are timestamps written as SS, MM:SS or HH:MM:SS.  An
empty START means the beginning of the video, an empty END means its
end.

FMT is an audio format such as \"mp3\" or \"flac\".  Empty, or the
word \"native\", keeps the format the video already carries, in which
case the audio is never re-encoded.

Downloading is done by `yt-audio-sample-gg-yt-dlp-program', cutting by
that same program or by `yt-audio-sample-gg-ffmpeg-program' depending
on `yt-audio-sample-gg-cut-method'.  Both run asynchronously and log
into `yt-audio-sample-gg-buffer-name'.  An already existing sample of
the same name may be overwritten."
  (interactive
   (list
    (read-string "YouTube link or video id: ")
    (read-directory-name "Output folder: "
                         (file-name-as-directory
                          (expand-file-name
                           yt-audio-sample-gg-output-directory)))
    (read-string "Start timestamp (empty = beginning): ")
    (read-string "End timestamp (empty = end): ")
    (completing-read "Audio format (empty = native): "
                     yt-audio-sample-gg-formats nil nil nil nil
                     yt-audio-sample-gg-default-format)))
  (let* ((url   (yt-audio-sample-gg--url video))
         (dir   (file-name-as-directory
                 (expand-file-name
                  (if (string-empty-p (string-trim (or dir "")))
                      yt-audio-sample-gg-output-directory
                    dir))))
         (start (yt-audio-sample-gg--parse-timestamp start))
         (end   (yt-audio-sample-gg--parse-timestamp end))
         (fmt   (yt-audio-sample-gg--normalize-format fmt)))
    (when (and start end (<= end start))
      (user-error "End timestamp must come after the start timestamp"))
    (yt-audio-sample-gg--check-program yt-audio-sample-gg-yt-dlp-program)
    (yt-audio-sample-gg--check-program yt-audio-sample-gg-ffmpeg-program)
    (make-directory dir t)
    (let ((suffix (yt-audio-sample-gg--suffix start end)))
      (if (and (or start end) (yt-audio-sample-gg--use-ffmpeg-cut-p))
          (yt-audio-sample-gg--download-then-cut url dir start end fmt suffix)
        (yt-audio-sample-gg--download-section url dir start end fmt suffix)))))

(provide 'yt-audio-sample-gg)
;;; yt-audio-sample-gg.el ends here
