;;; cdp8-sox.el --- SoX integration for cdp8-mode -*- lexical-binding: t; -*-
;;
;; Adds SoX effects as first-class cdp8-mode nodes with full schema
;; prompting support.  Load and call `cdp8-sox-setup' to activate.
;;
;; Usage in init.el:
;;   (with-eval-after-load 'cdp8-mode
;;     (require 'cdp8-sox)
;;     (cdp8-sox-setup))

(require 'cdp8-commands)

;; Forward declarations of variables defined in cdp8-mode.el.
(defvar cdp8-extra-commands)
(defvar cdp8-param-schemas)
(defvar cdp8-command-descriptions)

;;; Command list

(defvar cdp8-sox-commands
  '("sox reverb"
    "sox gain"
    "sox norm"
    "sox compand"
    "sox trim"
    "sox pad"
    "sox fade"
    "sox rate"
    "sox channels"
    "sox tempo"
    "sox pitch"
    "sox highpass"
    "sox lowpass"
    "sox equalizer"
    "sox repeat"
    "sox reverse"
    "sox echo"
    "sox chorus"
    "sox flanger"
    "sox overdrive"
    "sox silence")
  "SoX command specs for cdp8-mode.")

;;; Schemas
;;
;; Each entry uses :command-prefix "sox" so that the generated command is
;;   sox infile outfile EFFECT [params]
;; rather than the default CDP8 form  EFFECT infile outfile params.
;;
;; The :type literal param inserts the effect name at the right position
;; in the command string (after infile/outfile, before effect params).

(defvar cdp8-sox-param-schemas
  '(
    ;;; ── reverb ─────────────────────────────────────────────────────────────
    ("sox reverb"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"      :type wave-in)
      (:name "outfile"     :type wave-out)
      (:name "_eff"        :type literal :value "reverb")
      (:name "reverberance" :prompt "Reverberance % (0-100)"   :type integer :default 50)
      (:name "hf-damping"  :prompt "HF damping % (0-100)"      :type integer :default 50  :optional t)
      (:name "room-scale"  :prompt "Room scale % (0-100)"       :type integer :default 100 :optional t)
      (:name "stereo-depth" :prompt "Stereo depth % (0-100)"   :type integer :default 100 :optional t)
      (:name "pre-delay"   :prompt "Pre-delay (ms, 0-500)"      :type integer :default 0   :optional t)
      (:name "wet-gain"    :prompt "Wet gain dB (0 or negative)" :type number :default -3  :optional t)))

    ;;; ── gain / normalise ───────────────────────────────────────────────────
    ("sox gain"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"  :type wave-in)
      (:name "outfile" :type wave-out)
      (:name "_eff"    :type literal :value "gain")
      (:name "n"       :prompt "Normalise to 0 dBFS before applying gain"
             :type bool :flag "-n" :optional t)
      (:name "e"       :prompt "Equalise channels (balance)"
             :type bool :flag "-e" :optional t)
      (:name "dB"      :prompt "Gain in dB (positive=louder, negative=quieter)"
             :type number :default 0.0)))

    ("sox norm"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"  :type wave-in)
      (:name "outfile" :type wave-out)
      (:name "_eff"    :type literal :value "norm")
      (:name "level"   :prompt "Target peak level dBFS"
             :type number :default -0.1 :optional t)))

    ;;; ── dynamics ───────────────────────────────────────────────────────────
    ("sox compand"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"    :type wave-in)
      (:name "outfile"   :type wave-out)
      (:name "_eff"      :type literal :value "compand")
      (:name "attack-decay"
             :prompt "Attack,decay seconds (e.g. 0.02,0.2)"
             :type string :default "0.02,0.2")
      (:name "points"
             :prompt "Transfer curve: in-dB,out-dB pairs (e.g. -90,-90,-40,-30,0,-5)"
             :type string :default "-90,-90,-40,-30,0,-5")
      (:name "gain"
             :prompt "Overall output gain dB"
             :type number :default 0 :optional t)))

    ;;; ── cuts and silences ──────────────────────────────────────────────────
    ("sox trim"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"  :type wave-in)
      (:name "outfile" :type wave-out)
      (:name "_eff"    :type literal :value "trim")
      (:name "start"   :prompt "Start time (s, 0=beginning)" :type number :default 0.0)
      (:name "end"     :prompt "End time (s)"                :type number :default 5.0)))

    ("sox pad"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"  :type wave-in)
      (:name "outfile" :type wave-out)
      (:name "_eff"    :type literal :value "pad")
      (:name "before"  :prompt "Silence to add before (s)" :type number :default 0.0)
      (:name "after"   :prompt "Silence to add after (s)"  :type number :default 0.0)))

    ("sox fade"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"   :type wave-in)
      (:name "outfile"  :type wave-out)
      (:name "_eff"     :type literal :value "fade")
      (:name "type"
             :prompt "Curve type: t=linear h=half-sine q=quarter-sine l=log p=parabola"
             :type string :default "t" :optional t)
      (:name "fade-in"  :prompt "Fade-in duration (s, 0=none)"  :type number :default 0.0)
      (:name "stop"     :prompt "Stop position (s, 0=end)"       :type number :default 0.0 :optional t)
      (:name "fade-out" :prompt "Fade-out duration (s, 0=none)" :type number :default 0.0 :optional t)))

    ("sox silence"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"          :type wave-in)
      (:name "outfile"         :type wave-out)
      (:name "_eff"            :type literal :value "silence")
      (:name "above-periods"
             :prompt "Periods of silence to remove (1=leading, -1=trailing)"
             :type integer :default 1)
      (:name "duration"
             :prompt "Duration threshold for silence detection (s)"
             :type number :default 0.1)
      (:name "threshold"
             :prompt "Amplitude threshold (e.g. 1% or -50d)"
             :type string :default "1%")))

    ;;; ── format and rate ────────────────────────────────────────────────────
    ("sox rate"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"      :type wave-in)
      (:name "outfile"     :type wave-out)
      (:name "_eff"        :type literal :value "rate")
      (:name "v"           :prompt "Very high quality resampling"
             :type bool :flag "-v" :optional t)
      (:name "samplerate"  :prompt "Target sample rate (Hz)"
             :type integer :default 44100)))

    ("sox channels"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"  :type wave-in)
      (:name "outfile" :type wave-out)
      (:name "_eff"    :type literal :value "channels")
      (:name "count"   :prompt "Output channel count (1=mono, 2=stereo)"
             :type integer :default 2)))

    ;;; ── time and pitch ─────────────────────────────────────────────────────
    ("sox tempo"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"  :type wave-in)
      (:name "outfile" :type wave-out)
      (:name "_eff"    :type literal :value "tempo")
      (:name "s"       :prompt "Speech mode (optimised for voice)"
             :type bool :flag "-s" :optional t)
      (:name "m"       :prompt "Music mode (optimised for pitched material)"
             :type bool :flag "-m" :optional t)
      (:name "factor"  :prompt "Tempo factor (>1=faster, <1=slower, e.g. 1.2)"
             :type number :default 1.2)))

    ("sox pitch"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"  :type wave-in)
      (:name "outfile" :type wave-out)
      (:name "_eff"    :type literal :value "pitch")
      (:name "cents"   :prompt "Pitch shift in cents (100=1 semitone up, -200=2 down)"
             :type integer :default 0)))

    ;;; ── EQ and filters ─────────────────────────────────────────────────────
    ("sox highpass"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"  :type wave-in)
      (:name "outfile" :type wave-out)
      (:name "_eff"    :type literal :value "highpass")
      (:name "freq"    :prompt "Cutoff frequency (Hz)" :type number :default 80.0)
      (:name "width"   :prompt "Width (Q or Hz, e.g. 0.707q — omit for default)"
             :type string :default "0.707q" :optional t)))

    ("sox lowpass"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"  :type wave-in)
      (:name "outfile" :type wave-out)
      (:name "_eff"    :type literal :value "lowpass")
      (:name "freq"    :prompt "Cutoff frequency (Hz)" :type number :default 8000.0)
      (:name "width"   :prompt "Width (Q or Hz, e.g. 0.707q — omit for default)"
             :type string :default "0.707q" :optional t)))

    ("sox equalizer"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"  :type wave-in)
      (:name "outfile" :type wave-out)
      (:name "_eff"    :type literal :value "equalizer")
      (:name "freq"    :prompt "Centre frequency (Hz)" :type number :default 1000.0)
      (:name "width"   :prompt "Bandwidth (Q-value, e.g. 2.0q)" :type string :default "2.0q")
      (:name "gain"    :prompt "Gain dB (positive=boost, negative=cut)"
             :type number :default 0.0)))

    ;;; ── repetition and reversal ────────────────────────────────────────────
    ("sox repeat"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"  :type wave-in)
      (:name "outfile" :type wave-out)
      (:name "_eff"    :type literal :value "repeat")
      (:name "count"   :prompt "Repeat count (number of extra copies)"
             :type integer :default 3)))

    ("sox reverse"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"  :type wave-in)
      (:name "outfile" :type wave-out)
      (:name "_eff"    :type literal :value "reverse")))

    ;;; ── time-domain effects ────────────────────────────────────────────────
    ("sox echo"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"   :type wave-in)
      (:name "outfile"  :type wave-out)
      (:name "_eff"     :type literal :value "echo")
      (:name "gain-in"  :prompt "Input gain (0-1)"   :type number :default 0.8)
      (:name "gain-out" :prompt "Output gain (0-1)"  :type number :default 0.9)
      (:name "delay"    :prompt "Delay (ms)"         :type integer :default 500)
      (:name "decay"    :prompt "Decay factor (0-1)" :type number :default 0.5)))

    ("sox chorus"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"   :type wave-in)
      (:name "outfile"  :type wave-out)
      (:name "_eff"     :type literal :value "chorus")
      (:name "gain-in"  :prompt "Input gain (0-1)"                :type number :default 0.7)
      (:name "gain-out" :prompt "Output gain (0-1)"               :type number :default 0.9)
      (:name "delay"    :prompt "Delay (ms, typically 20-100)"    :type integer :default 55)
      (:name "decay"    :prompt "Decay (0-1)"                     :type number :default 0.4)
      (:name "speed"    :prompt "LFO speed (Hz)"                  :type number :default 0.25)
      (:name "depth"    :prompt "LFO depth (ms)"                  :type number :default 2.0)
      (:name "shape"    :prompt "LFO shape: -s sine, -t triangle" :type string :default "-s")))

    ("sox flanger"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"   :type wave-in)
      (:name "outfile"  :type wave-out)
      (:name "_eff"     :type literal :value "flanger")
      (:name "delay"    :prompt "Base delay (ms, 0-30)"    :type number :default 0.0  :optional t)
      (:name "depth"    :prompt "Modulation depth (ms)"    :type number :default 2.0  :optional t)
      (:name "regen"    :prompt "Feedback % (0-100)"       :type number :default 0.0  :optional t)
      (:name "width"    :prompt "Dry/wet balance (0-100)"  :type number :default 71.0 :optional t)
      (:name "speed"    :prompt "LFO speed (Hz)"           :type number :default 0.5  :optional t)
      (:name "shape"    :prompt "LFO shape: sine, triangle" :type string :default "sine" :optional t)
      (:name "phase"    :prompt "LFO phase shift (0-100)"  :type number :default 25.0 :optional t)
      (:name "interp"   :prompt "Interpolation: linear, quadratic" :type string :default "linear" :optional t)))

    ("sox overdrive"
     :command-prefix "sox"
     :output-type wave
     :params
     ((:name "infile"  :type wave-in)
      (:name "outfile" :type wave-out)
      (:name "_eff"    :type literal :value "overdrive")
      (:name "gain"    :prompt "Input gain above clip point (dB, >0)"
             :type number :default 20.0)
      (:name "colour"  :prompt "Harmonic colour (0=hard clip, 100=soft)"
             :type number :default 20.0 :optional t)))
    )
  "Parameter schemas for SoX effects in cdp8-mode.")

;;; Descriptions

(defvar cdp8-sox-command-descriptions
  '(("sox reverb"    . "Freeverb algorithmic reverb")
    ("sox gain"      . "Gain adjustment in dB, with optional normalise")
    ("sox norm"      . "Normalise peak level to 0 dBFS (or target level)")
    ("sox compand"   . "Compressor / expander for dynamics control")
    ("sox trim"      . "Keep audio between start and end time")
    ("sox pad"       . "Add silence before and/or after the audio")
    ("sox fade"      . "Fade in and/or out with selectable curve shape")
    ("sox silence"   . "Remove leading (or trailing) silence")
    ("sox rate"      . "Sample rate conversion")
    ("sox channels"  . "Change channel count (mono, stereo, etc.)")
    ("sox tempo"     . "Tempo change without pitch shift (WSOLA)")
    ("sox pitch"     . "Pitch shift in cents (100 cents = 1 semitone)")
    ("sox highpass"  . "High-pass filter")
    ("sox lowpass"   . "Low-pass filter")
    ("sox equalizer" . "Single parametric EQ band")
    ("sox repeat"    . "Repeat / loop the audio N extra times")
    ("sox reverse"   . "Time-reverse the audio")
    ("sox echo"      . "Simple single-tap delay / echo")
    ("sox chorus"    . "Chorus modulation (single voice)")
    ("sox flanger"   . "Flanger modulation")
    ("sox overdrive" . "Tube-style saturation / overdrive"))
  "Descriptions for SoX commands shown in completing-read.")

;;; Setup

;;;###autoload
(defun cdp8-sox-setup ()
  "Integrate SoX commands into cdp8-mode.
Call this after loading cdp8-mode, e.g.:
  (with-eval-after-load \\='cdp8-mode
    (require \\='cdp8-sox)
    (cdp8-sox-setup))"
  (interactive)
  (require 'cdp8-mode)
  ;; Merge commands into the extra-commands list (avoid duplicates).
  (dolist (spec cdp8-sox-commands)
    (unless (member spec cdp8-extra-commands)
      (setq cdp8-extra-commands (append cdp8-extra-commands (list spec)))))
  ;; Merge schemas (prepend so they shadow any accidental CDP8 clash).
  (dolist (entry cdp8-sox-param-schemas)
    (unless (assoc (car entry) cdp8-param-schemas)
      (setq cdp8-param-schemas (cons entry cdp8-param-schemas))))
  ;; Merge descriptions.
  (dolist (pair cdp8-sox-command-descriptions)
    (unless (assoc (car pair) cdp8-command-descriptions)
      (setq cdp8-command-descriptions
            (cons pair cdp8-command-descriptions))))
  (message "cdp8-sox: %d SoX commands registered"
           (length cdp8-sox-commands)))

(provide 'cdp8-sox)
;;; cdp8-sox.el ends here
