;;; This is the start of a mode to use csound live
;;; It's a work in progress!

;; first we start a csound deamon listening on port UDP 7777
(async-shell-command
 "csound --daemon --port=7777 --udp-echo --output=dac"
 "*CSOUND-SERVER*")

;; then we create a "process", but it's really an UDP socket to 7777
(setq csound (make-network-process
	      :name "Csound UDP Server"
	      :host "localhost"
	      :service 7777
	      :type 'datagram
	      :family 'ipv4))

;; so we send live commands to csound through the "process"
(process-send-string csound "0dbfs = 1")

;; multiline commands are sent within braces
(process-send-string csound "{
instr 1
    out oscili(0dbfs*0.5, p4)
endin
}")

;; we can also schedule instruments/notes
(process-send-string csound "schedule 1,0,2,660")
