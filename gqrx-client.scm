(module gqrx-client (get-frequency
                      set-frequency
                      get-mode-and-passband
                      set-mode-and-passband
                      get-signal-strength
                      get-squelch-threshold
                      set-squelch-threshold
                      get-recorder-status
                      set-recorder-status
                      open-connection
                      close-connection
                      send-aos
                      send-los
                      get-lnb-lo
                      set-lnb-lo
                      get-version)

  (import (chicken base)
          (chicken format)
          (chicken io)
          (chicken port)
          (chicken tcp)
          scheme)

  ; --------------------------------------------------------------------------

  (define MODES '((raw . "RAW")
                  (am . "AM")
                  (fm . "FM")
                  (wfm . "WFM")
                  (wfm-st . "WFM_ST")
                  (wfm-st-oirt . "WFM_ST_OIRT")
                  (lsb . "LSB")
                  (usb . "USB")
                  (cw . "CW")
                  (cwu . "CWU")
                  (cwr . "CWR")
                  (cwl . "CWL")))

  (define (parse-demod-mode m)
    (let ((mode (rassoc m MODES)))
      (or mode (error "Unknown mode" m))))

  (define (unparse-demod-mode m)
    (let ((mode (assoc m MODES)))
      (or mode (error "Unknown mode" m))))

  ; --------------------------------------------------------------------------

  (define RECORDER-STATUS '((on . "1") (off . "0")))

  (define (parse-recorder-status s)
    (let ((status (rassoc s RECORDER-STATUS)))
      (or status (error "Unknown status" s))))

  (define (unparse-recorder-status s)
    (let ((status (assoc s RECORDER-STATUS)))
      (or status (error "Unknown status" s))))

  ; --------------------------------------------------------------------------

  ; void -> handle
  (define (open-connection #!optional (host "127.0.0.1") (port 7356))
    (let-values (((in out) (tcp-connect host port)))
      (make-bidirectional-port in out)))

  (define (close-connection handle)
    (begin
      (write-line "q" handle)
      (close-input-port handle)
      (close-output-port handle)))

  ; --------------------------------------------------------------------------

  (define (check-reply handle)
    (let ((reply (read-line handle)))
      (unless (string=? reply "RPRT 0")
        (error "Command failed" reply))))

  (define (get-frequency handle)
    (begin
      (write-line "f" handle)
      (string->number (read-line handle))))

  (define (set-frequency handle freq)
    (begin
      (write-line (sprintf "F ~A" freq) handle)
      (check-reply handle)))

  (define (get-mode-and-passband handle)
    (begin
      (write-line "m" handle)
      (let ((mode (read-line handle))
            (passband (read-line handle)))
        (values (parse-demod-mode mode) passband))))

  (define (set-mode-and-passband handle mode #!optional (passband #f))
    (begin
      (write-line (sprintf "M ~A ~A" (unparse-demod-mode mode) (or passband "")))
      (check-reply handle)))

  (define (get-signal-strength handle)
    (begin
      (write-line "l STRENGTH" handle)
      (string->number (read-line handle))))

  (define (get-squelch-threshold handle)
    (begin
      (write-line "l SQL")
      (string->number (read-line handle))))

  (define (set-squelch-threshold handle threshold)
    (begin
      (write-line (sprintf "L SQL ~A" threshold) handle)
      (check-reply handle)))

  (define (get-recorder-status handle)
    (begin
      (write-line "u RECORD" handle)
      (parse-recorder-status (read-line handle))))

  (define (set-recorder-status handle status)
    (begin
      (write-line (sprintf "U RECORD ~A" (unparse-recorder-status status)) handle)
      (check-reply handle)))

  (define (send-aos handle)
    (begin
      (write-line "AOS" handle)
      (check-reply handle)))

  (define (send-los handle)
    (begin
      (write-line "LOS" handle)
      (check-reply handle)))

  (define (get-lnb-lo handle)
    (begin
      (write-line "LNB_LO" handle)
      (string->number (read-line handle))))

  (define (set-lnb-lo handle freq)
    (begin
      (write-line (sprintf "LNB_LO ~A" (number->string freq)) handle)
      (check-reply handle)))

  (define (get-version handle)
    (begin
      (write-line "_" handle)
      (read-line handle))))
