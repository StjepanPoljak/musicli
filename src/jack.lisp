(defpackage :jack
  (:use :cl :cffi)
  (:export :jack-client-open
	   :jack-options-t
	   :jack-status-t
	   :JackNoStartServer
	   :jack-client-close
	   :jack-port-register
	   :JackPortIsOutput))

(in-package :jack)

(define-foreign-library libjack
    (:darwin (:or "libjack"))
    (:unix (:or "libjack.so"))
    (t (:default "libjack")))
   
(use-foreign-library libjack)

(cffi:defcenum jack-options-t
    (JackNullOption 0)
    (JackNoStartServer 1)
    (JackUseExactName 2)
    (JackServerName 4)
    (JackLoadName 8)
    (JackLoadInit 16)
    (JackSessionID 32))

(cffi:defcenum jack-status-t
    (JackFailure 1)
    (JackInvalidOption 2)
    (JackNameNotUnique 4)
    (JackServerStarted 8)
    (JackServerFailed 16)
    (JackServerError 32)
    (JackNoSuchClient 64)
    (JackLoadFailure 128)
    (JackInitFailure 256)
    (JackShmFailure 512)
    (JackVersionError 1024)
    (JackBackendError 2048)
    (JackClientZombie 4096))

(cffi:defcfun ("jack_client_open" jack-client-open) :pointer
   (client-name :string)
   (options jack-options-t)
   (status :pointer))

(cffi:defcenum jack-port-flags-t
  (JackPortIsInput 1)
  (JackPortIsOutput 2)
  (JackPortIsPhysical 4)
  (JackPortCanMonitor 8)
  (JackPortIsTerminal 16))

(cffi:defcfun ("jack_port_register" jack-port-register) :pointer
  (client :pointer)
  (port-name :string)
  (port-type :string)
  (flags :unsigned-long)
  (buffer-size :unsigned-long))

(cffi:defcfun ("jack_client_close" jack-client-close) :int
   (client :pointer))
