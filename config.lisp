;; This file contains the configuration for the microgrid that's being
;; simulated.  This file is reloaded every time it is updated.
;; Changes can be made to the simulation by making changes to this
;; file and saving it, and the changes will take effect immediately.
;;
;; The simulator implementation doesn't have to be reloaded every time
;; we make a change to the simulation config.
(unless (boundp 'simulator-loaded)
  (setq simulator-loaded t)
  (load "sim/common.lisp")
  (load "sim/components.lisp"))

;; But the state of the simulator needs to be reset every time this
;; file is being reloaded.
(reset-state)


;; Simulator configuration
(setq state-update-interval-ms 200)


;; API service config
(setq socket-addr "[::1]:8800")  ;; Needs restart to take effect.
(setq retain-requests-duration-ms 60000)
(setq battery-interval 1000)
(setq inverter-interval 1000)
(setq meter-interval 200)
(setq ev-charger-interval 1000)


;; Microgrid config
(setq metadata '((microgrid-id . 2200)
                 (location . (52.52 13.405))))  ;; Berlin


;; Simulation config

;; This simulates the external factors that affect the microgrid,
;; including the consumer power and the state of the grid.
(every
 :milliseconds 200
 :call (lambda ()
         (setq consumer-power
               (+ 48000 (random 100)))

         (setq voltage-per-phase
               (list (+ 229.0 (/ (random 200) 100.0))
                     (+ 229.0 (/ (random 200) 100.0))
                     (+ 229.0 (/ (random 200) 100.0))))

         (setq power-factor-per-phase
               (list (+ 0.88 (/ (random 5) 100.0))
                     (+ 0.88 (/ (random 5) 100.0))
                     (+ 0.88 (/ (random 5) 100.0))))

         (setq ac-frequency
               (+ 49.99 (/ (random 4) 100.0)))))

;; Component defaults.  All these defaults can be overridden
;; separately for individual components, if necessary.
(setq battery-defaults '((initial-soc      . 90.0)
                         (soc-lower        . 10.0)
                         (soc-upper        . 90.0)
                         (capacity         . 92000.0)
                         (voltage          . 800.0)
                         (rated-bounds     . (-30000.0 30000.0))
                         (exclusion-bounds . (0.0 0.0))
                         (component-state  . idle)
                         (relay-state      . closed)))

(setq meter-defaults '((component-state . ok)))

(setq battery-inverter-defaults `((component-state . idle)
                                  (rated-bounds    . (-30000.0 30000.0))))

(setq solar-inverter-defaults `((component-state . idle)
                                (rated-bounds    . (-30000.0 0.0))))

(setq ev-charger-defaults
      (let* ((max-current-per-phase 16.0)
             (max-power-per-phase (seq-map
                                   (lambda (x) (* max-current-per-phase x))
                                   voltage-per-phase))
             (max-power (seq-reduce #'+ max-power-per-phase 0.0)))
        `((initial-soc     . 50.0)
          (soc-lower       . 0.0)
          (soc-upper       . 100.0)
          (component-state . ready)
          (cable-state     . ev-locked)
          (rated-bounds    . (0.0 ,max-power))
          (capacity        . 30000.0)
          (inclusion-lower . 0.0)
          (inclusion-upper . ,max-power))))


;; And finally, this builds the component graph/config of the
;; microgrid that's being simulated.
(make-grid
 :id 1
 :rated-fuse-current 100
 :successors (list
              ;; main-meter
              (make-meter
               :id 2
               :successors (list
                            ;; battery 1
                            (make-meter
                             :successors (list
                                          (make-battery-inverter
                                           :successors (list
                                                        (make-battery)))))

                            ;; battery 2
                            (make-meter
                             :successors (list
                                          (make-battery-inverter
                                           :successors (list
                                                        (make-battery)))))

                            ;; ev chargers
                            (make-meter
                             :successors (list
                                          (make-ev-charger)
                                          (make-ev-charger
                                           :config '((initial-soc . 10.0)
                                                     (cable-state . ev-locked)))))

                            ;; solar inverters
                            (make-meter
                             :successors (list
                                          (make-solar-inverter
                                           :sunlight% 100.0
                                           :config '((component-state . idle)
                                                     (rated-bounds . (-8000.0 0.0))))
                                          (make-solar-inverter :sunlight% 60.0)))

                            ;; consumer
                            (make-meter
                             :hidden t
                             :power 'consumer-power)))))

