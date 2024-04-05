(unless (boundp 'mock-config-loaded)
  (setq mock-config-loaded t)
  (load "config/common.lisp")
  (load "config/components.lisp"))

(reset-state)

(setq socket-addr "[::1]:8800")
(setq retain-requests-duration-ms 60000)
(setq state-update-interval-ms 200)


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


(setq battery-interval 200)
(setq inverter-interval 200)
(setq meter-interval 200)
(setq ev-charger-interval 1500)


(setq battery-defaults '((initial-soc      . 90.0)
                         (soc-lower        . 10.0)
                         (soc-upper        . 90.0)
                         (capacity         . 92000.0)
                         (voltage          . 800.0)
                         (rated-bounds     . (-30000.0 30000.0))
                         (exclusion-bounds . (0.0 0.0))
                         (component-state  . idle)
                         (relay-state      . closed)))


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

