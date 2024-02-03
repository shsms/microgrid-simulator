(unless (boundp 'mock-config-loaded)
  (setq mock-config-loaded t)
  (load "config/common.lisp")
  (load "config/components.lisp"))

(reset-state)

(setq socket-addr "[::1]:8800")
(setq retain-requests-duration-ms 60000)
(setq state-update-interval-ms 3000)


(setq ac-frequency 50.0)
(setq ac-voltage '(230.0 230.0 230.0))


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


(setq inverter-defaults `((component-state . idle)
                          (rated-bounds    . (-30000.0 30000.0))
                          (voltage         . ,ac-voltage)))


(setq meter-defaults `((voltage . ,ac-voltage)))


(setq ev-charger-defaults `((component-state . ready)
                            (cable-state     . unplugged)
                            (inclusion-lower . 0.0)
                            (inclusion-upper . ,(* 16.0 230.0)) ; 16A
                            (voltage         . ,ac-voltage)))


(make-grid
 :id 1
 :rated-fuse-current 100
 :successors (list
              (make-inv-bat-chain :bat-config '((initial-soc . 70)))
              (make-inv-bat-chain :bat-config '((relay-state . closed)))
              ;; consumer
              (make-meter :power 50000.0)))
