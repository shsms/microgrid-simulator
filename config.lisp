(load "config/common.lisp")
(load "config/components.lisp")


(setq socket-addr "[::1]:8800")
(setq retain-requests-duration-ms 5000)


(setq ac-frequency 50.0)
(setq ac-voltage '(230.0 230.0 230.0))


(setq battery-interval 500)
(setq inverter-interval 500)
(setq meter-interval 200)
(setq ev-charger-interval 1500)


(setq battery-defaults '((soc             . 88.0)
                         (soc-lower       . 10.0)
                         (soc-upper       . 90.0)
                         (capacity        . 92000.0)
                         (voltage         . 800.0)
                         (inclusion-lower . -30000.0)
                         (inclusion-upper . 30000.0)
                         (component-state . idle)
                         (relay-state     . closed)))


(setq inverter-defaults `((component-state . idle)
                          (inclusion-lower . -30000.0)
                          (inclusion-upper . 30000.0)
                          (voltage         . ,ac-voltage)))


(setq meter-defaults `((voltage . ,ac-voltage)))


(setq ev-charger-defaults `((component-state . ready)
                            (cable-state     . unplugged)
                            (inclusion-lower . 0.0)
                            (inclusion-upper . ,(* 16.0 230.0)) ; 16A
                            (voltage         . ,ac-voltage)))


(make-grid
 :id 1
 :successors (list
              (make-meter
               :id 2
               :successors (list
                            (make-inv-bat-chain
                             :bat-config '((soc . 10)))
                            (make-inv-bat-chain
                             :no-meter t)))))
