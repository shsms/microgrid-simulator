;; Need to restart the server for this to take effect.
(load "./graph-build-utils.lisp")

(setq socket-addr "[::1]:8800")

(setq ac-frequency 50.0)

(setq battery-interval 500)
(setq inverter-interval 500)
(setq meter-interval 200)
(setq ev-charger-interval 1500)


(setq battery-defaults '((soc-lower . 10.0)
                         (soc-upper . 90.0)
                         (capacity . 92000.0)
                         (voltage . 800.0)
                         (inclusion-lower . -30000.0)
                         (inclusion-upper . 30000.0)
                         (component-state . idle)
                         (relay-state . closed)))

(setq inverter-defaults '((component-state . idle)
                          (voltage . (230.0 230.0 230.0))
                          (inclusion-lower . -30000.0)
                          (inclusion-upper . 30000.0)
                          ))

(setq meter-defaults '((voltage . (230.0 230.0 230.0))))

(setq ev-charger-defaults `((component-state . ready)
                            (cable-state . unplugged)
                            (voltage . (230.0 230.0 230.0))
                            (inclusion-lower . 0.0)
                            (inclusion-upper . ,(* 16.0 230.0))
                            ))

(make-grid
 (list
  (cons 'successors
        `(,(make-inv-bat '((name . "inv_bat_0")
                           (no-meter . t)
                           (power . -40000.0)
                           (current . (10.0 21.2 10.5))
                           (soc . 90.0)))
          ,(make-inv-bat '((name . "inv_bat_1")
                           (no-meter . nil)
                           (power . -10000.0)
                           (current . (2.0 10.2 2.5))
                           (soc . 90.0)))
          ,(make-meter '((name . "consumer")
                         (power . 45000.0)
                         (current . (10.0 11.2 10.5))))
           ,(make-meter
             (list '(name . "ev-meter")
                   `(successors ,(make-ev-charger '((name . "ev-charger-0")
                                                    (id . 25)
                                                    (power . 2000.0)
                                                    (component-state . charging)
                                                    (cable-state . ev_locked)
                                                    (current . (2.0 1.2 2.5))))
                                ,(make-ev-charger '((name . "ev-charger-1")
                                                     (power . 10000.0)
                                                     (current . (2.0 1.2 2.5)))))))))))

;; (make-grid
;;  (list
;;   (cons 'successors
;;         (list (make-meter
;;                `((name . "grid-meter")
;;                  (power . 1000.0)
;;                  (dont-calculate . t)
;;                  (successors . (,(make-inv-bat '((name . "inv_bat_0")
;;                                                  (no-meter . t)
;;                                                  (power . -40000.0)
;;                                                  (current . (10.0 11.2 10.5))
;;                                                  (soc . 80.0)))

;;                                  ,(make-inv-bat '((name . "inv_bat_1")
;;                                                   (no-meter . nil)
;;                                                   (power . 10000.0)
;;                                                   (current . (2.0 1.2 2.5))
;;                                                   (soc . 80.0)))))))))))






