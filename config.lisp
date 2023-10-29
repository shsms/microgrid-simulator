;; Need to restart the server for this to take effect.
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
                          ))

(setq components-alist
      `(((id . 2)
         (name . "inv_0")
         (category . inverter)
         (type . battery)
         (stream . ((interval . ,inverter-interval)
                    (data . (lambda (id)
                              (inverter-data `((id . ,id)
                                               (power . -23444.0)
                                               (current . (-2.1 -3.0 -4.5))
                                               (component-state . discharging)
                                               (exclusion-lower . -300.0)
                                               (exclusion-upper . 300.0)
                                               ,@inverter-defaults))))))
         )

        ((id . 3)
         (name . "bat_0")
         (category . battery)
         (type . li_ion)
         (stream . ((interval . ,battery-interval)
                    (data . (lambda (id)
                              (battery-data `((id . ,id)
                                              (soc . 92.0)
                                              (power . -23444.0)
                                              (component-state . discharging)
                                              (exclusion-lower . -300.0)
                                              (exclusion-upper . 300.0)
                                              ,@battery-defaults)))))))

        ;; ((id . 5)
        ;;  (name . "bat_1")
        ;;  (category . battery)
        ;;  (stream . ((interval . battery-interval)
        ;;             (data . (lambda (id)
        ;;                       (battery-data `((id . ,id)
        ;;                                       (soc . 91.0)
        ;;                                       (power . -20000.0)
        ;;                                       (component-state . discharging)
        ;;                                       ,@battery-defaults)))))))
        ))



(setq connections-alist
      '((1 . 2)
        (2 . 3)))

