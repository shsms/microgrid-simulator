;; Need to restart the server for this to take effect.
(setq socket-addr "[::1]:8800")

(setq battery-defaults '((soc-lower . 10.0)
                         (soc-upper . 90.0)
                         (capacity . 92000.0)
                         (voltage . 800.0)
                         (inclusion-lower . -30000.0)
                         (inclusion-upper . 30000.0)
                         (component-state . idle)
                         (relay-state . closed)))

(setq components-alist
      `(((id . 1)
         (name . "bat_0")
         (category . battery)
         (type . li_ion)
         (stream . ((interval . 200)
                    (data . (lambda (id)
                              (battery-data `((id . ,id)
                                              (soc . 92.0)
                                              (power . -23444.0)
                                              (component-state . discharging)
                                              (exclusion-lower . -300.0)
                                              (exclusion-upper . 300.0)
                                              ,@battery-defaults)))))))

        ((id . 2)
         (name . "bat_1")
         (category . battery)
         (stream . ((interval . 200)
                    (data . (lambda (id)
                              (battery-data `((id . ,id)
                                              (soc . 91.0)
                                              (power . -20000.0)
                                              (component-state . discharging)
                                              ,@battery-defaults)))))))))



(setq connections-alist
      '((1 . 2)
        (2 . 3)))

