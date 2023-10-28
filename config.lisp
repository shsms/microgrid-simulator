;; Need to restart the server for this to take effect.
(setq socket-addr "[::1]:8800")

(setq battery-defaults '((soc-lower . 10.0)
                         (soc-upper . 90.0)
                         (capacity . 92000.0)
                         (voltage . 800.0)
                         (inclusion-lower . -30000.0)
                         (inclusion-upper . 30000.0)))

(setq components-alist
        `(((id . 1)
           (name . "bat_0")
           (category . "COMPONENT_CATEGORY_BATTERY")
           (stream . ((interval . 200)
                      (data . (lambda ()
                                (battery-data `((soc . 92.0)
                                                (power . -23444.0)
                                                (exclusion-lower . -300.0)
                                                (exclusion-upper . 300.0)
                                                ,@battery-defaults)))))))

          ((id . 2)
           (name . "bat_1")
           (category . "COMPONENT_CATEGORY_BATTERY")
           (stream . ((interval . 200)
                      (data . (lambda ()
                                (battery-data `((soc . 91.0)
                                                (power . -20000.0)
                                                ,@battery-defaults)))))))))



(setq connections-alist
      '((1 . 2)
        (2 . 3)))

