(defun make-inv-bat (&rest plist)
  (let* ((no-meter (plist-get plist :no-meter))
         (inv-id (get-comp-id))
         (bat-id (get-comp-id))
         (inv-power-symbol (power-symbol-from-id inv-id))
         (bat-power-symbol (power-symbol-from-id bat-id))
         (inv-power-expr inv-power-symbol)
         (bat-power-expr `(setq
                           ,bat-power-symbol
                           ,inv-power-expr))
         (battery (make-battery :id bat-id
                                :power bat-power-expr))
         (inverter (make-battery-inverter :id inv-id
                                          :power inv-power-expr)))

    (when (not (boundp inv-power-symbol))
      (eval `(setq ,inv-power-symbol 0.0))
      (eval `(setq ,bat-power-symbol 0.0)))

    (add-to-connections-alist inv-id bat-id)

    (if no-meter
        inverter
      (let* ((meter-id (get-comp-id))
             (meter-power-symbol (power-symbol-from-id meter-id))
             (meter-power-expr `(setq
                                 ,meter-power-symbol
                                 ,inv-power-expr))
             (meter (make-meter :id meter-id
                                :power meter-power-expr)))
        (add-to-connections-alist meter-id inv-id)
        meter))))


;;;;;;;;;;;;;;;
;; Batteries ;;
;;;;;;;;;;;;;;;

(defmacro battery-data-maker (data-alist defaults-alist)
  (component-data-maker 'battery-data
                        data-alist
                        defaults-alist
                        '(id soc soc-upper soc-lower
                          capacity power voltage type
                          component-state relay-state
                          inclusion-lower inclusion-upper
                          exclusion-lower exclusion-upper)))

(defun make-battery (&rest plist)
  (let ((id (or (plist-get plist :id) (get-comp-id)))
        (interval (or (plist-get plist :interval) battery-interval))
        (power (plist-get plist :power))
        (soc (plist-get plist :soc))
        (config (plist-get plist :config))
        (battery
         `((category . battery)
           (id       . ,id)
           (name     . ,(format "bat-%s" id))
           (stream   . ,(list
                         `(interval . ,interval)
                         `(data     . ,(battery-data-maker
                                        `((id    . ,id)
                                          ,@(when soc
                                              `((soc . ,soc)))
                                          (power . ,power)
                                          ,@config)
                                        battery-defaults)))))))
    (add-to-components-alist battery)
    battery))


;;;;;;;;;;;;;;;
;; Inverters ;;
;;;;;;;;;;;;;;;

(defmacro inverter-data-maker (data-alist defaults-alist)
  (component-data-maker 'inverter-data
                        data-alist
                        defaults-alist
                        '(id power current voltage component-state
                          inclusion-lower inclusion-upper)))

(defun make-battery-inverter (&rest plist)
  (let ((id (or (plist-get plist :id) (get-comp-id)))
        (interval (or (plist-get plist :interval) inverter-interval))
        (power (plist-get plist :power))
        (config (plist-get plist :config))
        (inverter
         `((category . inverter)
           (type     . battery)
           (id       . ,id)
           (name     . ,(format "inv-bat-%s" id))
           (stream   . ,(list
                         `(interval . ,interval)
                         `(data     . ,(inverter-data-maker
                                        `((id . ,id)
                                          (power . ,power)
                                          ,@config)
                                        inverter-defaults)))))))
    (add-to-components-alist inverter)
    inverter))


;;;;;;;;;;;;
;; Meters ;;
;;;;;;;;;;;;

(defmacro meter-data-maker (data-alist defaults-alist)
  (component-data-maker 'meter-data
                        data-alist
                        defaults-alist
                        '(id power current voltage)))



(defun make-meter (&rest plist)
  (let* ((id (or (plist-get plist :id) (get-comp-id)))
         (interval (or (plist-get plist :interval) meter-interval))
         (power (plist-get plist :power))
         (config (plist-get plist :config))
         (meter
          `((category . meter)
            (id       . ,id)
            (name     . ,(format "meter-%s" id))
            (stream   . ,(list
                          `(interval . ,interval)
                          `(data     . ,(meter-data-maker
                                         `((id    . ,id)
                                           (power . ,power)
                                           ,@config)
                                         meter-defaults)))))))
    (add-to-components-alist meter)
    meter))


;;;;;;;;;;
;; Grid ;;
;;;;;;;;;;

(defun make-grid (&rest plist)
  (let ((grid
         `((category . grid)
           (id       . ,(or (plist-get plist :id) (get-comp-id)))
           (name     . "grid"))))
    (add-to-components-alist grid)
    grid))
