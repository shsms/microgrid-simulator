(defun make-inv-bat-chain (&rest plist)
  (let* ((no-meter (plist-get plist :no-meter))
         (bat-config (plist-get plist :bat-config))
         (inv-config (plist-get plist :inv-config))
         (starting-power (or (plist-get plist :starting-power) 0.0))

         (inv-id (get-comp-id))
         (bat-id (get-comp-id))
         (inv-power-symbol (power-symbol-from-id inv-id))
         (bat-power-symbol (power-symbol-from-id bat-id))
         (inv-power-expr inv-power-symbol)
         (bat-power-expr `(setq
                           ,bat-power-symbol
                           ,inv-power-expr))
         (battery (make-battery :id bat-id
                                :power bat-power-expr
                                :config bat-config))
         (inverter (make-battery-inverter :id inv-id
                                          :power inv-power-expr
                                          :config inv-config)))

    (when (not (boundp inv-power-symbol))
      (eval `(setq ,inv-power-symbol starting-power)))

    (add-to-connections-alist inv-id bat-id)

    (if no-meter
        inverter
      (let* ((meter-id (get-comp-id))
             (meter (make-meter :id meter-id
                                :power inv-power-expr)))
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
           (name     . ,(format "bat-%s" id))
           (id       . ,id)
           ,@(when power
               `((power . ,power)))
           (stream   . ,(list
                         `(interval . ,interval)
                         `(data     . ,(battery-data-maker
                                        `((id    . ,id)
                                          ,@(when soc
                                              `((soc . ,soc)))
                                          ,@(when power
                                              `((power . ,power)))
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
        (successors (plist-get plist :successors))
        (inverter
         `((category . inverter)
           (type     . battery)
           (name     . ,(format "inv-bat-%s" id))
           (id       . ,id)
           ,@(when power
               `((power . ,power)))
           (stream   . ,(list
                         `(interval . ,interval)
                         `(data     . ,(inverter-data-maker
                                        `((id . ,id)
                                          ,@(when power
                                              `((power . ,power)))
                                          ,@config)
                                        inverter-defaults)))))))
    (add-to-components-alist inverter)
    (connect-successors id successors)
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
         (successors (plist-get plist :successors))
         (power-expr (if power
                         `((power . ,power))
                       (power-expr-from-successors successors)))
         (meter
          `((category . meter)
            (name     . ,(format "meter-%s" id))
            (id       . ,id)
            ,@power-expr
            (stream   . ,(list
                          `(interval . ,interval)
                          `(data     . ,(meter-data-maker
                                         `((id    . ,id)
                                           ,@power-expr
                                           ,@config)
                                         meter-defaults)))))))
    (add-to-components-alist meter)
    (connect-successors id successors)
    meter))


;;;;;;;;;;
;; Grid ;;
;;;;;;;;;;

(defun make-grid (&rest plist)
  (let ((id (or (plist-get plist :id) (get-comp-id)))
        (successors (plist-get plist :successors))
        (grid
         `((category . grid)
           (id       . ,id)
           (name     . "grid"))))
    (add-to-components-alist grid)
    (connect-successors id successors)
    grid))
