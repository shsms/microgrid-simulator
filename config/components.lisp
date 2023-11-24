(defun make-inv-bat-chain (&rest plist)
  (let* ((no-meter (plist-get plist :no-meter))
         (bat-config (plist-get plist :bat-config))
         (inv-config (plist-get plist :inv-config))
         (starting-power (or (plist-get plist :starting-power) 0.0))

         (inv-id (get-comp-id))
         (bat-id (get-comp-id))
         (inv-power-symbol (power-symbol-from-id inv-id))
         (inv-energy-symbol (energy-symbol-from-id inv-id))

         (capacity (alist-get 'capacity `(,@bat-config ,@battery-defaults)))
         (initial-soc (alist-get 'initial-soc `(,@bat-config ,@battery-defaults)))

         ;; using the inv id to make the bat soc symbol works in this
         ;; case because this function makes exactly 1 unique battery
         ;; per inverter.
         ;;
         ;; The soc calculation is done in `set-power-active', where
         ;; only the inverter id is available, and so the generated
         ;; expression needs be put in a dynamic variable
         ;; `bat-soc-expr-symbol', that can be accessed by the
         ;; inverter id.
         (bat-soc-symbol (soc-symbol-from-id inv-id))
         (bat-soc-expr `(setq ,bat-soc-symbol
                              (+ ,initial-soc
                                 (* 100.0 (/ ,inv-energy-symbol ,capacity)))))
         (bat-soc-expr-symbol (soc-expr-symbol-from-id inv-id))

         (battery (make-battery :id bat-id
                                :power inv-power-symbol
                                :soc bat-soc-symbol
                                :config bat-config))
         (inverter (make-battery-inverter :id inv-id
                                          :power inv-power-symbol
                                          :config inv-config)))

    (when (not (boundp inv-power-symbol))
      (eval `(setq ,inv-power-symbol starting-power))
      (eval `(setq ,inv-energy-symbol 0.0))
      (eval `(setq ,bat-soc-symbol ,initial-soc)))

    (eval `(setq ,bat-soc-expr-symbol bat-soc-expr))

    (add-to-connections-alist inv-id bat-id)

    (if no-meter
        inverter
      (let* ((meter-id (get-comp-id))
             (meter (make-meter :id meter-id
                                :power inv-power-symbol)))
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
  (let* ((id (or (plist-get plist :id) (get-comp-id)))
        (interval (or (plist-get plist :interval) battery-interval))
        (power (plist-get plist :power))
        (config (plist-get plist :config))
        (power-expr (when power
                      `((power . ,power)
                        (component-state . (power->component-state ,power)))))
        (soc-expr `((soc . ,(plist-get plist :soc))))
        (battery
         `((category . battery)
           (name     . ,(format "bat-%s" id))
           (id       . ,id)
           ,@power-expr
           (stream   . ,(list
                         `(interval . ,interval)
                         `(data     . ,(battery-data-maker
                                        `((id    . ,id)
                                          ,@soc-expr
                                          ,@power-expr
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
  (let* ((id (or (plist-get plist :id) (get-comp-id)))
        (interval (or (plist-get plist :interval) inverter-interval))
        (power (plist-get plist :power))
        (config (plist-get plist :config))
        (successors (plist-get plist :successors))
        (power-expr (when power
                      `((power . ,power)
                        (current . (ac-current-from-power ,power))
                        (component-state . (power->component-state ,power)))))
        (inverter
         `((category . inverter)
           (type     . battery)
           (name     . ,(format "inv-bat-%s" id))
           (id       . ,id)
           ,@power-expr
           (stream   . ,(list
                         `(interval . ,interval)
                         `(data     . ,(inverter-data-maker
                                        `((id . ,id)
                                          ,@power-expr
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
         (current-expr (if-let ((current (if power
                                             `(ac-current-from-power ,power)
                                             (make-current-expr successors)
                                             )))
                           `((current . ,current))))
         (power-expr (if-let ((power (or power
                                         (make-power-expr successors))))
                         `((power . ,power))))
         (meter
          `((category . meter)
            (name     . ,(format "meter-%s" id))
            (id       . ,id)
            ,@current-expr
            ,@power-expr
            (stream   . ,(list
                          `(interval . ,interval)
                          `(data     . ,(meter-data-maker
                                         `((id    . ,id)
                                           ,@current-expr
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
