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
         (battery (make-battery bat-id bat-power-expr))
         (inverter (make-battery-inverter inv-id inv-power-expr)))

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
             (meter (make-meter meter-id meter-power-expr)))
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

(defun make-battery (id power-expr)
  (let ((battery
         `((category . battery)
           (id       . ,id)
           (name     . ,(format "bat-%s" id))
           (stream   . ,(list
                         `(interval . ,battery-interval)
                         `(data     . ,(battery-data-maker
                                        `((id    . ,id)
                                          (power . ,power-expr))
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

(defun make-battery-inverter (id power-expr)
  (let ((inverter
         `((category . inverter)
           (type     . battery)
           (id       . ,id)
           (name     . ,(format "inv-bat-%s" id))
           (stream   . ,(list
                         `(interval . ,inverter-interval)
                         `(data     . ,(inverter-data-maker
                                        `((id . ,id)
                                          (power . ,power-expr))
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



(defun make-meter (id power-expr)
  (let ((meter
         `((category . meter)
           (id       . ,id)
           (name     . ,(format "meter-%s" id))
           (stream   . ,(list
                         `(interval . ,meter-interval)
                         `(data     . ,(meter-data-maker
                                        `((id . ,id)
                                          (power . ,power-expr))
                                        meter-defaults)))))))
    (add-to-components-alist meter)
    meter))
