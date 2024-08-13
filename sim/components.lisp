;;;;;;;;;;;;;;;
;; Batteries ;;
;;;;;;;;;;;;;;;

(defmacro battery-data-maker (data-alist defaults-alist)
  (component-data-maker data-alist
                        defaults-alist
                        '(id soc soc-upper soc-lower
                          capacity power voltage type
                          component-state relay-state
                          inclusion-lower inclusion-upper
                          exclusion-lower exclusion-upper)))

(defun make-battery (&rest plist)
  (let* ((id (or (plist-get plist :id) (get-comp-id)))

         (interval (or (plist-get plist :interval) battery-interval))

         (config  (plist-get plist :config))
         (config-alist `(,@config ,@battery-defaults))

         (power-symbol  (power-symbol-from-id  id))
         (energy-symbol (energy-symbol-from-id id))

         (capacity    (alist-get 'capacity    config-alist))
         (initial-soc (alist-get 'initial-soc config-alist))

         (soc-symbol (soc-symbol-from-id id))
         (soc-expr `(setq ,soc-symbol
                              (+ ,initial-soc
                                 ;; limit to 1 decimal place
                                 (/ (fround
                                     (* 1000.0 (/ ,energy-symbol ,capacity)))
                                    10.0))))

         (rated-bounds (or (alist-get 'rated-bounds config-alist) '(0.0 0.0)))
         (excl-bounds (or (alist-get 'exclusion-bounds config-alist) '(0.0 0.0)))

         (rated-lower (car rated-bounds))
         (rated-upper (cadr rated-bounds))

         (excl-lower (car excl-bounds))
         (excl-upper (cadr excl-bounds))

         (incl-lower-symbol (inclusion-lower-symbol-from-id id))
         (incl-upper-symbol (inclusion-upper-symbol-from-id id))

         (soc-lower (alist-get 'soc-lower config-alist))
         (soc-upper (alist-get 'soc-upper config-alist))

         (incl-lower-expr `(setq ,incl-lower-symbol
                                 (if (< (- ,soc-symbol ,soc-lower) 10.0)
                                     (* ,rated-lower
                                        (bounded-exp-decay ,(+ soc-lower 10.0)
                                                           ,soc-lower
                                                           ,soc-symbol
                                                           1.2
                                                           0.3))
                                     ,rated-lower)))
         (incl-upper-expr `(setq ,incl-upper-symbol
                                 (if (< (- ,soc-upper ,soc-symbol) 10.0)
                                     (* ,rated-upper
                                        (bounded-exp-decay ,(- soc-upper 10.0)
                                                           ,soc-upper
                                                           ,soc-symbol
                                                           1.2
                                                           0.3))
                                     ,rated-upper)))

         (is-healthy (is-healthy-battery config-alist))

         (power-expr (when is-healthy
                       `((power . ,power-symbol)
                         (`component-state . (power->component-state ,power-symbol)))))

         (soc-bounds-expr `((soc . ,soc-symbol)
                            (inclusion-lower . ,incl-lower-symbol)
                            (inclusion-upper . ,incl-upper-symbol)
                            (exclusion-lower . ,excl-lower)
                            (exclusion-upper . ,excl-upper)))
         (battery
          `((category . battery)
            (name     . ,(format "bat-%s" id))
            (id       . ,id)
            ,@power-expr
            ,@soc-bounds-expr
            (is-healthy . ,is-healthy)
            (stream   . ,(list
                          `(interval . ,interval)
                          (cons 'data
                                (macroexpand '(battery-data-maker
                                        `((id    . ,id)
                                          ,@soc-bounds-expr
                                          ,@power-expr)
                                        config-alist))))))))

    (log.trace (format "Adding battery %s. Healthy: %s" id is-healthy))

    (when (not (boundp power-symbol))
      (set power-symbol 0.0)
      (set energy-symbol 0.0)
      (set soc-symbol (eval initial-soc)))

    (setq state-update-functions
          (cons (eval (list 'lambda '(ms-since-last-call)
                            `(setq ,energy-symbol
                                   (+ ,energy-symbol ;; ->> ?
                                      (* ,power-symbol
                                         (/ ms-since-last-call
                                            ,(* 60.0 60.0 1000.0)))))
                            soc-expr
                            incl-lower-expr
                            incl-upper-expr
                            `(cond ((< ,power-symbol ,incl-lower-symbol)
                                    (setq ,power-symbol ,incl-lower-symbol))
                                   ((> ,power-symbol ,incl-upper-symbol)
                                    (setq ,power-symbol ,incl-upper-symbol)))))
                state-update-functions))

    (eval incl-lower-expr)
    (eval incl-upper-expr)

    (add-to-components-alist battery)

    battery))

;;;;;;;;;;;;;;;
;; Inverters ;;
;;;;;;;;;;;;;;;

(defmacro inverter-data-maker (data-alist defaults-alist)
  (component-data-maker data-alist
                        defaults-alist
                        '(id power current voltage component-state
                          per-phase-power inclusion-lower inclusion-upper)))

(defun make-battery-inverter (&rest plist)
  (let* ((id (or (plist-get plist :id) (get-comp-id)))
         (interval (or (plist-get plist :interval) inverter-interval))

         (config (plist-get plist :config))
         (config-alist `(,@config ,@battery-inverter-defaults))

         (successors (plist-get plist :successors))

         (rated-bounds (or (alist-get 'rated-bounds config-alist) '(0.0 0.0)))

         (rated-lower (car rated-bounds))
         (rated-upper (cadr rated-bounds))

         (is-healthy (is-healthy-inverter config-alist))

         (power-expr (when is-healthy
                       `((power . ,(make-power-expr successors))
                         (per-phase-power . (calc-per-phase-power ,(make-power-expr successors)))
                         (voltage . voltage-per-phase)
                         (current . (calc-per-phase-current
                                     ,(make-power-expr successors)))
                         (component-state . (power->component-state
                                             ,(make-power-expr successors))))))
         (bounds-expr `((inclusion-lower . ,rated-lower)
                        (inclusion-upper . ,rated-upper)))
         (bounds-check-func-symbol (bounds-check-func-symbol-from-id id))
         (set-power-func-symbol (set-power-func-symbol-from-id id))

         (inverter
          `((category . inverter)
            (type     . battery)
            (name     . ,(format "inv-bat-%s" id))
            (id       . ,id)
            ,@power-expr
            (stream   . ,(list
                          `(interval . ,interval)
                          (cons 'data
                                (macroexpand '(inverter-data-maker
                                        `((id . ,id)
                                          ,@bounds-expr
                                          ,@power-expr)
                                        config-alist))))))))

    (log.trace (format "Adding battery inverter %s. Healthy: %s" id is-healthy))

    (set bounds-check-func-symbol
         (if is-healthy
             (eval (list 'lambda '(power)
                         `(and
                           (,(make-battery-bounds-check-expr successors) power)
                           (<= ,rated-lower
                               power
                               ,rated-upper))))
             (eval (list 'lambda '(power)
                         (log.error "inverter is unhealthy")
                         nil))))

    (set set-power-func-symbol
         (let* ((healthy-batteries (seq-filter
                                    (lambda (b) (alist-get 'is-healthy b))
                                    successors))
                (num-batteries (length healthy-batteries))
                (expr ()))
           (dolist (battery healthy-batteries)
             (setq expr
                   (cons `(let ((power (/ power ,num-batteries)))
                            (if (not (equal
                                      power
                                      ,(power-symbol-from-id (alist-get 'id battery))))
                                (log.info (format "Setting power of battery %s to %s W (was: %s W)"
                                                  ,(alist-get 'id battery)
                                                  power
                                                  ,(power-symbol-from-id (alist-get 'id battery)))))
                            (setq ,(power-symbol-from-id (alist-get 'id battery))
                                  power)
                            )
                         expr)))
           (if (> num-batteries 0)
               `(lambda (power)
                  ,@expr)
               '(lambda (power)
                  (log.error "Can't set power: no healthy batteries")
                  nil))
           ))

    (add-to-components-alist inverter)
    (connect-successors id successors)
    inverter))

(defun make-solar-inverter (&rest plist)
  (let* ((id (or (plist-get plist :id) (get-comp-id)))
         (interval (or (plist-get plist :interval) inverter-interval))

         (sunlight% (plist-get plist :sunlight%))

         (config (plist-get plist :config))
         (config-alist `(,@config ,@solar-inverter-defaults))

         (power-symbol  (power-symbol-from-id id))
         (min-power-symbol (power-symbol-from-id (format "min-%s" id)))

         (rated-bounds (or (alist-get 'rated-bounds config-alist) '(0.0 0.0)))
         (rated-lower (car rated-bounds))
         (rated-upper (cadr rated-bounds))

         (is-healthy (is-healthy-inverter config-alist))

         (power-expr (when is-healthy
                       `((power . ,power-symbol)
                         (per-phase-power . (calc-per-phase-power ,power-symbol))
                         (voltage . voltage-per-phase)
                         (current . (calc-per-phase-current
                                     ,power-symbol))
                         (component-state . (power->component-state
                                             ,power-symbol)))))

         (bounds-check-func-symbol (bounds-check-func-symbol-from-id id))
         (set-power-func-symbol (set-power-func-symbol-from-id id))

         (inverter
          `((category . inverter)
            (type     . solar)
            (name     . ,(format "inv-pv-%s" id))
            (id       . ,id)
            ,@power-expr
            (stream   . ,(list
                          `(interval . ,interval)
                          (cons 'data
                                (macroexpand '(inverter-data-maker
                                        `((id . ,id)
                                          (inclusion-lower . ,rated-lower)
                                          (inclusion-upper . ,rated-upper)
                                          ,@power-expr)
                                        config-alist))))))))

    (log.trace (format "Adding solar inverter %s. Healthy: %s" id is-healthy))

    (when (not (boundp min-power-symbol))
      (set min-power-symbol rated-lower))

    (set power-symbol (max (eval min-power-symbol) (* rated-lower (/ sunlight% 100.0))))

    (set bounds-check-func-symbol
         (if is-healthy
             (list 'lambda '(power)
                   `(<= ,rated-lower power ,rated-upper))
             (list 'lambda '(power)
                   (log.error "inverter is unhealthy")
                   nil)))

    (set set-power-func-symbol
         (if is-healthy
             `(lambda (power)
                (let ((min-power ,(* rated-lower (/ sunlight% 100.0))))
                  (setq ,min-power-symbol (max power min-power))
                  (if (< power min-power)
                      (progn
                        (log.info
                         (format "Given power %s W is not available for inverter %s.  Limiting to %s W."
                                 power ,id min-power))
                        (setq ,power-symbol min-power))
                      (log.info (format "Setting power of inverter %s to %s W (was: %s W)"
                                        ,id
                                        power
                                        ,(power-symbol-from-id id)))
                      (setq ,power-symbol power))))
           '(lambda (power)
             (log.error "Can't set power: inverter is unhealthy")
             nil)))

    (add-to-components-alist inverter)
    inverter))


;;;;;;;;;;;;
;; Meters ;;
;;;;;;;;;;;;

(defmacro meter-data-maker (data-alist defaults-alist)
  (component-data-maker data-alist
                        defaults-alist
                        '(id power per-phase-power current voltage component-state)))



(defun make-meter (&rest plist)
  (let* ((id (or (plist-get plist :id) (get-comp-id)))
         (interval (or (plist-get plist :interval) meter-interval))
         (power (plist-get plist :power))

         (config (plist-get plist :config))
         (config-alist `(,@config ,@meter-defaults))

         (successors (plist-get plist :successors))
         (hidden (plist-get plist :hidden))
         (is-healthy (is-healthy-meter config-alist))
         (current-expr (when is-healthy
                         (if-let ((current (if power
                                               `(calc-per-phase-current ,power)
                                               (make-current-expr successors)
                                               )))
                             `((current . ,current)))))
         (power-expr (when is-healthy
                       (if-let ((power (or power
                                           (make-power-expr successors))))
                           `((power . ,power)
                             (per-phase-power . (calc-per-phase-power ,power))
                             (voltage . voltage-per-phase)))))
         (meter
          `((category . meter)
            (name     . ,(format "meter-%s" id))
            (id       . ,id)
            (hidden   . ,hidden)
            ,@current-expr
            ,@power-expr
            (stream   . ,(list
                          `(interval . ,interval)
                          (cons 'data
                                (macroexpand '(meter-data-maker
                                               `((id    . ,id)
                                                 ,@current-expr
                                                 ,@power-expr)
                                               config-alist))))))))

    (log.trace (format "Adding meter %s" id))

    (unless hidden
      (add-to-components-alist meter)
      (connect-successors id successors))
    meter))


;;;;;;;;;;;;;;;;;
;; EV Chargers ;;
;;;;;;;;;;;;;;;;;

(defmacro ev-charger-data-maker (data-alist defaults-alist)
  (component-data-maker data-alist
                        defaults-alist
                        '(id power current voltage component-state
                          cable-state inclusion-lower inclusion-upper)))

(defun make-ev-charger (&rest plist)
  (let* ((id (or (plist-get plist :id) (get-comp-id)))
         (interval (or (plist-get plist :interval) ev-charger-interval))
         (config (plist-get plist :config))
         (config-alist `(,@config ,@ev-charger-defaults))

         (power-symbol  (power-symbol-from-id id))
         (energy-symbol (energy-symbol-from-id id))

         (capacity    (alist-get 'capacity    config-alist))
         (initial-soc (alist-get 'initial-soc config-alist))

         (soc-symbol (soc-symbol-from-id id))
         (soc-expr `(setq ,soc-symbol
                          (+ ,initial-soc
                             ;; limit to 1 decimal place
                             (/ (fround
                                 (* 1000.0 (/ ,energy-symbol ,capacity)))
                                10.0))))


         (rated-bounds (or (alist-get 'rated-bounds config-alist) '(0.0 0.0)))
         (rated-lower (car rated-bounds))
         (rated-upper (cadr rated-bounds))

         (incl-lower-symbol (inclusion-lower-symbol-from-id id))
         (incl-upper-symbol (inclusion-upper-symbol-from-id id))

         (soc-lower (alist-get 'soc-lower config-alist))
         (soc-upper (alist-get 'soc-upper config-alist))

         (incl-lower 0.0)
         (incl-upper-expr `(setq ,incl-upper-symbol
                                 (if (< (- ,soc-upper ,soc-symbol) 10.0)
                                     (* ,rated-upper
                                        (bounded-exp-decay ,(- soc-upper 10.0)
                                                           ,soc-upper
                                                           ,soc-symbol
                                                           1.2
                                                           0.3))
                                     ,rated-upper)))

         (is-healthy (is-healthy-ev-charger config-alist))

         (power-expr (when is-healthy
                       `((power . ,power-symbol)
                         (current . (ac-current-from-power ,power-symbol))
                         (component-state . (power->ev-component-state ,power-symbol)))))

         (bounds-expr `((inclusion-lower . 0.0)
                        (inclusion-upper . ,rated-upper)))
         (bounds-check-func-symbol (bounds-check-func-symbol-from-id id))
         (set-power-func-symbol (set-power-func-symbol-from-id id))

         (ev-charger
          `((category . ev-charger)
            (name     . ,(format "ev-charger-%s" id))
            (id       . ,id)
            ,@power-expr
            (stream   . ,(list
                          `(interval . ,interval)
                          (cons 'data
                                (macroexpand '(ev-charger-data-maker
                                               `((id . ,id)
                                                 ,@bounds-expr
                                                 ,@power-expr)
                                               config-alist))))))))

    (log.trace (format "Adding ev-charger %s. Healthy: %s" id is-healthy))

    (when (not (boundp power-symbol))
      (set power-symbol 0.0)
      (set energy-symbol 0.0)
      (set soc-symbol (eval initial-soc)))

    (eval incl-upper-expr)
    (add-to-components-alist ev-charger)

    (setq state-update-functions
          (cons (list 'lambda '(ms-since-last-call)
                      `(eval (setq ,energy-symbol
                                   (+ ,energy-symbol ;; ->> ?
                                      (* ,power-symbol
                                         (/ ms-since-last-call
                                            ,(* 60.0 60.0 1000.0))))))
                      `(eval ,soc-expr)
                      `(eval ,incl-upper-expr)
                      `(cond ((< ,power-symbol 0.0)
                              (setq ,power-symbol 0.0))
                             ((> ,power-symbol ,incl-upper-symbol)
                              (setq ,power-symbol ,incl-upper-symbol))))
                state-update-functions))

    (set bounds-check-func-symbol
         (if is-healthy
             (list 'lambda '(power)
                   `(<= ,rated-lower
                        power
                        ,rated-upper))
             (list 'lambda '(power)
                   (log.error "ev-charger is unhealthy")
                   nil)))

    (set set-power-func-symbol
         (if is-healthy
             `(lambda (power)
                (if (< power ,(* 6.0 3 220.0))
                    (progn
                      (log.info
                       (format "Given power %s W is too low for ev-charger %s.  Not charging."
                               power ,id))
                      (setq ,power-symbol 0.0))
                    (log.info (format "Setting power of ev-charger %s to %s W (was: %s W)"
                                      ,id
                                      power
                                      ,(power-symbol-from-id id)))
                    (setq ,(power-symbol-from-id id) power)))
           '(lambda (power)
             (log.error "Can't set power: ev-charger is unhealthy")
             nil)))

    ev-charger))

;;;;;;;;;;
;; Grid ;;
;;;;;;;;;;

(defun make-grid (&rest plist)
  (let ((id (or (plist-get plist :id) (get-comp-id)))
        (successors (plist-get plist :successors))
        (rated-fuse-current (plist-get plist :rated-fuse-current))
        (grid
         `((category . grid)
           (id       . ,id)
           (name     . "grid")
           (rated-fuse-current . ,rated-fuse-current))))

    (log.trace (format "Adding grid connection %s" id))

    (add-to-components-alist grid)
    (connect-successors id successors)
    grid))
