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
                                                           0.0))
                                     ,rated-lower)))
         (incl-upper-expr `(setq ,incl-upper-symbol
                                 (if (< (- ,soc-upper ,soc-symbol) 10.0)
                                     (* ,rated-upper
                                        (bounded-exp-decay ,(- soc-upper 10.0)
                                                           ,soc-upper
                                                           ,soc-symbol
                                                           1.2
                                                           0.0))
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
         (config-alist `(,@config ,@inverter-defaults))

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


;;;;;;;;;;;;
;; Meters ;;
;;;;;;;;;;;;

(defmacro meter-data-maker (data-alist)
  (component-data-maker data-alist
                        nil
                        '(id power per-phase-power current voltage)))



(defun make-meter (&rest plist)
  (let* ((id (or (plist-get plist :id) (get-comp-id)))
         (interval (or (plist-get plist :interval) meter-interval))
         (power (plist-get plist :power))
         (config (plist-get plist :config))
         (successors (plist-get plist :successors))
         (current-expr (if-let ((current (if power
                                             `(calc-per-phase-current ,power)
                                             (make-current-expr successors)
                                             )))
                           `((current . ,current))))
         (power-expr (if-let ((power (or power
                                         (make-power-expr successors))))
                         `((power . ,power)
                           (per-phase-power . (calc-per-phase-power ,power))
                           (voltage . voltage-per-phase))))
         (meter
          `((category . meter)
            (name     . ,(format "meter-%s" id))
            (id       . ,id)
            ,@current-expr
            ,@power-expr
            (stream   . ,(list
                          `(interval . ,interval)
                          (cons 'data
                                (macroexpand '(meter-data-maker
                                        `((id    . ,id)
                                          ,@current-expr
                                          ,@power-expr
                                          ,@config)))))))))

    (log.trace (format "Adding meter %s" id))

    (add-to-components-alist meter)
    (connect-successors id successors)
    meter))


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
