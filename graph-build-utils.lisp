(defun get-comp-id (alist)
  (if (not (boundp 'comp--id--counter))
      (setq comp--id--counter 1000))
  (if-let ((id (alist-get 'id alist)))
      id
    (setq comp--id--counter (+ comp--id--counter 1))))


(defun add-to-components-alist (alist)
  (if (not (boundp 'components-alist))
      (setq components-alist nil))
  (setq components-alist (cons alist components-alist)))


(defun get-power (alist)
  (if-let ((power (alist-get 'power alist)))
      power
    (if (alist-get 'dont-calculate alist)
        nil
        (if-let ((successors (alist-get 'successors alist)))
            (let ((sum 0.0))
              (dolist (succ successors)
                (setq sum (+ sum (get-power succ))))
              sum)
          0.0))))


(defun get-current (alist)
  (if-let ((current (alist-get 'current alist)))
      current
    (if-let ((successors (alist-get 'successors alist)))
        (let ((sum '(0.0 0.0 0.0)))
          (dolist (succ successors)
            (let ((succ-current (get-current succ)))
              (setq sum (list (+ (car sum) (car succ-current))
                              (+ (cadr sum) (cadr succ-current))
                              (+ (caddr sum) (caddr succ-current))))))
          sum)
      '(0.0 0.0 0.0))))


(defun setup-connections (id alist)
  (if (not (boundp 'connections-alist))
      (setq connections-alist nil))

  (if-let ((successors (alist-get 'successors alist)))
      (dolist (succ successors)
        (setq connections-alist
              (cons (cons id (alist-get 'id succ)) connections-alist)))))


(defun make-grid (alist)
  (let ((id (get-comp-id alist))
        (grid `((id . ,id)
                (name . "grid")
                (category . grid))))
    (setup-connections id alist)
    (add-to-components-alist grid)
    grid))


(defun make-inv-bat (alist)
  (if-let ((power (alist-get 'power alist))
           (floatp power))
      (cond
        ((> power 0.0) (setq alist (cons '(component-state . charging) alist)))
        ((< power 0.0) (setq alist (cons '(component-state . discharging) alist)))
        (t (setq alist (cons '(component-state . idle) alist))))
    (setq alist (cons '(component-state . error) alist)))

  (let ((bat (make-battery alist)))
    (setq alist (cons (cons 'power nil) alist))
    (let ((inv (make-inverter `((successors . (,bat))
                                (type . battery)
                                ,@alist))))
      (if (alist-get 'no-meter alist)
          inv
        (make-meter (cons (cons 'successors (list inv)) alist))))))


(defun make-inv-pv (alist)
  (if-let ((power (alist-get 'power alist))
           (floatp power))
      (cond
        ((> power 0.0)
         (print (format "Warning: PV inverter %s has positive power: %f"
                        (alist-get 'name alist)
                        power))
         (setq alist (cons '(component-state . charging) alist)))
        ((< power 0.0) (setq alist (cons '(component-state . discharging) alist)))
        (t (setq alist (cons '(component-state . idle) alist))))
    (setq alist (cons '(component-state . error) alist)))

  (let ((inv (make-inverter `((type . pv)
                              ,@alist))))
    (if (alist-get 'no-meter alist)
        inv
      (make-meter (cons (cons 'successors (list inv)) alist)))))


(defmacro meter-data-maker (alist)
  (let ((args-alist))
    (dolist (key '(id power current voltage))
      (let ((val (alist-get key alist)))
        (if val
            (setq args-alist (cons (cons key val) args-alist)))))


    (let ((res (list 'lambda '(_) `(meter-data
                               (quote ,args-alist)
                               ))))
      res)))


(defun make-meter (alist)
  (let ((id (get-comp-id alist))
        (meter `((id . ,id)
                 (name . ,(alist-get 'name alist))
                 (category . meter)
                 (power . ,(get-power alist))
                 (current . ,(get-current alist))
                 (stream . ,(list (cons 'interval meter-interval)
                                  (cons 'data (eval `(meter-data-maker
                                                      (,(cons 'id id)
                                                        (power . ,(get-power alist))
                                                        (current . ,(get-current alist))
                                                        ,@alist
                                                        ,@meter-defaults)))))))))
    (setup-connections id alist)
    (add-to-components-alist meter)
    meter))

(defmacro inverter-data-maker (alist)
        (let ((args-alist))
        (dolist (key '(id power current voltage component-state
                       inclusion-lower inclusion-upper))

        (let ((val (alist-get key alist)))
                (if val
                (setq args-alist (cons (cons key val) args-alist)))))

        (let ((res (list 'lambda '(_) `(inverter-data
                                        (quote ,args-alist)
                                        ))))
          res)))


(defun make-inverter (alist)
  (let ((id (get-comp-id alist))
        (inverter `((id . ,id)
                    (name . ,(alist-get 'name alist))
                    (category . inverter)
                    (type . ,(alist-get 'type alist))
                    (power . ,(get-power alist))
                    (current . ,(get-current alist))
                    (stream . ,(list (cons 'interval inverter-interval)
                                     (cons 'data (eval `(inverter-data-maker
                                                         (,(cons 'id id)
                                                           (power . ,(get-power alist))
                                                           (current . ,(get-current alist))
                                                           ,@alist
                                                           ,@inverter-defaults)))))))))
    (setup-connections id alist)
    (add-to-components-alist inverter)
    inverter))


(defmacro ev-charger-data-maker (alist)
        (let ((args-alist))
          (dolist (key '(id power current voltage component-state cable-state
                         inclusion-lower inclusion-upper
                         exclusion-lower exclusion-upper))

        (let ((val (alist-get key alist)))
                (if val
                (setq args-alist (cons (cons key val) args-alist)))))

        (let ((res (list 'lambda '(_) `(ev-charger-data
                                        (quote ,args-alist)
                                        ))))
        res)))


(defun make-ev-charger (alist)
  (let ((id (get-comp-id alist))
        (ev-charger `((id . ,id)
                      (name . ,(alist-get 'name alist))
                      (category . ev_charger)
                      (power . ,(get-power alist))
                      (current . ,(get-current alist))
                      (stream . ,(list (cons 'interval ev-charger-interval)
                                       (cons 'data (eval `(ev-charger-data-maker
                                                           (,(cons 'id id)
                                                             (power . ,(get-power alist))
                                                             (current . ,(get-current alist))
                                                             ,@alist
                                                             ,@ev-charger-defaults)))))))))
    (setup-connections id alist)
    (add-to-components-alist ev-charger)
    ev-charger))


(defmacro battery-data-maker (alist)
        (let ((args-alist))
          (dolist (key '(id soc soc-upper soc-lower
                         capacity power voltage type
                         component-state relay-state
                         inclusion-lower inclusion-upper
                         exclusion-lower exclusion-upper))

        (let ((val (alist-get key alist)))
                (if val
                (setq args-alist (cons (cons key val) args-alist)))))

        (let ((res (list 'lambda '(_) `(battery-data
                                        (quote ,args-alist)
                                        ))))
        res)))


(defun make-battery (alist)
  (let ((id (get-comp-id alist))
        (battery `((id . ,id)
                   (name . ,(alist-get 'name alist))
                   (category . battery)
                   (power . ,(get-power alist))
                   (current . ,(get-current alist))
                   (stream . ,(list (cons 'interval battery-interval)
                                    (cons 'data (eval `(battery-data-maker
                                                        (,(cons 'id id)
                                                          (power . ,(get-power alist))
                                                          (current . ,(get-current alist))
                                                          ,@alist
                                                          ,@battery-defaults)))))))))
    (setup-connections id alist)
    (add-to-components-alist battery)
    battery))
