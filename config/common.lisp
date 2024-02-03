;; Reset variables to their initial state everytime the script is
;; reloaded, so that the same components and connections are not added
;; multiple times.
(defun reset-state ()
  (setq comp--id--counter 1000)
  (setq connections-alist nil)
  (setq components-alist nil)
  (setq state-update-functions nil))

(defun get-comp-id ()
  (setq comp--id--counter (+ comp--id--counter 1)))


(defun power-symbol-from-id (id)
  (intern (format "component-power-%s" id)))


(defun energy-symbol-from-id (id)
  (intern (format "component-energy-%s" id)))


(defun soc-symbol-from-id (id)
  (intern (format "component-soc-%s" id)))


(defun inclusion-upper-symbol-from-id (id)
  (intern (format "component-inclusion-upper-%s" id)))


(defun inclusion-lower-symbol-from-id (id)
  (intern (format "component-inclusion-lower-%s" id)))


(defun bounds-check-func-symbol-from-id (id)
  (intern (format "component-bounds-check-func-%s" id)))


(defun add-to-connections-alist (id-from id-to)
  (setq connections-alist (cons (cons id-from id-to)
                                connections-alist)))


(defun add-to-components-alist (alist)
  (setq components-alist (cons alist
                               components-alist)))


(defun connect-components (alist-from alist-to)
  (let ((id-from (alist-get 'id alist-from))
        (id-to (alist-get 'id alist-to)))
    (add-to-connections-alist id-from id-to)))


(defun connect-successors (id successors)
  (dolist (successor successors)
    (add-to-connections-alist id (alist-get 'id successor))))


(defun make-power-expr (successors)
  (let ((expr ()))
    (dolist (successor successors)
      (if-let ((power (alist-get 'power successor)))
          (setq expr (cons power expr))))
    (when expr (cons '+ expr))))


(defun make-current-expr (successors)
  (let ((p1-expr ())
        (p2-expr ())
        (p3-expr ()))
    (dolist (successor successors)
      (if-let ((current (alist-get 'current successor)))
          (progn
            (setq p1-expr (cons `(nth 0 ,current) p1-expr))
            (setq p2-expr (cons `(nth 1 ,current) p2-expr))
            (setq p3-expr (cons `(nth 2 ,current) p3-expr)))))
    (when p1-expr (list 'list
                        (setq p1-expr (cons '+ p1-expr))
                        (setq p2-expr (cons '+ p2-expr))
                        (setq p3-expr (cons '+ p3-expr))))))


(defun set-power-active (id power)
  (let* ((power-symbol (power-symbol-from-id id))
         (bounds-check-func (eval (bounds-check-func-symbol-from-id id)))
         (original-power (eval power-symbol))
         (power (ftruncate power)))

    (if (funcall bounds-check-func power)
        (progn
          (set power-symbol power)
          (when (not (equal original-power power))
            (log.info (format "Setting power for component %d to %f (was %f))"
                              id
                              power
                              original-power)))
          nil)
        (let ((err (format "Requested power %f is out of bounds for component id %d" power id)))
          (log.warn err)
          err))))


(defun component-data-maker (method data-alist defaults-alist keys)
  (let ((data-alist (eval data-alist))
        (defaults-alist (eval defaults-alist))
        (args-alist))

    (dolist (key keys)
      (if-let ((val (alist-get key data-alist)))
          (setq args-alist (cons (cons key val) args-alist))
        (if-let ((val (alist-get key defaults-alist)))
            (setq args-alist (cons (cons key `(quote ,val)) args-alist)))))

    (list 'lambda '(_) `(,method ,args-alist))))


(defun quote-each-value (alist)
  (let (new-list)
    (dolist (item alist new-list)
      (setq new-list (cons (cons (car item) `(quote ,(cdr item))) new-list)))))


(defun ac-current-from-power (power)
  (let ((per-phase-power (/ power 3)))
    (mapcar '(lambda (voltage) (/ per-phase-power voltage)) ac-voltage)))


(defun is-healthy-battery (bat)
  (let ((comp-state (alist-get 'component-state bat))
        (relay-state (alist-get 'relay-state bat)))
    (and (or (eq comp-state 'idle)
             (eq comp-state 'charging)
             (eq comp-state 'discharging))
         (eq relay-state 'closed))))

(defun is-healthy-inverter (inv)
  (let ((comp-state (alist-get 'component-state inv)))
    (or (eq comp-state 'idle)
        (eq comp-state 'charging)
        (eq comp-state 'discharging))))

(defun power->component-state (power)
  (cond
    ((not (numberp power)) nil)
    ((> power 0.0) 'charging)
    ((< power 0.0) 'discharging)
    (:else       'idle)))


(defun bounded-exp-decay (start stop val base min_val)
  (let* ((base (max base 1.1))
         (factor (/ 10.0 (- stop start)))
         (stop (+ start (* (- stop start) factor)))
         (val (+ start (* (- val start) factor)))
         (shift (- min_val (expt base (- start stop 1)))))
    (cond
      ((>= val stop) 0.0)
      ((< val start) 1.0)
      (t (+ shift (* (- 1.0 shift)
                     (expt base (- start val))))))))
