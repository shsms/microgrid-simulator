;; Reset variables to their initial state everytime the script is
;; reloaded, so that the same components and connections are not added
;; multiple times.
(setq comp--id--counter 1000)
(setq connections-alist nil)
(setq components-alist nil)


(defun get-comp-id ()
  (setq comp--id--counter (+ comp--id--counter 1)))


(defun power-symbol-from-id (id)
  (intern (format "component-power-%s" id)))


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
         (original-value (eval power-symbol)))
    (eval `(setq ,power-symbol power))
    (log.info (format "Setting power for component %d to %f (was %f))"
                   id
                   power
                   original-value))))


(defun component-data-maker (method data-alist defaults-alist keys)
  (let ((input-alist (append (eval data-alist) (eval defaults-alist)))
        (args-alist))
    (dolist (key keys)
      (when-let ((val (alist-get key input-alist)))
          (setq args-alist (cons (cons key val) args-alist))))

    (list 'lambda '(_) `(,method ,args-alist))))


(defun ac-current-from-power (power)
  (let ((per-phase-power (/ power 3)))
    (mapcar '(lambda (voltage) (/ per-phase-power voltage)) ac-voltage)))
