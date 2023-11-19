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


(defun set-power-active (id power)
  (let* ((power-symbol (power-symbol-from-id id))
         (original-value (eval power-symbol)))
    (eval `(setq ,power-symbol power))
    (print (format "set %s from %s to %s"
                   power-symbol
                   original-value
                   (eval power-symbol)))))


(defun component-data-maker (method data-alist defaults-alist keys)
  (let ((input-alist (append (eval data-alist) (eval defaults-alist)))
        (args-alist))
    (dolist (key keys)
      (when-let ((val (alist-get key input-alist)))
          (setq args-alist (cons (cons key val) args-alist))))

    (list 'lambda '(_) `(,method ,args-alist))))
