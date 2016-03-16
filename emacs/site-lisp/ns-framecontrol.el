;; package ns-framecontrol-

(defun ns-framecontrol-get-coord (&optional frame)
  "Gets various coordinates of a frame."
  (let* ((p (frame-parameters frame))
         (aa (cdr (assq 'left p)))
         (a (if (listp aa) (cadr aa) aa))
         (bb (cdr (assq 'top p)))
         (b (if (listp bb) (cadr bb) bb)))
    (list a b (frame-pixel-width frame) (frame-pixel-height frame))))

;; A rectangle is a list of (x1 y1 x2 y2).
;; fc means frame-coordinates.
;; mal is monitor attributes list.

(defun rectangles-intersection-area (r1 r2)
  "Returns the area of the intersection of two rectangles r1 r2."
  (* (max 0 (- (min (+ (first r1) (third r1)) (+ (first r2) (third r2)))
               (max (first r1) (first r2))))
     (max 0 (- (min (+ (second r1) (fourth r1)) (+ (second r2) (fourth r2)))
               (max (second r1) (second r2))))))

(defun max-by (f coll)
  "Returns (x . i) where x is the element in coll maximizing f, and
   i is its index. "
  (when coll
    (let ((i 1) (j 0) (y (funcall f (car coll))))
      (dolist (x (cdr coll))
        (let ((y-new (funcall f x)))
          (when (< y y-new)
            (setq y y-new)
            (setq j i)))
        (incf i))
      (cons (nth j coll) j))))

(defun min-by (f coll)
  "Returns (x . i) where x is the element in coll minimizing f, and
   i is its index. "
  (when coll
    (let ((i 1) (j 0) (y (funcall f (car coll))))
      (dolist (x (cdr coll))
        (let ((y-new (funcall f x)))
          (when (> y y-new)
            (setq y y-new)
            (setq j i)))
        (incf i))
      (cons (nth j coll) j))))

(defun min-max-by (f coll)
  "Returns ((x-min . min-index) (x-max . max-index)) where x-min is a value
   x in coll minimizing f(x), and min-index is its index in coll.  Similarly,
   x-max and max-index maximize f(x)."
  (when coll
    (let* ((i 1)
           (i-min 0) (x-min (car coll)) (y-min (funcall f x-min))
           (i-max 0) (x-max x-min) (y-max y-min))
      (dolist (x (cdr coll))
        (let ((y-new (funcall f x)))
          (when (> y-min y-new)
            (setq y-min y-new)
            (setq i-min i)
            (setq x-min x))
          (when (< y-max y-new)
            (setq y-max y-new)
            (setq i-max i)
            (setq x-max x)))
        (incf i))
      (list (cons x-min i-min) (cons x-max i-max)))))

(defun ns-framecontrol-get-current-workarea-and-index (fc mal)
  "Given a frame-coord and a monitor attributes list, find the
   intersection area and index of the current monitor."
  (max-by (lambda (x) (rectangles-intersection-area fc x))
          (mapcar (lambda (l) (cdr (assq 'workarea l)))
                  mal)))

(defun ns-framecontrol-get-current-mal (fc mal)
  (nth (cdr (ns-framecontrol-get-current-workarea-and-index
             fc mal))
       mal))

(defun ns-framecontrol-get-current-monitor-frames (fc mal)
  (cdr (assq 'frames (ns-framecontrol-get-current-mal
                      fc mal))))

(defun ns-framecontrol-get-current-monitor-interesting-lines (fc m)
  (let* ((w (car (ns-framecontrol-get-current-workarea-and-index fc m)))
         (xs (list (first w) (third w)))
         (ys (list (second w) (fourth w))))
    (dolist (frame (ns-framecontrol-get-current-monitor-frames fc m))
      (let ((r (ns-framecontrol-get-coord frame)))
        (nconc xs (list (first r) (third r)))
        (nconc ys (list (second r) (fourth r)))))
    (list (remove-duplicates (sort xs '<))
          (remove-duplicates (sort ys '<)))))

(defun get-workarea-coord (&optional frame)
  (let* ((r (ns-framecontrol-get-coord frame))
         (g-and-index (ns-framecontrol-get-current-workarea-and-index
                       r (display-monitor-attributes-list)))
         (g (car g-and-index)))
    (list
     ;; Workarea's x-left, y-top, x-right, y-bottom
     g
     ;; Frame coords (x-left, y-top, width, height)
     r
     ;; Range for x-left and y-top of the frame so that it lies in the workarea.
     ;; Order: x1 y1 x2 y2.
     (list (first g) (second g)
           (+ (first g) (third g) (- (third r)))
           (+ ;; (second g)
            (fourth g) (- (fourth r))))
     ;; Index of g
     (cdr g-and-index))))

(defun random-normalish (a)
  (if (< a 16) (random a)
    (let ((r (/ a 8)))
      (+ (random r) (random r) (random r) (random r)
         (random r) (random r) (random r) (random r)))))

(defun random-between (a b)
  (cond ((= a b) a)
        ((< a b) (+ a (random (- b a))))
        (t (+ b (random (- a b))))))

;; (defun closest-next (boundaries inc-sorted-coll direction)
;;   (if (= direction ')
;;    (dolist (b boundaries)
;;      (dolist (x inc-sorted-coll)
;;        (if (< x b))
;;        )))
;;   (mapcar
;;    (lambda (b)
;;      (car (min-by (lambda (x) (let ((diff (* direction (- x b))))
;;                                 (if (pos?))) (* direction (- x b))) coll) ())
;;      )
;;    boundaries)
;;   ;; Binary search and find the next item af
;;   ;; binary search each of the boundary items in coll.  Find best place
;;   ;; direction -1 means towards -inf, +1 means towards +inf.
;;   (min-by
;;    (lambda (x)
;;      (let ((best-b (first boundaries)))
;;        (dolist (b boundaries)
;;          (if (> (- x b) (- x best-b)))))
;;      (first min-by)
;;      )
;;    coll))

(defun get-workarea (&optional frame)
  (nth (fourth (get-workarea-coord frame)) (display-monitor-attributes-list)))

(defun get-workarea-frames (&optional frame)
  ;; Issue: all frames on this monitor, regardless of their "virtual desktop".
  (cdr (assq 'frames (get-workarea))))

(defun set-frame-pos (&optional frame x y)
  (set-frame-position frame x y))

;; ------------------------------------------------------------

(defun pop-up-frame-parameters-on-this-workspace ()
  (let* ((g (first (get-workarea-coord)))
         (initial-width 585)
         (initial-height 494)
         (x (random-between (first g)
                            (+ (first g) (third g) (- initial-width))))
         (y (random-between (second g)
                            (+ ;; (second g)
                             (fourth g) (- initial-height)))))
    `(pop-up-frame-parameters . ((top . ,y) (left . ,x)))))

(defun make-frame-scratch ()
  (interactive)
  (display-buffer "*scratch*"
                  `((display-buffer-pop-up-frame)
                    (reusable-frames . 0)
                    (inhibit-same-window . t)
                    ,(pop-up-frame-parameters-on-this-workspace))
                  nil))

(defun ns-framecontrol-ns-framecontrol-nudge (&optional frame)
  (interactive)
  (let ((bounds (third (get-workarea-coord frame))))
    (set-frame-pos frame
                   (random-between (first bounds) (third bounds))
                   (random-between (second bounds) (fourth bounds)))))

(defun ns-framecontrol-toggle-frame-vertical-size (&optional frame)
  (interactive)
  (let* ((grb (get-workarea-coord frame))
         (g (first grb))
         (r (second grb))
         (bounds (third grb))
         (p (frame-parameters frame))
         (max-num-lines (- (floor (/ (fourth g) 14.3)) 1))) ;; line-height?
    (if (< 45 (cdr (assq 'height p)))
        (progn
          (set-frame-height frame
                            (or (cdr (assq 'last-height p)) 35))
          (set-frame-pos frame
                         (first r) (or (cdr (assq 'last-top p)) (second g))))
      (set-frame-parameter frame 'last-height (cdr (assq 'height p)))
      (set-frame-parameter frame 'last-top (second r))
      (set-frame-pos frame (first r) (second g))
      (set-frame-height frame max-num-lines))))

(defun nudge-fn (x)
  (cond ((= x 0) 0)
        ((< x 0) (- (nudge-fn (- x))))
        ((< x 4) 0)
        ((< x 16) (- x 4))
        ((< x 64) (- x 16))
        ((< x 256) (- x 64))
        (t (- x 128))))

(defun ns-framecontrol-nudge-towards (a b)
  (+ b (nudge-fn (- a b))))

;; IN PROGRESS.
(defun ns-framecontrol-nudge-left (&optional frame)
  (interactive)
  (let* ((fc (ns-framecontrol-get-coord nil))
         (m (display-monitor-attributes-list))
         (xs-ys (ns-framecontrol-get-current-monitor-interesting-lines fc m))
         (xs (first xs-ys))
         (ys (second xs-ys))
         (grb (get-workarea-coord frame))
         (r (second grb))
         (bounds (third grb)))
    (set-frame-pos frame
                   (ns-framecontrol-nudge-towards (first r) (first bounds)) (second r))))

(defun ns-framecontrol-nudge-right (&optional frame)
  (interactive)
  (let* ((grb (get-workarea-coord frame))
         (r (second grb))
         (bounds (third grb)))
    (set-frame-pos frame
                   (ns-framecontrol-nudge-towards (first r) (third bounds)) (second r))))

(defun ns-framecontrol-nudge-up (&optional frame)
  (interactive)
  (let* ((grb (get-workarea-coord frame))
         (r (second grb))
         (bounds (third grb)))
    (set-frame-pos frame
                   (first r) (ns-framecontrol-nudge-towards (second r) (second bounds)))))

(defun ns-framecontrol-nudge-down (&optional frame)
  (interactive)
  (let* ((grb (get-workarea-coord frame))
         (r (second grb))
         (bounds (third grb)))
    (set-frame-pos frame
                   (first r)
                   (ns-framecontrol-nudge-towards (second r) (fourth bounds)))))

(global-set-key (kbd "s-;") 'ns-framecontrol-nudge)
(global-set-key (kbd "s-S-<left>") 'ns-framecontrol-nudge-left)
(global-set-key (kbd "s-S-<right>") 'ns-framecontrol-nudge-right)
(global-set-key (kbd "s-S-<up>") 'ns-framecontrol-nudge-up)
(global-set-key (kbd "s-S-<down>") 'ns-framecontrol-nudge-down)
(global-set-key (kbd "s-=") 'toggle-frame-maximized)
(global-set-key (kbd "s-\\") 'ns-framecontrol-toggle-frame-vertical-size)
(global-set-key (kbd "s-n") 'make-frame-scratch)

(provide 'ns-framecontrol)
