;; package ns-framecontrol

;; A rectangle is a list of (x1 y1 x2 y2).
;; fc means frame-coordinates.
;; mal is monitor attributes list.

(defun rectangles-intersection-area (r1 r2)
  "Return the area of the intersection of two rectangles r1 r2."
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

(defun random-normalish (a)
  (if (< a 16) (random a)
    (let ((r (/ a 8)))
      (+ (random r) (random r) (random r) (random r)
         (random r) (random r) (random r) (random r)))))

(defun random-between (a b)
  (cond ((= a b) a)
        ((< a b) (+ a (random (- b a))))
        (t (+ b (random (- a b))))))

(defun random-element (coll)
  (nth (random (length coll)) coll))

;; ----------------------------------------------------------------------

(defun ns-framecontrol-get-frame-coord (&optional frame)
  "Get the coords rectangle (c) of a frame.  The coordinates are the (x, y) of
   the top left point and the width, height of the frame.  For nil input,
   return the coords of the currently selected frame."
  (let* ((p (frame-parameters frame))
         (aa (cdr (assq 'left p)))
         (a (if (listp aa) (cadr aa) aa))
         (bb (cdr (assq 'top p)))
         (b (if (listp bb) (cadr bb) bb)))
    (list a b (frame-pixel-width frame) (frame-pixel-height frame))))

(defun ns-framecontrol-get-current-workarea-and-index (fc mal)
  "Given a frame-coord (fc) and a monitor attributes list (mal), find the
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
      (let ((r (ns-framecontrol-get-frame-coord frame)))
        (nconc xs (list (- (first r) (third fc))
                        (first r)
                        (+ (first r) (third r) (- (third fc)))
                        (+ (first r) (third r))))
        (nconc ys (list (- (second r) (fourth fc))
                        (second r)
                        (+ (second r) (fourth r) (- (fourth fc)))
                        (+ (second r) (fourth r))))))
    (list (remove-duplicates (sort xs '<))
          (remove-duplicates (sort ys '<)))))

(defun ns-framecontrol-get-workarea-coord (&optional frame)
  "Returns ((x-left y-top x-right y-bottom) of workarea
            (x-left, y-top, w, h) of frame
            (min-x min-y max-x max-y) for (x-left, y-top) in the workarea
            index of workarea)"
  (let* ((r (ns-framecontrol-get-frame-coord frame))
         (g-and-index (ns-framecontrol-get-current-workarea-and-index
                       r (display-monitor-attributes-list)))
         (g (car g-and-index)))
    (list
     ;; Workarea's x-left, y-top, x-right, y-bottom
     g
     ;; Frame coords (x-left, y-top, width, height)
     r
     ;; Bounds (min-x, min-y, max-x, max-y) for (x-left, y-top) of the
     ;; frame. When (x-left, y-top) is moved within this range, the frame will
     ;; lie in the workarea.
     (list (first g) (second g)
           (- (third g) (third r))
           (- (fourth g) (fourth r)))
     ;; Index of g
     (cdr g-and-index))))

(defun get-workarea (&optional frame)
  (nth (fourth (ns-framecontrol-get-workarea-coord frame))
       (display-monitor-attributes-list)))

(defun get-workarea-frames (&optional frame)
  ;; Issue: all frames on this monitor, regardless of their "virtual desktop".
  (cdr (assq 'frames (get-workarea))))

(defun set-frame-pos (&optional frame x y)
  (set-frame-position frame x y))

;; ------------------------------------------------------------

(defun pop-up-frame-parameters-on-this-workspace ()
  (let* ((g (first (ns-framecontrol-get-workarea-coord)))
         (initial-width 585)
         (initial-height 494)
         (x (random-between (first g)
                            (- (third g) initial-width)))
         (y (random-between (second g)
                            (- (fourth g) initial-height))))
    `(pop-up-frame-parameters . ((top . ,y) (left . ,x)))))

(defun ns-framecontrol-make-frame-scratch ()
  (interactive)
  (display-buffer "*scratch*"
                  `((display-buffer-pop-up-frame)
                    (reusable-frames . 0)
                    (inhibit-same-window . t)
                    ,(pop-up-frame-parameters-on-this-workspace))
                  nil))

(defun ns-framecontrol-nudge (&optional frame)
  (interactive)
  (let ((bounds (third (ns-framecontrol-get-workarea-coord frame))))
    (set-frame-pos frame
                   (random-between (first bounds) (third bounds))
                   (random-between (second bounds) (fourth bounds)))))

(defun ns-framecontrol-toggle-frame-vertical-size (&optional frame)
  (interactive)
  (let* ((grbi (ns-framecontrol-get-workarea-coord frame))
         (g (first grbi))
         (r (second grbi))
         (bounds (third grbi))
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
      (set-frame-position frame (first r) (second g))
      (set-frame-height frame max-num-lines))))

(defun ns-framecontrol-nudge-fn (x)
  (cond ((< x 0) (- (ns-framecontrol-nudge-fn (- x))))
        ((<= x 160) 0)
        (t (- x 160))))

(defun ns-framecontrol-nudge-towards (a b)
  (+ b (ns-framecontrol-nudge-fn (- a b))))

(defun ns-framecontrol-nudge-frame-in-direction (frame direction)
  (let* ((xs-and-ys (ns-framecontrol-get-current-monitor-interesting-lines
                     (ns-framecontrol-get-frame-coord nil)
                     (display-monitor-attributes-list)))
         (grbi (ns-framecontrol-get-workarea-coord frame))
         (r (second grbi))
         (bounds (third grbi)))
    (cond ((eq direction :left)
           (set-frame-pos frame
                          (ns-framecontrol-nudge-towards
                           (first r)
                           (or
                            (car (last (remove-if-not
                                        (lambda (x)
                                          (<= (first bounds) x (- (first r) 1)))
                                        (first xs-and-ys))))
                            (first bounds)))
                          (second r)))
          ((eq direction :right)
           (set-frame-pos frame
                          (ns-framecontrol-nudge-towards
                           (first r)
                           (or
                            (first (remove-if-not
                                    (lambda (x)
                                      (<= (+ (first r) 1) x (third bounds)))
                                    (first xs-and-ys)))
                            (third bounds)))
                          (second r)))
          ((eq direction :up)
           (set-frame-pos frame
                          (first r)
                          (ns-framecontrol-nudge-towards
                           (second r)
                           (or
                            (first (remove-if-not
                                    (lambda (y)
                                      (<= (second bounds) y (- (second r) 1)))
                                    (second xs-and-ys)))
                            (second bounds)))))
          ((eq direction :down)
           (set-frame-pos frame
                          (first r)
                          (ns-framecontrol-nudge-towards
                           (second r)
                           (or
                            (first (remove-if-not
                                    (lambda (y)
                                      (<= (+ (second r) 1) y (fourth bounds)))
                                    (second xs-and-ys)))
                            (fourth bounds))))))))

(defun ns-framecontrol-nudge-left (&optional frame)
  (interactive)
  (ns-framecontrol-nudge-frame-in-direction frame :left))

(defun ns-framecontrol-nudge-right (&optional frame)
  (interactive)
  (ns-framecontrol-nudge-frame-in-direction frame :right))

(defun ns-framecontrol-nudge-up (&optional frame)
  (interactive)
  (ns-framecontrol-nudge-frame-in-direction frame :up))

(defun ns-framecontrol-nudge-down (&optional frame)
  (interactive)
  (ns-framecontrol-nudge-frame-in-direction frame :down))

(global-set-key (kbd "s-;") 'ns-framecontrol-nudge)
(global-set-key (kbd "s-S-<left>") 'ns-framecontrol-nudge-left)
(global-set-key (kbd "s-S-<right>") 'ns-framecontrol-nudge-right)
(global-set-key (kbd "s-S-<up>") 'ns-framecontrol-nudge-up)
(global-set-key (kbd "s-S-<down>") 'ns-framecontrol-nudge-down)
(global-set-key (kbd "s-=") 'toggle-frame-maximized)
(global-set-key (kbd "s-\\") 'ns-framecontrol-toggle-frame-vertical-size)
(global-set-key (kbd "s-n") 'ns-framecontrol-make-frame-scratch)

(provide 'ns-framecontrol)
