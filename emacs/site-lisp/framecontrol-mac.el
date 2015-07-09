;; ------------------------------------------------------------
;; Tiling.
(defun get-frame-coord (&optional frame)
  "Gets various coordinates of a frame."
  (let* ((p (frame-parameters frame))
         (aa (cdr (assq 'left p)))
         (a (if (listp aa) (cadr aa) aa))
         (bb (cdr (assq 'top p)))
         (b (if (listp bb) (cadr bb) bb)))
    (list a b (frame-pixel-width frame) (frame-pixel-height frame))))

(defun r-intersection-area (r1 r2)
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

(defun get-current-workarea-and-index (frame-coord monitor-attributes-list)
  "Given a frame-coord and a monitor attributes list, find the
   intersection area and index of the current monitor."
  (max-by (lambda (x) (r-intersection-area frame-coord x))
          (mapcar (lambda (l) (cdr (assq 'workarea l)))
                  monitor-attributes-list)))

(defun get-current-monitor-attributes-list (frame-coord
                                            monitor-attributes-list)
  (nth (cdr (get-current-workarea-and-index
             frame-coord monitor-attributes-list))
       monitor-attributes-list))

(defun get-current-monitor-frames (frame-coord monitor-attributes-list)
  (cdr (assq 'frames (get-current-monitor-attributes-list
                      frame-coord monitor-attributes-list))))

(defun get-current-monitor-interesting-lines (fc m)
  (let* ((w (car (get-current-workarea-and-index fc m)))
        (xs (list (first w) (third w)))
        (ys (list (second w) (fourth w))))
    (dolist (frame (get-current-monitor-frames fc m))
      (let ((r (get-frame-coord frame)))
        (nconc xs (list (first r) (third r)))
        (nconc ys (list (second r) (fourth r)))))
    (list (remove-duplicates (sort xs '<))
          (remove-duplicates (sort ys '<)))))

(defun get-workarea-coord (&optional frame)
  (let* ((r (get-frame-coord frame))
         (g-and-index (get-current-workarea-and-index
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
(global-set-key (kbd "s-n") 'make-frame-scratch)

(defun make-frame-agenda ()
  (interactive)
  (display-buffer (find-file-noselect "~/org/a.org")
                  `((display-buffer-pop-up-frame)
                    (reusable-frames . 0)
                    (inhibit-same-window . t)
                    ,(pop-up-frame-parameters-on-this-workspace))
                  nil))
(global-set-key (kbd "s-A") 'make-frame-agenda)

(defun make-frame-s-txt ()
  (interactive)
  (find-file-noselect "~/tmp/s.txt")
  (display-buffer "s.txt"
                  `((display-buffer-pop-up-frame)
                    (reusable-frames . 0)
                    (inhibit-same-window . t)
                    ,(pop-up-frame-parameters-on-this-workspace))
                  nil))
(global-set-key (kbd "s-S") 'make-frame-s-txt)

(defun nudge-frame (&optional frame)
  (interactive)
  (let ((bounds (third (get-workarea-coord frame))))
    (set-frame-pos frame
                   (random-between (first bounds) (third bounds))
                   (random-between (second bounds) (fourth bounds)))))

(defun toggle-frame-vertical-size (&optional frame)
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

(global-set-key (kbd "s-\\") 'toggle-frame-vertical-size)

(defun nudge-fn (x)
  (cond ((= x 0) 0)
        ((< x 0) (- (nudge-fn (- x))))
        ((< x 4) 0)
        ((< x 16) (- x 4))
        ((< x 64) (- x 16))
        ((< x 256) (- x 64))
        (t (- x 128))))

(defun nudge-towards (a b)
  (+ b (nudge-fn (- a b))))

;; IN PROGRESS.
(defun frame-nudge-left (&optional frame)
  (interactive)
  (let* ((fc (get-frame-coord nil))
        (m (display-monitor-attributes-list))
        (xs-ys (get-current-monitor-interesting-lines fc m))
        (xs (first xs-ys))
        (ys (second xs-ys))
        (grb (get-workarea-coord frame))
        (r (second grb))
        (bounds (third grb)))
    (set-frame-pos frame
                   (nudge-towards (first r) (first bounds)) (second r))))

(defun frame-nudge-right (&optional frame)
  (interactive)
  (let* ((grb (get-workarea-coord frame))
         (r (second grb))
         (bounds (third grb)))
    (set-frame-pos frame
                   (nudge-towards (first r) (third bounds)) (second r))))

(defun frame-nudge-up (&optional frame)
  (interactive)
  (let* ((grb (get-workarea-coord frame))
         (r (second grb))
         (bounds (third grb)))
    (set-frame-pos frame
                   (first r) (nudge-towards (second r) (second bounds)))))

(defun frame-nudge-down (&optional frame)
  (interactive)
  (let* ((grb (get-workarea-coord frame))
         (r (second grb))
         (bounds (third grb)))
    (set-frame-pos frame
                   (first r) (nudge-towards (second r) (fourth bounds)))))

(global-set-key (kbd "s-;") 'nudge-frame)
(global-set-key (kbd "s-S-<left>") 'frame-nudge-left)
(global-set-key (kbd "s-S-<right>") 'frame-nudge-right)
(global-set-key (kbd "s-S-<up>") 'frame-nudge-up)
(global-set-key (kbd "s-S-<down>") 'frame-nudge-down)
(global-set-key (kbd "s-=") 'toggle-frame-maximized)

(provide 'framecontrol-mac)
