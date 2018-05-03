;; -*- lexical-binding: t -*-
;; package ns-framecontrol

;; Questions/TODOs:
;; 1. List frames on the current workspace.
;; 2. Reuse (ns-frame-edges) and (ns-frame-geometry).

;; A rectangle is a list of (x1 y1 x2 y2).
;; fc means frame-coordinates.
;; mal is monitor attributes list for all monitors.

;; grbi: (g = (x-left y-top x-right y-bottom) of workarea
;;        rect = (x-left, y-top, w, h) of frame
;;        bounds = (min-x min-y max-x max-y) for (x-left, y-top) in the workarea
;;        index of workarea)

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

(defun min-and-max-by (f coll)
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

(defvar ns-framecontrol-titlebar-height 22
  "Height of the title area of a frame.")

(defun ns-framecontrol-get-frame-coord (&optional frame)
  "Get the coords rectangle (c) of a frame.  The coordinates are
   (left top width height), that is, the (x, y) of the top left point and the
   width, height of the frame.  When `frame' is nil, assumes the currently
   selected frame."
  (let* ((p (frame-parameters frame))
         (aa (cdr (assq 'left p)))
         (a (if (listp aa) (cadr aa) aa))
         (bb (cdr (assq 'top p)))
         (b (if (listp bb) (cadr bb) bb)))
    (list a b
          (frame-pixel-width frame)
          (frame-pixel-height frame))))

(defun ns-framecontrol-get-gi (fc mal)
  "Given a frame-coord (fc) and a monitor attributes list (mal), find the
   intersection area and index of the current monitor."
  (max-by (lambda (x) (rectangles-intersection-area fc x))
          ;; 'geometry was 'workarea; But it didn't quite work well for multiple
          ;; monitors.
          (mapcar (lambda (l) (cdr (assq 'geometry l)))
                  mal)))

(defun ns-framecontrol-get-current-mal (fc mal)
  (nth (cdr (ns-framecontrol-get-gi fc mal)) mal))

(defun ns-framecontrol-get-current-monitor-frames (fc mal)
  (cdr (assq 'frames (ns-framecontrol-get-current-mal
                      fc mal))))

(defun ns-framecontrol-get-grbi (&optional frame)
  "Returns ((x-left y-top x-right y-bottom) of workarea aka g
            (x-left, y-top, w, h) of frame aka r
            (min-x min-y max-x max-y) for (x-left, y-top) in the workarea aka b
            index of workarea) aka i"
  (let* ((mal (display-monitor-attributes-list))
         (r (ns-framecontrol-get-frame-coord frame))
         (gi (ns-framecontrol-get-gi r mal))
         (gg (car gi))
         (g (list (first gg) (second gg)
                  (+ (first gg) (third gg)) (+ (second gg) (fourth gg)))))
    (list
     ;; Workarea's x-left, y-top, x-right, y-bottom
     g
     ;; Frame coords (x-left, y-top, width, height)
     r
     ;; Bounds (min-x, min-y, max-x, max-y) for (x-left, y-top) of the
     ;; frame. When (x-left, y-top) is moved within this range, the frame will
     ;; lie in the workarea.
     (list (first g) (+ ns-framecontrol-titlebar-height (second g))
           (- (third g) (third r))
           (- (fourth g) (fourth r) ns-framecontrol-titlebar-height))
     ;; Index of g
     (cdr gi))))

;; ------------------------------------------------------------

(defun pop-up-frame-parameters-on-this-workspace ()
  (let* ((g (first (ns-framecontrol-get-grbi)))
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

(defun ns-framecontrol-make-frame-tmp ()
  (interactive)
  (switch-to-buffer-other-frame (get-buffer-create (make-temp-name "tmp "))))

(defun ns-framecontrol-nudge (&optional frame)
  (interactive)
  (let ((bounds (third (ns-framecontrol-get-grbi frame))))
    (modify-frame-parameters
     frame
     `((left . ,(random-between (first bounds) (third bounds)))
       (top . ,(random-between (second bounds) (fourth bounds)))))))

(defun ns-framecontrol-toggle-frame-vertical-size (&optional frame)
  (interactive)
  (let* ((grbi (ns-framecontrol-get-grbi frame))
         (g (first grbi))
         (r (second grbi))
         (bounds (third grbi))
         (p (frame-parameters frame))
         (available-vertical-space (- (fourth g)
                                      (second g)
                                      ns-framecontrol-titlebar-height))
         ;; 4.0 is perhaps the total height of the vertical box lines (around
         ;; modeline, etc.).
         (line-height (/ (- (fourth r) 4.0) (cdr (assq 'height p))))
         (max-num-lines (floor (/ available-vertical-space line-height))))
    (modify-frame-parameters
     frame
     (if (< 45 (cdr (assq 'height p)))
         `((height . ,(or (cdr (assq 'last-height p)) 35))
           (left . ,(first r))
           (top . ,(or (cdr (assq 'last-top p)) (second g))))
       `((height . ,max-num-lines)
         (left . ,(first r))
         (top . ,(second g))
         (last-height . ,(cdr (assq 'height p)))
         (last-top . ,(second r)))))))

(defun ns-framecontrol-nudge-fn (x)
  (cond ((< x 0) (- (ns-framecontrol-nudge-fn (- x))))
        ((<= x 290) 0)
        (t (- x 290))))

(defun ns-framecontrol-nudge-towards (a b)
  (+ b (ns-framecontrol-nudge-fn (- a b))))

(defun ns-framecontrol-next-directional-strict (initial
                                                params-fn
                                                order-fn
                                                all-frames
                                                current-value
                                                current-size)
  (let ((best initial))
    (dolist (frame all-frames)
      (dolist (value (funcall params-fn
                              (ns-framecontrol-get-frame-coord frame)
                              current-size))
        (when (funcall order-fn best value current-value)
          (setq best value))))
    best))

(defun ns-framecontrol-nudge-frame-in-direction-hor (r current-width)
  (list (- (first r) current-width)
        (first r)
        (+ (first r) (third r) (- current-width))
        (+ (first r) (third r))))

(defun ns-framecontrol-nudge-frame-in-direction-ver (r current-height)
  (list (- (second r) (+ ns-framecontrol-titlebar-height current-height))
        (second r)
        (+ (second r) (fourth r) (- current-height))
        (+ (second r) ns-framecontrol-titlebar-height (fourth r))))

(defun ns-framecontrol-nudge-frame-in-direction (frame direction)
  (let* ((frame (or frame (selected-frame)))
         (fc (ns-framecontrol-get-frame-coord frame))
         (mal (display-monitor-attributes-list))
         (grbi (ns-framecontrol-get-grbi frame))
         (bounds (third grbi))
         (frames (ns-framecontrol-get-current-monitor-frames fc mal))
         (left (first fc))
         (top (second fc))
         (width (third fc))
         (height (fourth fc))
         (value (case direction
                  (:left (ns-framecontrol-next-directional-strict
                          (first bounds)
                          'ns-framecontrol-nudge-frame-in-direction-hor
                          '< frames left width))
                  (:right (ns-framecontrol-next-directional-strict
                           (third bounds)
                           'ns-framecontrol-nudge-frame-in-direction-hor
                           '> frames left width))
                  (:up (ns-framecontrol-next-directional-strict
                        (second bounds)
                        'ns-framecontrol-nudge-frame-in-direction-ver
                        '< frames top height))
                  (:down (ns-framecontrol-next-directional-strict
                          (fourth bounds)
                          'ns-framecontrol-nudge-frame-in-direction-ver
                          '> frames top height)))))
    (when value
      (message "value: %s" value)
      (modify-frame-parameters
       frame
       (case direction
         (:left `((left . ,(ns-framecontrol-nudge-towards left value))))
         (:right `((left . ,(ns-framecontrol-nudge-towards left value))))
         (:up `((top . ,(ns-framecontrol-nudge-towards top value))))
         (:down `((top . ,(ns-framecontrol-nudge-towards top value)))))))))

(defun ns-framecontrol-next-directional (initial
                                         param-fn
                                         order-fn
                                         all-frames
                                         current-frame
                                         current-frame-fc)
  (let ((best (cons initial nil))
        (seen-current nil))
    ;; Assuming we are trying to find the next frame to the left.  If there are
    ;; no frames f with f.left = current-frame.left, find the largest f.left
    ;; with f.left < current-frame.left.  If there are some frame f with the
    ;; same left, find f immediately before current-frame in the list.
    (dolist (frame all-frames)
      (let ((value (funcall param-fn (ns-framecontrol-get-frame-coord frame))))
        (cond ((eq frame current-frame)
               (setq seen-current t))
              ((funcall order-fn
                        (first best) value (funcall param-fn current-frame-fc))
               (setq best (cons value frame)))
              ((and seen-current (= value (funcall param-fn current-frame-fc)))
               (setq best (cons value frame))
               (return)))))
    (cdr best)))

(defun ns-framecontrol-select-frame-in-direction (frame direction)
  (let* ((frame (or frame (selected-frame)))
         (fc (ns-framecontrol-get-frame-coord frame))
         (mal (display-monitor-attributes-list))
         (frames (ns-framecontrol-get-current-monitor-frames fc mal))
         (next-frame (case direction
                       (:left (ns-framecontrol-next-directional
                               -100000 'first '< frames frame fc))
                       (:right (ns-framecontrol-next-directional
                                100000 'first '> (nreverse frames) frame fc))
                       (:up (ns-framecontrol-next-directional
                             -100000 'second '< frames frame fc))
                       (:down (ns-framecontrol-next-directional
                               100000 'second '> (nreverse frames) frame fc)))))
    (when next-frame
      (select-frame-set-input-focus next-frame))))

(defun ns-framecontrol-nudger (dir)
  #'(lambda (&optional frame)
      (interactive)
      (ns-framecontrol-nudge-frame-in-direction frame dir)))

(defun ns-framecontrol-directional-selector (dir)
  #'(lambda (&optional frame)
      (interactive)
      (ns-framecontrol-select-frame-in-direction frame dir)))

(defun ns-framecontrol-vertframe (n)
  (interactive "nNumber of vertical divisions: ")
  (make-frame)
  (set-frame-parameter nil 'width (- (* n 83) 3))
  (dotimes (i (- n 1))
    (split-window-right))
  (balance-windows)
  (ns-framecontrol-toggle-frame-vertical-size))

;; TODO: Iterations should happen when the Command key is held down and ` is
;; typed repeatedly, not when Command-` is typed followed by more ` keys.
(defun ns-framecontrol-select-mru ()
  (interactive)
  (lexical-let ((keymap (make-sparse-keymap))
                (frames (cdr (frame-list-z-order))))
    (when frames
      (select-frame-set-input-focus (pop frames))

      (define-key keymap (kbd "`")
        (lambda () (interactive)
          (when frames (select-frame-set-input-focus (pop frames)))))
      (set-transient-map keymap t))))

(global-set-key (kbd "s-;") 'ns-framecontrol-nudge)
(global-set-key (kbd "s-<left>") (ns-framecontrol-directional-selector :left))
(global-set-key (kbd "s-<right>") (ns-framecontrol-directional-selector :right))
(global-set-key (kbd "s-<up>") (ns-framecontrol-directional-selector :up))
(global-set-key (kbd "s-<down>") (ns-framecontrol-directional-selector :down))
(global-set-key (kbd "s-S-<left>") (ns-framecontrol-nudger :left))
(global-set-key (kbd "s-S-<right>") (ns-framecontrol-nudger :right))
(global-set-key (kbd "s-S-<up>") (ns-framecontrol-nudger :up))
(global-set-key (kbd "s-S-<down>") (ns-framecontrol-nudger :down))
(global-set-key (kbd "s-B") (ns-framecontrol-nudger :left))
(global-set-key (kbd "s-F") (ns-framecontrol-nudger :right))
(global-set-key (kbd "s-P") (ns-framecontrol-nudger :up))
(global-set-key (kbd "s-N") (ns-framecontrol-nudger :down))
(global-set-key (kbd "s-=") 'toggle-frame-maximized)
(global-set-key (kbd "s-\\") 'ns-framecontrol-toggle-frame-vertical-size)
(global-set-key (kbd "s-n") 'ns-framecontrol-make-frame-scratch)
(global-set-key (kbd "s-d") prefix-arg)
(global-set-key (kbd "s-d t") 'ns-framecontrol-make-frame-tmp)
(global-set-key (kbd "s-`") 'ns-framecontrol-select-mru)

(provide 'ns-framecontrol)
