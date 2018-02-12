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

(defun ns-framecontrol-get-gi (fc mal)
  "Given a frame-coord (fc) and a monitor attributes list (mal), find the
   intersection area and index of the current monitor."
  (max-by (lambda (x) (rectangles-intersection-area fc x))
          (mapcar (lambda (l) (cdr (assq 'workarea l)))
                  mal)))

(defun ns-framecontrol-get-current-mal (fc mal)
  (nth (cdr (ns-framecontrol-get-gi fc mal)) mal))

(defun ns-framecontrol-get-current-monitor-frames (fc mal)
  (cdr (assq 'frames (ns-framecontrol-get-current-mal
                      fc mal))))

(defun ns-framecontrol-get-current-monitor-interesting-lines (fc mal)
  (let* ((g (car (ns-framecontrol-get-gi fc mal)))
         (xs (list (first g) (third g)))
         (ys (list (second g) (fourth g))))
    (dolist (frame (ns-framecontrol-get-current-monitor-frames fc mal))
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
     (list (first g) (second g)
           (- (third g) (third r))
           (- (fourth g) (fourth r)))
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
         (max-num-lines (- (floor (/ (fourth g) 14.3)) 1))) ;; line-height?
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
        ((<= x 160) 0)
        (t (- x 160))))

(defun ns-framecontrol-nudge-towards (a b)
  (+ b (ns-framecontrol-nudge-fn (- a b))))

(defun ns-framecontrol-nudge-frame-in-direction (frame direction)
  (let* ((mal (display-monitor-attributes-list))
         (xs-and-ys (ns-framecontrol-get-current-monitor-interesting-lines
                     (ns-framecontrol-get-frame-coord frame) mal))
         (grbi (ns-framecontrol-get-grbi frame))
         (r (second grbi))
         (bounds (third grbi)))
    (modify-frame-parameters
     frame
     (case direction
       (:left `((left . ,(ns-framecontrol-nudge-towards
                          (first r)
                          (or
                           (car (last (remove-if-not
                                       (lambda (x)
                                         (<= (first bounds) x (- (first r) 1)))
                                       (first xs-and-ys))))
                           (first bounds))))))
       (:right `((left . ,(ns-framecontrol-nudge-towards
                           (first r)
                           (or
                            (first (remove-if-not
                                    (lambda (x)
                                      (<= (+ (first r) 1) x (third bounds)))
                                    (first xs-and-ys)))
                            (third bounds))))))
       (:up `((top . ,(ns-framecontrol-nudge-towards
                       (second r)
                       (or
                        (first (remove-if-not
                                (lambda (y)
                                  (<= (second bounds) y (- (second r) 1)))
                                (second xs-and-ys)))
                        (second bounds))))))
       (:down `((top . ,(ns-framecontrol-nudge-towards
                         (second r)
                         (or
                          (first (remove-if-not
                                  (lambda (y)
                                    (<= (+ (second r) 1) y (fourth bounds)))
                                  (second xs-and-ys)))
                          (fourth bounds))))))))))

(defun ns-framecontrol-nudger (dir)
  #'(lambda (&optional frame)
      (interactive)
      (ns-framecontrol-nudge-frame-in-direction frame dir)))

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

;; TODO: Implement some select-by-direction commands.

(provide 'ns-framecontrol)
