;; -*- lexical-binding: t -*-
;; package ns-frames

;; Questions/TODOs:
;; 1. List frames on the current workspace (Space).
;;
;; Notation:
;;   edges: (x-left y-top x-right y-bottom)
;;   rect: (x-left y-top width height)

(defun random-between (a b)
  (cond ((= a b) a)
        ((< a b) (+ a (random (- b a))))
        (t (+ b (random (- a b))))))

;; Getting 'workarea from monitor attributes seems bogus for the second monitor
;; and beyond.  Compute top menubar height from the first monitor, and use the
;; to compute workarea.
(defun ns-frames--workarea-edges (frame)
  (let* ((geometry-rect (frame-monitor-geometry frame))
         (first-monitor (first (display-monitor-attributes-list)))
         (top-menubar-height (- (fourth (cdr (assq 'geometry first-monitor)))
                                (fourth (cdr (assq 'workarea first-monitor))))))
    (list (first geometry-rect)
          (+ (second geometry-rect) top-menubar-height)
          (+ (first geometry-rect) (third geometry-rect))
          (+ (second geometry-rect) (fourth geometry-rect)))))

(defun ns-frames--bounds (frame)
  "Bounds rectangle (min-x, min-y, max-x, max-y).  When (x-left, y-top) of the
   frame is within this rectangle, the frame will lie within the workarea."
  (let* ((outer-edges (ns-frame-edges frame 'outer-edges))
         (workarea-edges (ns-frames--workarea-edges frame))
         (frame-width (- (third outer-edges) (first outer-edges)))
         (frame-height (- (fourth outer-edges) (second outer-edges))))
    (list
     (first workarea-edges)
     (second workarea-edges)
     (- (third workarea-edges) frame-width)
     (- (fourth workarea-edges) frame-height))))

(defun ns-frames-make-frame-scratch ()
  (interactive)
  (let* ((edges (ns-frames--workarea-edges nil))
         (width 585)
         (height 494))
    (display-buffer
     "*scratch*"
     `((display-buffer-pop-up-frame)
       (reusable-frames . 0)
       (inhibit-same-window . t)
       (pop-up-frame-parameters
        . ((left . ,(random-between (first edges) (- (third edges) width)))
           (top . ,(random-between (second edges) (- (fourth edges) height))))))
     nil)))

(defun ns-frames-make-frame-tmp ()
  (interactive)
  (switch-to-buffer-other-frame (get-buffer-create (make-temp-name "tmp "))))

(defun ns-frames-place-randomly ()
  (interactive)
  (let ((bounds (ns-frames--bounds (selected-frame))))
    (modify-frame-parameters
     frame
     `((left . ,(random-between (first bounds) (third bounds)))
       (top . ,(random-between (second bounds) (fourth bounds)))))))

(defun ns-frames-toggle-frame-vertical-expansion ()
  (interactive)
  (let* ((frame (selected-frame))
         (outer-edges (ns-frame-edges frame 'outer-edges))
         (workarea-edges (ns-frames--workarea-edges frame))
         (available-vertical-space (- (fourth workarea-edges)
                                      (second workarea-edges)))
         (p (frame-parameters frame))
         ;; 4.0 is perhaps the total height of the vertical box lines (around
         ;; modeline, etc.).
         (line-height (/ (- (frame-pixel-height) 4.0) (cdr (assq 'height p))))
         (max-height-lines (floor (/ available-vertical-space line-height))))
    (modify-frame-parameters
     frame
     (if (< 45 (cdr (assq 'height p)))
         `((height . ,(or (cdr (assq 'last-height p)) 35))
           (left . ,(first outer-edges))
           (top . ,(or (cdr (assq 'last-top p)) (second workarea-edges))))
       `((height . ,max-height-lines)
         (left . ,(first outer-edges))
         (top . ,(second workarea-edges))
         (last-height . ,(cdr (assq 'height p)))
         (last-top . ,(second outer-edges)))))))

(defun ns-frames--nudge (frame axis initial order)
  (let* ((best initial)
         (edges (ns-frame-edges frame 'outer-edges))
         (a0 (case axis (:x (first edges)) (:y (second edges))))
         (b0 (case axis (:x (third edges)) (:y (fourth edges)))))
    (dolist (frame (cdr (assq 'frames (frame-monitor-attributes frame))))
      (let* ((edges (ns-frame-edges frame 'outer-edges))
             (a (case axis (:x (first edges)) (:y (second edges))))
             (b (case axis (:x (third edges)) (:y (fourth edges)))))
        (dolist (edge (list (- a (- b0 a0)) a (- b (- b0 a0)) b))
          (when (funcall order best edge a0)
            (setq best edge)))))
    (unless (= a0 best)
      (modify-frame-parameters
       frame (list (cons (case axis (:x 'left) (:y 'top))
                         (ns-frames--nudge-towards
                          a0 best (case axis (:x 290) (:y 265)))))))))

(defun ns-frames--nudge-towards (a b threshold)
  (cond ((< b (- a threshold)) (- a threshold))
        ((> b (+ a threshold)) (+ a threshold))
        (t b)))

(defun ns-frames-nudge-frame-in-direction (frame direction)
  (let* ((frame (or frame (selected-frame)))
         (bounds (ns-frames--bounds frame)))
    (case direction
      (:left (ns-frames--nudge frame :x (first bounds) '<))
      (:right (ns-frames--nudge frame :x (third bounds) '>))
      (:up (ns-frames--nudge frame :y (second bounds) '<))
      (:down (ns-frames--nudge frame :y (fourth bounds) '>)))))

(defun ns-frames-nudger (dir)
  #'(lambda ()
      (interactive)
      (ns-frames-nudge-frame-in-direction (selected-frame) dir)))

(defun ns-frames--select-directional
    (current-frame param-fn initial order-fn all-frames)
  (let ((best (cons initial nil))
        (current-edges (ns-frame-edges current-frame 'outer-edges))
        (seen-current nil))
    ;; Assuming we are trying to find the next frame to the left.  If there are
    ;; no frames f with f.left = current-frame.left, find the largest f.left
    ;; with f.left < current-frame.left.  If there are some frame f with the
    ;; same left, find f immediately before current-frame in the list.
    (dolist (frame all-frames)
      (let ((value (funcall param-fn (ns-frame-edges frame 'outer-edges))))
        (cond ((eq frame current-frame)
               (setq seen-current t))
              ((funcall order-fn
                        (first best) value (funcall param-fn current-edges))
               (setq best (cons value frame)))
              ((and seen-current (= value (funcall param-fn current-edges)))
               (setq best (cons value frame))
               (return)))))
    (when (cdr best)
      (select-frame-set-input-focus (cdr best)))))

(defun ns-frames-select-frame-in-direction (frame direction)
  (let* ((frames (cdr (assq 'frames (frame-monitor-attributes frame)))))
    (case direction
      (:left (ns-frames--select-directional frame 'first -99999 '< frames))
      (:right (ns-frames--select-directional frame 'first 99999 '> (nreverse
                                                                    frames)))
      (:up (ns-frames--select-directional frame 'second -99999 '< frames))
      (:down (ns-frames--select-directional frame 'second 99999 '> (nreverse
                                                                    frames))))))

(defun ns-frames-directional-selector (dir)
  #'(lambda ()
      (interactive)
      (ns-frames-select-frame-in-direction (selected-frame) dir)))

(defun ns-frames-vertframe (n)
  (interactive "nNumber of vertical divisions: ")
  (make-frame)
  (set-frame-parameter nil 'width (- (* n 83) 3))
  (dotimes (i (- n 1))
    (split-window-right))
  (balance-windows)
  (ns-frames-toggle-frame-vertical-expansion))

;; TODO: Iterations should happen when the Command key is held down and ` is
;; typed repeatedly, not when Command-` is typed followed by more ` keys.
(defun ns-frames-select-mru ()
  (interactive)
  (lexical-let ((keymap (make-sparse-keymap))
                (frames (cdr (frame-list-z-order))))
    (when frames
      (select-frame-set-input-focus (pop frames))

      (define-key keymap (kbd "`")
        (lambda () (interactive)
          (when frames (select-frame-set-input-focus (pop frames)))))
      (set-transient-map keymap t))))

(global-set-key (kbd "s--") 'ns-frames-place-randomly)
(global-set-key (kbd "s-<left>")  (ns-frames-directional-selector :left))
(global-set-key (kbd "s-<right>") (ns-frames-directional-selector :right))
(global-set-key (kbd "s-<up>")    (ns-frames-directional-selector :up))
(global-set-key (kbd "s-<down>")  (ns-frames-directional-selector :down))
(global-set-key (kbd "s-S-<left>")  (ns-frames-nudger :left))
(global-set-key (kbd "s-S-<right>") (ns-frames-nudger :right))
(global-set-key (kbd "s-S-<up>")    (ns-frames-nudger :up))
(global-set-key (kbd "s-S-<down>")  (ns-frames-nudger :down))
(global-set-key (kbd "s-b") (ns-frames-directional-selector :left))
(global-set-key (kbd "s-f") (ns-frames-directional-selector :right))
(global-set-key (kbd "s-p") (ns-frames-directional-selector :up))
(global-set-key (kbd "s-n") (ns-frames-directional-selector :down))
(global-set-key (kbd "s-B") (ns-frames-nudger :left))
(global-set-key (kbd "s-F") (ns-frames-nudger :right))
(global-set-key (kbd "s-P") (ns-frames-nudger :up))
(global-set-key (kbd "s-N") (ns-frames-nudger :down))
(global-set-key (kbd "s-=") 'toggle-frame-maximized)
(global-set-key (kbd "s-\\") 'ns-frames-toggle-frame-vertical-expansion)
(global-set-key (kbd "s-t") 'ns-frames-make-frame-scratch)
(global-set-key (kbd "s-;") 'ns-frames-make-frame-scratch)
(global-set-key (kbd "s-`") 'ns-frames-select-mru)
(global-set-key (kbd "s-d") prefix-arg)
(global-set-key (kbd "s-d t") 'ns-frames-make-frame-tmp)

;; s-p was ns-print-buffer, s-t was the font thing

(provide 'ns-frames)
