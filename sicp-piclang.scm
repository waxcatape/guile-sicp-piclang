(define-module (sicp-piclang)
  #:export (segments->painter
            segments->shape
            transform-painter
            make-vect
            xcor-vect
            ycor-vect
            add-vect
            sub-vect
            scale-vect
            make-segment
            start-segment
            end-segment
            make-frame
            origin-frame
            edge1-frame
            edge2-frame
            paint-lines
            wave
            flip-vert
            flip-horiz
            below
            beside
            up-split
            right-split
            corner-split
            square-limit))

(use-modules (pict))

(define (make-vect x y)
  ;; a vector is a list of two coordinates. An alternate representation would be a cons pair.
  (list x y))

(define xcor-vect car)

(define ycor-vect cadr)

(define (add-vect v w)
  (list (+ (xcor-vect v) (xcor-vect w))
        (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
  (list (- (xcor-vect v) (xcor-vect w))
        (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect s v)
  (list (* s (xcor-vect v))
        (* s (ycor-vect v))))

(define (make-frame origin edge1 edge2)
  ;; a frame is a list of three vectors
  (list origin edge1 edge2))

(define origin-frame car)

(define edge1-frame cadr)

(define edge2-frame caddr)

(define (make-segment start end)
  ;; a segment is a list of two vectors
  (list start end))

(define start-segment car)

(define end-segment cadr)

(define segments->shape list)

(define superimpose-shapes append)

(define (frame-coord-map frame)
  ;; Returns a function for adjusting a frame by a vector 
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect 
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))

(define (draw-line start end)
  ;; take two vectors, returns a line SVG object for pict
  (line (xcor-vect start)
        (ycor-vect start)
        (xcor-vect end)
        (ycor-vect end)))

(define (segments->painter segment-list)
  ;; takes a list of segments, returns a "painter" lambda, which applies a frame
  ;; to those segments, and then maps over the result with draw-line to make a
  ;; list of SVG line objects which pict can combine.
  (lambda (frame)
    (map
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))


;; NOTE: in the text, draw-line is a function which triggers an action in
;; some graphics driver, and returns nothing. Because of this, (map) was
;; originally (for-each). Thus the final result would have been thrown away.

(define (transform-painter painter origin corner1 corner2)
  ;; produces a new painter by applying the supplied painter to an affine transformation specified by the
  ;; three argument vectors. Is it possible to merge frame-coord-map and transform-painter somehow?
  (lambda (frame)
    (let* ((m (frame-coord-map frame))
           (new-origin (m origin)))
      (painter (make-frame new-origin
                           (sub-vect (m corner1) new-origin)
                           (sub-vect (m corner2) new-origin))))))

(define (paint-lines painter)
  ;; use pict to compile an SVG with the elements described by painter
  (let ((Frame (make-frame (make-vect 0 0)
                           (make-vect 500 0)
                           (make-vect 0 500))))
    (apply lt-superimpose
           (painter Frame))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)  ;; new origin
                     (make-vect 1.0 1.0)  ;; new end of edge1
                     (make-vect 0.0 0.0)));; new end of edge2

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (beside painter1 painter2)
  (let* ((split-point (make-vect 0.5 0.0))
          (paint-left  (transform-painter
                        painter1
                        (make-vect 0.0 0.0)
                        split-point
                        (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))))
    (lambda (frame)
      (superimpose-shapes ;; <- the key change
       (paint-left frame)
       (paint-right frame)))))

(define (below painter1 painter2)
  (let* ((split-point (make-vect 0.0 0.5))
         (paint-below (transform-painter painter1
                                         split-point
                                         (make-vect 1.0 0.5)
                                         (make-vect 0.0 1.0)))
         (paint-above (transform-painter painter2
                                         (make-vect 0.0 0.0)
                                         (make-vect 1.0 0.0)
                                         split-point)))
    (lambda (frame)
      (superimpose-shapes
       (paint-above frame)
       (paint-below frame)))))


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let* ((up (up-split painter (- n 1)))
             (right (right-split painter (- n 1)))
             (top-left (beside up up))
             (bottom-right (below right right))
             (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner)))))

(define (square-limit painter n)
  (let* ((quarter (corner-split painter n))
         (half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half)))

;; wave starts here

(define legs
  (segments->shape (make-segment (make-vect 0.35 0.65)
                                 (make-vect 0.25 1))
                   (make-segment (make-vect 0.65 0.65)
                                 (make-vect 0.75 1))
                   (make-segment (make-vect 0.3 1)
                                 (make-vect 0.5 0.7))
                   (make-segment (make-vect 0.7 1)
                                 (make-vect 0.5 0.7))))

(define left-arm
  (segments->shape (make-segment (make-vect 0.45 0.2)
                                 (make-vect 0.35 0.15))
                   (make-segment (make-vect 0.35 0.15)
                                 (make-vect 0.15 0.3))
                   (make-segment (make-vect 0.15 0.3)
                                 (make-vect 0 0.15))
                   (make-segment (make-vect 0.4 0.3)
                                 (make-vect 0.15 0.45))
                   (make-segment (make-vect 0.15 0.45)
                                 (make-vect 0 0.2))))

(define right-arm
  (segments->shape (make-segment (make-vect 0.55 0.2)
                                 (make-vect 0.65 0.15))
                   (make-segment (make-vect 0.65 0.15)
                                 (make-vect 1 0.55))
                   (make-segment (make-vect 0.6 0.3)
                                 (make-vect 1 0.65))))

(define torso
  (segments->shape (make-segment (make-vect 0.4 0.3)
                                 (make-vect 0.35 0.65))
                   (make-segment (make-vect 0.6 0.3)
                                 (make-vect 0.65 0.65))))
(define head
  (segments->shape (make-segment (make-vect 0.45 0)
                                 (make-vect 0.4 0.1))
                   (make-segment (make-vect 0.55 0)
                                 (make-vect 0.6 0.1))
                   (make-segment (make-vect 0.4 0.1)
                                 (make-vect 0.45 0.2))
                   (make-segment (make-vect 0.6 0.1)
                                 (make-vect 0.55 0.2))))

(define wave
  (segments->painter (superimpose-shapes head torso legs right-arm left-arm)))
