(in-package #:terminal-backend-lisp/tests)

;;; Kitty graphics. The parity corpus checks the details against the
;;; libghostty-vt oracle; these pin the protocol surface.

(deftest kitty-graphics-claimed
  (ok (tp:backend-supports-p (make-instance 'tl:lisp-backend) :kitty-graphics)))

(deftest kitty-transmit-place-read-delete
  (with-lisp (term :cols 10 :rows 3)
    (tp:terminal-resize term 10 3 :cell-width-px 10 :cell-height-px 20)
    (tp:terminal-feed term (octets "{esc}_Ga=T,i=7,f=24,s=2,v=1,c=2,r=1,q=2;/wAAAP8A{st}"))
    (tp:with-frame (frame term)
      (let ((p (first (tp:frame-images frame))))
        (ok (typep p 'tp:image-placement))
        (ok (= 7 (tp:image-placement-image-id p)))
        (ok (equal (list (tp:image-placement-cols p) (tp:image-placement-rows p)) '(2 1)))))
    (let ((image (tp:terminal-image term 7)))
      (ok (equal (list (tp:terminal-image-width image) (tp:terminal-image-height image)
                       (tp:terminal-image-format image))
                 '(2 1 :rgb)))
      (ok (equalp (tp:terminal-image-pixels image) #(255 0 0 0 255 0))))
    (ok (null (tp:terminal-image term 99)))
    (tp:terminal-feed term (octets "{esc}_Ga=d,d=I,i=7,q=2{st}"))
    (tp:with-frame (frame term)
      (ok (null (tp:frame-images frame))))
    ;; d=I (upper case) also frees the image data.
    (ok (null (tp:terminal-image term 7)))))

(deftest kitty-images-are-per-terminal-and-reset
  (with-lisp (term)
    (tp:terminal-feed term (octets "{esc}_Ga=t,i=1,f=24,s=1,v=1,q=2;AAAA{st}"))
    (ok (tp:terminal-image term 1))
    (tp:terminal-reset term)
    (ok (null (tp:terminal-image term 1)))))
