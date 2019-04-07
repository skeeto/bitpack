;;; bitpack-tests.el -- tests for bitpack -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'bitpack)

(ert-deftest bitpack-f64 ()
  (dolist (byte-order '(:> :<))
    ;; Positive NaN
    (with-temp-buffer
      (bitpack-store-f64 byte-order +0.0e+NaN)
      (setf (point) (point-min))
      (let ((result (bitpack-load-f64 byte-order)))
        (should (isnan result))
        (should (> (copysign 1.0 result) 0))))
    ;; Negative NaN
    (with-temp-buffer
      (bitpack-store-f64 byte-order -0.0e+NaN)
      (setf (point) (point-min))
      (let ((result (bitpack-load-f64 byte-order)))
        (should (isnan result))
        (should (< (copysign 1.0 result) 0))))
    ;; Positive Infinity
    (with-temp-buffer
      (bitpack-store-f64 byte-order +1.0e+INF)
      (setf (point) (point-min))
      (let ((result (bitpack-load-f64 byte-order)))
        (should (= 1.0e+INF result))))
    ;; Negative Infinity
    (with-temp-buffer
      (bitpack-store-f64 byte-order -1.0e+INF)
      (setf (point) (point-min))
      (let ((result (bitpack-load-f64 byte-order)))
        (should (= -1.0e+INF result)))))
  ;; Check specific known value
  (with-temp-buffer
    (bitpack-store-f64 :> pi)
    (should (equal (buffer-string)
                   (string #x40 #x09 #x21 #xfb #x54 #x44 #x2d #x18))))
  ;; Check many round trips
  (let ((floats (list pi)))
    (dotimes (_ 10000)
      (push (cl-random 1.0) floats))
    (dotimes (_ 1000)
      (push (cl-random 1000.0) floats))
    (dotimes (_ 1000)
      (push (cl-random 1000000.0) floats))
    (dotimes (_ 10000)
      (push (- (cl-random 1.0)) floats))
    (dotimes (_ 1000)
      (push (- (cl-random 1000.0)) floats))
    (dotimes (_ 1000)
      (push (- (cl-random 1000000.0)) floats))
    (dolist (byte-order '(:> :<))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (dolist (float floats)
          (bitpack-store-f64 byte-order float))
        (setf (point) (point-min))
        (dolist (float floats)
          (should (eql (bitpack-load-f64 byte-order) float)))))))

(ert-deftest bitpack-f32 ()
  (dolist (byte-order '(:> :<))
    ;; Positive NaN
    (with-temp-buffer
      (bitpack-store-f32 byte-order +0.0e+NaN)
      (setf (point) (point-min))
      (let ((result (bitpack-load-f32 byte-order)))
        (should (isnan result))
        (should (> (copysign 1.0 result) 0))))
    ;; Negative NaN
    (with-temp-buffer
      (bitpack-store-f32 byte-order -0.0e+NaN)
      (setf (point) (point-min))
      (let ((result (bitpack-load-f32 byte-order)))
        (should (isnan result))
        (should (< (copysign 1.0 result) 0))))
    ;; Positive Infinity
    (with-temp-buffer
      (bitpack-store-f32 byte-order +1.0e+INF)
      (setf (point) (point-min))
      (let ((result (bitpack-load-f32 byte-order)))
        (should (= 1.0e+INF result))))
    ;; Negative Infinity
    (with-temp-buffer
      (bitpack-store-f32 byte-order -1.0e+INF)
      (setf (point) (point-min))
      (let ((result (bitpack-load-f32 byte-order)))
        (should (= -1.0e+INF result)))))
  ;; Check specific known value
  (with-temp-buffer
    (bitpack-store-f32 :> pi)
    (should (equal (buffer-string) (string #x40 #x49 #x0f #xdb))))
  ;; Check integral values
  (let ((floats ()))
    (dotimes (_ 1000)
      (push (float (cl-random 1000000)) floats))
    (dotimes (_ 1000)
      (push (- (float (cl-random 1000000))) floats))
    (dolist (byte-order '(:> :<))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (dolist (float floats)
          (bitpack-store-f32 byte-order float))
        (setf (point) (point-min))
        (dolist (float floats)
          (should (eql (bitpack-load-f32 byte-order) float)))))))

(ert-deftest bitpack-i64()
  ;; Check unrepresentable integer
  (with-temp-buffer
    (save-excursion
      (insert #x80 0 0 0 0 0 0 0))
    (should-error (bitpack-load-u64 :>)
                  :type 'arith-error))
  ;; Check that u64 doesn't return negative
  (with-temp-buffer
    (save-excursion
      (bitpack-store-i64 :> -1))
    (should-error (bitpack-load-u64 :>)
                  :type 'arith-error))
  ;; Check that s64 still works
  (with-temp-buffer
    (save-excursion
      (bitpack-store-i64 :> -1))
    (should (eql (bitpack-load-s64 :>) -1))))

(defun bitpack-benchmark ()
  (princ
   (format "bitpack-store-f64 %S\n"
           (benchmark-run 500
             (with-temp-buffer
               (dotimes (_ 1000)
                 (bitpack-store-f64 :> pi))))))
  (princ
   (format "bitpack-load-f64 %S\n"
           (with-temp-buffer
             (dotimes (_ 1000)
               (bitpack-store-f64 :> (cl-random 1000.0)))
             (benchmark-run 2500
               (setf (point) (point-min))
               (while (< (point) (point-max))
                 (bitpack-load-f64 :>)))))))

(provide 'bitpack-tests)

;;; bitpack-tests.el ends here
