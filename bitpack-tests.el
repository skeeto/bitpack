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
    (bitpack-store-f64 :> float-pi)
    (should (equal (buffer-string)
                   (string #x40 #x09 #x21 #xfb #x54 #x44 #x2d #x18))))
  ;; Check many round trips
  (let ((floats (list float-pi)))
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
    (bitpack-store-f32 :> float-pi)
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

(defun random-integers (min max n)
  "Return N random integers between MIN (inclusive) and MAX (exclusive)."
  (let ((list ()))
    (dotimes (_ n)
      (push (+ (cl-random (- max min)) min) list))
    list))

(defun drive (encode decode min max n &optional dir)
  (let ((list (random-integers min max n)))
    (if dir
        (with-temp-buffer
          (save-excursion
            (dolist (x list)
              (funcall encode dir x)))
          (dolist (x list)
            (should (eql (funcall decode dir) x))))
      (with-temp-buffer
        (save-excursion
          (dolist (x list)
            (funcall encode x)))
        (dolist (x list)
          (should (eql (funcall decode) x)))))))

(ert-deftest bitpack-u8 ()
  (drive #'bitpack-store-i8 #'bitpack-load-u8 0 #x100 200))

(ert-deftest bitpack-s8 ()
  (drive #'bitpack-store-i8 #'bitpack-load-s8 (- #x80) #x80 200))

(ert-deftest bitpack-u16 ()
  (let ((min 0)
        (max #x10000)
        (n   10000))
    (drive #'bitpack-store-i16 #'bitpack-load-u16 min max n :>)
    (drive #'bitpack-store-i16 #'bitpack-load-u16 min max n :<)))

(ert-deftest bitpack-s16 ()
  (let ((min (- #x8000))
        (max #x8000)
        (n   10000))
    (drive #'bitpack-store-i16 #'bitpack-load-s16 min max n :>)
    (drive #'bitpack-store-i16 #'bitpack-load-s16 min max n :<)))

(ert-deftest bitpack-u32 ()
  (let ((min 0)
        (max #x100000000)
        (n   10000))
    (drive #'bitpack-store-i32 #'bitpack-load-u32 min max n :>)
    (drive #'bitpack-store-i32 #'bitpack-load-u32 min max n :<)))

(ert-deftest bitpack-s32 ()
  (let ((min (- #x80000000))
        (max #x80000000)
        (n   10000))
    (drive #'bitpack-store-i32 #'bitpack-load-s32 min max n :>)
    (drive #'bitpack-store-i32 #'bitpack-load-s32 min max n :<)))

(ert-deftest bitpack-u64 ()
  (let ((min 0)
        (max most-positive-fixnum)
        (n   10000))
    (drive #'bitpack-store-i64 #'bitpack-load-u64 min max n :>)
    (drive #'bitpack-store-i64 #'bitpack-load-u64 min max n :<)))

(ert-deftest bitpack-s64 ()
  (let ((min most-negative-fixnum)
        (max most-positive-fixnum)
        (n   10000))
    (drive #'bitpack-store-i64 #'bitpack-load-s64 min max n :>)
    (drive #'bitpack-store-i64 #'bitpack-load-s64 min max n :<)))

(defun single-64 (decode dir x)
  (with-temp-buffer
    (save-excursion
      (bitpack-store-i64 dir x))
    (eql (funcall decode dir) x)))

(ert-deftest bitpack-64-validate ()
  (should (single-64 #'bitpack-load-u64 :> most-positive-fixnum))
  (should (single-64 #'bitpack-load-u64 :< most-positive-fixnum))
  (should (single-64 #'bitpack-load-s64 :> most-negative-fixnum))
  (should (single-64 #'bitpack-load-s64 :< most-negative-fixnum))
  (should (single-64 #'bitpack-load-s64 :> -1))
  (should (single-64 #'bitpack-load-s64 :< -1))
  (unless (fboundp 'bignump)
    (should-error (single-64 #'bitpack-load-u64 :< -1))
    (should-error (single-64 #'bitpack-load-u64 :< most-negative-fixnum))
    (with-temp-buffer
      (insert #x80 0 0 0 0 0 0 0)
      (should-error (bitpack-load-s64 :>)))))

(defun bitpack-benchmark ()
  (princ
   (format "bitpack-store-f64 %S\n"
           (benchmark-run 500
             (with-temp-buffer
               (dotimes (_ 1000)
                 (bitpack-store-f64 :> float-pi))))))
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
