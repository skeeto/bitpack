# bitpack: an Emacs Lisp structure packing library

`bitpack` is similar to the built-in `bindat` package. However, this
package can encode IEEE 754 floating point values, both single
(32-bit) and double precision (64-bit).

In Emacs 26.1 on modern x86-64 hardware, it stores ~290k double
precision floats per second, and loads them at ~1.3M per second.

Requires either:

* Emacs 27 (any)
* Emacs 24.3 through 26.x (64-bit or `--with-wide-int` 32-bit)

Use `make check` to verify that your particular Emacs build works
correctly with this package.

## API

Rather than return and accept unibyte strings, each function operates
on the current buffer. This buffer must not be mulibyte (i.e.
`set-buffer-multibyte` to nil).

The `byte-order` argument can be `:>` (big endian) or `:<` little
endian.

```el
;; Store floats
(bitpack-store-f32 byte-order x)
(bitpack-store-f64 byte-order x)

;; Store integers
(bitpack-store-i8 x)
(bitpack-store-i16 byte-order x)
(bitpack-store-i32 byte-order x)
(bitpack-store-i64 byte-order x)

;; Load floats
(bitpack-load-f32 byte-order)
(bitpack-load-f64 byte-order)

;; Load integers
(bitpack-load-u8)
(bitpack-load-s8)
(bitpack-load-u16 byte-order)
(bitpack-load-s16 byte-order)
(bitpack-load-u32 byte-order)
(bitpack-load-s32 byte-order)
(bitpack-load-u64 byte-order)
(bitpack-load-s64 byte-order)
```

When writing values, the sign doesn't matter. When reading values, the
sign determines how the value is interpreted.

## Example

Packing and unpacking floating a point value:

```el
(with-temp-buffer
  (set-buffer-multibyte nil)
  (bitpack-store-f64 :> pi)
  (buffer-string))
;; => "\x40\x09\x21\xfb\x54\x44\x2d\x18"

(with-temp-buffer
  (set-buffer-multibyte nil)
  (save-excursion
    (insert #x40 #x09 #x21 #xfb #x54 #x44 #x2d #x18))
  (bitpack-load-f64 :>))
;; => 3.141592653589793
```

This writes a `middle-c.wav` file with three seconds of a middle C tone:

```el
(with-temp-file "middle-c.wav"
  (set-buffer-multibyte nil)
  (let ((hz 44100)
        (seconds 3)
        (freq 261.6)) ;; middle C
    (insert "RIFF")
    (bitpack-store-i32 :< -1)       ; file length
    (insert "WAVE")
    (insert "fmt ")
    (bitpack-store-i32 :< 16)       ; struct size
    (bitpack-store-i16 :< 1)        ; PCM
    (bitpack-store-i16 :< 1)        ; mono
    (bitpack-store-i32 :< hz)       ; sample rate (i.e. 44.1 kHz)
    (bitpack-store-i32 :< (* 2 hz)) ; byte rate
    (bitpack-store-i16 :< 2)        ; block size
    (bitpack-store-i16 :< 16)       ; bits per sample
    (insert "data")
    (bitpack-store-i32 :< -1)       ; byte length
    (dotimes (i (* seconds hz))
      (let* ((time (/ i (float hz)))
             (value (sin (* time freq 2.0 pi)))
             (sample (* 32767 value)))
        (bitpack-store-i16 :< (round sample))))))
```
