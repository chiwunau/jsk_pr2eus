;; url-encode.l
;; Author: Yuki Furuta <furushchev@jsk.imi.i.u-tokyo.ac.jp>

(defvar +extra-unreserved-characters+ "-_.~:/=?&")

(defun unreserved-char-p (c)
  (or (<= #\A c #\Z)
      (<= #\a c #\z)
      (<= #\0 c #\9)
      (find c +extra-unreserved-characters+ :test #'lisp::char=)))

(defun zero-padding (s str-len)
  (if (> str-len (length s))
      (dotimes (i (- str-len (length s)))
        (setq s (format nil "0~A" s))))
  s)

(defun urlencode (str &optional (ss *standard-output*) (queryp t))
    (dolist (c (coerce str cons))
      (cond
       ((unreserved-char-p c)
        (write-byte c ss))
       ((lisp::char= c #\Return)
        (write-byte #\Newline ss))
       ((and queryp (lisp::char= c #\Space))
        (write-byte #\+ ss))
       (t
        (format ss "%~A" (zero-padding
                          (format nil "~X" c) 2))))))

(defun encoded-url-string (url-string)
  (with-output-to-string (s)
   (urlencode url-string s)
   (get-output-stream-string s)))

(provide :url-encode) ;; end of url-encode.l
