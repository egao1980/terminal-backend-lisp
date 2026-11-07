(in-package #:terminal-backend-lisp)

;;; Display width of a code point: 0 (combining / zero-width), 1 or 2
;;; (East Asian Wide / Fullwidth and emoji presentation). Literal sorted range
;;; tables, hand-condensed from Unicode 15 EastAsianWidth.txt,
;;; emoji-data.txt (Emoji_Presentation) and the Mn/Me/Cf categories.

(defparameter +wide-ranges+
  (coerce
   '(#x1100 #x115F  #x231A #x231B  #x2329 #x232A  #x23E9 #x23EC  #x23F0 #x23F0
     #x23F3 #x23F3  #x25FD #x25FE  #x2614 #x2615  #x2648 #x2653  #x267F #x267F
     #x2693 #x2693  #x26A1 #x26A1  #x26AA #x26AB  #x26BD #x26BE  #x26C4 #x26C5
     #x26CE #x26CE  #x26D4 #x26D4  #x26EA #x26EA  #x26F2 #x26F3  #x26F5 #x26F5
     #x26FA #x26FA  #x26FD #x26FD  #x2705 #x2705  #x270A #x270B  #x2728 #x2728
     #x274C #x274C  #x274E #x274E  #x2753 #x2755  #x2757 #x2757  #x2795 #x2797
     #x27B0 #x27B0  #x27BF #x27BF  #x2B1B #x2B1C  #x2B50 #x2B50  #x2B55 #x2B55
     #x2E80 #x3029  #x302E #x303E  #x3041 #x3098  #x309B #x33FF  #x3400 #x4DBF
     #x4E00 #x9FFF  #xA000 #xA4CF  #xA960 #xA97F  #xAC00 #xD7A3  #xF900 #xFAFF
     #xFE10 #xFE19  #xFE30 #xFE6F  #xFF00 #xFF60  #xFFE0 #xFFE6
     #x16FE0 #x16FE4  #x17000 #x18CFF  #x1B000 #x1B2FF  #x1F004 #x1F004
     #x1F0CF #x1F0CF  #x1F18E #x1F18E  #x1F191 #x1F19A  #x1F200 #x1F202
     #x1F210 #x1F23B  #x1F240 #x1F248  #x1F250 #x1F251  #x1F260 #x1F265
     #x1F300 #x1F320  #x1F32D #x1F335  #x1F337 #x1F37C  #x1F37E #x1F393
     #x1F3A0 #x1F3CA  #x1F3CF #x1F3D3  #x1F3E0 #x1F3F0  #x1F3F4 #x1F3F4
     #x1F3F8 #x1F43E  #x1F440 #x1F440  #x1F442 #x1F4FC  #x1F4FF #x1F53D
     #x1F54B #x1F54E  #x1F550 #x1F567  #x1F57A #x1F57A  #x1F595 #x1F596
     #x1F5A4 #x1F5A4  #x1F5FB #x1F64F  #x1F680 #x1F6C5  #x1F6CC #x1F6CC
     #x1F6D0 #x1F6D2  #x1F6D5 #x1F6D7  #x1F6DC #x1F6DF  #x1F6EB #x1F6EC
     #x1F6F4 #x1F6FC  #x1F7E0 #x1F7EB  #x1F7F0 #x1F7F0  #x1F90C #x1F93A
     #x1F93C #x1F945  #x1F947 #x1F9FF  #x1FA70 #x1FAFF  #x20000 #x2FFFD
     #x30000 #x3FFFD)
   '(simple-array fixnum (*)))
  "Flat sorted (low high) pairs of width-2 code points.")

(defparameter +zero-width-ranges+
  (coerce
   '(#x0300 #x036F  #x0483 #x0489  #x0591 #x05BD  #x05BF #x05BF  #x05C1 #x05C2
     #x05C4 #x05C5  #x05C7 #x05C7  #x0610 #x061A  #x064B #x065F  #x0670 #x0670
     #x06D6 #x06DC  #x06DF #x06E4  #x06E7 #x06E8  #x06EA #x06ED  #x0711 #x0711
     #x0730 #x074A  #x07A6 #x07B0  #x07EB #x07F3  #x0816 #x082D  #x0859 #x085B
     #x08D3 #x0902  #x093A #x093A  #x093C #x093C  #x0941 #x0948  #x094D #x094D
     #x0951 #x0957  #x0962 #x0963  #x0981 #x0981  #x09BC #x09BC  #x09C1 #x09C4
     #x09CD #x09CD  #x0E31 #x0E31  #x0E34 #x0E3A  #x0E47 #x0E4E  #x0EB1 #x0EB1
     #x0EB4 #x0EBC  #x0EC8 #x0ECE  #x0F18 #x0F19  #x0F35 #x0F35  #x0F37 #x0F37
     #x0F39 #x0F39  #x0F71 #x0F7E  #x0F80 #x0F84  #x1160 #x11FF  #x135D #x135F
     #x1712 #x1714  #x17B4 #x17B5  #x17B7 #x17BD  #x17C6 #x17C6  #x17C9 #x17D3
     #x180B #x180F  #x1AB0 #x1AFF  #x1DC0 #x1DFF  #x200B #x200F  #x202A #x202E
     #x2060 #x2064  #x20D0 #x20FF  #x2CEF #x2CF1  #x2DE0 #x2DFF  #x302A #x302D
     #x3099 #x309A  #xA66F #xA672  #xA674 #xA67D  #xA69E #xA69F  #xA6F0 #xA6F1
     #xFB1E #xFB1E  #xFE00 #xFE0F  #xFE20 #xFE2F  #xFEFF #xFEFF
     #x1D167 #x1D169  #x1D17B #x1D182  #xE0001 #xE0001  #xE0020 #xE007F
     #xE0100 #xE01EF)
   '(simple-array fixnum (*)))
  "Flat sorted (low high) pairs of zero-width code points (combining marks,
format characters, variation selectors, Hangul medial/final jamo).")

(defun %in-ranges-p (code ranges)
  (declare (type fixnum code) (type (simple-array fixnum (*)) ranges))
  (let ((lo 0) (hi (1- (floor (length ranges) 2))))
    (declare (type fixnum lo hi))
    (loop while (<= lo hi)
          do (let ((mid (floor (+ lo hi) 2)))
               (cond ((< code (aref ranges (* 2 mid))) (setf hi (1- mid)))
                     ((> code (aref ranges (1+ (* 2 mid)))) (setf lo (1+ mid)))
                     (t (return-from %in-ranges-p t)))))
    nil))

(defun %char-width (code)
  "→ 0, 1 or 2 columns for code point CODE."
  (declare (type fixnum code))
  (cond ((< code #x300) 1)
        ((%in-ranges-p code +zero-width-ranges+) 0)
        ((< code #x1100) 1)
        ((%in-ranges-p code +wide-ranges+) 2)
        (t 1)))
