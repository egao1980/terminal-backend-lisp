(in-package #:terminal-backend-lisp)

;;; The terminal object and the helpers every handler shares: active screen,
;;; pen, erase blank, replies, index / reverse index, scrollback, viewport
;;; mapping and full reset.

(defstruct parser
  (state :ground :type keyword)
  (params (make-array 8 :adjustable t :fill-pointer 0)) ; of lists (p sub…); NIL = empty
  (current '() :type list)          ; finished subparams of the param being read, reversed
  (digits nil :type (or null fixnum))
  (seen nil :type boolean)          ; any parameter byte since CSI entry
  (private nil)                     ; #\? #\> #\< #\= or NIL
  (intermediates '() :type list)    ; characters 0x20-0x2F, reversed
  (osc (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (osc-overflow nil :type boolean)
  (utf8-code 0 :type fixnum)
  (utf8-need 0 :type fixnum)
  (utf8-min 0 :type fixnum))

(defclass lisp-terminal (tp:terminal)
  ((cols :initarg :cols :accessor %cols)
   (rows :initarg :rows :accessor %rows)
   (scrollback-lines :initarg :scrollback-lines :reader %scrollback-lines)
   (primary :initform nil :accessor %primary)
   (alternate :initform nil :accessor %alternate)
   (active :initform :primary :accessor %active-name)  ; :primary | :alternate
   (scrollback :initform nil :accessor %scrollback)     ; ring of lines (primary only)
   (viewport-offset :initform 0 :accessor %offset)     ; rows scrolled back from the bottom
   (dec-modes :initform (make-hash-table) :accessor %dec-modes)
   (ansi-modes :initform (make-hash-table) :accessor %ansi-modes)
   (title :initform nil :accessor %title)
   (pwd :initform nil :accessor %pwd)
   (hyperlink :initform nil :accessor %hyperlink)      ; OSC 8 URI or NIL
   (cursor-style :initform :block :accessor %cursor-style)
   (initial-palette :initarg :palette :reader %initial-palette)
   (palette :initform nil :accessor %palette)          ; (simple-vector 256) of tp:rgb
   (foreground :initarg :foreground :accessor %foreground)
   (background :initarg :background :accessor %background)
   (cursor-color :initarg :cursor-color :initform nil :accessor %cursor-color)
   (epoch :initform 0 :accessor %epoch)                ; bumped ⇒ frames go :full
   (last-char :initform nil :accessor %last-char)      ; for REP
   (style-memo :initform (cons nil nil) :accessor %style-memo)
   (parser :initform (make-parser) :accessor %parser))
  (:documentation "A headless VT emulator written in portable Common Lisp."))

(defmethod initialize-instance :after ((term lisp-terminal) &key)
  (%full-reset term))

(declaim (inline %screen))
(defun %screen (term)
  (if (eq (%active-name term) :primary) (%primary term) (%alternate term)))

(defun %bump-epoch (term)
  (setf (%epoch term) (logand (1+ (%epoch term)) most-positive-fixnum)))

;;; ------------------------------------------------------------------
;;; Modes (storage only; side effects live in modes.lisp)

(defun %mode-table (term kind)
  (ecase kind (:dec (%dec-modes term)) (:ansi (%ansi-modes term))))

(defun %mode (term kind number)
  (values (gethash number (%mode-table term kind))))

(defun %known-mode-p (kind number)
  (find-if (lambda (entry) (and (eq (second entry) kind) (eql (third entry) number)))
           tp:+mode-names+))

(defun %default-modes (term)
  (clrhash (%dec-modes term))
  (clrhash (%ansi-modes term))
  (setf (gethash 7 (%dec-modes term)) t
        (gethash 25 (%dec-modes term)) t))

;;; ------------------------------------------------------------------
;;; Pen, blanks, replies

(defun %blank (term)
  "Erase / insert fill: text \"\", :NARROW, carrying only the pen's background."
  (let ((bg (tp:style-background (screen-pen (%screen term)))))
    (if (eq bg :default)
        +blank-cell+
        (tp:make-cell :style (tp:make-style :background bg)))))

(defun %cell-style (term)
  "The pen with the current OSC 8 hyperlink merged in. Memoised so a run of
characters shares one style object."
  (let ((pen (screen-pen (%screen term)))
        (link (%hyperlink term)))
    (if (null link)
        pen
        (let ((memo (%style-memo term)))
          (if (and (eq (car memo) pen) (cdr memo)
                   (eq (tp:style-hyperlink (cdr memo)) link))
              (cdr memo)
              (let ((style (tp:copy-style pen)))
                (setf (tp:style-hyperlink style) link)
                (setf (%style-memo term) (cons pen style))
                style))))))

(defun %reply (term string)
  "Send STRING (ASCII) back to the program."
  (tp:call-host term #'tp:host-write-pty (%ascii-octets string)))

;;; ------------------------------------------------------------------
;;; Index, reverse index, scrollback

(defun %scrollback-active-p (term)
  (and (eq (%active-name term) :primary) (%scrollback term)))

(defun %push-scrollback (term lines)
  (let ((ring (%scrollback term)))
    (dolist (line lines)
      (let ((evicted (ring-push ring line)))
        ;; A scrolled-back viewport stays pinned to the content it shows.
        (when (and (plusp (%offset term)) (not evicted))
          (incf (%offset term)))))
    (setf (%offset term) (min (%offset term) (ring-count ring)))))

(defun %scroll-region-up (term n)
  "Scroll the active region up N lines. The full-screen region on the
primary screen feeds scrollback."
  (let* ((s (%screen term))
         (top (screen-top s)) (bottom (screen-bottom s))
         (gone (%scroll-up s top bottom n (%blank term))))
    (when (and (%scrollback-active-p term)
               (zerop top) (= bottom (1- (screen-rows s))))
      (%push-scrollback term gone))))

(defun %index (term)
  "IND / LF: move down, scrolling at the bottom margin."
  (let ((s (%screen term)))
    (setf (screen-pending-wrap s) nil)
    (cond ((= (screen-y s) (screen-bottom s)) (%scroll-region-up term 1))
          ((< (screen-y s) (1- (screen-rows s))) (incf (screen-y s))))))

(defun %reverse-index (term)
  "RI: move up, scrolling down at the top margin."
  (let ((s (%screen term)))
    (setf (screen-pending-wrap s) nil)
    (cond ((= (screen-y s) (screen-top s))
           (%scroll-down s (screen-top s) (screen-bottom s) 1 (%blank term)))
          ((plusp (screen-y s)) (decf (screen-y s))))))

;;; ------------------------------------------------------------------
;;; Viewport

(defun %viewport-offset (term)
  (if (eq (%active-name term) :primary) (%offset term) 0))

(defun %history-count (term)
  (if (eq (%active-name term) :primary) (ring-count (%scrollback term)) 0))

(defun %viewport-line (term y)
  "The line shown at viewport row Y."
  (let* ((n (%history-count term))
         (abs (+ (- n (%viewport-offset term)) y)))
    (if (< abs n)
        (ring-ref (%scrollback term) abs)
        (%line-at (%screen term) (- abs n)))))

;;; ------------------------------------------------------------------
;;; Full reset (RIS and TERMINAL-RESET)

(defun %full-reset (term)
  (let ((cols (%cols term)) (rows (%rows term)))
    (setf (%primary term) (make-screen cols rows)
          (%alternate term) (make-screen cols rows)
          (%active-name term) :primary
          (%scrollback term) (make-ring (%scrollback-lines term))
          (%offset term) 0
          (%title term) nil
          (%pwd term) nil
          (%hyperlink term) nil
          (%cursor-style term) :block
          (%palette term) (copy-seq (%initial-palette term))
          (%last-char term) nil
          (%style-memo term) (cons nil nil))
    (%default-modes term)
    (%bump-epoch term)
    term))
