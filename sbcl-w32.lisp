;; Copyright (c) 2024 Arthur Miller
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#-sbcl
(error "Attempt to load an SBCL-specific file in another implementation.")

(in-package :sb-win32)

(sb-alien:define-alien-type word (sb-alien:unsigned 16))
(sb-alien:define-alien-type unix-file-mode (sb-alien:unsigned 32))

(defmacro defconst (name value &optional doc)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defconstant ,name ,value ,doc))
     (export ',name)
     ',name))

(defconst ENABLE_PROCESSED_INPUT              #x0001)
(defconst ENABLE_LINE_INPUT                   #x0002)
(defconst ENABLE_ECHO_INPUT                   #x0004)
(defconst ENABLE_WINDOW_INPUT                 #x0008)
(defconst ENABLE_MOUSE_INPUT                  #x0010)
(defconst ENABLE_INSERT_MODE                  #x0020)
(defconst ENABLE_QUICK_EDIT_MODE              #x0040)
(defconst ENABLE_EXTENDED_FLAGS               #x0080)
(defconst ENABLE_VIRTUAL_TERMINAL_INPUT       #x0200)

(defun get-std-handle (handle)
  (get-std-handle-or-null handle))

  ;; https://learn.microsoft.com/en-us/windows/console/console-structures

(sb-alien:define-alien-type smallrect
    (sb-alien:struct smallrect
                     (left sb-alien:short)
                     (top sb-alien:short)
                     (right sb-alien:short)
                     (bottom sb-alien:short)))

(sb-alien:define-alien-type coord
    (sb-alien:struct coord
                     (x sb-alien:short)
                     (y sb-alien:short)))

(sb-alien:define-alien-type consolescreenbufferinfo
    (sb-alien:struct consolescreenbufferinfo
                     (dwSize coord)
                     (dwCursorPosition coord)
                     (wAttributes word)
                     (srWindow smallrect)
                     (srMaximumWindowSize smallrect)))

(defun get-console-screen-buffer-info (out)
  (sb-alien:with-alien ((info (sb-alien:struct consolescreenbufferinfo)))
    (sb-win32::syscall (("GetConsoleScreenBufferInfo") lispbool
                        sb-win32:handle (* (sb-alien:struct consolescreenbufferinfo)))
                       (values (sb-alien:slot info 'dwSize)
                               (sb-alien:slot info 'dwCursorPosition)
                               (sb-alien:slot info 'wAttributes)
                               (sb-alien:slot info 'srWindow)
                               (sb-alien:slot info 'srMaximumWindowSize))
                       out (sb-alien:addr info))))

(defun get-console-mode (console-handle)
  ;;(declare (type sb-win32:handle console-handle))
  (with-alien ((mode dword))
    (with-sysfun (afunc "GetConsoleMode" bool handle (* dword))
      (when (zerop (alien-funcall afunc console-handle (addr mode)))
        (let ((err (get-last-error)))
          (unless (= err 0)
            (win32-error "GetConsoleMode" err))
          (alien-funcall afunc console-handle (addr mode))))
      mode)))

(define-alien-routine ("SetConsoleMode" set-console-mode) bool
  (stream handle)
  (mode unix-file-mode))

(dolist (sym '('coord 'smallrect '+std-input-handle+ '+std-output-handle+
               '+std-error-handle+ 'get-console-screen-buffer-info 'uname
               'get-std-handle 'setvbuf 'get-console-mode 'set-console-mode))
       (export sym :sb-win32))

;;; sbcl-w32.lisp ends here
