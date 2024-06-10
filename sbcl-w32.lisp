;; sbcl-w32.lisp
;; Copyright (C) 2024  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

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

;; (defconst _IOFBF #x00)
;; (defconst _IONBF #x04)
;; (defconst _IOLBF #x40)

(defconst ENABLE_PROCESSED_INPUT              #x0001)
(defconst ENABLE_LINE_INPUT                   #x0002)
(defconst ENABLE_ECHO_INPUT                   #x0004)
(defconst ENABLE_WINDOW_INPUT                 #x0008)
(defconst ENABLE_MOUSE_INPUT                  #x0010)
(defconst ENABLE_INSERT_MODE                  #x0020)
(defconst ENABLE_QUICK_EDIT_MODE              #x0040)
(defconst ENABLE_EXTENDED_FLAGS               #x0080)

(defconst ENABLE_AUTO_POSITION                #x0100)
(defconst ENABLE_VIRTUAL_TERMINAL_INPUT       #x0200)

(defconst ENABLE_PROCESSED_OUTPUT             #x0001)
(defconst ENABLE_WRAP_AT_EOL_OUTPUT           #x0002)
(defconst ENABLE_VIRTUAL_TERMINAL_PROCESSING  #x0004)
(defconst DISABLE_NEWLINE_AUTO_RETURN         #x0008)
(defconst ENABLE_LVB_GRID_WORLDWIDE           #x0010)

;; (sb-alien:define-alien-routine ("setvbuf" setvbuf) int
;;   (file handle)
;;   (buffer (* t))
;;   (mode unix-file-mode)
;;   (size size-t))

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

;;(defstruct utsname sysname nodename release version machine)
;; This has to be replaced with something better
(defun uname ()
  "First-aid system info to get things going on portably."
  ;; (values
  ;;  (format "~a" (sb-posix:getenv "OS"))
  ;;  (get-computer-name)
  ;;  (multiple-value-bind
  ;;        (maj min rev anon sp) (get-version-x)
  ;;    (declare (ignore anon sp))
  ;;    (format "~a.~a.~a" maj min rev)
  ;;    (format "~a.~a.~a" maj min rev))
  ;;  )
  )

(dolist (sym '('coord 'smallrect '+std-input-handle+ '+std-output-handle+
               '+std-error-handle+ 'get-console-screen-buffer-info 'uname
               'get-std-handle 'setvbuf 'get-console-mode 'set-console-mode))
       (export sym :sb-win32))

;;; sbcl-w32.lisp ends here
