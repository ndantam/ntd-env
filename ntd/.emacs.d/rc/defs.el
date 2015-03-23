;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

;;;;;;;;;;;;
;;  DEFS  ;;
;;;;;;;;;;;;

(defun all (args)
  (cond
   ((null args) t)
   ((car args) (all (cdr args)))
   (t nil)))

(defun any (args)
  (cond
   ((null args) nil)
   ((car args) t)
   (t (any (cdr args)))))

(defun host-match (name)
  (string-match (concat "^" (regexp-quote name) ".*")
                (system-name)))

(defmacro if-host (name then-clause &optional else-clause)
  `(if ,(if (atom name)
            `(host-match  ,name)
          `(any (mapcar 'host-match
                        (quote ,name))))
     ,then-clause
     ,else-clause))

(defmacro when-host (name &rest forms)
  (declare (indent 1))

  `(if-host ,name
            (progn ,@forms)))


(defmacro unless-host (name &rest forms)
  (declare (indent 1))
  `(if-host ,name nil (progn ,@forms)))
