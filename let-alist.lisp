;; Original copyright (C) 2014-2022 Free Software Foundation, Inc.

;; This is modified (translated from emacs lisp to Common Lisp) let-alist.el
;; file from GNU Emacs. As such, it is under GNU GPL.

(defpackage #:let-alist
  (:use #:cl)
  (:export #:let-alist))

(in-package #:let-alist)

(defun remove-dot (symbol)
  "Return SYMBOL, sans an initial dot."
  (let ((name (symbol-name symbol)))
    (if (eq #\. (char name 0))
        (intern (subseq name 1) 'keyword)
      symbol)))

(defun deep-dot-search (body)
  "Return alist of symbols inside BODY that start with a `.'.
Perform a deep search and return an alist where each car is the
symbol, and each cdr is the same symbol without the `.'."
  (cond
   ((symbolp body)
    (let ((name (symbol-name body)))
      (when (eq (char name 0) #\.)
        ;; Return the cons cell inside a list, so it can be appended
        ;; with other results in the clause below.
        (list (cons body (intern (subseq name 1) 'keyword))))))
   ((vectorp body)
    (apply #'nconc (map 'list #'deep-dot-search body)))
   ((not (consp body)) nil)
   ((eq (car body) 'let-alist)
    ;; For nested ‘let-alist’ forms, ignore symbols appearing in the
    ;; inner body because they don’t refer to the alist currently
    ;; being processed.
    (deep-dot-search (cadr body)))
   (t (append (deep-dot-search (car body))
              (deep-dot-search (cdr body))))))

(defun access-sexp (symbol variable)
  "Return a sexp used to access SYMBOL inside VARIABLE."
  (let* ((clean (remove-dot symbol))
         (name (symbol-name clean)))
    (if (eq #\. (char name 0))
        clean
      (list-to-sexp
       (mapcar (lambda (name) (intern name 'keyword))
               (nreverse (split-sequence:split-sequence #\. name)))
       variable))))

(defun list-to-sexp (list var)
  "Turn symbols LIST into recursive calls to `cdr' `assoc' on VAR."
  `(cdr (assoc ',(car list)
              ,(if (cdr list) (list-to-sexp (cdr list) var)
                 var))))

;;; The actual macro.
(defmacro let-alist (alist &rest body)
  "Let-bind dotted symbols to their cdrs in ALIST and execute BODY.
Dotted symbol is any symbol starting with a `.'.  Only those present
in BODY are let-bound and this search is done at compile time."
  (let ((var (make-symbol "alist")))
    `(let ((,var ,alist))
       (let ,(mapcar (lambda (x) `(,(car x) ,(access-sexp (car x) var)))
                     (remove-duplicates (deep-dot-search body)))
         ,@body))))
