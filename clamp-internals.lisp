(defpackage "CLAMP.__CLAMP_INTERNALS__"
  (:use :cl)
  (:export
   :py-object
   :make-py-object
   :py-object-type
   :py-object-size
   :py-object-value
   :py-object-attrs
   :make-py-instance
   :py-type
   :make-py-type
   :py-type-name
   :py-type-bases
   :py-type-attrs
   :py-type-basicsize
   :py-type-itemsize
   :py-type-flags
   :py-type-attr
   :py-object-attr
   :py-lookup-attr
   :py-call-attr
   :py-callable
   :make-py-callable
   :py-callable-name
   :py-callable-fn
   :py-callable-binding-kind
   :py-callable-owner-type
   :*py-object-type*
   :*py-type-type*
   :*py-none*
   :*py-false*
   :*py-true*
   :py-bool
   :py-truthy-p
   :py-and
   :py-or
   :py-eq
   :py-ne
   :py-lt
   :py-le
   :py-gt
   :py-ge
   :py-not
   :py-display
   :make-py-list
   :py-append
   :py-getitem
   :py-setitem))

(in-package "CLAMP.__CLAMP_INTERNALS__")

;; Private runtime representation for Python objects inside Clamp.
;; Keep this separate from CLAMP.__builtins__ so user-visible Python globals
;; do not accidentally gain access to internal implementation details.
;;
;; This mirrors CPython's object model where every value has a fixed type
;; pointer and variable-size objects carry an explicit logical size. SBCL's GC
;; owns memory management, so Clamp does not model CPython's reference counts.
(defstruct py-object
  type
  size
  value
  (attrs (make-hash-table :test #'equal)))


(defun make-py-instance (type &key value attrs size)
  (unless (py-type-p type)
    (error "Python object type must be a py-type, got ~S" type))
  (let ((obj (make-py-object :type type :value value :size size)))
    (when attrs
      (maphash (lambda (name attr)
                 (setf (gethash name (py-object-attrs obj)) attr))
               attrs))
    obj))

;; Internal representation of a Python type object. User-defined classes can be
;; modeled with this rather than relying on CLOS semantics.
(defstruct (py-type (:include py-object))
  name
  (bases '())
  (basicsize 0)
  (itemsize 0)
  (flags 0))

(defparameter *py-type-type*
  (make-py-type :name "type" :basicsize 1))

(setf (py-object-type *py-type-type*) *py-type-type*)

(defparameter *py-object-type*
  (make-py-type :type *py-type-type* :name "object" :basicsize 1))

(setf (py-type-bases *py-type-type*) (list *py-object-type*))

(defparameter *py-none-type*
  (make-py-type :type *py-type-type*
                :name "NoneType"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-bool-type*
  (make-py-type :type *py-type-type*
                :name "bool"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-none*
  (make-py-object :type *py-none-type* :value nil))

(defparameter *py-false*
  (make-py-object :type *py-bool-type* :value nil))

(defparameter *py-true*
  (make-py-object :type *py-bool-type* :value t))

(defun py-bool (value)
  (if value *py-true* *py-false*))

(defun py-truthy-p (value)
  (cond
    ((eq value *py-true*) t)
    ((or (eq value *py-false*) (eq value *py-none*)) nil)
    ((py-list-object-p value) (> (or (py-object-size value) 0) 0))
    ((numberp value) (not (zerop value)))
    ((stringp value) (> (length value) 0))
    ((null value) nil)
    (t t)))

(defmacro py-or (&rest forms)
  (cond
    ((null forms) '*py-false*)
    ((null (rest forms)) (first forms))
    (t
     (let ((value (gensym "PY-OR-VALUE-")))
       `(let ((,value ,(first forms)))
          (if (py-truthy-p ,value)
              ,value
              (py-or ,@(rest forms))))))))

(defmacro py-and (&rest forms)
  (cond
    ((null forms) '*py-true*)
    ((null (rest forms)) (first forms))
    (t
     (let ((value (gensym "PY-AND-VALUE-")))
       `(let ((,value ,(first forms)))
          (if (py-truthy-p ,value)
              (py-and ,@(rest forms))
              ,value))))))

(defun py-bool-value (value)
  (cond
    ((eq value *py-true*) 1)
    ((eq value *py-false*) 0)
    (t nil)))

(defun py-normalize-bool-number (value)
  (let ((bool-value (py-bool-value value)))
    (if bool-value bool-value value)))

(defun py-eq (left right)
  (let ((normalized-left (py-normalize-bool-number left))
        (normalized-right (py-normalize-bool-number right)))
    (py-bool
     (cond
       ((or (eq left *py-none*) (eq right *py-none*))
        (eq left right))
       ((and (numberp normalized-left) (numberp normalized-right))
        (= normalized-left normalized-right))
       ((and (stringp left) (stringp right))
        (string= left right))
       (t (eq left right))))))

(defun py-ne (left right)
  (py-bool (not (py-truthy-p (py-eq left right)))))

(defun py-ordered-values (left right operation)
  (let ((normalized-left (py-normalize-bool-number left))
        (normalized-right (py-normalize-bool-number right)))
    (cond
      ((and (numberp normalized-left) (numberp normalized-right))
       (values normalized-left normalized-right))
      ((and (stringp left) (stringp right))
       (values left right))
      (t
       (error "Unsupported Python comparison ~A between ~S and ~S"
              operation left right)))))

(defun py-lt (left right)
  (multiple-value-bind (ordered-left ordered-right)
      (py-ordered-values left right "<")
    (py-bool
     (if (and (stringp ordered-left) (stringp ordered-right))
         (string< ordered-left ordered-right)
         (< ordered-left ordered-right)))))

(defun py-le (left right)
  (multiple-value-bind (ordered-left ordered-right)
      (py-ordered-values left right "<=")
    (py-bool
     (if (and (stringp ordered-left) (stringp ordered-right))
         (not (null (or (string< ordered-left ordered-right)
                        (string= ordered-left ordered-right))))
         (<= ordered-left ordered-right)))))

(defun py-gt (left right)
  (multiple-value-bind (ordered-left ordered-right)
      (py-ordered-values left right ">")
    (py-bool
     (if (and (stringp ordered-left) (stringp ordered-right))
         (string> ordered-left ordered-right)
         (> ordered-left ordered-right)))))

(defun py-ge (left right)
  (multiple-value-bind (ordered-left ordered-right)
      (py-ordered-values left right ">=")
    (py-bool
     (if (and (stringp ordered-left) (stringp ordered-right))
         (not (null (or (string> ordered-left ordered-right)
                        (string= ordered-left ordered-right))))
         (>= ordered-left ordered-right)))))

(defun py-not (value)
  (py-bool (not (py-truthy-p value))))

;; Internal representation of Python-callable behavior.
;;
;; BINDING-KIND distinguishes how descriptor binding should work when the
;; callable is retrieved from a class:
;;   :function         plain function attribute
;;   :instance-method  binds the instance as the first argument
;;   :class-method     binds the owning class as the first argument
;;   :static-method    no implicit binding
(defstruct py-callable
  name
  fn
  (binding-kind :function)
  owner-type)

(defun py-type-attr (type name)
  (gethash name (py-type-attrs type)))

(defun (setf py-type-attr) (value type name)
  (setf (gethash name (py-type-attrs type)) value))

(defun py-object-attr (obj name)
  (gethash name (py-object-attrs obj)))

(defun (setf py-object-attr) (value obj name)
  (setf (gethash name (py-object-attrs obj)) value))

(defun py-find-type-attr (type name)
  (multiple-value-bind (attr found) (gethash name (py-type-attrs type))
    (if found
        (values attr t)
        (loop for base in (py-type-bases type)
              do (multiple-value-bind (base-attr base-found)
                     (py-find-type-attr base name)
                   (when base-found
                     (return (values base-attr t))))
              finally (return (values nil nil))))))

(defun py-lookup-attr (obj name)
  (unless (py-object-p obj)
    (error "Cannot look up Python attribute ~S on non-object ~S" name obj))
  (multiple-value-bind (attr found) (gethash name (py-object-attrs obj))
    (when found
      (return-from py-lookup-attr attr)))
  (multiple-value-bind (attr found) (py-find-type-attr (py-object-type obj) name)
    (when found
      (return-from py-lookup-attr attr)))
  (error "Python object of type ~A has no attribute ~S"
         (py-type-name (py-object-type obj))
         name))

(defun py-invoke-callable (callable &rest args)
  (cond
    ((py-callable-p callable)
     (apply (py-callable-fn callable) args))
    ((functionp callable)
     (apply callable args))
    (t
     (error "Python attribute is not callable: ~S" callable))))

(defun py-call-attr (obj name &rest args)
  (apply #'py-invoke-callable (py-lookup-attr obj name) obj args))

(defstruct (py-list-object (:include py-object))
  (allocated 0))

(defparameter *py-list-type*
  (make-py-type :type *py-type-type*
                :name "list"
                :bases (list *py-object-type*)
                :basicsize 1
                :itemsize 1))

(defun py-list-storage (obj operation)
  (unless (eq (py-object-type obj) *py-list-type*)
    (error "~A only supports list objects, got ~S" operation obj))
  (py-object-value obj))

(setf (py-type-attr *py-list-type* "append")
      (lambda (obj value)
        (let ((storage (py-list-storage obj "append")))
          (vector-push-extend value storage)
          (setf (py-object-size obj) (fill-pointer storage))
          (setf (py-list-object-allocated obj) (array-total-size storage)))
        nil))

(setf (py-type-attr *py-list-type* "__getitem__")
      (lambda (obj index)
        (aref (py-list-storage obj "__getitem__") index)))

(setf (py-type-attr *py-list-type* "__setitem__")
      (lambda (obj index value)
        (setf (aref (py-list-storage obj "__setitem__") index) value)
        nil))

(defun make-py-list (&rest values)
  (let ((storage (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (value values)
      (vector-push-extend value storage))
    (make-py-list-object :type *py-list-type*
                         :size (fill-pointer storage)
                         :value storage
                         :allocated (array-total-size storage))))

(defun py-display (value &optional (stream *standard-output*))
  (cond
    ((eq value *py-none*) (princ "None" stream))
    ((eq value *py-true*) (princ "True" stream))
    ((eq value *py-false*) (princ "False" stream))
    ((py-list-object-p value)
     (princ "[" stream)
     (loop for index from 0 below (or (py-object-size value) 0)
           do (progn
                (when (> index 0)
                  (princ ", " stream))
                (py-display (aref (py-object-value value) index) stream)))
     (princ "]" stream))
    (t (princ value stream))))

(defun py-append (obj value)
  (py-call-attr obj "append" value))

(defun py-getitem (obj index)
  (py-call-attr obj "__getitem__" index))

(defun py-setitem (obj index value)
  (py-call-attr obj "__setitem__" index value))
