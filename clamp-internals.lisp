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
   :py-string-iterator-object
   :py-string-reverse-iterator-object
   :py-tuple-iterator-object
   :py-tuple-reverse-iterator-object
   :py-enumerate-object
   :py-zip-object
   :py-filter-object
   :py-range-object
   :py-range-iterator-object
   :py-slice-object
   :make-py-slice
   :py-slice-object-start
   :py-slice-object-stop
   :py-slice-object-step
   :*py-object-type*
   :*py-type-type*
   :*py-none*
   :*py-false*
   :*py-true*
   :py-bool
   :py-truthy-p
   :py-and
   :py-or
   :py-len
   :py-hash
   :py-reversed
   :py-min
   :py-max
   :py-sum
   :py-sorted
   :py-list
   :py-tuple
   :py-add
   :py-iadd
   :py-mul
   :py-imul
   :py-truediv
   :py-floordiv
   :py-mod
   :py-divmod
   :py-is
   :py-is-not
   :py-eq
   :py-ne
   :py-contains
   :py-not-contains
   :py-lt
   :py-le
   :py-gt
   :py-ge
   :py-abs
   :py-neg
   :py-pos
   :py-not
   :py-repr
   :py-display
   :py-exception
   :py-exception-value
   :py-exception-object
   :py-raise
   :*py-stop-iteration*
   :py-stop-iteration-p
   :py-enumerate
   :py-zip
   :py-filter
   :py-range
   :py-all
   :py-any
   :py-iter
   :py-next
   :py-next-item
   :make-py-list
   :make-py-tuple
   :py-append
   :py-insert
   :py-pop
   :py-getitem
   :py-setitem
   :py-delitem))

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

(defparameter *py-base-exception-type*
  (make-py-type :type *py-type-type*
                :name "BaseException"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-exception-type*
  (make-py-type :type *py-type-type*
                :name "Exception"
                :bases (list *py-base-exception-type*)
                :basicsize 1))

(defparameter *py-stop-iteration-type*
  (make-py-type :type *py-type-type*
                :name "StopIteration"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-slice-type*
  (make-py-type :type *py-type-type*
                :name "slice"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-range-type*
  (make-py-type :type *py-type-type*
                :name "range"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-range-iterator-type*
  (make-py-type :type *py-type-type*
                :name "range_iterator"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-none*
  (make-py-object :type *py-none-type* :value nil))

(defparameter *py-false*
  (make-py-object :type *py-bool-type* :value nil))

(defparameter *py-true*
  (make-py-object :type *py-bool-type* :value t))

(defstruct (py-slice-object (:include py-object))
  start
  stop
  step)

(defun make-py-slice (start stop step)
  (make-py-slice-object :type *py-slice-type*
                        :start start
                        :stop stop
                        :step step))


(defun py-bool (value)
  (if value *py-true* *py-false*))

(defun py-truthy-p (value)
  (cond
    ((eq value *py-true*) t)
    ((or (eq value *py-false*) (eq value *py-none*)) nil)
    ((py-list-object-p value) (> (or (py-object-size value) 0) 0))
    ((py-tuple-object-p value) (> (or (py-object-size value) 0) 0))
    ((py-range-object-p value) (> (py-range-object-length value) 0))
    ((numberp value) (not (zerop value)))
    ((stringp value) (> (length value) 0))
    ((null value) nil)
    (t t)))

(defun py-len (value)
  (cond
    ((py-list-object-p value) (or (py-object-size value) 0))
    ((py-tuple-object-p value) (or (py-object-size value) 0))
    ((py-range-object-p value) (py-range-object-length value))
    ((stringp value) (length value))
    (t
     (error "Python object of type ~A has no len()"
            (if (py-object-p value)
                (py-type-name (py-object-type value))
                (type-of value))))))

(defparameter +py-uhash-width+ 64)
(defparameter +py-uhash-modulus+ (ash 1 +py-uhash-width+))
(defparameter +py-uhash-mask+ (1- +py-uhash-modulus+))
(defparameter +py-hash-sign-bit+ (ash 1 (1- +py-uhash-width+)))
(defparameter +py-tuple-hash-xxprime-1+ 11400714785074694791)
(defparameter +py-tuple-hash-xxprime-2+ 14029467366897019727)
(defparameter +py-tuple-hash-xxprime-5+ 2870177450012600261)

(defun py-uhash (value)
  (logand value +py-uhash-mask+))

(defun py-uhash-rotate-left-31 (value)
  (py-uhash
   (logior (ash value 31)
           (ash value -33))))

(defun py-signed-hash (value)
  (let ((unsigned (py-uhash value)))
    (if (>= unsigned +py-hash-sign-bit+)
        (- unsigned +py-uhash-modulus+)
        unsigned)))

(defun py-int-hash (value)
  (let ((hash value))
    (when (= hash -1)
      (setf hash -2))
    hash))

(defun py-tuple-hash (obj)
  (let ((cached-hash (py-tuple-object-hash obj)))
    (unless (= cached-hash -1)
      (return-from py-tuple-hash cached-hash)))
  (let* ((size (or (py-object-size obj) 0))
         (storage (py-tuple-storage obj "hash"))
         (acc +py-tuple-hash-xxprime-5+))
    (loop for index from 0 below size
          for lane = (py-uhash (py-hash (aref storage index)))
          do (setf acc (py-uhash
                        (* (py-uhash-rotate-left-31
                            (py-uhash
                             (+ acc (* lane +py-tuple-hash-xxprime-2+))))
                           +py-tuple-hash-xxprime-1+))))
    (setf acc (py-uhash
               (+ acc
                  (logxor size
                          (logxor +py-tuple-hash-xxprime-5+ 3527539)))))
    (when (= acc +py-uhash-mask+)
      (setf acc 1546275796))
    (let ((hash (py-signed-hash acc)))
      (setf (py-tuple-object-hash obj) hash)
      hash)))

(defun py-hash (value)
  (let ((normalized-value (py-normalize-bool-number value)))
    (cond
      ((eq value *py-none*) #xFCA86420)
      ((integerp normalized-value)
       (py-int-hash normalized-value))
      ((py-tuple-object-p value)
       (py-tuple-hash value))
      ((py-list-object-p value)
       (error "unhashable type: 'list'"))
      (t
       (error "Python object of type ~A is not hashable by Clamp yet"
              (if (py-object-p value)
                  (py-type-name (py-object-type value))
                  (type-of value)))))))

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

(defun py-list-eq (left right)
  (let ((left-size (or (py-object-size left) 0))
        (right-size (or (py-object-size right) 0)))
    (and (= left-size right-size)
         (let ((left-storage (py-object-value left))
               (right-storage (py-object-value right)))
           (loop for index from 0 below left-size
                 always (py-truthy-p
                         (py-eq (aref left-storage index)
                                (aref right-storage index))))))))

(defun py-tuple-eq (left right)
  (let ((left-size (or (py-object-size left) 0))
        (right-size (or (py-object-size right) 0)))
    (and (= left-size right-size)
         (let ((left-storage (py-object-value left))
               (right-storage (py-object-value right)))
           (loop for index from 0 below left-size
                 always (py-truthy-p
                         (py-eq (aref left-storage index)
                                (aref right-storage index))))))))

(defun py-list-compare (left right operation)
  (let* ((left-size (or (py-object-size left) 0))
         (right-size (or (py-object-size right) 0))
         (left-storage (py-object-value left))
         (right-storage (py-object-value right))
         (shared-size (min left-size right-size))
         (differing-index
           (loop for index from 0 below shared-size
                 unless (py-truthy-p
                         (py-eq (aref left-storage index)
                                (aref right-storage index)))
                   return index)))
    (if differing-index
        (let ((left-item (aref left-storage differing-index))
              (right-item (aref right-storage differing-index)))
          (case operation
            (:lt (py-lt left-item right-item))
            (:le (py-le left-item right-item))
            (:gt (py-gt left-item right-item))
            (:ge (py-ge left-item right-item))
            (otherwise (error "Unsupported Python list comparison ~A" operation))))
        (py-bool
         (case operation
           (:lt (< left-size right-size))
           (:le (<= left-size right-size))
           (:gt (> left-size right-size))
           (:ge (>= left-size right-size))
           (otherwise (error "Unsupported Python list comparison ~A" operation)))))))

(defun py-tuple-compare (left right operation)
  (let* ((left-size (or (py-object-size left) 0))
         (right-size (or (py-object-size right) 0))
         (left-storage (py-object-value left))
         (right-storage (py-object-value right))
         (shared-size (min left-size right-size))
         (differing-index
           (loop for index from 0 below shared-size
                 unless (py-truthy-p
                         (py-eq (aref left-storage index)
                                (aref right-storage index)))
                   return index)))
    (if differing-index
        (let ((left-item (aref left-storage differing-index))
              (right-item (aref right-storage differing-index)))
          (case operation
            (:lt (py-lt left-item right-item))
            (:le (py-le left-item right-item))
            (:gt (py-gt left-item right-item))
            (:ge (py-ge left-item right-item))
            (otherwise (error "Unsupported Python tuple comparison ~A" operation))))
        (py-bool
         (case operation
           (:lt (< left-size right-size))
           (:le (<= left-size right-size))
           (:gt (> left-size right-size))
           (:ge (>= left-size right-size))
           (otherwise (error "Unsupported Python tuple comparison ~A" operation)))))))

(defun py-eq (left right)
  (let ((normalized-left (py-normalize-bool-number left))
        (normalized-right (py-normalize-bool-number right)))
    (py-bool
     (cond
       ((or (eq left *py-none*) (eq right *py-none*))
        (eq left right))
       ((and (py-list-object-p left) (py-list-object-p right))
        (py-list-eq left right))
       ((and (py-tuple-object-p left) (py-tuple-object-p right))
        (py-tuple-eq left right))
       ((and (numberp normalized-left) (numberp normalized-right))
        (= normalized-left normalized-right))
       ((and (stringp left) (stringp right))
        (string= left right))
       (t (eq left right))))))

(defun py-ne (left right)
  (py-bool (not (py-truthy-p (py-eq left right)))))

(defun py-is (left right)
  (py-bool (eq left right)))

(defun py-is-not (left right)
  (py-bool (not (eq left right))))

(defun py-contains (item container)
  (cond
    ((py-list-object-p container)
     (let ((storage (py-object-value container))
           (size (or (py-object-size container) 0)))
       (py-bool
        (loop for index from 0 below size
              thereis (py-truthy-p (py-eq (aref storage index) item))))))
    ((py-tuple-object-p container)
     (let ((storage (py-object-value container))
           (size (or (py-object-size container) 0)))
       (py-bool
        (loop for index from 0 below size
              thereis (py-truthy-p (py-eq (aref storage index) item))))))
    ((py-range-object-p container)
     (py-range-contains container item))
    ((stringp container)
     (unless (stringp item)
       (error "'in <string>' requires string as left operand, got ~S" item))
     (py-bool (search item container)))
    (t
     (error "Python object of type ~A is not a container"
            (if (py-object-p container)
                (py-type-name (py-object-type container))
                (type-of container))))))

(defun py-not-contains (item container)
  (py-bool (not (py-truthy-p (py-contains item container)))))

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
  (cond
    ((and (py-list-object-p left) (py-list-object-p right))
     (py-list-compare left right :lt))
    ((and (py-tuple-object-p left) (py-tuple-object-p right))
     (py-tuple-compare left right :lt))
    (t
     (multiple-value-bind (ordered-left ordered-right)
         (py-ordered-values left right "<")
       (py-bool
        (if (and (stringp ordered-left) (stringp ordered-right))
            (string< ordered-left ordered-right)
            (< ordered-left ordered-right)))))))

(defun py-le (left right)
  (cond
    ((and (py-list-object-p left) (py-list-object-p right))
     (py-list-compare left right :le))
    ((and (py-tuple-object-p left) (py-tuple-object-p right))
     (py-tuple-compare left right :le))
    (t
     (multiple-value-bind (ordered-left ordered-right)
         (py-ordered-values left right "<=")
       (py-bool
        (if (and (stringp ordered-left) (stringp ordered-right))
            (not (null (or (string< ordered-left ordered-right)
                           (string= ordered-left ordered-right))))
            (<= ordered-left ordered-right)))))))

(defun py-gt (left right)
  (cond
    ((and (py-list-object-p left) (py-list-object-p right))
     (py-list-compare left right :gt))
    ((and (py-tuple-object-p left) (py-tuple-object-p right))
     (py-tuple-compare left right :gt))
    (t
     (multiple-value-bind (ordered-left ordered-right)
         (py-ordered-values left right ">")
       (py-bool
        (if (and (stringp ordered-left) (stringp ordered-right))
            (string> ordered-left ordered-right)
            (> ordered-left ordered-right)))))))

(defun py-ge (left right)
  (cond
    ((and (py-list-object-p left) (py-list-object-p right))
     (py-list-compare left right :ge))
    ((and (py-tuple-object-p left) (py-tuple-object-p right))
     (py-tuple-compare left right :ge))
    (t
     (multiple-value-bind (ordered-left ordered-right)
         (py-ordered-values left right ">=")
       (py-bool
        (if (and (stringp ordered-left) (stringp ordered-right))
            (not (null (or (string> ordered-left ordered-right)
                           (string= ordered-left ordered-right))))
            (>= ordered-left ordered-right)))))))

(defun py-abs (value)
  (let ((normalized-value (py-normalize-bool-number value)))
    (if (numberp normalized-value)
        (abs normalized-value)
        (error "bad operand type for abs(): ~S" value))))

(defun py-pos (value)
  (let ((normalized-value (py-normalize-bool-number value)))
    (if (numberp normalized-value)
        normalized-value
        (error "bad operand type for unary +: ~S" value))))

(defun py-neg (value)
  (let ((normalized-value (py-normalize-bool-number value)))
    (if (numberp normalized-value)
        (- normalized-value)
        (error "bad operand type for unary -: ~S" value))))

(defun py-not (value)
  (py-bool (not (py-truthy-p value))))

(defstruct (py-exception-object (:include py-object))
  (args '()))

(defun make-py-exception (type &rest args)
  (make-py-exception-object :type type :value args :args args))

(defparameter *py-stop-iteration*
  (make-py-exception *py-stop-iteration-type*))

(define-condition py-exception (error)
  ((value :initarg :value :reader py-exception-value))
  (:report (lambda (condition stream)
             (let ((value (py-exception-value condition)))
               (if (py-exception-object-p value)
                   (princ (py-type-name (py-object-type value)) stream)
                   (princ value stream))))))

(defun py-raise (exception)
  (error 'py-exception :value exception))

(defun py-stop-iteration-p (value)
  (cond
    ((typep value 'py-exception)
     (py-stop-iteration-p (py-exception-value value)))
    ((py-exception-object-p value)
     (eq (py-object-type value) *py-stop-iteration-type*))
    (t nil)))

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

(defstruct (py-tuple-object (:include py-object))
  (hash -1))

(defstruct (py-list-iterator-object (:include py-object))
  sequence
  (index 0))

(defstruct (py-list-reverse-iterator-object (:include py-object))
  sequence
  (index -1))

(defstruct (py-string-iterator-object (:include py-object))
  sequence
  (index 0))

(defstruct (py-string-reverse-iterator-object (:include py-object))
  sequence
  (index -1))

(defstruct (py-tuple-iterator-object (:include py-object))
  sequence
  (index 0))

(defstruct (py-tuple-reverse-iterator-object (:include py-object))
  sequence
  (index -1))

(defstruct (py-enumerate-object (:include py-object))
  iterator
  (index 0)
  result)

(defstruct (py-zip-object (:include py-object))
  iterators
  result)

(defstruct (py-filter-object (:include py-object))
  predicate
  iterator)

(defstruct (py-range-object (:include py-object))
  start
  stop
  step
  length)

(defstruct (py-range-iterator-object (:include py-object))
  range
  (index 0))

(defparameter *py-list-type*
  (make-py-type :type *py-type-type*
                :name "list"
                :bases (list *py-object-type*)
                :basicsize 1
                :itemsize 1))

(defparameter *py-tuple-type*
  (make-py-type :type *py-type-type*
                :name "tuple"
                :bases (list *py-object-type*)
                :basicsize 1
                :itemsize 1))

(defparameter *py-list-iterator-type*
  (make-py-type :type *py-type-type*
                :name "list_iterator"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-list-reverse-iterator-type*
  (make-py-type :type *py-type-type*
                :name "list_reverseiterator"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-string-iterator-type*
  (make-py-type :type *py-type-type*
                :name "str_iterator"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-string-reverse-iterator-type*
  (make-py-type :type *py-type-type*
                :name "reversed"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-tuple-iterator-type*
  (make-py-type :type *py-type-type*
                :name "tuple_iterator"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-tuple-reverse-iterator-type*
  (make-py-type :type *py-type-type*
                :name "reversed"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-enumerate-type*
  (make-py-type :type *py-type-type*
                :name "enumerate"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-zip-type*
  (make-py-type :type *py-type-type*
                :name "zip"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-filter-type*
  (make-py-type :type *py-type-type*
                :name "filter"
                :bases (list *py-object-type*)
                :basicsize 1))

(defun py-list-storage (obj operation)
  (unless (eq (py-object-type obj) *py-list-type*)
    (error "~A only supports list objects, got ~S" operation obj))
  (py-object-value obj))

(defun py-tuple-storage (obj operation)
  (unless (eq (py-object-type obj) *py-tuple-type*)
    (error "~A only supports tuple objects, got ~S" operation obj))
  (py-object-value obj))

(defun py-list-index (obj index)
  (if (< index 0)
      (+ index (or (py-object-size obj) 0))
      index))

(defun py-list-valid-index-p (index size)
  (and (>= index 0) (< index size)))

(defun py-list-normalized-index (obj index operation)
  (let* ((size (or (py-object-size obj) 0))
         (normalized-index (py-list-index obj index)))
    (unless (py-list-valid-index-p normalized-index size)
      (error "~A index out of range" operation))
    normalized-index))

(defun py-slice-bound (value default)
  (if (eq value *py-none*) default value))

(defun py-list-normalize-slice-index (index size lower upper)
  (let ((normalized-index index))
    (when (< normalized-index 0)
      (incf normalized-index size))
    (cond
      ((< normalized-index lower) lower)
      ((> normalized-index upper) upper)
      (t normalized-index))))

(defun py-list-slice-parameters (slice size)
  (let* ((raw-step (py-slice-bound (py-slice-object-step slice) 1))
         (step raw-step))
    (when (= step 0)
      (error "slice step cannot be zero"))
    (if (> step 0)
        (let* ((start (py-list-normalize-slice-index
                       (py-slice-bound (py-slice-object-start slice) 0)
                       size 0 size))
               (stop (py-list-normalize-slice-index
                      (py-slice-bound (py-slice-object-stop slice) size)
                      size 0 size))
               (slice-length (if (< start stop)
                                 (1+ (floor (1- (- stop start)) step))
                                 0)))
          (values start step slice-length stop))
        (let* ((start (if (eq (py-slice-object-start slice) *py-none*)
                          (1- size)
                          (py-list-normalize-slice-index
                           (py-slice-object-start slice)
                           size -1 (1- size))))
               (stop (if (eq (py-slice-object-stop slice) *py-none*)
                         -1
                         (py-list-normalize-slice-index
                          (py-slice-object-stop slice)
                          size -1 (1- size))))
               (slice-length (if (> start stop)
                                 (1+ (floor (1- (- start stop)) (- step)))
                                 0)))
          (values start step slice-length stop)))))

(defun py-list-slice (obj slice)
  (let* ((storage (py-list-storage obj "__getitem__"))
         (size (or (py-object-size obj) 0))
         (result-storage (make-array 0 :adjustable t :fill-pointer 0)))
    (multiple-value-bind (start step slice-length)
        (py-list-slice-parameters slice size)
      (loop for offset from 0 below slice-length
            for index = start then (+ index step)
            do (vector-push-extend (aref storage index) result-storage)))
    (make-py-list-object :type *py-list-type*
                         :size (fill-pointer result-storage)
                         :value result-storage
                         :allocated (array-total-size result-storage))))

(defun py-tuple-slice (obj slice)
  (let* ((storage (py-tuple-storage obj "__getitem__"))
         (size (or (py-object-size obj) 0)))
    (multiple-value-bind (start step slice-length)
        (py-list-slice-parameters slice size)
      (let ((result-storage (make-array slice-length)))
        (loop for offset from 0 below slice-length
              for index = start then (+ index step)
              do (setf (aref result-storage offset) (aref storage index)))
        (make-py-tuple-object :type *py-tuple-type*
                              :size slice-length
                              :value result-storage)))))

(defun py-string-normalized-index (value index)
  (let* ((size (length value))
         (normalized-index (if (< index 0) (+ index size) index)))
    (unless (py-list-valid-index-p normalized-index size)
      (error "string index out of range"))
    normalized-index))

(defun py-string-slice (value slice)
  (multiple-value-bind (start step slice-length)
      (py-list-slice-parameters slice (length value))
    (with-output-to-string (stream)
      (loop for offset from 0 below slice-length
            for index = start then (+ index step)
            do (princ (char value index) stream)))))

(defun py-string-getitem (value index)
  (if (py-slice-object-p index)
      (py-string-slice value index)
      (let ((normalized-index (py-string-normalized-index value index)))
        (subseq value normalized-index (1+ normalized-index)))))

(setf (py-type-attr *py-list-type* "append")
      (lambda (obj value)
        (let ((storage (py-list-storage obj "append")))
          (vector-push-extend value storage)
          (setf (py-object-size obj) (fill-pointer storage))
          (setf (py-list-object-allocated obj) (array-total-size storage)))
        *py-none*))

(defun py-list-insert-index (size index)
  (let ((normalized-index index))
    (when (< normalized-index 0)
      (incf normalized-index size)
      (when (< normalized-index 0)
        (setf normalized-index 0)))
    (when (> normalized-index size)
      (setf normalized-index size))
    normalized-index))

(setf (py-type-attr *py-list-type* "insert")
      (lambda (obj index value)
        (let* ((storage (py-list-storage obj "insert"))
               (size (or (py-object-size obj) 0))
               (normalized-index (py-list-insert-index size index)))
          (vector-push-extend *py-none* storage)
          (loop for i downfrom size above normalized-index
                do (setf (aref storage i) (aref storage (1- i))))
          (setf (aref storage normalized-index) value)
          (setf (py-object-size obj) (fill-pointer storage))
          (setf (py-list-object-allocated obj) (array-total-size storage)))
        *py-none*))

(defun py-list-extend-iterable (obj iterable)
  (let ((storage (py-list-storage obj "extend")))
    (cond
      ((py-list-object-p iterable)
       (let ((source-storage (py-list-storage iterable "extend"))
             (source-size (or (py-object-size iterable) 0)))
         (loop for index from 0 below source-size
               do (vector-push-extend (aref source-storage index) storage))))
      ((py-tuple-object-p iterable)
       (let ((source-storage (py-tuple-storage iterable "extend"))
             (source-size (or (py-object-size iterable) 0)))
         (loop for index from 0 below source-size
               do (vector-push-extend (aref source-storage index) storage))))
      (t
       (let ((iterator (py-iter iterable)))
         (loop
           (multiple-value-bind (item found) (py-next-item iterator)
             (unless found
               (return))
             (vector-push-extend item storage))))))
    (setf (py-object-size obj) (fill-pointer storage))
    (setf (py-list-object-allocated obj) (array-total-size storage)))
  *py-none*)

(setf (py-type-attr *py-list-type* "extend")
      (lambda (obj iterable)
        (py-list-extend-iterable obj iterable)))

(setf (py-type-attr *py-list-type* "clear")
      (lambda (obj)
        (py-list-storage obj "clear")
        (setf (py-object-value obj)
              (make-array 0 :adjustable t :fill-pointer 0))
        (setf (py-object-size obj) 0)
        (setf (py-list-object-allocated obj) 0)
        *py-none*))

(setf (py-type-attr *py-list-type* "copy")
      (lambda (obj)
        (let* ((storage (py-list-storage obj "copy"))
               (size (or (py-object-size obj) 0))
               (copied-storage (make-array 0 :adjustable t :fill-pointer 0)))
          (loop for index from 0 below size
                do (vector-push-extend (aref storage index) copied-storage))
          (make-py-list-object :type *py-list-type*
                               :size (fill-pointer copied-storage)
                               :value copied-storage
                               :allocated (array-total-size copied-storage)))))

(setf (py-type-attr *py-list-type* "count")
      (lambda (obj value)
        (let ((storage (py-list-storage obj "count"))
              (size (or (py-object-size obj) 0))
              (count 0))
          (loop for index from 0 below size
                when (py-truthy-p (py-eq (aref storage index) value))
                  do (incf count))
          count)))

(setf (py-type-attr *py-list-type* "__contains__")
      (lambda (obj value)
        (py-contains value obj)))

(setf (py-type-attr *py-list-type* "__eq__")
      (lambda (obj value)
        (py-eq obj value)))

(setf (py-type-attr *py-list-type* "__ne__")
      (lambda (obj value)
        (py-ne obj value)))

(setf (py-type-attr *py-list-type* "__lt__")
      (lambda (obj value)
        (py-lt obj value)))

(setf (py-type-attr *py-list-type* "__le__")
      (lambda (obj value)
        (py-le obj value)))

(setf (py-type-attr *py-list-type* "__gt__")
      (lambda (obj value)
        (py-gt obj value)))

(setf (py-type-attr *py-list-type* "__ge__")
      (lambda (obj value)
        (py-ge obj value)))

(setf (py-type-attr *py-list-type* "__len__")
      (lambda (obj)
        (py-list-storage obj "__len__")
        (or (py-object-size obj) 0)))

(setf (py-type-attr *py-list-type* "__add__")
      (lambda (obj value)
        (py-add obj value)))

(setf (py-type-attr *py-list-type* "__iadd__")
      (lambda (obj value)
        (py-iadd obj value)))

(setf (py-type-attr *py-list-type* "__mul__")
      (lambda (obj value)
        (py-mul obj value)))

(setf (py-type-attr *py-list-type* "__rmul__")
      (lambda (obj value)
        (py-mul value obj)))

(setf (py-type-attr *py-list-type* "__imul__")
      (lambda (obj value)
        (py-imul obj value)))

(defun py-list-slice-index (size index)
  (let ((normalized-index index))
    (when (< normalized-index 0)
      (incf normalized-index size)
      (when (< normalized-index 0)
        (setf normalized-index 0)))
    normalized-index))

(setf (py-type-attr *py-list-type* "index")
      (lambda (obj value &optional (start 0) (stop most-positive-fixnum))
        (let* ((storage (py-list-storage obj "index"))
               (size (or (py-object-size obj) 0))
               (normalized-start (py-list-slice-index size start))
               (normalized-stop (py-list-slice-index size stop)))
          (loop for index from normalized-start below (min normalized-stop size)
                when (py-truthy-p (py-eq (aref storage index) value))
                  return index
                finally (error "list.index(x): x not in list")))))

(setf (py-type-attr *py-list-type* "reverse")
      (lambda (obj)
        (let* ((storage (py-list-storage obj "reverse"))
               (size (or (py-object-size obj) 0)))
          (loop for left from 0 below (floor size 2)
                for right downfrom (1- size)
                do (rotatef (aref storage left) (aref storage right))))
        *py-none*))

(setf (py-type-attr *py-list-type* "sort")
      (lambda (obj)
        (let ((storage (py-list-storage obj "sort")))
          (stable-sort storage
                       (lambda (left right)
                         (py-truthy-p (py-lt left right)))))
        *py-none*))

(defun py-list-delete-index (obj normalized-index)
  (let* ((storage (py-list-storage obj "delete"))
         (size (or (py-object-size obj) 0))
         (size-after-delete (1- size))
         (value (aref storage normalized-index)))
    (loop for i from normalized-index below size-after-delete
          do (setf (aref storage i) (aref storage (1+ i))))
    (vector-pop storage)
    (setf (py-object-size obj) (fill-pointer storage))
    value))

(defun py-list-delete-slice (obj slice)
  (let* ((storage (py-list-storage obj "delete"))
         (size (or (py-object-size obj) 0))
         (delete-flags (make-array size :initial-element nil)))
    (multiple-value-bind (start step slice-length)
        (py-list-slice-parameters slice size)
      (loop for offset from 0 below slice-length
            for index = start then (+ index step)
            do (setf (aref delete-flags index) t)))
    (let ((write-index 0))
      (loop for read-index from 0 below size
            unless (aref delete-flags read-index)
              do (progn
                   (setf (aref storage write-index) (aref storage read-index))
                   (incf write-index)))
      (loop while (> (fill-pointer storage) write-index)
            do (vector-pop storage)))
    (setf (py-object-size obj) (fill-pointer storage))
    (setf (py-list-object-allocated obj) (array-total-size storage))
    *py-none*))

(setf (py-type-attr *py-list-type* "pop")
      (lambda (obj &optional (index -1))
        (let* ((storage (py-list-storage obj "pop"))
               (size (or (py-object-size obj) 0)))
          (when (= size 0)
            (error "pop from empty list"))
          (let ((normalized-index (py-list-index obj index)))
            (unless (py-list-valid-index-p normalized-index size)
              (error "pop index out of range"))
            (py-list-delete-index obj normalized-index)))))

(setf (py-type-attr *py-list-type* "remove")
      (lambda (obj value)
        (let* ((storage (py-list-storage obj "remove"))
               (size (or (py-object-size obj) 0))
               (match-index
                 (loop for index from 0 below size
                       when (py-truthy-p (py-eq (aref storage index) value))
                         return index)))
          (if match-index
              (progn
                (py-list-delete-index obj match-index)
                *py-none*)
              (error "list.remove(x): x not in list")))))

(setf (py-type-attr *py-list-type* "__getitem__")
      (lambda (obj index)
        (if (py-slice-object-p index)
            (py-list-slice obj index)
            (aref (py-list-storage obj "__getitem__")
                  (py-list-normalized-index obj index "list")))))

(setf (py-type-attr *py-list-type* "__setitem__")
      (lambda (obj index value)
        (setf (aref (py-list-storage obj "__setitem__")
                    (py-list-normalized-index obj index "list"))
              value)
        *py-none*))

(setf (py-type-attr *py-list-type* "__delitem__")
      (lambda (obj index)
        (if (py-slice-object-p index)
            (py-list-delete-slice obj index)
            (py-list-delete-index obj (py-list-normalized-index obj index "list")))
        *py-none*))

(setf (py-type-attr *py-list-type* "__repr__")
      (lambda (obj)
        (with-output-to-string (stream)
          (py-repr obj stream))))

(setf (py-type-attr *py-tuple-type* "__contains__")
      (lambda (obj value)
        (py-contains value obj)))

(setf (py-type-attr *py-tuple-type* "__eq__")
      (lambda (obj value)
        (py-eq obj value)))

(setf (py-type-attr *py-tuple-type* "__ne__")
      (lambda (obj value)
        (py-ne obj value)))

(setf (py-type-attr *py-tuple-type* "__lt__")
      (lambda (obj value)
        (py-lt obj value)))

(setf (py-type-attr *py-tuple-type* "__le__")
      (lambda (obj value)
        (py-le obj value)))

(setf (py-type-attr *py-tuple-type* "__gt__")
      (lambda (obj value)
        (py-gt obj value)))

(setf (py-type-attr *py-tuple-type* "__ge__")
      (lambda (obj value)
        (py-ge obj value)))

(setf (py-type-attr *py-tuple-type* "__len__")
      (lambda (obj)
        (py-tuple-storage obj "__len__")
        (or (py-object-size obj) 0)))

(setf (py-type-attr *py-tuple-type* "count")
      (lambda (obj value)
        (let ((storage (py-tuple-storage obj "count"))
              (size (or (py-object-size obj) 0))
              (count 0))
          (loop for index from 0 below size
                when (py-truthy-p (py-eq (aref storage index) value))
                  do (incf count))
          count)))

(setf (py-type-attr *py-tuple-type* "index")
      (lambda (obj value &optional (start 0) (stop most-positive-fixnum))
        (let* ((storage (py-tuple-storage obj "index"))
               (size (or (py-object-size obj) 0))
               (normalized-start (py-list-slice-index size start))
               (normalized-stop (py-list-slice-index size stop)))
          (loop for index from normalized-start below (min normalized-stop size)
                when (py-truthy-p (py-eq (aref storage index) value))
                  return index
                finally (error "tuple.index(x): x not in tuple")))))

(setf (py-type-attr *py-tuple-type* "__getitem__")
      (lambda (obj index)
        (if (py-slice-object-p index)
            (py-tuple-slice obj index)
            (aref (py-tuple-storage obj "__getitem__")
                  (py-list-normalized-index obj index "tuple")))))

(setf (py-type-attr *py-tuple-type* "__add__")
      (lambda (obj value)
        (py-add obj value)))

(setf (py-type-attr *py-tuple-type* "__mul__")
      (lambda (obj value)
        (py-mul obj value)))

(setf (py-type-attr *py-tuple-type* "__rmul__")
      (lambda (obj value)
        (py-mul value obj)))

(setf (py-type-attr *py-tuple-type* "__repr__")
      (lambda (obj)
        (with-output-to-string (stream)
          (py-repr obj stream))))

(setf (py-type-attr *py-tuple-type* "__hash__")
      (lambda (obj)
        (py-hash obj)))

(setf (py-type-attr *py-tuple-type* "__getnewargs__")
      (lambda (obj)
        (py-tuple-storage obj "__getnewargs__")
        (make-py-tuple obj)))

(defun make-py-list (&rest values)
  (let ((storage (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (value values)
      (vector-push-extend value storage))
    (make-py-list-object :type *py-list-type*
                         :size (fill-pointer storage)
                         :value storage
                         :allocated (array-total-size storage))))

(defun make-py-tuple (&rest values)
  (let* ((size (length values))
         (storage (make-array size)))
    (loop for value in values
          for index from 0
          do (setf (aref storage index) value))
    (make-py-tuple-object :type *py-tuple-type*
                          :size size
                          :value storage)))

(defun py-add (left right)
  (let ((normalized-left (py-normalize-bool-number left))
        (normalized-right (py-normalize-bool-number right)))
    (cond
      ((and (numberp normalized-left) (numberp normalized-right))
       (+ normalized-left normalized-right))
      ((and (stringp left) (stringp right))
       (concatenate 'string left right))
      ((and (py-list-object-p left) (py-list-object-p right))
       (let* ((left-storage (py-list-storage left "+"))
              (right-storage (py-list-storage right "+"))
              (result-storage (make-array 0 :adjustable t :fill-pointer 0)))
         (loop for index from 0 below (or (py-object-size left) 0)
               do (vector-push-extend (aref left-storage index) result-storage))
         (loop for index from 0 below (or (py-object-size right) 0)
               do (vector-push-extend (aref right-storage index) result-storage))
         (make-py-list-object :type *py-list-type*
                              :size (fill-pointer result-storage)
                              :value result-storage
                              :allocated (array-total-size result-storage))))
      ((and (py-tuple-object-p left) (py-tuple-object-p right))
       (let* ((left-size (or (py-object-size left) 0))
              (right-size (or (py-object-size right) 0))
              (result-size (+ left-size right-size))
              (result-storage (make-array result-size)))
         (loop for index from 0 below left-size
               do (setf (aref result-storage index)
                        (aref (py-tuple-storage left "+") index)))
         (loop for index from 0 below right-size
               do (setf (aref result-storage (+ left-size index))
                        (aref (py-tuple-storage right "+") index)))
         (make-py-tuple-object :type *py-tuple-type*
                               :size result-size
                               :value result-storage)))
      (t
       (error "Unsupported Python + between ~S and ~S" left right)))))

(defun py-iadd (left right)
  (if (py-list-object-p left)
      (progn
        (py-list-extend-iterable left right)
        left)
      (py-add left right)))

(defun py-list-repeat (items count)
  (let* ((source-storage (py-list-storage items "*"))
         (source-size (or (py-object-size items) 0))
         (repeat-count (max count 0))
         (output-size (* source-size repeat-count))
         (result-storage (make-array output-size
                                     :adjustable t
                                     :fill-pointer 0)))
    (dotimes (_ repeat-count)
      (loop for index from 0 below source-size
            do (vector-push-extend (aref source-storage index) result-storage)))
    (make-py-list-object :type *py-list-type*
                         :size (fill-pointer result-storage)
                         :value result-storage
                         :allocated (array-total-size result-storage))))

(defun py-tuple-repeat (items count)
  (let* ((source-storage (py-tuple-storage items "*"))
         (source-size (or (py-object-size items) 0))
         (repeat-count (max count 0))
         (output-size (* source-size repeat-count))
         (result-storage (make-array output-size)))
    (dotimes (repeat repeat-count)
      (loop for index from 0 below source-size
            do (setf (aref result-storage (+ (* repeat source-size) index))
                     (aref source-storage index))))
    (make-py-tuple-object :type *py-tuple-type*
                          :size output-size
                          :value result-storage)))

(defun py-list-inplace-repeat (items count)
  (let* ((storage (py-list-storage items "*="))
         (input-size (or (py-object-size items) 0)))
    (cond
      ((or (= input-size 0) (= count 1))
       items)
      ((< count 1)
       (setf (py-object-value items)
             (make-array 0 :adjustable t :fill-pointer 0))
       (setf (py-object-size items) 0)
       (setf (py-list-object-allocated items) 0)
       items)
      (t
       (dotimes (_ (1- count))
         (loop for index from 0 below input-size
               do (vector-push-extend (aref storage index) storage)))
       (setf (py-object-size items) (fill-pointer storage))
       (setf (py-list-object-allocated items) (array-total-size storage))
       items))))

(defun py-string-repeat (value count)
  (let ((repeat-count (max count 0)))
    (with-output-to-string (stream)
      (dotimes (_ repeat-count)
        (princ value stream)))))

(defun py-mul (left right)
  (let ((normalized-left (py-normalize-bool-number left))
        (normalized-right (py-normalize-bool-number right)))
    (cond
      ((and (numberp normalized-left) (numberp normalized-right))
       (* normalized-left normalized-right))
      ((and (py-list-object-p left) (integerp normalized-right))
       (py-list-repeat left normalized-right))
      ((and (integerp normalized-left) (py-list-object-p right))
       (py-list-repeat right normalized-left))
      ((and (py-tuple-object-p left) (integerp normalized-right))
       (py-tuple-repeat left normalized-right))
      ((and (integerp normalized-left) (py-tuple-object-p right))
       (py-tuple-repeat right normalized-left))
      ((and (stringp left) (integerp normalized-right))
       (py-string-repeat left normalized-right))
      ((and (integerp normalized-left) (stringp right))
       (py-string-repeat right normalized-left))
      (t
       (error "Unsupported Python * between ~S and ~S" left right)))))

(defun py-truediv (left right)
  (let ((normalized-left (py-normalize-bool-number left))
        (normalized-right (py-normalize-bool-number right)))
    (if (and (numberp normalized-left) (numberp normalized-right))
        (float (/ normalized-left normalized-right))
        (error "Unsupported Python / between ~S and ~S" left right))))

(defun py-floordiv (left right)
  (let ((normalized-left (py-normalize-bool-number left))
        (normalized-right (py-normalize-bool-number right)))
    (if (and (numberp normalized-left) (numberp normalized-right))
        (floor normalized-left normalized-right)
        (error "Unsupported Python // between ~S and ~S" left right))))

(defun py-mod (left right)
  (let ((normalized-left (py-normalize-bool-number left))
        (normalized-right (py-normalize-bool-number right)))
    (if (and (numberp normalized-left) (numberp normalized-right))
        (mod normalized-left normalized-right)
        (error "Unsupported Python % between ~S and ~S" left right))))

(defun py-divmod (left right)
  (make-py-tuple (py-floordiv left right)
                 (py-mod left right)))

(defun py-imul (left right)
  (let ((normalized-right (py-normalize-bool-number right)))
    (if (and (py-list-object-p left) (integerp normalized-right))
        (py-list-inplace-repeat left normalized-right)
        (py-mul left right))))

(defun py-iterator-p (obj)
  (and (py-object-p obj)
       (or (eq (py-object-type obj) *py-list-iterator-type*)
           (eq (py-object-type obj) *py-list-reverse-iterator-type*)
           (eq (py-object-type obj) *py-string-iterator-type*)
           (eq (py-object-type obj) *py-string-reverse-iterator-type*)
           (eq (py-object-type obj) *py-tuple-iterator-type*)
           (eq (py-object-type obj) *py-tuple-reverse-iterator-type*)
           (eq (py-object-type obj) *py-enumerate-type*)
           (eq (py-object-type obj) *py-zip-type*)
           (eq (py-object-type obj) *py-filter-type*)
           (eq (py-object-type obj) *py-range-iterator-type*))))

(defun py-forward-list-iterator-p (obj)
  (and (py-object-p obj)
       (eq (py-object-type obj) *py-list-iterator-type*)))

(defun py-reverse-list-iterator-p (obj)
  (and (py-object-p obj)
       (eq (py-object-type obj) *py-list-reverse-iterator-type*)))

(defun py-string-iterator-p (obj)
  (and (py-object-p obj)
       (eq (py-object-type obj) *py-string-iterator-type*)))

(defun py-reverse-string-iterator-p (obj)
  (and (py-object-p obj)
       (eq (py-object-type obj) *py-string-reverse-iterator-type*)))

(defun py-tuple-iterator-p (obj)
  (and (py-object-p obj)
       (eq (py-object-type obj) *py-tuple-iterator-type*)))

(defun py-reverse-tuple-iterator-p (obj)
  (and (py-object-p obj)
       (eq (py-object-type obj) *py-tuple-reverse-iterator-type*)))

(defun py-range-iterator-p (obj)
  (and (py-object-p obj)
       (eq (py-object-type obj) *py-range-iterator-type*)))

(defun py-enumerate (iterable &optional (start 0))
  (let ((normalized-start (py-normalize-bool-number start)))
    (unless (integerp normalized-start)
      (error "enumerate() start must be an integer, got ~S" start))
    (make-py-enumerate-object
     :type *py-enumerate-type*
     :iterator (py-iter iterable)
     :index normalized-start
     :result (make-py-tuple *py-none* *py-none*))))

(defun py-zip (&rest iterables)
  (let ((iterator-tuple
          (apply #'make-py-tuple
                 (loop for iterable in iterables
                       collect (py-iter iterable))))
        (result-tuple
          (apply #'make-py-tuple
                 (loop for _ in iterables
                       collect *py-none*))))
    (make-py-zip-object :type *py-zip-type*
                        :iterators iterator-tuple
                        :result result-tuple)))

(defun py-filter (predicate iterable)
  (make-py-filter-object :type *py-filter-type*
                         :predicate predicate
                         :iterator (py-iter iterable)))

(defun py-range-length (start stop step)
  (cond
    ((and (> step 0) (< start stop))
     (1+ (floor (- stop 1 start) step)))
    ((and (< step 0) (> start stop))
     (1+ (floor (- start 1 stop) (- step))))
    (t 0)))

(defun py-range-integer-argument (value name)
  (let ((normalized-value (py-normalize-bool-number value)))
    (unless (integerp normalized-value)
      (error "range() ~A must be an integer, got ~S" name value))
    normalized-value))

(defun py-range (&rest args)
  (let (start stop step)
    (case (length args)
      (1
       (setf start 0
             stop (py-range-integer-argument (first args) "stop")
             step 1))
      (2
       (setf start (py-range-integer-argument (first args) "start")
             stop (py-range-integer-argument (second args) "stop")
             step 1))
      (3
       (setf start (py-range-integer-argument (first args) "start")
             stop (py-range-integer-argument (second args) "stop")
             step (py-range-integer-argument (third args) "step"))
       (when (= step 0)
         (error "range() arg 3 must not be zero")))
      (0
       (error "range expected at least 1 argument, got 0"))
      (otherwise
       (error "range expected at most 3 arguments, got ~A" (length args))))
    (make-py-range-object :type *py-range-type*
                          :start start
                          :stop stop
                          :step step
                          :length (py-range-length start stop step))))

(defun py-range-item (range index)
  (+ (py-range-object-start range)
     (* index (py-range-object-step range))))

(defun py-range-slice (range slice)
  (multiple-value-bind (start step slice-length stop)
      (py-list-slice-parameters slice (py-range-object-length range))
    (let* ((substep (* (py-range-object-step range) step))
           (substart (py-range-item range start))
           (substop (py-range-item range stop)))
      (py-range substart substop substep))))

(defun py-range-normalized-index (range index)
  (let* ((length (py-range-object-length range))
         (normalized-index (if (< index 0) (+ index length) index)))
    (unless (py-list-valid-index-p normalized-index length)
      (error "range object index out of range"))
    normalized-index))

(defun py-range-contains (range value)
  (let ((candidate (py-normalize-bool-number value))
        (start (py-range-object-start range))
        (stop (py-range-object-stop range))
        (step (py-range-object-step range)))
    (py-bool
     (and (integerp candidate)
          (if (> step 0)
              (and (<= start candidate) (< candidate stop))
              (and (< stop candidate) (<= candidate start)))
          (= (mod (- candidate start) step) 0)))))

(defun py-iter (obj)
  (cond
    ((py-iterator-p obj) obj)
    ((stringp obj)
     (make-py-string-iterator-object :type *py-string-iterator-type*
                                     :sequence obj
                                     :index 0))
    ((eq (py-object-type obj) *py-list-type*)
     (make-py-list-iterator-object :type *py-list-iterator-type*
                                   :sequence obj
                                   :index 0))
    ((eq (py-object-type obj) *py-tuple-type*)
     (make-py-tuple-iterator-object :type *py-tuple-iterator-type*
                                    :sequence obj
                                    :index 0))
    ((eq (py-object-type obj) *py-range-type*)
     (make-py-range-iterator-object :type *py-range-iterator-type*
                                    :range obj
                                    :index 0))
    (t
     (error "Python object of type ~A is not iterable"
            (if (py-object-p obj)
                (py-type-name (py-object-type obj))
                (type-of obj))))))

(defun py-reversed (obj)
  (cond
    ((stringp obj)
     (make-py-string-reverse-iterator-object
      :type *py-string-reverse-iterator-type*
      :sequence obj
      :index (1- (length obj))))
    ((py-list-object-p obj)
     (make-py-list-reverse-iterator-object
      :type *py-list-reverse-iterator-type*
      :sequence obj
      :index (1- (or (py-object-size obj) 0))))
    ((py-tuple-object-p obj)
     (make-py-tuple-reverse-iterator-object
      :type *py-tuple-reverse-iterator-type*
      :sequence obj
      :index (1- (or (py-object-size obj) 0))))
    ((py-range-object-p obj)
     (let ((length (py-range-object-length obj)))
       (make-py-range-iterator-object
        :type *py-range-iterator-type*
        :range (if (= length 0)
                   (py-range 0)
                   (py-range (py-range-item obj (1- length))
                             (- (py-range-object-start obj)
                                (py-range-object-step obj))
                             (- (py-range-object-step obj))))
        :index 0)))
    (t
     (error "Python object of type ~A is not reversible"
            (if (py-object-p obj)
                (py-type-name (py-object-type obj))
                (type-of obj))))))

(defun py-extreme (operation args)
  (let ((best nil)
        (found nil)
        (iterator nil))
    (labels ((consider (item)
               (unless found
                 (setf best item)
                 (setf found t)
                 (return-from consider))
               (when (py-truthy-p
                      (ecase operation
                        (:min (py-lt item best))
                        (:max (py-gt item best))))
                 (setf best item))))
      (cond
        ((null args)
         (error "~A expected at least 1 argument, got 0"
                (if (eq operation :min) "min" "max")))
        ((null (rest args))
         (setf iterator (py-iter (first args)))
         (loop
           (multiple-value-bind (item item-found) (py-next-item iterator)
             (unless item-found
               (return))
             (consider item))))
        (t
         (dolist (item args)
           (consider item)))))
    (unless found
      (error "~A() iterable argument is empty"
             (if (eq operation :min) "min" "max")))
    best))

(defun py-min (&rest args)
  (py-extreme :min args))

(defun py-max (&rest args)
  (py-extreme :max args))

(defun py-sum (iterable &optional (start 0))
  (when (stringp start)
    (error "sum() can't sum strings [use ''.join(seq) instead]"))
  (let ((result start)
        (iterator (py-iter iterable)))
    (loop
      (multiple-value-bind (item found) (py-next-item iterator)
        (unless found
          (return result))
        (setf result (py-add result item))))))

(defun py-sorted (iterable)
  (let ((result (make-py-list))
        (iterator (py-iter iterable)))
    (loop
      (multiple-value-bind (item found) (py-next-item iterator)
        (unless found
          (return))
        (py-append result item)))
    (py-call-attr result "sort")
    result))

(defun py-list (&optional (iterable *py-none*))
  (let ((result (make-py-list)))
    (unless (eq iterable *py-none*)
      (py-list-extend-iterable result iterable))
    result))

(defun py-tuple (&optional (iterable *py-none*))
  (cond
    ((eq iterable *py-none*)
     (make-py-tuple))
    ((py-tuple-object-p iterable)
     iterable)
    (t
     (let ((items '())
           (iterator (py-iter iterable)))
       (loop
         (multiple-value-bind (item found) (py-next-item iterator)
           (unless found
             (return))
           (push item items)))
       (apply #'make-py-tuple (nreverse items))))))

(defun py-all (iterable)
  (let ((iterator (py-iter iterable)))
    (loop
      (multiple-value-bind (item found) (py-next-item iterator)
        (unless found
          (return *py-true*))
        (unless (py-truthy-p item)
          (return *py-false*))))))

(defun py-any (iterable)
  (let ((iterator (py-iter iterable)))
    (loop
      (multiple-value-bind (item found) (py-next-item iterator)
        (unless found
          (return *py-false*))
        (when (py-truthy-p item)
          (return *py-true*))))))

(defun py-next (iterator)
  (cond
    ((py-forward-list-iterator-p iterator)
     (let* ((sequence (py-list-iterator-object-sequence iterator))
            (index (py-list-iterator-object-index iterator))
            (size (or (py-object-size sequence) 0)))
       (if (and (>= index 0) (< index size))
           (prog1
               (aref (py-object-value sequence) index)
             (setf (py-list-iterator-object-index iterator) (1+ index)))
           (progn
             (setf (py-list-iterator-object-index iterator) -1)
             (py-raise *py-stop-iteration*)))))
    ((py-reverse-list-iterator-p iterator)
     (let* ((sequence (py-list-reverse-iterator-object-sequence iterator))
            (index (py-list-reverse-iterator-object-index iterator))
            (size (or (py-object-size sequence) 0)))
       (if (and (>= index 0) (< index size))
           (prog1
               (aref (py-object-value sequence) index)
             (setf (py-list-reverse-iterator-object-index iterator) (1- index)))
           (progn
             (setf (py-list-reverse-iterator-object-index iterator) -1)
             (py-raise *py-stop-iteration*)))))
    ((py-string-iterator-p iterator)
     (let* ((sequence (py-string-iterator-object-sequence iterator))
            (index (py-string-iterator-object-index iterator))
            (size (length sequence)))
       (if (and (>= index 0) (< index size))
           (prog1
               (subseq sequence index (1+ index))
             (setf (py-string-iterator-object-index iterator) (1+ index)))
           (progn
             (setf (py-string-iterator-object-index iterator) -1)
             (py-raise *py-stop-iteration*)))))
    ((py-reverse-string-iterator-p iterator)
     (let* ((sequence (py-string-reverse-iterator-object-sequence iterator))
            (index (py-string-reverse-iterator-object-index iterator))
            (size (length sequence)))
       (if (and (>= index 0) (< index size))
           (prog1
               (subseq sequence index (1+ index))
             (setf (py-string-reverse-iterator-object-index iterator) (1- index)))
           (progn
             (setf (py-string-reverse-iterator-object-index iterator) -1)
             (py-raise *py-stop-iteration*)))))
    ((py-tuple-iterator-p iterator)
     (let* ((sequence (py-tuple-iterator-object-sequence iterator))
            (index (py-tuple-iterator-object-index iterator))
            (size (or (py-object-size sequence) 0)))
       (if (and (>= index 0) (< index size))
           (prog1
               (aref (py-object-value sequence) index)
             (setf (py-tuple-iterator-object-index iterator) (1+ index)))
           (progn
             (setf (py-tuple-iterator-object-index iterator) -1)
             (py-raise *py-stop-iteration*)))))
    ((py-reverse-tuple-iterator-p iterator)
     (let* ((sequence (py-tuple-reverse-iterator-object-sequence iterator))
            (index (py-tuple-reverse-iterator-object-index iterator))
            (size (or (py-object-size sequence) 0)))
       (if (and (>= index 0) (< index size))
           (prog1
               (aref (py-object-value sequence) index)
             (setf (py-tuple-reverse-iterator-object-index iterator) (1- index)))
           (progn
             (setf (py-tuple-reverse-iterator-object-index iterator) -1)
             (py-raise *py-stop-iteration*)))))
    ((py-enumerate-object-p iterator)
     (let* ((index (py-enumerate-object-index iterator))
            (next-item (py-next (py-enumerate-object-iterator iterator)))
            (result (make-py-tuple index next-item)))
       (setf (py-enumerate-object-index iterator) (1+ index))
       (setf (py-enumerate-object-result iterator) result)
       result))
    ((py-zip-object-p iterator)
     (let* ((iterators (py-zip-object-iterators iterator))
            (iterator-count (or (py-object-size iterators) 0)))
       (when (= iterator-count 0)
         (py-raise *py-stop-iteration*))
       (let ((items
               (loop for index from 0 below iterator-count
                     collect (py-next (aref (py-object-value iterators) index)))))
         (let ((result (apply #'make-py-tuple items)))
           (setf (py-zip-object-result iterator) result)
           result))))
    ((py-filter-object-p iterator)
     (loop
       (let* ((item (py-next (py-filter-object-iterator iterator)))
              (predicate (py-filter-object-predicate iterator))
              (result (if (eq predicate *py-none*)
                          item
                          (py-invoke-callable predicate item))))
         (when (py-truthy-p result)
           (return item)))))
    ((py-range-iterator-p iterator)
     (let* ((range (py-range-iterator-object-range iterator))
            (index (py-range-iterator-object-index iterator))
            (length (py-range-object-length range)))
       (if (< index length)
           (prog1
               (py-range-item range index)
             (setf (py-range-iterator-object-index iterator) (1+ index)))
           (py-raise *py-stop-iteration*))))
    (t
     (error "Expected Python iterator, got ~S" iterator))))

(defun py-next-item (iterator)
  (handler-case
      (values (py-next iterator) t)
    (py-exception (condition)
      (if (py-stop-iteration-p condition)
          (values nil nil)
          (error condition)))))

(defun py-list-iterator-length-hint (iterator)
  (let* ((sequence (py-list-iterator-object-sequence iterator))
         (index (py-list-iterator-object-index iterator))
         (length-remaining (if (>= index 0)
                               (- (or (py-object-size sequence) 0) index)
                               0)))
    (max length-remaining 0)))

(defun py-list-reverse-iterator-length-hint (iterator)
  (let* ((sequence (py-list-reverse-iterator-object-sequence iterator))
         (index (py-list-reverse-iterator-object-index iterator))
         (length-remaining (1+ index))
         (size (or (py-object-size sequence) 0)))
    (if (or (< length-remaining 0) (< size length-remaining))
        0
        length-remaining)))

(defun py-string-iterator-length-hint (iterator)
  (let* ((sequence (py-string-iterator-object-sequence iterator))
         (index (py-string-iterator-object-index iterator))
         (length-remaining (if (>= index 0)
                               (- (length sequence) index)
                               0)))
    (max length-remaining 0)))

(defun py-string-reverse-iterator-length-hint (iterator)
  (let* ((sequence (py-string-reverse-iterator-object-sequence iterator))
         (index (py-string-reverse-iterator-object-index iterator))
         (length-remaining (1+ index))
         (size (length sequence)))
    (if (or (< length-remaining 0) (< size length-remaining))
        0
        length-remaining)))

(defun py-tuple-iterator-length-hint (iterator)
  (let* ((sequence (py-tuple-iterator-object-sequence iterator))
         (index (py-tuple-iterator-object-index iterator))
         (length-remaining (if (>= index 0)
                               (- (or (py-object-size sequence) 0) index)
                               0)))
    (max length-remaining 0)))

(defun py-tuple-reverse-iterator-length-hint (iterator)
  (let* ((sequence (py-tuple-reverse-iterator-object-sequence iterator))
         (index (py-tuple-reverse-iterator-object-index iterator))
         (length-remaining (1+ index))
         (size (or (py-object-size sequence) 0)))
    (if (or (< length-remaining 0) (< size length-remaining))
        0
        length-remaining)))

(defun py-range-iterator-length-hint (iterator)
  (let* ((range (py-range-iterator-object-range iterator))
         (index (py-range-iterator-object-index iterator)))
    (max (- (py-range-object-length range) index) 0)))

(setf (py-type-attr *py-list-type* "__iter__")
      (lambda (obj)
        (py-iter obj)))

(setf (py-type-attr *py-list-iterator-type* "__iter__")
      (lambda (iterator)
        (py-iter iterator)))

(setf (py-type-attr *py-list-iterator-type* "__next__")
      (lambda (iterator)
        (py-next iterator)))

(setf (py-type-attr *py-list-iterator-type* "__length_hint__")
      (lambda (iterator)
        (py-list-iterator-length-hint iterator)))

(setf (py-type-attr *py-list-type* "__reversed__")
      (lambda (obj)
        (py-reversed obj)))

(setf (py-type-attr *py-list-reverse-iterator-type* "__iter__")
      (lambda (iterator)
        (py-iter iterator)))

(setf (py-type-attr *py-list-reverse-iterator-type* "__next__")
      (lambda (iterator)
        (py-next iterator)))

(setf (py-type-attr *py-list-reverse-iterator-type* "__length_hint__")
      (lambda (iterator)
        (py-list-reverse-iterator-length-hint iterator)))

(setf (py-type-attr *py-string-iterator-type* "__iter__")
      (lambda (iterator)
        (py-iter iterator)))

(setf (py-type-attr *py-string-iterator-type* "__next__")
      (lambda (iterator)
        (py-next iterator)))

(setf (py-type-attr *py-string-iterator-type* "__length_hint__")
      (lambda (iterator)
        (py-string-iterator-length-hint iterator)))

(setf (py-type-attr *py-string-reverse-iterator-type* "__iter__")
      (lambda (iterator)
        (py-iter iterator)))

(setf (py-type-attr *py-string-reverse-iterator-type* "__next__")
      (lambda (iterator)
        (py-next iterator)))

(setf (py-type-attr *py-string-reverse-iterator-type* "__length_hint__")
      (lambda (iterator)
        (py-string-reverse-iterator-length-hint iterator)))

(setf (py-type-attr *py-tuple-type* "__iter__")
      (lambda (obj)
        (py-iter obj)))

(setf (py-type-attr *py-tuple-iterator-type* "__iter__")
      (lambda (iterator)
        (py-iter iterator)))

(setf (py-type-attr *py-tuple-iterator-type* "__next__")
      (lambda (iterator)
        (py-next iterator)))

(setf (py-type-attr *py-tuple-iterator-type* "__length_hint__")
      (lambda (iterator)
        (py-tuple-iterator-length-hint iterator)))

(setf (py-type-attr *py-tuple-reverse-iterator-type* "__iter__")
      (lambda (iterator)
        (py-iter iterator)))

(setf (py-type-attr *py-tuple-reverse-iterator-type* "__next__")
      (lambda (iterator)
        (py-next iterator)))

(setf (py-type-attr *py-tuple-reverse-iterator-type* "__length_hint__")
      (lambda (iterator)
        (py-tuple-reverse-iterator-length-hint iterator)))

(setf (py-type-attr *py-enumerate-type* "__iter__")
      (lambda (iterator)
        (py-iter iterator)))

(setf (py-type-attr *py-enumerate-type* "__next__")
      (lambda (iterator)
        (py-next iterator)))

(setf (py-type-attr *py-zip-type* "__iter__")
      (lambda (iterator)
        (py-iter iterator)))

(setf (py-type-attr *py-zip-type* "__next__")
      (lambda (iterator)
        (py-next iterator)))

(setf (py-type-attr *py-filter-type* "__iter__")
      (lambda (iterator)
        (py-iter iterator)))

(setf (py-type-attr *py-filter-type* "__next__")
      (lambda (iterator)
        (py-next iterator)))

(setf (py-type-attr *py-range-type* "__iter__")
      (lambda (obj)
        (py-iter obj)))

(setf (py-type-attr *py-range-type* "__len__")
      (lambda (obj)
        (py-range-object-length obj)))

(setf (py-type-attr *py-range-type* "__contains__")
      (lambda (obj value)
        (py-range-contains obj value)))

(setf (py-type-attr *py-range-type* "__getitem__")
      (lambda (obj index)
        (if (py-slice-object-p index)
            (py-range-slice obj index)
            (py-range-item obj (py-range-normalized-index obj index)))))

(setf (py-type-attr *py-range-type* "__reversed__")
      (lambda (obj)
        (py-reversed obj)))

(setf (py-type-attr *py-range-type* "count")
      (lambda (obj value)
        (if (py-truthy-p (py-range-contains obj value)) 1 0)))

(setf (py-type-attr *py-range-type* "index")
      (lambda (obj value)
        (unless (py-truthy-p (py-range-contains obj value))
          (error "~S is not in range" value))
        (floor (- (py-normalize-bool-number value)
                  (py-range-object-start obj))
               (py-range-object-step obj))))

(setf (py-type-attr *py-range-iterator-type* "__iter__")
      (lambda (iterator)
        (py-iter iterator)))

(setf (py-type-attr *py-range-iterator-type* "__next__")
      (lambda (iterator)
        (py-next iterator)))

(setf (py-type-attr *py-range-iterator-type* "__length_hint__")
      (lambda (iterator)
        (py-range-iterator-length-hint iterator)))

(defun py-string-repr (value stream)
  (princ "'" stream)
  (loop for char across value
        do (case char
             (#\\ (princ "\\\\" stream))
             (#\' (princ "\\'" stream))
             (#\Newline (princ "\\n" stream))
             (otherwise (princ char stream))))
  (princ "'" stream))

(defun py-repr (value &optional (stream *standard-output*))
  (cond
    ((stringp value) (py-string-repr value stream))
    ((py-list-object-p value)
     (princ "[" stream)
     (loop for index from 0 below (or (py-object-size value) 0)
           do (progn
                (when (> index 0)
                  (princ ", " stream))
                (py-repr (aref (py-object-value value) index) stream)))
     (princ "]" stream))
    ((py-tuple-object-p value)
     (princ "(" stream)
     (loop for index from 0 below (or (py-object-size value) 0)
           do (progn
                (when (> index 0)
                  (princ ", " stream))
                (py-repr (aref (py-object-value value) index) stream)))
     (when (= (or (py-object-size value) 0) 1)
       (princ "," stream))
     (princ ")" stream))
    ((py-range-object-p value)
     (if (= (py-range-object-step value) 1)
         (format stream "range(~A, ~A)"
                 (py-range-object-start value)
                 (py-range-object-stop value))
         (format stream "range(~A, ~A, ~A)"
                 (py-range-object-start value)
                 (py-range-object-stop value)
                 (py-range-object-step value))))
    (t (py-display value stream))))

(defun py-display (value &optional (stream *standard-output*))
  (cond
    ((eq value *py-none*) (princ "None" stream))
    ((eq value *py-true*) (princ "True" stream))
    ((eq value *py-false*) (princ "False" stream))
    ((stringp value) (princ value stream))
    ((py-stop-iteration-p value) (princ "StopIteration" stream))
    ((py-forward-list-iterator-p value) (princ "<list_iterator>" stream))
    ((py-reverse-list-iterator-p value) (princ "<list_reverseiterator>" stream))
    ((py-string-iterator-p value) (princ "<str_iterator>" stream))
    ((py-reverse-string-iterator-p value) (princ "<reversed>" stream))
    ((py-tuple-iterator-p value) (princ "<tuple_iterator>" stream))
    ((py-reverse-tuple-iterator-p value) (princ "<reversed>" stream))
    ((py-enumerate-object-p value) (princ "<enumerate>" stream))
    ((py-zip-object-p value) (princ "<zip>" stream))
    ((py-filter-object-p value) (princ "<filter>" stream))
    ((py-range-object-p value) (py-repr value stream))
    ((py-range-iterator-p value) (princ "<range_iterator>" stream))
    ((py-list-object-p value) (py-repr value stream))
    ((py-tuple-object-p value) (py-repr value stream))
    (t (princ value stream))))

(defun py-append (obj value)
  (py-call-attr obj "append" value))

(defun py-insert (obj index value)
  (py-call-attr obj "insert" index value))

(defun py-pop (obj &optional (index -1))
  (py-call-attr obj "pop" index))

(defun py-getitem (obj index)
  (if (stringp obj)
      (py-string-getitem obj index)
      (py-call-attr obj "__getitem__" index)))

(defun py-setitem (obj index value)
  (py-call-attr obj "__setitem__" index value))

(defun py-delitem (obj index)
  (py-call-attr obj "__delitem__" index))
