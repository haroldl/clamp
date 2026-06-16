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
   :py-module-object
   :py-module-object-name
   :py-module-object-source-path
   :py-module-object-package-name
   :*py-current-module*
   :*py-module-search-paths*
   :*py-module-loader*
   :py-enter-module
   :py-set-global
   :py-import-builtin
   :py-import-name
   :py-import-from
   :py-import-star
   :py-type-of
   :py-id
   :py-callable
   :py-isinstance
   :make-py-callable
   :py-callable-name
   :py-callable-fn
   :py-callable-binding-kind
   :py-callable-owner-type
   :py-string-iterator-object
   :py-string-reverse-iterator-object
   :py-bytes-iterator-object
   :py-tuple-iterator-object
   :py-tuple-reverse-iterator-object
   :py-enumerate-object
   :py-zip-object
   :py-filter-object
   :py-map-object
   :py-path-object
   :py-bytes-object
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
   :*py-not-implemented*
   :py-bool
   :py-truthy-p
   :py-and
   :py-or
   :py-len
   :py-hash
   :py-reversed
   :py-callable
   :py-min
   :py-max
   :py-sum
   :py-sorted
   :py-list
   :py-tuple
   :py-round
   :py-bin
   :py-oct
   :py-hex
   :py-chr
   :py-ord
   :py-add
   :py-iadd
   :py-mul
   :py-imul
   :py-pow
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
   :py-invert
   :py-not
   :py-str
   :py-ascii
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
   :py-map
   :py-range
   :py-all
   :py-any
   :py-iter
   :py-next
   :py-next-item
   :make-py-list
   :make-py-dict-from-pairs
   :make-py-tuple
   :py-append
   :py-insert
   :py-pop
   :py-getitem
   :py-setitem
   :py-delitem))

(in-package "CLAMP.__CLAMP_INTERNALS__")


(sb-alien:load-shared-object "libm.so.6")
(sb-alien:define-alien-routine ("cbrt" c-cbrt) sb-alien:double (x sb-alien:double))
(sb-alien:define-alien-routine ("erf" c-erf) sb-alien:double (x sb-alien:double))
(sb-alien:define-alien-routine ("erfc" c-erfc) sb-alien:double (x sb-alien:double))
(sb-alien:define-alien-routine ("expm1" c-expm1) sb-alien:double (x sb-alien:double))
(sb-alien:define-alien-routine ("fma" c-fma) sb-alien:double (x sb-alien:double) (y sb-alien:double) (z sb-alien:double))
(sb-alien:define-alien-routine ("lgamma" c-lgamma) sb-alien:double (x sb-alien:double))
(sb-alien:define-alien-routine ("nextafter" c-nextafter) sb-alien:double (x sb-alien:double) (y sb-alien:double))
(sb-alien:define-alien-routine ("tgamma" c-tgamma) sb-alien:double (x sb-alien:double))

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
  (flags 0)
  number-bool-fn
  mapping-length-fn
  sequence-length-fn)

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

(defparameter *py-not-implemented-type*
  (make-py-type :type *py-type-type*
                :name "NotImplementedType"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-bool-type*
  (make-py-type :type *py-type-type*
                :name "bool"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-int-type*
  (make-py-type :type *py-type-type*
                :name "int"
                :bases (list *py-object-type*)
                :basicsize 1))

(setf (py-type-bases *py-bool-type*) (list *py-int-type*))

(defparameter *py-float-type*
  (make-py-type :type *py-type-type*
                :name "float"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-str-type*
  (make-py-type :type *py-type-type*
                :name "str"
                :bases (list *py-object-type*)
                :basicsize 1
                :sequence-length-fn #'length))

(defparameter *py-bytes-type*
  (make-py-type :type *py-type-type*
                :name "bytes"
                :bases (list *py-object-type*)
                :basicsize 1
                :itemsize 1
                :sequence-length-fn
                (lambda (obj)
                  (or (py-object-size obj) 0))))

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

(defparameter *py-module-type*
  (make-py-type :type *py-type-type*
                :name "module"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-module-spec-type*
  (make-py-type :type *py-type-type*
                :name "ModuleSpec"
                :bases (list *py-object-type*)
                :basicsize 1))

(setf (gethash "__module__" (py-object-attrs *py-module-spec-type*)) "_frozen_importlib")

(defparameter *py-source-file-loader-type*
  (make-py-type :type *py-type-type*
                :name "SourceFileLoader"
                :bases (list *py-object-type*)
                :basicsize 1))

(setf (gethash "__module__" (py-object-attrs *py-source-file-loader-type*)) "_frozen_importlib_external")

(defparameter *py-buffered-reader-type*
  (make-py-type :type *py-type-type*
                :name "BufferedReader"
                :bases (list *py-object-type*)
                :basicsize 1))

(setf (gethash "__module__" (py-object-attrs *py-buffered-reader-type*)) "_io")

(defparameter *py-file-reader-type*
  (make-py-type :type *py-type-type*
                :name "FileReader"
                :bases (list *py-object-type*)
                :basicsize 1))

(setf (gethash "__module__" (py-object-attrs *py-file-reader-type*)) "importlib.resources.readers")

(defparameter *py-path-type*
  (make-py-type :type *py-type-type*
                :name "PosixPath"
                :bases (list *py-object-type*)
                :basicsize 1))

(setf (gethash "__module__" (py-object-attrs *py-path-type*)) "pathlib")

(defparameter *py-none*
  (make-py-object :type *py-none-type* :value nil))

(defparameter *py-not-implemented*
  (make-py-object :type *py-not-implemented-type* :value nil))

(setf (gethash "__hash__" (py-type-attrs *py-module-spec-type*)) *py-none*)

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

(defun py-type-slot-truth (type value)
  (let ((number-bool (py-type-number-bool-fn type))
        (mapping-length (py-type-mapping-length-fn type))
        (sequence-length (py-type-sequence-length-fn type)))
    (cond
      (number-bool (> (funcall number-bool value) 0))
      (mapping-length (> (funcall mapping-length value) 0))
      (sequence-length (> (funcall sequence-length value) 0))
      (t t))))

(defun py-type-slot-length (type value)
  (let ((sequence-length (py-type-sequence-length-fn type))
        (mapping-length (py-type-mapping-length-fn type)))
    (cond
      (sequence-length (funcall sequence-length value))
      (mapping-length (funcall mapping-length value))
      (t nil))))

(defun py-truthy-p (value)
  (cond
    ((eq value *py-true*) t)
    ((or (eq value *py-false*) (eq value *py-none*)) nil)
    ((py-object-p value) (py-type-slot-truth (py-object-type value) value))
    ((numberp value) (not (zerop value)))
    ((stringp value) (py-type-slot-truth *py-str-type* value))
    ((null value) nil)
    (t t)))

(defun py-len (value)
  (let ((length
          (cond
            ((py-object-p value)
             (py-type-slot-length (py-object-type value) value))
            ((stringp value)
             (py-type-slot-length *py-str-type* value))
            (t nil))))
    (if length
        length
        (error "Python object of type ~A has no len()"
               (if (py-object-p value)
                   (py-type-name (py-object-type value))
                   (type-of value))))))

(defun py-type-of (value)
  (cond
    ((py-module-object-p value) *py-module-type*)
    ((py-object-p value) (py-object-type value))
    ((integerp value) *py-int-type*)
    ((floatp value) *py-float-type*)
    ((stringp value) *py-str-type*)
    (t
     (error "Python object type for ~S is not modeled by Clamp yet" value))))

(defvar *py-identity-next-id* 1)
(defvar *py-identities* (make-hash-table :test #'eq))

(defun py-id (value)
  (multiple-value-bind (id found) (gethash value *py-identities*)
    (if found
        id
        (let ((new-id *py-identity-next-id*))
          (incf *py-identity-next-id*)
          (setf (gethash value *py-identities*) new-id)
          new-id))))

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

(defun py-string-hash (value)
  (py-int-hash (py-signed-hash (sxhash value))))

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
      ((eq value *py-not-implemented*) #xFBA98765)
      ((integerp normalized-value)
       (py-int-hash normalized-value))
      ((stringp value)
       (py-string-hash value))
      ((py-tuple-object-p value)
       (py-tuple-hash value))
      ((py-range-object-p value)
       (py-range-hash value))
      ((py-source-file-loader-object-p value)
       (py-source-file-loader-hash value))
      ((py-module-spec-object-p value)
       (error "unhashable type: 'ModuleSpec'"))
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

(defun py-int-bit-length (value)
  (let ((normalized-value (py-normalize-bool-number value)))
    (unless (integerp normalized-value)
      (error "int.bit_length() expected an integer, got ~S" value))
    (integer-length (abs normalized-value))))

(defun py-int-bit-count (value)
  (let ((normalized-value (py-normalize-bool-number value)))
    (unless (integerp normalized-value)
      (error "int.bit_count() expected an integer, got ~S" value))
    (logcount (abs normalized-value))))

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

(defun py-range-eq (left right)
  (let ((left-length (py-range-object-length left))
        (right-length (py-range-object-length right)))
    (and (= left-length right-length)
         (or (= left-length 0)
             (and (= (py-range-object-start left)
                     (py-range-object-start right))
                  (or (= left-length 1)
                      (= (py-range-object-step left)
                         (py-range-object-step right))))))))

(defun py-object-attrs-eq (left right)
  (let ((left-attrs (py-object-attrs left))
        (right-attrs (py-object-attrs right)))
    (and (= (hash-table-count left-attrs)
            (hash-table-count right-attrs))
         (loop for key being the hash-keys of left-attrs
                 using (hash-value left-value)
               always (multiple-value-bind (right-value found)
                          (gethash key right-attrs)
                        (and found
                             (py-truthy-p
                              (py-eq left-value right-value))))))))

(defun py-source-file-loader-eq (left right)
  (and (py-source-file-loader-object-p right)
       (eq (py-object-type left) (py-object-type right))
       (py-object-attrs-eq left right)))

(defun py-source-file-loader-hash (loader)
  (py-int-hash
   (py-signed-hash
    (logxor (py-uhash (py-hash (py-source-file-loader-object-name loader)))
            (py-uhash (py-hash (py-source-file-loader-object-path loader)))))))

(defun py-spec-field-eq (left right reader)
  (py-truthy-p
   (py-eq (or (funcall reader left) *py-none*)
          (or (funcall reader right) *py-none*))))

(defun py-module-spec-eq (left right)
  (and (py-module-spec-object-p right)
       (py-spec-field-eq left right #'py-module-spec-object-name)
       (py-spec-field-eq left right #'py-module-spec-object-loader)
       (py-spec-field-eq left right #'py-module-spec-object-origin)
       (py-spec-field-eq left right
                         #'py-module-spec-object-submodule-search-locations)
       (py-spec-field-eq left right #'py-module-spec-object-cached)
       (eq (py-bool (py-module-spec-object-has-location left))
           (py-bool (py-module-spec-object-has-location right)))))

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
       ((and (py-range-object-p left) (py-range-object-p right))
        (py-range-eq left right))
       ((py-module-spec-object-p left)
        (py-module-spec-eq left right))
       ((py-source-file-loader-object-p left)
        (py-source-file-loader-eq left right))
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
    ((py-dict-object-p container)
     (multiple-value-bind (value found)
         (gethash item (py-dict-storage container "__contains__"))
       (declare (ignore value))
       (py-bool found)))
    ((stringp container)
     (unless (stringp item)
       (error "'in <string>' requires string as left operand, got ~S" item))
     (py-bool (search item container)))
    ((py-iterator-p container)
     (loop
       (multiple-value-bind (value found) (py-next-item container)
         (unless found
           (return *py-false*))
         (when (py-truthy-p (py-eq value item))
           (return *py-true*)))))
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

(defun py-round (value &optional (ndigits *py-none*))
  (if (eq ndigits *py-none*)
      (py-call-attr value "__round__")
      (error "round() with ndigits is not supported by Clamp yet")))

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

(defun py-invert (value)
  (let ((normalized-value (py-normalize-bool-number value)))
    (if (integerp normalized-value)
        (- (1+ normalized-value))
        (error "bad operand type for unary ~: ~S" value))))

(defun py-not (value)
  (py-bool (not (py-truthy-p value))))

(defun py-str (value)
  (cond
    ((stringp value) value)
    (t
     (with-output-to-string (stream)
       (py-display value stream)))))

(defun py-ascii-escape-char (char stream)
  (let ((code (char-code char)))
    (cond
      ((<= code #x7f)
       (write-char char stream))
      ((<= code #xff)
       (format stream "\\x~A" (string-downcase (format nil "~2,'0x" code))))
      ((<= code #xffff)
       (format stream "\\u~A" (string-downcase (format nil "~4,'0x" code))))
      (t
       (format stream "\\U~A" (string-downcase (format nil "~8,'0x" code)))))))

(defun py-ascii (value)
  (let ((repr (with-output-to-string (stream)
                (py-repr value stream))))
    (with-output-to-string (stream)
      (loop for char across repr
            do (py-ascii-escape-char char stream)))))

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

(defun py-sync-object-attr (obj name value)
  (when (py-module-spec-object-p obj)
    (cond
      ((string= name "name")
       (setf (py-module-spec-object-name obj) value))
      ((string= name "loader")
       (setf (py-module-spec-object-loader obj) value))
      ((string= name "loader_state")
       (setf (py-module-spec-object-loader-state obj) value))
      ((string= name "origin")
       (setf (py-module-spec-object-origin obj)
             (unless (eq value *py-none*) value)))
      ((string= name "submodule_search_locations")
       (setf (py-module-spec-object-submodule-search-locations obj) value))
      ((or (string= name "cached") (string= name "_cached"))
       (setf (py-module-spec-object-cached obj) value)
       (setf (gethash "cached" (py-object-attrs obj)) value)
       (setf (gethash "_cached" (py-object-attrs obj)) value))
      ((or (string= name "has_location") (string= name "_set_fileattr"))
       (let ((truth-value (py-bool (py-truthy-p value))))
         (setf (py-module-spec-object-set-fileattr obj)
               (py-truthy-p value))
         (setf (py-module-spec-object-has-location obj)
               (py-truthy-p value))
         (setf (gethash "has_location" (py-object-attrs obj)) truth-value)
         (setf (gethash "_set_fileattr" (py-object-attrs obj)) truth-value)))))
  (when (py-source-file-loader-object-p obj)
    (cond
      ((string= name "name")
       (setf (py-source-file-loader-object-name obj) value))
      ((string= name "path")
       (setf (py-source-file-loader-object-path obj) value)))
    (let ((dict (py-source-file-loader-object-namespace-dict obj)))
      (when (and dict
                 (not (string= name "__dict__"))
                 (not (py-dict-has-key-p dict name)))
        (vector-push-extend name (py-dict-object-keys dict))
        (setf (py-object-size dict) (hash-table-count (py-object-attrs obj))))))
  (when (py-file-reader-object-p obj)
    (when (string= name "path")
      (setf (py-file-reader-object-path obj) value))
    (let ((dict (py-file-reader-object-namespace-dict obj)))
      (when (and dict
                 (not (string= name "__dict__"))
                 (not (py-dict-has-key-p dict name)))
        (vector-push-extend name (py-dict-object-keys dict))
        (setf (py-object-size dict) (hash-table-count (py-object-attrs obj))))))
  (when (and (py-module-object-p obj)
             (not (string= name "__dict__")))
    (py-module-dict-note-key obj name)))

(defun (setf py-object-attr) (value obj name)
  (setf (gethash name (py-object-attrs obj)) value)
  (py-sync-object-attr obj name value)
  value)

(defvar *py-current-module* nil)
(defvar *py-module-search-paths* nil)
(defvar *py-module-loader* nil)
(defvar *py-sys-modules* (make-hash-table :test #'equal))
(defvar *py-builtin-module-builders* (make-hash-table :test #'equal))

(defstruct (py-module-spec-object (:include py-object))
  name
  loader
  loader-state
  origin
  cached
  set-fileattr
  submodule-search-locations
  (has-location nil)
  (initializing nil)
  (uninitialized-submodules '()))

(defstruct (py-source-file-loader-object (:include py-object))
  name
  path
  namespace-dict)

(defstruct (py-file-reader-object (:include py-object))
  path
  namespace-dict)

(defstruct (py-path-object (:include py-object))
  path)

(defun py-path-string (path)
  (if (py-path-object-p path)
      (py-path-object-path path)
      path))

(defun py-path-name (path)
  (py-directory-entry-name (py-path-string path)))

(defun make-py-path (path)
  (let* ((path-string (py-path-string path))
         (obj (make-py-path-object :type *py-path-type*
                                   :path path-string)))
    (setf (py-object-attr obj "name") (py-path-name obj))
    obj))

(defstruct (py-buffered-reader-object (:include py-object))
  data
  path
  (position 0)
  (closed nil))

(defun py-module-spec-parent (spec)
  (let ((name (gethash "name" (py-object-attrs spec)))
        (submodule-search-locations
          (gethash "submodule_search_locations" (py-object-attrs spec))))
    (if (eq submodule-search-locations *py-none*)
        (let ((pos (position #\. name :from-end t)))
          (if pos (subseq name 0 pos) ""))
        name)))

(defun py-module-spec-cached (spec)
  (let ((cached (py-module-spec-object-cached spec)))
    (when (and (eq cached *py-none*)
               (py-module-spec-object-origin spec)
               (py-module-spec-object-set-fileattr spec))
      (setf cached (or (py-source-cache-path
                        (py-module-spec-object-origin spec))
                       *py-none*))
      (setf (py-module-spec-object-cached spec) cached)
      (setf (gethash "cached" (py-object-attrs spec)) cached)
      (setf (gethash "_cached" (py-object-attrs spec)) cached))
    cached))

(defun py-module-package-name (name)
  (concatenate 'string "CLAMP.__module__." name))

(defun make-clamp-source-file-loader (name source-path)
  (let ((loader (make-py-source-file-loader-object
                 :type *py-source-file-loader-type*
                 :name name
                 :path source-path)))
    (setf (py-object-attr loader "name") name)
    (setf (py-object-attr loader "path") source-path)
    loader))

(defun py-file-reader-loader-directory (loader)
  (py-package-source-directory
   (py-path-string
    (if (py-source-file-loader-object-p loader)
        (py-source-file-loader-object-path loader)
        (py-lookup-attr loader "path")))))

(defun py-file-reader-init-from-loader (reader loader)
  (let ((path (make-py-path (py-file-reader-loader-directory loader))))
    (setf (py-file-reader-object-path reader) path)
    (setf (py-object-attr reader "path") path))
  *py-none*)

(defun make-clamp-file-reader (loader)
  (let ((reader (make-py-file-reader-object
                 :type *py-file-reader-type*)))
    (py-file-reader-init-from-loader reader loader)
    reader))

(defun py-file-reader-resource-path (reader resource)
  (namestring (merge-pathnames resource
                               (uiop:ensure-directory-pathname
                                (py-path-string
                                 (py-file-reader-object-path reader))))))

(defun py-directory-entry-name (path)
  (let ((file-name (file-namestring path)))
    (if (> (length file-name) 0)
        file-name
        (let ((directory (pathname-directory
                          (uiop:ensure-directory-pathname path))))
          (first (last directory))))))

(defun py-file-reader-contents (reader)
  (let* ((directory (uiop:ensure-directory-pathname
                     (py-path-string
                      (py-file-reader-object-path reader))))
         (entries (append (uiop:directory-files directory)
                          (uiop:subdirectories directory))))
    (py-iter
     (apply (function make-py-list)
            (mapcar (function py-directory-entry-name) entries)))))

(defun py-file-reader-resource-p (reader resource)
  (let ((path (probe-file (py-file-reader-resource-path reader resource))))
    (py-bool (and path (not (uiop:directory-pathname-p path))))))

(defun py-path-joinpath (path &rest resources)
  (let ((joined (py-path-string path)))
    (dolist (resource resources)
      (setf joined
            (namestring (merge-pathnames (py-path-string resource)
                                         (uiop:ensure-directory-pathname
                                          joined)))))
    (make-py-path joined)))

(defun py-path-exists-p (path)
  (not (null (probe-file (py-path-string path)))))

(defun py-path-file-p (path)
  (let ((probe (probe-file (py-path-string path))))
    (and probe (not (uiop:directory-pathname-p probe)))))

(defun py-path-directory-p (path)
  (let ((probe (probe-file (py-path-string path))))
    (and probe (uiop:directory-pathname-p probe))))

(defun py-path-iterdir (path)
  (let* ((directory (uiop:ensure-directory-pathname (py-path-string path)))
         (entries (append (uiop:directory-files directory)
                          (uiop:subdirectories directory))))
    (py-iter
     (apply (function make-py-list)
            (mapcar (lambda (entry)
                      (make-py-path (namestring entry)))
                    entries)))))

(defun make-clamp-buffered-reader (path)
  (let ((reader (make-py-buffered-reader-object
                 :type *py-buffered-reader-type*
                 :path path
                 :data (py-read-file-bytes path))))
    (setf (py-object-attr reader "closed") *py-false*)
    (setf (py-object-attr reader "name") path)
    reader))

(defun py-buffered-reader-read (reader &optional size)
  (when (py-buffered-reader-object-closed reader)
    (error "read of closed file"))
  (let* ((storage (py-object-value (py-buffered-reader-object-data reader)))
         (storage-size (length storage))
         (position (py-buffered-reader-object-position reader))
         (normalized-size (and size
                               (not (eq size *py-none*))
                               (py-normalize-bool-number size)))
         (read-size (cond
                      ((null normalized-size)
                       (- storage-size position))
                      ((not (integerp normalized-size))
                       (error "argument should be integer or None, not ~A"
                              (py-type-name (py-type-of size))))
                      ((< normalized-size 0)
                       (- storage-size position))
                      (t
                       (min normalized-size (- storage-size position)))))
         (result-storage (make-array read-size :element-type (quote (unsigned-byte 8)))))
    (loop for offset from 0 below read-size
          do (setf (aref result-storage offset)
                   (aref storage (+ position offset))))
    (incf (py-buffered-reader-object-position reader) read-size)
    (make-py-bytes-from-vector result-storage)))

(defun py-buffered-reader-peek (reader &optional size)
  (when (py-buffered-reader-object-closed reader)
    (error "peek of closed file"))
  (let* ((storage (py-object-value (py-buffered-reader-object-data reader)))
         (storage-size (length storage))
         (position (py-buffered-reader-object-position reader))
         (normalized-size (and size
                               (not (eq size *py-none*))
                               (py-normalize-bool-number size))))
    (unless (or (null normalized-size) (integerp normalized-size))
      (error "argument should be integer or None, not ~A"
             (py-type-name (py-type-of size))))
    (let* ((remaining (- storage-size position))
           (read-size (if (and normalized-size (= normalized-size 0))
                          0
                          remaining))
           (result-storage (make-array read-size :element-type (quote (unsigned-byte 8)))))
      (loop for offset from 0 below read-size
            do (setf (aref result-storage offset)
                     (aref storage (+ position offset))))
      (make-py-bytes-from-vector result-storage))))

(defun py-buffered-reader-readline (reader &optional size)
  (when (py-buffered-reader-object-closed reader)
    (error "readline of closed file"))
  (let* ((storage (py-object-value (py-buffered-reader-object-data reader)))
         (storage-size (length storage))
         (position (py-buffered-reader-object-position reader))
         (normalized-size (and size
                               (not (eq size *py-none*))
                               (py-normalize-bool-number size)))
         (limit (cond
                  ((null normalized-size)
                   storage-size)
                  ((not (integerp normalized-size))
                   (error "argument should be integer or None, not ~A"
                          (py-type-name (py-type-of size))))
                  ((< normalized-size 0)
                   storage-size)
                  ((= normalized-size 0)
                   position)
                  (t
                   (min storage-size (+ position normalized-size))))))
    (loop while (and (< position limit)
                     (not (= (aref storage position) 10)))
          do (incf position))
    (when (< position limit)
      (incf position))
    (let* ((start (py-buffered-reader-object-position reader))
           (read-size (- position start))
           (result-storage (make-array read-size :element-type (quote (unsigned-byte 8)))))
      (loop for offset from 0 below read-size
            do (setf (aref result-storage offset)
                     (aref storage (+ start offset))))
      (setf (py-buffered-reader-object-position reader) position)
      (make-py-bytes-from-vector result-storage))))

(defun py-buffered-reader-readlines (reader &optional hint)
  (let ((normalized-hint (and hint
                              (not (eq hint *py-none*))
                              (py-normalize-bool-number hint)))
        (lines '())
        (total-size 0))
    (when (and normalized-hint (not (integerp normalized-hint)))
      (error "integer argument expected, got ~A" (py-type-name (py-type-of hint))))
    (loop
      for line = (py-buffered-reader-readline reader)
      for line-size = (or (py-object-size line) 0)
      while (> line-size 0)
      do (progn
           (push line lines)
           (incf total-size line-size)
           (when (and normalized-hint
                      (> normalized-hint 0)
                      (>= total-size normalized-hint))
             (return))))
    (apply (function make-py-list) (nreverse lines))))

(defun py-buffered-reader-tell (reader)
  (when (py-buffered-reader-object-closed reader)
    (error "tell of closed file"))
  (py-buffered-reader-object-position reader))

(defun py-buffered-reader-seek (reader target &optional (whence 0))
  (when (py-buffered-reader-object-closed reader)
    (error "seek of closed file"))
  (let* ((storage (py-object-value (py-buffered-reader-object-data reader)))
         (storage-size (length storage))
         (normalized-target (py-normalize-bool-number target))
         (normalized-whence (py-normalize-bool-number whence)))
    (unless (integerp normalized-target)
      (error "an integer is required"))
    (unless (integerp normalized-whence)
      (error "an integer is required"))
    (unless (<= 0 normalized-whence 2)
      (error "whence value ~A unsupported" normalized-whence))
    (let ((new-position
            (case normalized-whence
              (0 normalized-target)
              (1 (+ (py-buffered-reader-object-position reader) normalized-target))
              (2 (+ storage-size normalized-target)))))
      (when (< new-position 0)
        (error "negative seek position ~A" new-position))
      (setf (py-buffered-reader-object-position reader) new-position)
      new-position)))

(defun py-source-file-loader-check-name (loader fullname)
  (let ((name (or fullname (py-source-file-loader-object-name loader))))
    (unless (string= (py-source-file-loader-object-name loader) name)
      (error "loader for ~A cannot handle ~A"
             (py-source-file-loader-object-name loader)
             name))
    name))

(setf (py-type-attr *py-source-file-loader-type* "__init__")
      (lambda (loader fullname path)
        (setf (py-object-attr loader "name") fullname)
        (setf (py-object-attr loader "path") path)
        *py-none*))

(setf (py-type-attr *py-source-file-loader-type* "get_filename")
      (lambda (loader &optional fullname)
        (py-source-file-loader-check-name loader fullname)
        (py-source-file-loader-object-path loader)))

(setf (py-type-attr *py-source-file-loader-type* "get_data")
      (lambda (loader path)
        (declare (ignore loader))
        (py-read-file-bytes path)))

(setf (py-type-attr *py-source-file-loader-type* "get_resource_reader")
      (lambda (loader module)
        (py-source-file-loader-check-name loader module)
        (make-clamp-file-reader loader)))

(setf (py-type-attr *py-source-file-loader-type* "path_stats")
      (lambda (loader path)
        (declare (ignore loader))
        (py-path-stats path)))

(setf (py-type-attr *py-source-file-loader-type* "path_mtime")
      (lambda (loader path)
        (declare (ignore loader path))
        (error "OSError")))

(setf (py-type-attr *py-source-file-loader-type* "set_data")
      (lambda (loader path data)
        (declare (ignore loader))
        (py-write-file-bytes path data)))

(setf (py-type-attr *py-source-file-loader-type* "_cache_bytecode")
      (lambda (loader source-path bytecode-path data)
        (declare (ignore source-path))
        (funcall (py-type-attr (py-object-type loader) "set_data")
                 loader
                 bytecode-path
                 data)))

(setf (py-type-attr *py-source-file-loader-type* "get_source")
      (lambda (loader fullname)
        (let* ((path (funcall (py-type-attr (py-object-type loader) "get_filename")
                              loader
                              fullname))
               (data (funcall (py-type-attr (py-object-type loader) "get_data")
                              loader
                              path)))
          (py-decode-source-bytes data))))

(setf (py-type-attr *py-source-file-loader-type* "create_module")
      (lambda (loader spec)
        (declare (ignore loader spec))
        *py-none*))

(setf (py-type-attr *py-source-file-loader-type* "exec_module")
      (lambda (loader module)
        (unless (py-module-object-p module)
          (error "exec_module() expected module object, got ~S" module))
        (py-source-file-loader-check-name loader (py-object-attr module "__name__"))
        (unless *py-module-loader*
          (error "Clamp module loader is not installed"))
        (let ((*py-current-module* module))
          (py-ensure-module-package module)
          (funcall *py-module-loader*
                   (py-module-object-source-path module)
                   (py-module-object-name module)
                   (py-module-object-package-name module)))
        *py-none*))

(setf (py-type-attr *py-source-file-loader-type* "load_module")
      (lambda (loader &optional fullname)
        (py-source-file-loader-check-name loader fullname)
        (py-import-module (or fullname (py-source-file-loader-object-name loader)))))

(setf (py-type-attr *py-source-file-loader-type* "is_package")
      (lambda (loader fullname)
        (let* ((name (py-source-file-loader-check-name loader fullname))
               (filename-base (pathname-name (py-source-file-loader-object-path loader)))
               (tail-pos (position #\. name :from-end t))
               (tail-name (if tail-pos (subseq name (1+ tail-pos)) name)))
          (py-bool (and (string= filename-base "__init__")
                        (not (string= tail-name "__init__")))))))

(setf (py-type-attr *py-source-file-loader-type* "__repr__")
      (lambda (loader)
        (with-output-to-string (stream)
          (py-repr loader stream))))

(setf (py-type-attr *py-source-file-loader-type* "__eq__")
      (lambda (loader other)
        (py-bool (py-source-file-loader-eq loader other))))

(setf (py-type-attr *py-source-file-loader-type* "__ne__")
      (lambda (loader other)
        (py-ne loader other)))

(setf (py-type-attr *py-source-file-loader-type* "__hash__")
      (lambda (loader)
        (py-source-file-loader-hash loader)))

(setf (py-type-attr *py-file-reader-type* "resource_path")
      (lambda (reader resource)
        (py-file-reader-resource-path reader resource)))

(setf (py-type-attr *py-file-reader-type* "__init__")
      (lambda (reader loader)
        (py-file-reader-init-from-loader reader loader)))

(setf (py-type-attr *py-file-reader-type* "open_resource")
      (lambda (reader resource)
        (make-clamp-buffered-reader
         (py-file-reader-resource-path reader resource))))

(setf (py-type-attr *py-file-reader-type* "files")
      (lambda (reader)
        (py-file-reader-object-path reader)))

(setf (py-type-attr *py-file-reader-type* "is_resource")
      (lambda (reader resource)
        (py-file-reader-resource-p reader resource)))

(setf (py-type-attr *py-file-reader-type* "contents")
      (lambda (reader)
        (py-file-reader-contents reader)))

(setf (py-type-attr *py-path-type* "joinpath")
      (lambda (path &rest resources)
        (apply #'py-path-joinpath path resources)))

(setf (py-type-attr *py-path-type* "__truediv__")
      (lambda (path resource)
        (py-path-joinpath path resource)))

(setf (py-type-attr *py-path-type* "iterdir")
      (lambda (path)
        (py-path-iterdir path)))

(setf (py-type-attr *py-path-type* "is_file")
      (lambda (path)
        (py-bool (py-path-file-p path))))

(setf (py-type-attr *py-path-type* "is_dir")
      (lambda (path)
        (py-bool (py-path-directory-p path))))

(setf (py-type-attr *py-path-type* "exists")
      (lambda (path)
        (py-bool (py-path-exists-p path))))

(setf (py-type-attr *py-path-type* "open")
      (lambda (path &optional (mode "r"))
        (unless (or (string= mode "rb") (string= mode "r"))
          (error "unsupported file mode: ~A" mode))
        (make-clamp-buffered-reader (py-path-string path))))

(setf (py-type-attr *py-path-type* "read_bytes")
      (lambda (path)
        (py-read-file-bytes (py-path-string path))))

(setf (py-type-attr *py-path-type* "read_text")
      (lambda (path &optional (encoding *py-none*))
        (py-decode-text-bytes (py-read-file-bytes (py-path-string path)) encoding)))

(setf (py-type-attr *py-buffered-reader-type* "read")
      (lambda (reader &optional size)
        (py-buffered-reader-read reader size)))

(setf (py-type-attr *py-buffered-reader-type* "__iter__")
      (lambda (reader)
        (py-iter reader)))

(setf (py-type-attr *py-buffered-reader-type* "__next__")
      (lambda (reader)
        (py-next reader)))

(setf (py-type-attr *py-buffered-reader-type* "read1")
      (lambda (reader &optional size)
        (py-buffered-reader-read reader size)))

(setf (py-type-attr *py-buffered-reader-type* "peek")
      (lambda (reader &optional size)
        (py-buffered-reader-peek reader size)))

(setf (py-type-attr *py-buffered-reader-type* "readline")
      (lambda (reader &optional size)
        (py-buffered-reader-readline reader size)))

(setf (py-type-attr *py-buffered-reader-type* "readlines")
      (lambda (reader &optional hint)
        (py-buffered-reader-readlines reader hint)))

(setf (py-type-attr *py-buffered-reader-type* "tell")
      (lambda (reader)
        (py-buffered-reader-tell reader)))

(setf (py-type-attr *py-buffered-reader-type* "seek")
      (lambda (reader target &optional (whence 0))
        (py-buffered-reader-seek reader target whence)))

(setf (py-type-attr *py-buffered-reader-type* "seekable")
      (lambda (reader)
        (declare (ignore reader))
        *py-true*))

(setf (py-type-attr *py-buffered-reader-type* "readable")
      (lambda (reader)
        (declare (ignore reader))
        *py-true*))

(setf (py-type-attr *py-buffered-reader-type* "writable")
      (lambda (reader)
        (declare (ignore reader))
        *py-false*))

(setf (py-type-attr *py-buffered-reader-type* "isatty")
      (lambda (reader)
        (when (py-buffered-reader-object-closed reader)
          (error "I/O operation on closed file"))
        *py-false*))

(setf (py-type-attr *py-buffered-reader-type* "flush")
      (lambda (reader)
        (when (py-buffered-reader-object-closed reader)
          (error "I/O operation on closed file."))
        *py-none*))

(setf (py-type-attr *py-buffered-reader-type* "__enter__")
      (lambda (reader)
        reader))

(setf (py-type-attr *py-buffered-reader-type* "__exit__")
      (lambda (reader exc-type exc-value traceback)
        (declare (ignore exc-type exc-value traceback))
        (funcall (py-type-attr (py-object-type reader) "close") reader)))

(setf (py-type-attr *py-buffered-reader-type* "close")
      (lambda (reader)
        (setf (py-buffered-reader-object-closed reader) t)
        (setf (py-object-attr reader "closed") *py-true*)
        *py-none*))

(defun make-clamp-module-spec (name source-path package-p loader)
  (let* ((cached (and source-path (py-source-cache-path source-path)))
         (submodule-search-locations
           (if package-p
               (make-py-list (py-package-source-directory source-path))
               *py-none*))
         (uninitialized-submodules (make-py-list))
         (spec (make-py-module-spec-object
                :type *py-module-spec-type*
                :name name
                :loader loader
                :loader-state *py-none*
                :origin source-path
                :cached cached
                :set-fileattr (not (null source-path))
                :submodule-search-locations submodule-search-locations
                :has-location (not (null source-path))
                :uninitialized-submodules uninitialized-submodules)))
    (setf (py-object-attr spec "name") name)
    (setf (py-object-attr spec "loader") loader)
    (setf (py-object-attr spec "loader_state") *py-none*)
    (setf (py-object-attr spec "origin") (or source-path *py-none*))
    (setf (py-object-attr spec "cached") (or cached *py-none*))
    (setf (py-object-attr spec "_cached") (or cached *py-none*))
    (setf (py-object-attr spec "has_location") (py-bool source-path))
    (setf (py-object-attr spec "_set_fileattr") (py-bool source-path))
    (setf (py-object-attr spec "submodule_search_locations")
          submodule-search-locations)
    (setf (py-object-attr spec "_initializing") *py-false*)
    (setf (py-object-attr spec "_uninitialized_submodules")
          uninitialized-submodules)
    spec))

(defun py-set-module-initializing (module value)
  (setf (py-module-object-initializing module) value)
  (let ((spec (py-object-attr module "__spec__")))
    (when (py-module-spec-object-p spec)
      (setf (py-module-spec-object-initializing spec) value)
      (setf (py-object-attr spec "_initializing") (py-bool value)))))

(defun py-module-spec-repr (spec stream)
  (princ "ModuleSpec(name=" stream)
  (py-repr (py-module-spec-object-name spec) stream)
  (princ ", loader=" stream)
  (py-repr (py-module-spec-object-loader spec) stream)
  (let ((origin (py-module-spec-object-origin spec)))
    (when origin
      (princ ", origin=" stream)
      (py-repr origin stream)))
  (let ((submodule-search-locations
          (py-module-spec-object-submodule-search-locations spec)))
    (unless (eq submodule-search-locations *py-none*)
      (princ ", submodule_search_locations=" stream)
      (py-repr submodule-search-locations stream)))
  (princ ")" stream))

(setf (py-type-attr *py-module-spec-type* "__repr__")
      (lambda (spec)
        (with-output-to-string (stream)
          (py-repr spec stream))))

(setf (py-type-attr *py-module-spec-type* "__eq__")
      (lambda (spec other)
        (if (py-module-spec-object-p other)
            (py-bool (py-module-spec-eq spec other))
            *py-not-implemented*)))

(setf (py-type-attr *py-module-spec-type* "__ne__")
      (lambda (spec other)
        (if (py-module-spec-object-p other)
            (py-ne spec other)
            *py-not-implemented*)))

(defun make-clamp-module (name &key source-path package-name package-p)
  (let ((module (make-py-module-object :type *py-module-type*
                                       :name name
                                       :source-path source-path
                                       :package-name (or package-name (py-module-package-name name))))
        (loader (if source-path
                    (make-clamp-source-file-loader name source-path)
                    *py-none*)))
    (setf (py-object-attr module "__name__") name)
    (setf (py-object-attr module "__doc__") *py-none*)
    (setf (py-object-attr module "__package__")
          (if package-p
              name
              (let ((pos (position #\. name :from-end t)))
                (if pos (subseq name 0 pos) ""))))
    (setf (py-object-attr module "__loader__") loader)
    (setf (py-object-attr module "__spec__") *py-none*)
    (setf (py-object-attr module "__repr__")
          (lambda ()
            (with-output-to-string (stream)
              (py-repr module stream))))
    (when source-path
      (py-set-module-source-path module source-path))
    (unless (string= name "__main__")
      (setf (py-object-attr module "__spec__")
            (make-clamp-module-spec name source-path package-p loader)))
    module))

(defun py-enter-module (name source-path package-name)
  (let ((module (or (gethash name *py-sys-modules*)
                    (make-clamp-module name :source-path source-path :package-name package-name))))
    (when source-path
      (py-set-module-source-path module source-path))
    (when package-name
      (setf (py-module-object-package-name module) package-name))
    (py-bind-module-metadata-globals module)
    (setf (gethash name *py-sys-modules*) module)
    (setf *py-current-module* module)
    module))

(defmacro py-set-global (name symbol value)
  `(progn
     (setq ,symbol ,value)
     (when *py-current-module*
       (setf (py-object-attr *py-current-module* ,name) ,symbol))
     ,symbol))

(defun py-module-root-name (name)
  (let ((pos (position #\. name)))
    (if pos (subseq name 0 pos) name)))

(defun py-module-parent-name (name)
  (let ((pos (position #\. name :from-end t)))
    (and pos (subseq name 0 pos))))

(defun py-module-child-name (name)
  (let ((pos (position #\. name :from-end t)))
    (if pos (subseq name (1+ pos)) name)))

(defun split-string-on-char (value char)
  (let ((parts '()) (start 0))
    (loop for pos = (position char value :start start)
          do (if pos
                 (progn
                   (push (subseq value start pos) parts)
                   (setf start (1+ pos)))
                 (progn
                   (push (subseq value start) parts)
                   (return))))
    (nreverse parts)))

(defun py-module-path-components (name)
  (split-string-on-char name #\.))

(defun py-probe-file (path)
  (let ((probe (probe-file path)))
    (and probe (namestring probe))))

(defun py-package-source-directory (source-path)
  (let ((directory (namestring (uiop:pathname-directory-pathname source-path))))
    (if (and (> (length directory) 1)
             (char= (char directory (1- (length directory))) #\/))
        (subseq directory 0 (1- (length directory)))
        directory)))

(defun py-string-suffix-p (value suffix)
  (let ((value-size (length value))
        (suffix-size (length suffix)))
    (and (<= suffix-size value-size)
         (string= value suffix :start1 (- value-size suffix-size)))))

(defun py-source-cache-path (source-path)
  (cond
    ((py-string-suffix-p source-path ".py")
     (let* ((source (pathname source-path))
            (source-directory (uiop:pathname-directory-pathname source))
            (cache-directory (merge-pathnames "__pycache__/" source-directory))
            (cache-filename (format nil "~A.cpython-314.pyc" (pathname-name source))))
       (namestring (merge-pathnames cache-filename cache-directory))))
    ((py-string-suffix-p source-path ".pyc")
     source-path)
    (t nil)))

(defun py-set-module-source-path (module source-path)
  (setf (py-module-object-source-path module) source-path)
  (setf (py-object-attr module "__file__") source-path)
  (setf (py-object-attr module "__cached__") (py-source-cache-path source-path)))

(defun py-find-module-source (name)
  (let* ((components (py-module-path-components name))
         (relative-file (format nil "~{~A~^/~}.py" components))
         (relative-init (format nil "~{~A~^/~}/__init__.py" components)))
    (loop for root in (or *py-module-search-paths* (list (namestring (uiop:getcwd))))
          for file-path = (merge-pathnames relative-file (uiop:ensure-directory-pathname root))
          for init-path = (merge-pathnames relative-init (uiop:ensure-directory-pathname root))
          for file = (py-probe-file file-path)
          for init = (py-probe-file init-path)
          when file do (return (values file nil))
          when init do (return (values init t))
          finally (return (values nil nil)))))

(defun py-ensure-module-package (module)
  (let* ((package-name (py-module-object-package-name module))
         (package (or (find-package package-name)
                      (make-package package-name :use '("CLAMP.__builtins__")))))
    (setf (py-module-object-package-name module) (package-name package))
    package))

(defun py-bind-module-global-symbol (module name)
  (multiple-value-bind (value found)
      (gethash name (py-object-attrs module))
    (when found
      (let* ((package (py-ensure-module-package module))
             (symbol (intern (string-upcase name) package)))
        (setf (symbol-value symbol) value)))))

(defun py-bind-module-metadata-globals (module)
  (dolist (name (quote ("__name__"
                        "__doc__"
                        "__package__"
                        "__loader__"
                        "__spec__"
                        "__file__"
                        "__cached__"
                        "__path__")))
    (py-bind-module-global-symbol module name)))

(defun py-parent-uninitialized-submodules (name)
  (let ((parent-name (py-module-parent-name name)))
    (when parent-name
      (let* ((parent (gethash parent-name *py-sys-modules*))
             (spec (and parent (py-object-attr parent "__spec__"))))
        (when (py-module-spec-object-p spec)
          (py-module-spec-object-uninitialized-submodules spec))))))

(defun py-load-module (name)
  (multiple-value-bind (cached found) (gethash name *py-sys-modules*)
    (when found
      (return-from py-load-module cached)))
  (multiple-value-bind (builder found) (gethash name *py-builtin-module-builders*)
    (when found
      (let ((module (funcall builder)))
        (setf (gethash name *py-sys-modules*) module)
        (return-from py-load-module module))))
  (let ((parent-name (py-module-parent-name name)))
    (when parent-name
      (py-load-module parent-name)
      (multiple-value-bind (cached found) (gethash name *py-sys-modules*)
        (when found
          (return-from py-load-module cached)))
      (let ((parent (gethash parent-name *py-sys-modules*)))
        (unless (and parent
                     (nth-value 1 (gethash "__path__" (py-object-attrs parent))))
          (error "No module named '~A'; '~A' is not a package"
                 name
                 parent-name)))))
  (multiple-value-bind (source-path package-p) (py-find-module-source name)
    (unless source-path
      (error "No module named '~A'" name))
    (let ((module (make-clamp-module name :source-path source-path :package-p package-p)))
      (py-set-module-initializing module t)
      (setf (gethash name *py-sys-modules*) module)
      (when package-p
        (let ((spec (py-object-attr module "__spec__")))
          (setf (py-object-attr module "__path__")
                (py-module-spec-object-submodule-search-locations spec))))
      (let ((parent-uninitialized-submodules
              (py-parent-uninitialized-submodules name)))
        (when parent-uninitialized-submodules
          (py-append parent-uninitialized-submodules (py-module-child-name name)))
        (unwind-protect
            (let ((*py-current-module* module))
              (py-ensure-module-package module)
              (handler-case
                  (progn
                    (unless *py-module-loader*
                      (error "Clamp module loader is not installed"))
                    (funcall *py-module-loader* source-path name (py-module-object-package-name module))
                    (py-set-module-initializing module nil))
                (error (condition)
                  (remhash name *py-sys-modules*)
                  (error condition))))
          (when parent-uninitialized-submodules
            (py-pop parent-uninitialized-submodules))))
      (let ((parent-name (py-module-parent-name name)))
        (when parent-name
          (let ((parent (gethash parent-name *py-sys-modules*)))
            (when parent
              (setf (py-object-attr parent (py-module-child-name name)) module)))))
      module)))

(defun py-import-module (name)
  (py-load-module name))

(defun py-import-name (name &optional fromlist)
  (let ((module (py-import-module name)))
    (if (and fromlist (> (length fromlist) 0))
        module
        (py-import-module (py-module-root-name name)))))

(defun py-import-fromlist-names (fromlist)
  (cond
    ((or (null fromlist) (eq fromlist *py-none*))
     '())
    ((stringp fromlist)
     (loop for char across fromlist
           collect (string char)))
    ((py-list-object-p fromlist)
     (let ((storage (py-object-value fromlist))
           (size (or (py-object-size fromlist) 0)))
       (loop for index from 0 below size
             collect (aref storage index))))
    ((py-tuple-object-p fromlist)
     (let ((storage (py-object-value fromlist))
           (size (or (py-object-size fromlist) 0)))
       (loop for index from 0 below size
             collect (aref storage index))))
    (t
     (if (py-truthy-p fromlist)
         (error "'~A' object is not iterable" (py-type-name (py-type-of fromlist)))
         '()))))

(defun py-import-handle-fromlist (module fromlist &optional (recursive nil))
  (dolist (name fromlist)
    (unless (stringp name)
      (error "Item in ~A must be str, not ~A"
             (if recursive
                 (concatenate 'string (py-module-object-name module) ".__all__")
                 "``from list''")
             (py-type-name (py-type-of name))))
    (cond
      ((and (string= name "*") (not recursive))
       (multiple-value-bind (all found) (gethash "__all__" (py-object-attrs module))
         (when found
           (py-import-handle-fromlist
            module
            (py-import-fromlist-names all)
            t))))
      ((not (string= name "*"))
       (multiple-value-bind (attr found) (gethash name (py-object-attrs module))
         (declare (ignore attr))
         (unless found
           (let ((full-name (concatenate 'string
                                         (py-module-object-name module)
                                         "."
                                         name)))
             (multiple-value-bind (source-path package-p) (py-find-module-source full-name)
               (declare (ignore package-p))
               (when source-path
                 (py-import-module full-name)))))))))
  module)

(defun py-import-star-bind (module name value)
  (when *py-current-module*
    (setf (py-object-attr *py-current-module* name) value)
    (let* ((package (py-ensure-module-package *py-current-module*))
           (symbol (intern (string-upcase name) package)))
      (setf (symbol-value symbol) value)))
  value)

(defun py-import-star-names (module)
  (multiple-value-bind (all found) (gethash "__all__" (py-object-attrs module))
    (if found
        (progn
          (py-import-handle-fromlist module (list "*"))
          (py-import-fromlist-names all))
        (let ((names (quote ())))
          (maphash (lambda (name value)
                     (declare (ignore value))
                     (when (and (stringp name)
                                (> (length name) 0)
                                (not (char= (char name 0) #\_)))
                       (push name names)))
                   (py-object-attrs module))
          names))))

(defun py-import-star (name)
  (let ((module (py-import-name name (list "*"))))
    (dolist (import-name (py-import-star-names module))
      (unless (stringp import-name)
        (error "Item in ~A.__all__ must be str, not ~A"
               (py-module-object-name module)
               (py-type-name (py-type-of import-name))))
      (py-import-star-bind module import-name (py-import-from module import-name)))
    *py-none*))

(defun py-import-builtin (name &optional
                          (globals *py-none*)
                          (locals *py-none*)
                          (fromlist *py-none*)
                          (level 0))
  (declare (ignore globals locals))
  (unless (stringp name)
    (error "module name must be a string"))
  (let ((normalized-level (py-normalize-bool-number level)))
    (unless (integerp normalized-level)
      (error "level must be an integer"))
    (when (< normalized-level 0)
      (error "level must be >= 0"))
    (when (> normalized-level 0)
      (error "relative imports are not supported yet"))
    (when (= (length name) 0)
      (error "Empty module name")))
  (let ((module (py-import-module name)))
    (if (py-truthy-p fromlist)
        (if (nth-value 1 (gethash "__path__" (py-object-attrs module)))
            (py-import-handle-fromlist module (py-import-fromlist-names fromlist))
            module)
        (py-import-module (py-module-root-name name)))))

(defun py-import-from (module name)
  (multiple-value-bind (attr found) (gethash name (py-object-attrs module))
    (when found
      (return-from py-import-from attr)))
  (let ((full-name (concatenate 'string (py-module-object-name module) "." name)))
    (multiple-value-bind (cached found) (gethash full-name *py-sys-modules*)
      (when found
        (return-from py-import-from cached)))
    (handler-case
        (py-import-module full-name)
      (error ()
        (error "cannot import name '~A' from '~A'" name (py-module-object-name module))))))

(defun py-register-builtin-module (name builder)
  (setf (gethash name *py-builtin-module-builders*) builder))

(defun py-math-number (value function-name)
  (let ((normalized-value (py-normalize-bool-number value)))
    (unless (numberp normalized-value)
      (error "must be real number, not ~A" (py-type-name (py-type-of value))))
    normalized-value))

(defun py-math-float (value)
  (coerce value 'double-float))

(defun py-math-unary (function-name fn value)
  (py-math-float
   (funcall fn (py-math-float (py-math-number value function-name)))))

(defun py-math-binary (function-name fn left right)
  (py-math-float
   (funcall fn
            (py-math-float (py-math-number left function-name))
            (py-math-float (py-math-number right function-name)))))

(defun py-math-domain-check (function-name value predicate)
  (unless (funcall predicate value)
    (error "math domain error in ~A()" function-name))
  value)

(defun py-math-sqrt (value)
  (py-math-unary "sqrt"
                 (lambda (x)
                   (when (< x 0)
                     (error "expected a nonnegative input, got ~A" (py-float-string x)))
                   (sqrt x))
                 value))

(defun py-math-log (value &optional (base *py-none*))
  (let ((x (py-math-number value "log")))
    (unless (> x 0)
      (error "expected a positive input"))
    (let ((result (if (eq base *py-none*)
                      (log x)
                      (let ((base-value (py-math-number base "log")))
                        (unless (and (> base-value 0) (/= base-value 1))
                          (error "expected a positive input"))
                        (/ (log x) (log base-value))))))
      (py-math-float result))))

(defun py-math-log10 (value)
  (py-math-log value 10))

(defun py-math-log2 (value)
  (py-math-log value 2))

(defun py-math-exp (value)
  (py-math-unary "exp" #'exp value))

(defun py-math-pow (left right)
  (let ((x (py-math-number left "pow"))
        (y (py-math-number right "pow")))
    (when (or (and (= x 0) (< y 0))
              (and (< x 0) (not (integerp y))))
      (error "math domain error"))
    (py-math-float (expt x y))))

(defun py-math-floor (value)
  (floor (py-math-number value "floor")))

(defun py-math-ceil (value)
  (ceiling (py-math-number value "ceil")))

(defun py-math-trunc (value)
  (truncate (py-math-number value "trunc")))

(defun py-math-factorial (value)
  (let ((n (py-math-number value "factorial")))
    (unless (and (integerp n) (>= n 0))
      (error "factorial() only accepts non-negative integral values"))
    (loop with result = 1
          for i from 2 to n
          do (setf result (* result i))
          finally (return result))))

(defun py-math-gcd (&rest values)
  (reduce #'gcd
          (mapcar (lambda (value)
                    (let ((n (py-math-number value "gcd")))
                      (unless (integerp n)
                        (error "gcd() arguments must be integers"))
                      n))
                  values)
          :initial-value 0))

(defun py-math-lcm (&rest values)
  (reduce (lambda (left right)
            (if (or (= left 0) (= right 0))
                0
                (abs (/ (* left right) (gcd left right)))))
          (mapcar (lambda (value)
                    (let ((n (py-math-number value "lcm")))
                      (unless (integerp n)
                        (error "lcm() arguments must be integers"))
                      n))
                  values)
          :initial-value 1))

(defun py-math-fmod (left right)
  (let ((x (py-math-float (py-math-number left "fmod")))
        (y (py-math-float (py-math-number right "fmod"))))
    (when (= y 0.0d0)
      (error "math domain error"))
    (py-math-float (rem x y))))

(defun py-math-remainder (left right)
  (let ((x (py-math-number left "remainder"))
        (y (py-math-number right "remainder")))
    (when (= y 0)
      (error "math domain error in remainder()"))
    (py-math-float (- x (* y (round (/ x y)))))))

(defun py-math-copysign (left right)
  (let ((magnitude (abs (py-math-number left "copysign")))
        (sign (py-math-number right "copysign")))
    (py-math-float (if (minusp sign) (- magnitude) magnitude))))

(defun py-math-degrees (value)
  (py-math-float (* (py-math-number value "degrees") (/ 180 pi))))

(defun py-math-radians (value)
  (py-math-float (* (py-math-number value "radians") (/ pi 180))))

(defun py-math-hypot (&rest coordinates)
  (py-math-float
   (sqrt (reduce #'+
                 (mapcar (lambda (value)
                           (let ((x (py-math-number value "hypot")))
                             (* x x)))
                         coordinates)
                 :initial-value 0))))

(defun py-math-dist (left right)
  (let ((left-items '())
        (right-items '()))
    (let ((iterator (py-iter left)))
      (loop
        (multiple-value-bind (item found) (py-next-item iterator)
          (unless found (return))
          (push (py-math-number item "dist") left-items))))
    (let ((iterator (py-iter right)))
      (loop
        (multiple-value-bind (item found) (py-next-item iterator)
          (unless found (return))
          (push (py-math-number item "dist") right-items))))
    (unless (= (length left-items) (length right-items))
      (error "both points must have the same number of dimensions"))
    (py-math-float
     (sqrt (loop for x in left-items
                 for y in right-items
                 sum (let ((delta (- x y))) (* delta delta)))))))

(defun py-math-isfinite (value)
  (let ((x (py-math-number value "isfinite")))
    (py-bool (and (not (sb-ext:float-infinity-p (float x 1.0d0)))
                  (not (sb-ext:float-nan-p (float x 1.0d0)))))))

(defun py-math-isinf (value)
  (py-bool (sb-ext:float-infinity-p (float (py-math-number value "isinf") 1.0d0))))

(defun py-math-isnan (value)
  (py-bool (sb-ext:float-nan-p (float (py-math-number value "isnan") 1.0d0))))

(defun py-math-isclose (left right &optional (rel-tol 1.0e-09) (abs-tol 0.0))
  (let ((a (py-math-float (py-math-number left "isclose")))
        (b (py-math-float (py-math-number right "isclose")))
        (relative-tolerance (py-math-float (py-math-number rel-tol "isclose")))
        (absolute-tolerance (py-math-float (py-math-number abs-tol "isclose"))))
    (when (or (< relative-tolerance 0) (< absolute-tolerance 0))
      (error "tolerances must be non-negative"))
    (cond
      ((or (sb-ext:float-nan-p a) (sb-ext:float-nan-p b))
       *py-false*)
      ((= a b)
       *py-true*)
      ((or (sb-ext:float-infinity-p a) (sb-ext:float-infinity-p b))
       *py-false*)
      (t
       (let ((diff (abs (- a b))))
         (py-bool (or (<= diff absolute-tolerance)
                      (<= diff (* relative-tolerance (abs a)))
                      (<= diff (* relative-tolerance (abs b))))))))))

(defun py-math-nan ()
  (sb-kernel:make-double-float #x7ff80000 0))

(defun py-math-finite-p (value)
  (let ((x (py-math-float value)))
    (and (not (sb-ext:float-infinity-p x))
         (not (sb-ext:float-nan-p x)))))

(defun py-math-integer (value function-name)
  (let ((n (py-normalize-bool-number value)))
    (unless (integerp n)
      (error "~A() arguments must be integers" function-name))
    n))

(defun py-math-nonnegative-integer (value function-name name)
  (let ((n (py-math-integer value function-name)))
    (when (< n 0)
      (error "~A must be a non-negative integer" name))
    n))

(defun py-math-two-arg-domain (function-name fn left right)
  (let* ((x (py-math-float (py-math-number left function-name)))
         (y (py-math-float (py-math-number right function-name)))
         (r (funcall fn x y)))
    (when (and (sb-ext:float-nan-p r)
               (not (sb-ext:float-nan-p x))
               (not (sb-ext:float-nan-p y)))
      (error "math domain error"))
    r))

(defun py-math-acosh (value)
  (py-math-unary "acosh"
                 (lambda (x)
                   (acosh (py-math-domain-check "acosh" x (lambda (n) (>= n 1)))))
                 value))

(defun py-math-atanh (value)
  (py-math-unary "atanh"
                 (lambda (x)
                   (unless (< (abs x) 1)
                     (error "expected a number between -1 and 1, got ~A" (py-float-string x)))
                   (atanh x))
                 value))

(defun py-math-cbrt (value)
  (py-math-unary "cbrt" #'c-cbrt value))

(defun py-math-exp2 (value)
  (py-math-unary "exp2" (lambda (x) (expt 2.0d0 x)) value))

(defun py-math-expm1 (value)
  (py-math-unary "expm1" #'c-expm1 value))

(defun py-math-log1p (value)
  (let ((x (py-math-number value "log1p")))
    (py-math-domain-check "log1p" x (lambda (n) (> n -1)))
    (py-math-float (log (1+ x)))))

(defun py-math-gamma (value)
  (let* ((x (py-math-float (py-math-number value "gamma"))))
    (when (and (<= x 0) (= x (truncate x)))
      (error "expected a noninteger or positive integer, got ~A" (py-float-string x)))
    (let ((result (c-tgamma x)))
    (when (and (or (sb-ext:float-nan-p result)
                   (sb-ext:float-infinity-p result))
               (py-math-finite-p x))
      (if (and (<= x 0) (= x (truncate x)))
          (error "expected a noninteger or positive integer, got ~A" (py-float-string x))
          (error "math range error")))
      result)))

(defun py-math-lgamma (value)
  (let* ((x (py-math-float (py-math-number value "lgamma")))
         (result (c-lgamma x)))
    (when (and (or (sb-ext:float-nan-p result)
                   (sb-ext:float-infinity-p result))
               (py-math-finite-p x)
               (<= x 0)
               (= x (truncate x)))
      (error "math domain error"))
    result))

(defun py-math-fsum (iterable)
  (let ((partials '()))
    (let ((iterator (py-iter iterable)))
      (loop
        (multiple-value-bind (item found) (py-next-item iterator)
          (unless found (return))
          (let ((x (py-math-float (py-math-number item "fsum")))
                (new-partials '()))
            (dolist (y partials)
              (when (< (abs x) (abs y))
                (rotatef x y))
              (let* ((hi (+ x y))
                     (yr (- hi x))
                     (lo (- y yr)))
                (unless (= lo 0.0d0)
                  (push lo new-partials))
                (setf x hi)))
            (when (or (/= x 0.0d0) (null new-partials))
              (push x new-partials))
            (setf partials (nreverse new-partials))))))
    (py-math-float (reduce #'+ partials :initial-value 0.0d0))))

(defun py-math-isqrt (value)
  (let ((n (py-math-integer value "isqrt")))
    (when (< n 0)
      (error "isqrt() argument must be nonnegative"))
    (isqrt n)))

(defun py-math-frexp (value)
  (let ((x (py-math-float (py-math-number value "frexp"))))
    (if (or (= x 0.0d0)
            (sb-ext:float-infinity-p x)
            (sb-ext:float-nan-p x))
        (make-py-tuple x 0)
        (multiple-value-bind (significand exponent sign) (integer-decode-float x)
          (let* ((digits (float-digits x))
                 (mantissa (* sign (/ significand (expt 2.0d0 digits))))
                 (adjusted-exponent (+ exponent digits)))
            (make-py-tuple (py-math-float mantissa) adjusted-exponent))))))

(defun py-math-ldexp (value exponent)
  (let ((x (py-math-float (py-math-number value "ldexp")))
        (n (py-math-integer exponent "ldexp")))
    (py-math-float (scale-float x n))))

(defun py-math-modf (value)
  (let ((x (py-math-float (py-math-number value "modf"))))
    (cond
      ((sb-ext:float-infinity-p x)
       (make-py-tuple (if (minusp x) -0.0d0 0.0d0) x))
      ((sb-ext:float-nan-p x)
       (make-py-tuple x x))
      (t
       (multiple-value-bind (integer fractional) (truncate x)
         (declare (ignore integer))
         (make-py-tuple (py-math-float fractional)
                        (py-math-float (- x fractional))))))))

(defun py-math-fma (x y z)
  (let ((left (py-math-float (py-math-number x "fma")))
        (right (py-math-float (py-math-number y "fma")))
        (addend (py-math-float (py-math-number z "fma"))))
    (c-fma left right addend)))

(defun py-math-prod (iterable &optional (start 1))
  (let ((result start)
        (iterator (py-iter iterable)))
    (loop
      (multiple-value-bind (item found) (py-next-item iterator)
        (unless found (return result))
        (setf result (py-mul result item))))))

(defun py-math-sumprod (left right)
  (let ((left-iterator (py-iter left))
        (right-iterator (py-iter right))
        (total 0))
    (loop
      (multiple-value-bind (left-item left-found) (py-next-item left-iterator)
        (multiple-value-bind (right-item right-found) (py-next-item right-iterator)
          (unless (eq left-found right-found)
            (error "Inputs are not the same length"))
          (unless left-found
            (return total))
          (setf total (py-add total (py-mul left-item right-item))))))))

(defun py-math-perm (n &optional (k *py-none*))
  (let* ((normalized-n (py-math-nonnegative-integer n "perm" "n"))
         (normalized-k (if (eq k *py-none*)
                           normalized-n
                           (py-math-nonnegative-integer k "perm" "k"))))
    (if (> normalized-k normalized-n)
        0
        (loop with result = 1
              for i from 0 below normalized-k
              do (setf result (* result (- normalized-n i)))
              finally (return result)))))

(defun py-math-comb (n k)
  (let ((normalized-n (py-math-nonnegative-integer n "comb" "n"))
        (normalized-k (py-math-nonnegative-integer k "comb" "k")))
    (if (> normalized-k normalized-n)
        0
        (let ((k (min normalized-k (- normalized-n normalized-k))))
          (truncate (py-math-perm normalized-n k)
                    (py-math-factorial k))))))

(defun py-math-nextafter (x y &optional (steps *py-none*))
  (let ((current (py-math-float (py-math-number x "nextafter")))
        (target (py-math-float (py-math-number y "nextafter")))
        (count (if (eq steps *py-none*) 1 (py-math-integer steps "nextafter"))))
    (when (< count 0)
      (error "steps must be a non-negative integer"))
    (loop repeat count
          do (setf current (c-nextafter current target))
          when (= current target) do (return))
    current))

(defun py-math-ulp (value)
  (let ((x (abs (py-math-float (py-math-number value "ulp")))))
    (cond
      ((sb-ext:float-nan-p x) x)
      ((sb-ext:float-infinity-p x) x)
      (t
       (let ((next (c-nextafter x sb-ext:double-float-positive-infinity)))
         (if (sb-ext:float-infinity-p next)
             (- x (c-nextafter x sb-ext:double-float-negative-infinity))
             (- next x)))))))

(defun make-clamp-math-module ()
  (let ((module (make-clamp-module "math")))
    (setf (py-object-attr module "__doc__") "Clamp built-in math module")
    (setf (py-object-attr module "pi") (py-math-float pi))
    (setf (py-object-attr module "e") (py-math-float (exp 1.0d0)))
    (setf (py-object-attr module "tau") (py-math-float (* 2.0d0 pi)))
    (setf (py-object-attr module "inf") sb-ext:double-float-positive-infinity)
    (setf (py-object-attr module "nan") (py-math-nan))
    (setf (py-object-attr module "acos")
          (lambda (x)
            (py-math-unary "acos"
                           (lambda (n)
                             (unless (<= -1 n 1)
                               (error "expected a number in range from -1 up to 1, got ~A" (py-float-string n)))
                             (acos n))
                           x)))
    (setf (py-object-attr module "acosh") #'py-math-acosh)
    (setf (py-object-attr module "asin")
          (lambda (x)
            (py-math-unary "asin"
                           (lambda (n)
                             (unless (<= -1 n 1)
                               (error "expected a number in range from -1 up to 1, got ~A" (py-float-string n)))
                             (asin n))
                           x)))
    (setf (py-object-attr module "asinh") (lambda (x) (py-math-unary "asinh" #'asinh x)))
    (setf (py-object-attr module "atan") (lambda (x) (py-math-unary "atan" #'atan x)))
    (setf (py-object-attr module "atan2") (lambda (y x) (py-math-binary "atan2" #'atan y x)))
    (setf (py-object-attr module "atanh") #'py-math-atanh)
    (setf (py-object-attr module "cbrt") #'py-math-cbrt)
    (setf (py-object-attr module "ceil") #'py-math-ceil)
    (setf (py-object-attr module "comb") #'py-math-comb)
    (setf (py-object-attr module "copysign") #'py-math-copysign)
    (setf (py-object-attr module "cos") (lambda (x) (py-math-unary "cos" #'cos x)))
    (setf (py-object-attr module "cosh") (lambda (x) (py-math-unary "cosh" #'cosh x)))
    (setf (py-object-attr module "degrees") #'py-math-degrees)
    (setf (py-object-attr module "dist") #'py-math-dist)
    (setf (py-object-attr module "erf") (lambda (x) (py-math-unary "erf" #'c-erf x)))
    (setf (py-object-attr module "erfc") (lambda (x) (py-math-unary "erfc" #'c-erfc x)))
    (setf (py-object-attr module "exp") #'py-math-exp)
    (setf (py-object-attr module "exp2") #'py-math-exp2)
    (setf (py-object-attr module "expm1") #'py-math-expm1)
    (setf (py-object-attr module "fabs") (lambda (x) (py-math-float (abs (py-math-number x "fabs")))))
    (setf (py-object-attr module "factorial") #'py-math-factorial)
    (setf (py-object-attr module "floor") #'py-math-floor)
    (setf (py-object-attr module "fma") #'py-math-fma)
    (setf (py-object-attr module "fmod") #'py-math-fmod)
    (setf (py-object-attr module "frexp") #'py-math-frexp)
    (setf (py-object-attr module "fsum") #'py-math-fsum)
    (setf (py-object-attr module "gamma") #'py-math-gamma)
    (setf (py-object-attr module "gcd") #'py-math-gcd)
    (setf (py-object-attr module "hypot") #'py-math-hypot)
    (setf (py-object-attr module "isclose") #'py-math-isclose)
    (setf (py-object-attr module "isfinite") #'py-math-isfinite)
    (setf (py-object-attr module "isinf") #'py-math-isinf)
    (setf (py-object-attr module "isnan") #'py-math-isnan)
    (setf (py-object-attr module "isqrt") #'py-math-isqrt)
    (setf (py-object-attr module "lcm") #'py-math-lcm)
    (setf (py-object-attr module "ldexp") #'py-math-ldexp)
    (setf (py-object-attr module "lgamma") #'py-math-lgamma)
    (setf (py-object-attr module "log") #'py-math-log)
    (setf (py-object-attr module "log1p") #'py-math-log1p)
    (setf (py-object-attr module "log10") #'py-math-log10)
    (setf (py-object-attr module "log2") #'py-math-log2)
    (setf (py-object-attr module "modf") #'py-math-modf)
    (setf (py-object-attr module "nextafter") #'py-math-nextafter)
    (setf (py-object-attr module "perm") #'py-math-perm)
    (setf (py-object-attr module "pow") #'py-math-pow)
    (setf (py-object-attr module "prod") #'py-math-prod)
    (setf (py-object-attr module "radians") #'py-math-radians)
    (setf (py-object-attr module "remainder") #'py-math-remainder)
    (setf (py-object-attr module "sin") (lambda (x) (py-math-unary "sin" #'sin x)))
    (setf (py-object-attr module "sinh") (lambda (x) (py-math-unary "sinh" #'sinh x)))
    (setf (py-object-attr module "sqrt") #'py-math-sqrt)
    (setf (py-object-attr module "sumprod") #'py-math-sumprod)
    (setf (py-object-attr module "tan") (lambda (x) (py-math-unary "tan" #'tan x)))
    (setf (py-object-attr module "tanh") (lambda (x) (py-math-unary "tanh" #'tanh x)))
    (setf (py-object-attr module "trunc") #'py-math-trunc)
    (setf (py-object-attr module "ulp") #'py-math-ulp)
    module))

(py-register-builtin-module "math" #'make-clamp-math-module)

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
  (when (and (py-object-p obj) (py-range-object-p obj))
    (cond
      ((string= name "start")
       (return-from py-lookup-attr (py-range-object-start obj)))
      ((string= name "stop")
       (return-from py-lookup-attr (py-range-object-stop obj)))
      ((string= name "step")
       (return-from py-lookup-attr (py-range-object-step obj)))))
  (when (and (py-type-p obj) (string= name "__name__"))
    (return-from py-lookup-attr (py-type-name obj)))
  (when (py-type-p obj)
    (multiple-value-bind (attr found) (gethash name (py-object-attrs obj))
      (when found
        (return-from py-lookup-attr attr))))
  (when (and (py-module-spec-object-p obj) (string= name "parent"))
    (return-from py-lookup-attr (py-module-spec-parent obj)))
  (when (and (py-module-spec-object-p obj) (string= name "cached"))
    (return-from py-lookup-attr (py-module-spec-cached obj)))
  (when (and (py-module-object-p obj) (string= name "__dict__"))
    (return-from py-lookup-attr (py-module-dict obj)))
  (when (and (py-source-file-loader-object-p obj) (string= name "__dict__"))
    (return-from py-lookup-attr (py-source-file-loader-dict obj)))
  (when (and (py-file-reader-object-p obj) (string= name "__dict__"))
    (return-from py-lookup-attr (py-file-reader-dict obj)))
  (when (py-object-p obj)
    (multiple-value-bind (attr found) (gethash name (py-object-attrs obj))
      (when found
        (return-from py-lookup-attr attr))))
  (multiple-value-bind (attr found) (py-find-type-attr (py-type-of obj) name)
    (when found
      (return-from py-lookup-attr attr)))
  (error "Python object of type ~A has no attribute ~S"
         (py-type-name (py-type-of obj))
         name))

(defun py-invoke-callable (callable &rest args)
  (cond
    ((py-callable-p callable)
     (apply (py-callable-fn callable) args))
    ((functionp callable)
     (apply callable args))
    (t
     (error "Python attribute is not callable: ~S" callable))))

(defun py-callable (value)
  (py-bool
   (or (functionp value)
       (py-callable-p value))))

(defun py-type-subtype-p (derived cls)
  (cond
    ((eq derived cls) t)
    ((not (py-type-p derived)) nil)
    (t
     (loop for base in (py-type-bases derived)
           thereis (py-type-subtype-p base cls)))))

(defun py-isinstance (obj class-or-tuple)
  (cond
    ((py-type-p class-or-tuple)
     (py-bool (py-type-subtype-p (py-type-of obj) class-or-tuple)))
    ((py-tuple-object-p class-or-tuple)
     (let ((storage (py-object-value class-or-tuple))
           (size (or (py-object-size class-or-tuple) 0)))
       (py-bool
        (loop for index from 0 below size
              thereis (py-truthy-p
                       (py-isinstance obj (aref storage index)))))))
    (t
     (error "isinstance() arg 2 must be a type or tuple of types"))))

(defun py-call-attr (obj name &rest args)
  (let ((callable (py-lookup-attr obj name)))
    (if (py-module-object-p obj)
        (apply #'py-invoke-callable callable args)
        (apply #'py-invoke-callable callable obj args))))

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

(defstruct (py-bytes-iterator-object (:include py-object))
  sequence
  (index 0))

(defstruct (py-module-object (:include py-object))
  name
  source-path
  package-name
  namespace-dict
  (initializing nil))

(defstruct (py-bytes-object (:include py-object)))


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

(defstruct (py-map-object (:include py-object))
  function
  iterators)

(defstruct (py-range-object (:include py-object))
  start
  stop
  step
  length)

(defstruct (py-range-iterator-object (:include py-object))
  range
  (index 0))

(defstruct (py-dict-object (:include py-object))
  (keys (make-array 0 :adjustable t :fill-pointer 0))
  namespace-owner)

(defparameter *py-dict-type*
  (make-py-type :type *py-type-type*
                :name "dict"
                :bases (list *py-object-type*)
                :basicsize 1
                :mapping-length-fn
                (lambda (obj)
                  (or (py-object-size obj) 0))))

(defparameter *py-list-type*
  (make-py-type :type *py-type-type*
                :name "list"
                :bases (list *py-object-type*)
                :basicsize 1
                :itemsize 1
                :sequence-length-fn
                (lambda (obj)
                  (or (py-object-size obj) 0))))

(defparameter *py-tuple-type*
  (make-py-type :type *py-type-type*
                :name "tuple"
                :bases (list *py-object-type*)
                :basicsize 1
                :itemsize 1
                :sequence-length-fn
                (lambda (obj)
                  (or (py-object-size obj) 0))))

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

(defparameter *py-bytes-iterator-type*
  (make-py-type :type *py-type-type*
                :name "bytes_iterator"
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

(defparameter *py-map-type*
  (make-py-type :type *py-type-type*
                :name "map"
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

(defun py-bytes-storage (obj operation)
  (unless (eq (py-object-type obj) *py-bytes-type*)
    (error "~A only supports bytes objects, got ~S" operation obj))
  (py-object-value obj))

(defun py-dict-storage (obj operation)
  (unless (eq (py-object-type obj) *py-dict-type*)
    (error "~A only supports dict objects, got ~S" operation obj))
  (py-object-value obj))

(defun py-dict-set-entry (obj key value)
  (let ((storage (py-dict-storage obj "__setitem__")))
    (multiple-value-bind (old-value found)
        (gethash key storage)
      (declare (ignore old-value))
      (unless found
        (vector-push-extend key (py-dict-object-keys obj))
        (incf (py-object-size obj)))
      (setf (gethash key storage) value)))
  (let ((owner (py-dict-object-namespace-owner obj)))
    (when (and owner (stringp key) (not (string= key "__dict__")))
      (py-sync-object-attr owner key value)))
  value)

(defun py-dict-delete-entry (obj key)
  (let ((storage (py-dict-storage obj "__delitem__")))
    (multiple-value-bind (old-value found)
        (gethash key storage)
      (declare (ignore old-value))
      (unless found
        (error "~S" key))
      (remhash key storage)
      (decf (py-object-size obj))
      (let* ((keys (py-dict-object-keys obj))
             (size (fill-pointer keys))
             (position (position key keys :test #'equal :end size)))
        (when position
          (loop for index from position below (1- size)
                do (setf (aref keys index) (aref keys (1+ index))))
          (vector-pop keys)))))
  (let ((owner (py-dict-object-namespace-owner obj)))
    (when (and owner (stringp key) (not (string= key "__dict__")))
      (remhash key (py-object-attrs owner))))
  *py-none*)

(defun py-dict-clear (obj)
  (let ((storage (py-dict-storage obj "clear")))
    (clrhash storage)
    (setf (fill-pointer (py-dict-object-keys obj)) 0)
    (setf (py-object-size obj) 0))
  *py-none*)

(defun py-dict-copy (obj)
  (let ((copy (make-py-dict-object :type *py-dict-type*
                                   :size 0
                                   :value (make-hash-table :test #'equal)))
        (storage (py-dict-storage obj "copy"))
        (keys (py-dict-object-keys obj)))
    (loop for index from 0 below (fill-pointer keys)
          for key = (aref keys index)
          do (py-dict-set-entry copy key (gethash key storage)))
    copy))

(defun make-py-dict-for-storage (storage &optional namespace-owner)
  (let ((dict (make-py-dict-object :type *py-dict-type*
                                   :size 0
                                   :value storage
                                   :namespace-owner namespace-owner)))
    (maphash (lambda (key value)
               (declare (ignore value))
               (unless (string= key "__dict__")
                 (vector-push-extend key (py-dict-object-keys dict))
                 (incf (py-object-size dict))))
             storage)
    dict))

(defun py-dict-has-key-p (dict key)
  (loop for index from 0 below (fill-pointer (py-dict-object-keys dict))
        thereis (equal (aref (py-dict-object-keys dict) index) key)))

(defun py-module-dict-note-key (module name)
  (let ((dict (py-module-object-namespace-dict module)))
    (when (and dict (not (py-dict-has-key-p dict name)))
      (vector-push-extend name (py-dict-object-keys dict))
      (setf (py-object-size dict) (hash-table-count (py-object-attrs module))))))

(defun py-module-dict (module)
  (or (py-module-object-namespace-dict module)
      (setf (py-module-object-namespace-dict module)
            (make-py-dict-for-storage (py-object-attrs module) module))))

(defun py-source-file-loader-dict (loader)
  (or (py-source-file-loader-object-namespace-dict loader)
      (setf (py-source-file-loader-object-namespace-dict loader)
            (make-py-dict-for-storage (py-object-attrs loader) loader))))

(defun py-file-reader-dict (reader)
  (or (py-file-reader-object-namespace-dict reader)
      (setf (py-file-reader-object-namespace-dict reader)
            (make-py-dict-for-storage (py-object-attrs reader) reader))))

(defun make-py-dict-from-pairs (&rest pairs)
  (let ((dict (make-py-dict-object :type *py-dict-type*
                                   :size 0
                                   :value (make-hash-table :test #'equal))))
    (dolist (pair pairs)
      (destructuring-bind (key value) pair
        (py-dict-set-entry dict key value)))
    dict))

(defun make-py-bytes-from-vector (storage)
  (make-py-bytes-object :type *py-bytes-type*
                        :size (length storage)
                        :value storage))

(defun py-read-file-bytes (path)
  (with-open-file (stream path :direction :input
                               :element-type (quote (unsigned-byte 8)))
    (let* ((size (file-length stream))
           (storage (make-array size :element-type (quote (unsigned-byte 8)))))
      (read-sequence storage stream)
      (make-py-bytes-from-vector storage))))

(defun py-write-file-bytes (path data)
  (let ((storage (py-bytes-storage data "set_data")))
    (handler-case
        (progn
          (ensure-directories-exist path)
          (with-open-file (stream path :direction :output
                                       :element-type (quote (unsigned-byte 8))
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
            (write-sequence storage stream)))
      (file-error () nil)
      (sb-int:simple-file-error () nil)))
  *py-none*)

(defun py-decode-source-bytes (data)
  "Decode source bytes for SourceLoader.get_source, including universal newlines."
  (let ((storage (py-bytes-storage data "get_source")))
    (with-output-to-string (stream)
      (loop for index from 0 below (length storage)
            for byte = (aref storage index)
            do (cond
                 ((= byte 13)
                  (when (and (< (1+ index) (length storage))
                             (= (aref storage (1+ index)) 10))
                    (incf index))
                  (write-char #\Newline stream))
                 ((= byte 10)
                  (write-char #\Newline stream))
                 (t
                  (write-char (code-char byte) stream)))))))

(defun py-decode-text-bytes (data &optional (encoding *py-none*))
  (unless (or (eq encoding *py-none*)
              (null encoding)
              (and (stringp encoding)
                   (or (string-equal encoding "utf-8")
                       (string-equal encoding "utf8"))))
    (error "unsupported encoding: ~A" encoding))
  (let* ((storage (py-bytes-storage data "read_text"))
         (decoded (sb-ext:octets-to-string storage :external-format :utf-8)))
    (with-output-to-string (stream)
      (loop for index from 0 below (length decoded)
            for char = (char decoded index)
            do (cond
                 ((char= char #\Return)
                  (when (and (< (1+ index) (length decoded))
                             (char= (char decoded (1+ index)) #\Newline))
                    (incf index))
                  (write-char #\Newline stream))
                 ((char= char #\Newline)
                  (write-char #\Newline stream))
                 (t
                  (write-char char stream)))))))

(defun py-path-size (path)
  (with-open-file (stream path :direction :input
                               :element-type (quote (unsigned-byte 8)))
    (file-length stream)))

(defun py-path-mtime (path)
  (- (coerce (file-write-date path) 'double-float) 2208988800.0d0))

(defun py-path-stats (path)
  (make-py-dict-from-pairs
   (list "mtime" (py-path-mtime path))
   (list "size" (py-path-size path))))

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
      (cond
        ((= slice-length 0)
         (make-py-tuple))
        ((and (= start 0) (= step 1) (= slice-length size))
         obj)
        (t
         (let ((result-storage (make-array slice-length)))
           (loop for offset from 0 below slice-length
                 for index = start then (+ index step)
                 do (setf (aref result-storage offset) (aref storage index)))
           (make-py-tuple-object :type *py-tuple-type*
                                 :size slice-length
                                 :value result-storage)))))))

(defun py-bytes-slice (obj slice)
  (let* ((storage (py-bytes-storage obj "__getitem__"))
         (size (or (py-object-size obj) 0)))
    (multiple-value-bind (start step slice-length)
        (py-list-slice-parameters slice size)
      (let ((result-storage (make-array slice-length :element-type (quote (unsigned-byte 8)))))
        (loop for offset from 0 below slice-length
              for index = start then (+ index step)
              do (setf (aref result-storage offset) (aref storage index)))
        (make-py-bytes-from-vector result-storage)))))

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

(defun py-string-adjust-bound (bound size default)
  (let ((index (if (eq bound *py-none*)
                   default
                   (py-normalize-bool-number bound))))
    (when (< index 0)
      (incf index size))
    (cond
      ((< index 0) 0)
      ((> index size) size)
      (t index))))

(defun py-string-count (value substring &optional
                        (start *py-none*)
                        (end *py-none*))
  (unless (stringp substring)
    (error "must be str, not ~S" substring))
  (let* ((size (length value))
         (substring-size (length substring))
         (adjusted-start (py-string-adjust-bound start size 0))
         (adjusted-end (py-string-adjust-bound end size size))
         (span-size (- adjusted-end adjusted-start)))
    (cond
      ((< span-size 0) 0)
      ((= substring-size 0) (1+ span-size))
      ((< span-size substring-size) 0)
      (t
       (let ((count 0)
             (position adjusted-start))
         (loop
           (let ((match (search substring value
                                :start2 position
                                :end2 adjusted-end)))
             (unless match
               (return count))
             (incf count)
             (setf position (+ match substring-size)))))))))

(defun py-string-find (value substring &optional
                       (start *py-none*)
                       (end *py-none*))
  (unless (stringp substring)
    (error "must be str, not ~S" substring))
  (let* ((size (length value))
         (raw-start (if (eq start *py-none*)
                        0
                        (py-normalize-bool-number start)))
         (adjusted-start (py-string-adjust-bound start size 0))
         (adjusted-end (py-string-adjust-bound end size size)))
    (if (or (> raw-start size) (> adjusted-start adjusted-end))
        -1
        (or (search substring value
                    :start2 adjusted-start
                    :end2 adjusted-end)
            -1))))

(defun py-string-rfind (value substring &optional
                        (start *py-none*)
                        (end *py-none*))
  (unless (stringp substring)
    (error "must be str, not ~S" substring))
  (let* ((size (length value))
         (raw-start (if (eq start *py-none*)
                        0
                        (py-normalize-bool-number start)))
         (adjusted-start (py-string-adjust-bound start size 0))
         (adjusted-end (py-string-adjust-bound end size size)))
    (if (or (> raw-start size) (> adjusted-start adjusted-end))
        -1
        (or (search substring value
                    :start2 adjusted-start
                    :end2 adjusted-end
                    :from-end t)
            -1))))

(defun py-string-index (value substring &optional
                        (start *py-none*)
                        (end *py-none*))
  (let ((result (py-string-find value substring start end)))
    (if (= result -1)
        (error "substring not found")
        result)))

(defun py-string-rindex (value substring &optional
                         (start *py-none*)
                         (end *py-none*))
  (let ((result (py-string-rfind value substring start end)))
    (if (= result -1)
        (error "substring not found")
        result)))

(defun py-string-tailmatch-one (value substring start end direction operation)
  (unless (stringp substring)
    (error "~A first arg must be str or a tuple of str, not ~S"
           operation substring))
  (let* ((size (length value))
         (adjusted-start
           (let ((index (if (eq start *py-none*)
                            0
                            (py-normalize-bool-number start))))
             (when (< index 0)
               (incf index size)
               (when (< index 0)
                 (setf index 0)))
             index))
         (adjusted-end (py-string-adjust-bound end size size))
         (substring-size (length substring))
         (offset-limit (- adjusted-end substring-size)))
    (cond
      ((< offset-limit adjusted-start) *py-false*)
      ((= substring-size 0) *py-true*)
      (t
       (let ((offset (if (> direction 0)
                         offset-limit
                         adjusted-start)))
         (py-bool
          (string= value substring
                   :start1 offset
                   :end1 (+ offset substring-size))))))))

(defun py-string-tailmatch (value substring start end direction operation)
  (if (py-tuple-object-p substring)
      (let ((storage (py-tuple-storage substring operation))
            (size (or (py-object-size substring) 0)))
        (loop for index from 0 below size
              for item = (aref storage index)
              do (progn
                   (unless (stringp item)
                     (error "tuple for ~A must only contain str, not ~S"
                            operation item))
                   (when (py-truthy-p
                          (py-string-tailmatch-one value item start end direction operation))
                     (return *py-true*)))
              finally (return *py-false*)))
      (py-string-tailmatch-one value substring start end direction operation)))

(defun py-string-startswith (value prefix &optional
                             (start *py-none*)
                             (end *py-none*))
  (py-string-tailmatch value prefix start end -1 "startswith"))

(defun py-string-endswith (value suffix &optional
                           (start *py-none*)
                           (end *py-none*))
  (py-string-tailmatch value suffix start end 1 "endswith"))

(defun py-string-join (separator iterable)
  (unless (stringp separator)
    (error "separator: expected str instance, got ~S" separator))
  (let ((items '())
        (iterator (py-iter iterable)))
    (loop
      (multiple-value-bind (item found) (py-next-item iterator)
        (unless found
          (return))
        (unless (stringp item)
          (error "sequence item ~A: expected str instance, got ~S"
                 (length items)
                 item))
        (push item items)))
    (with-output-to-string (stream)
      (loop for item in (nreverse items)
            for first = t then nil
            do (progn
                 (unless first
                   (princ separator stream))
                 (princ item stream))))))

(defun py-string-expandtabs (value &optional (tabsize 8))
  (let ((normalized-tabsize (py-normalize-bool-number tabsize)))
    (unless (integerp normalized-tabsize)
      (error "integer argument expected, got ~S" tabsize))
    (with-output-to-string (stream)
      (let ((line-position 0))
        (loop for char across value
              do (cond
                   ((char= char #\Tab)
                    (when (> normalized-tabsize 0)
                      (let ((spaces (- normalized-tabsize
                                       (mod line-position normalized-tabsize))))
                        (loop repeat spaces do (write-char #\Space stream))
                        (incf line-position spaces))))
                   (t
                    (write-char char stream)
                    (incf line-position)
                    (when (or (char= char #\Newline)
                              (= (char-code char) 13))
                      (setf line-position 0)))))))))

(defun py-string-removeprefix (value prefix)
  (unless (stringp prefix)
    (error "removeprefix() argument must be str, not ~S" prefix))
  (let ((prefix-size (length prefix)))
    (if (and (<= prefix-size (length value))
             (string= value prefix :start1 0 :end1 prefix-size))
        (subseq value prefix-size)
        value)))

(defun py-string-removesuffix (value suffix)
  (unless (stringp suffix)
    (error "removesuffix() argument must be str, not ~S" suffix))
  (let ((suffix-size (length suffix))
        (value-size (length value)))
    (if (and (> suffix-size 0)
             (<= suffix-size value-size)
             (string= value suffix :start1 (- value-size suffix-size)))
        (subseq value 0 (- value-size suffix-size))
        value)))

(defun py-string-replace (value old new &optional (count -1))
  (unless (stringp old)
    (error "replace() argument 1 must be str, not ~S" old))
  (unless (stringp new)
    (error "replace() argument 2 must be str, not ~S" new))
  (let ((normalized-count (py-normalize-bool-number count)))
    (unless (integerp normalized-count)
      (error "replace() argument 3 must be int, not ~S" count))
    (cond
      ((= normalized-count 0) value)
      ((string= old new) value)
      ((= (length old) 0)
       (let ((remaining normalized-count))
         (with-output-to-string (stream)
           (loop for index from 0 to (length value)
                 do (progn
                      (when (or (< normalized-count 0) (> remaining 0))
                        (princ new stream)
                        (when (> normalized-count 0)
                          (decf remaining)))
                      (when (< index (length value))
                        (write-char (char value index) stream)))))))
      (t
       (let ((remaining normalized-count)
             (position 0)
             (old-size (length old)))
         (with-output-to-string (stream)
           (loop
             (let ((match (and (or (< normalized-count 0) (> remaining 0))
                               (search old value :start2 position))))
               (unless match
                 (princ (subseq value position) stream)
                 (return))
               (princ (subseq value position match) stream)
               (princ new stream)
               (setf position (+ match old-size))
               (when (> normalized-count 0)
                 (decf remaining))))))))))

(defun py-string-partition (value separator &optional (from-end nil))
  (unless (stringp separator)
    (error "partition() argument must be str, not ~S" separator))
  (when (= (length separator) 0)
    (error "empty separator"))
  (let ((match (search separator value :from-end from-end)))
    (if match
        (make-py-tuple
         (subseq value 0 match)
         separator
         (subseq value (+ match (length separator))))
        (if from-end
            (make-py-tuple "" "" value)
            (make-py-tuple value "" "")))))

(defun py-string-split-explicit (value separator maxsplit)
  (unless (stringp separator)
    (error "must be str or None, not ~S" separator))
  (when (= (length separator) 0)
    (error "empty separator"))
  (let ((items '())
        (position 0)
        (splits 0)
        (separator-size (length separator)))
    (loop
      (let ((match (and (or (< maxsplit 0) (< splits maxsplit))
                        (search separator value :start2 position))))
        (unless match
          (push (subseq value position) items)
          (return))
        (push (subseq value position match) items)
        (setf position (+ match separator-size))
        (incf splits)))
    (apply #'make-py-list (nreverse items))))

(defun py-string-split-whitespace (value maxsplit)
  (let ((items '())
        (size (length value))
        (position 0)
        (splits 0))
    (labels ((whitespacep (index)
               (py-string-default-strip-char-p (char value index)))
             (skip-whitespace ()
               (loop while (and (< position size) (whitespacep position))
                     do (incf position))))
      (loop
        (skip-whitespace)
        (when (>= position size)
          (return))
        (when (and (>= maxsplit 0) (>= splits maxsplit))
          (push (subseq value position) items)
          (return))
        (let ((start position))
          (loop while (and (< position size) (not (whitespacep position)))
                do (incf position))
          (push (subseq value start position) items)
          (incf splits))))
    (apply #'make-py-list (nreverse items))))

(defun py-string-split (value &optional (separator *py-none*) (maxsplit -1))
  (let ((normalized-maxsplit (py-normalize-bool-number maxsplit)))
    (unless (integerp normalized-maxsplit)
      (error "integer argument expected, got ~S" maxsplit))
    (if (eq separator *py-none*)
        (py-string-split-whitespace value normalized-maxsplit)
        (py-string-split-explicit value separator normalized-maxsplit))))

(defun py-string-rsplit-explicit (value separator maxsplit)
  (unless (stringp separator)
    (error "must be str or None, not ~S" separator))
  (when (= (length separator) 0)
    (error "empty separator"))
  (let ((items '())
        (position (length value))
        (splits 0)
        (separator-size (length separator)))
    (loop
      (let ((match (and (or (< maxsplit 0) (< splits maxsplit))
                        (search separator value :end2 position :from-end t))))
        (unless match
          (push (subseq value 0 position) items)
          (return))
        (push (subseq value (+ match separator-size) position) items)
        (setf position match)
        (incf splits)))
    (apply #'make-py-list items)))

(defun py-string-rsplit-whitespace (value maxsplit)
  (let ((items '())
        (position (length value))
        (splits 0))
    (labels ((whitespacep (index)
               (py-string-default-strip-char-p (char value index)))
             (skip-whitespace ()
               (loop while (and (> position 0) (whitespacep (1- position)))
                     do (decf position))))
      (skip-whitespace)
      (loop
        (when (<= position 0)
          (return))
        (when (and (>= maxsplit 0) (>= splits maxsplit))
          (push (subseq value 0 position) items)
          (return))
        (let ((end position))
          (loop while (and (> position 0) (not (whitespacep (1- position))))
                do (decf position))
          (push (subseq value position end) items)
          (incf splits))
        (skip-whitespace)))
    (apply #'make-py-list items)))

(defun py-string-rsplit (value &optional (separator *py-none*) (maxsplit -1))
  (let ((normalized-maxsplit (py-normalize-bool-number maxsplit)))
    (unless (integerp normalized-maxsplit)
      (error "integer argument expected, got ~S" maxsplit))
    (if (eq separator *py-none*)
        (py-string-rsplit-whitespace value normalized-maxsplit)
        (py-string-rsplit-explicit value separator normalized-maxsplit))))

(defun py-string-linebreak-p (char)
  (member (char-code char)
          '(#x000a #x000b #x000c #x000d #x001c #x001d #x001e #x0085 #x2028 #x2029)))

(defun py-string-splitlines (value &optional (keepends *py-false*))
  (let ((items '())
        (size (length value))
        (i 0)
        (j 0)
        (keepends-p (py-truthy-p keepends)))
    (loop while (< i size)
          do (progn
               (loop while (and (< i size)
                                (not (py-string-linebreak-p (char value i))))
                     do (incf i))
               (let ((eol i))
                 (when (< i size)
                   (if (and (char= (char value i) #\Return)
                            (< (1+ i) size)
                            (char= (char value (1+ i)) #\Newline))
                       (incf i 2)
                       (incf i))
                   (when keepends-p
                     (setf eol i)))
                 (push (subseq value j eol) items)
                 (setf j i))))
    (apply #'make-py-list (nreverse items))))

(defun py-string-lower (value)
  (string-downcase value))

(defun py-string-folded-full-char (char)
  (case (char-code char)
    (#x00df "ss")
    (otherwise (string (char-downcase char)))))

(defun py-string-casefold (value)
  (if (py-truthy-p (py-string-isascii value))
      (string-downcase value)
      (with-output-to-string (stream)
        (loop for char across value
              do (princ (py-string-folded-full-char char) stream)))))

(defun py-string-upper (value)
  (string-upcase value))

(defun py-string-capitalize (value)
  (if (= (length value) 0)
      value
      (concatenate 'string
                   (string-upcase (subseq value 0 1))
                   (string-downcase (subseq value 1)))))

(defun py-string-fill-char (fillchar operation)
  (unless (and (stringp fillchar)
               (= (length fillchar) 1))
    (error "~A() argument 2 must be a single character string" operation))
  (char fillchar 0))

(defun py-string-ljust (value width &optional (fillchar " "))
  (let* ((normalized-width (py-normalize-bool-number width))
         (size (length value)))
    (unless (integerp normalized-width)
      (error "str.ljust() width must be an integer, got ~S" width))
    (if (>= size normalized-width)
        value
        (concatenate 'string
                     value
                     (make-string (- normalized-width size)
                                  :initial-element
                                  (py-string-fill-char fillchar "ljust"))))))

(defun py-string-rjust (value width &optional (fillchar " "))
  (let* ((normalized-width (py-normalize-bool-number width))
         (size (length value)))
    (unless (integerp normalized-width)
      (error "str.rjust() width must be an integer, got ~S" width))
    (if (>= size normalized-width)
        value
        (concatenate 'string
                     (make-string (- normalized-width size)
                                  :initial-element
                                  (py-string-fill-char fillchar "rjust"))
                     value))))

(defun py-string-zfill (value width)
  (let* ((normalized-width (py-normalize-bool-number width))
         (size (length value)))
    (unless (integerp normalized-width)
      (error "str.zfill() width must be an integer, got ~S" width))
    (if (>= size normalized-width)
        value
        (let* ((fill (- normalized-width size))
               (padded (concatenate 'string
                                    (make-string fill :initial-element #\0)
                                    value)))
          (when (and (> size 0)
                     (member (char value 0) '(#\+ #\-) :test #'char=))
            (setf (char padded 0) (char value 0))
            (setf (char padded fill) #\0))
          padded))))

(defun py-string-center (value width &optional (fillchar " "))
  (let* ((normalized-width (py-normalize-bool-number width))
         (size (length value)))
    (unless (integerp normalized-width)
      (error "str.center() width must be an integer, got ~S" width))
    (if (>= size normalized-width)
        value
        (let* ((margin (- normalized-width size))
               (left (+ (floor margin 2)
                        (if (and (oddp margin) (oddp normalized-width)) 1 0)))
               (right (- margin left))
               (fill (py-string-fill-char fillchar "center")))
          (concatenate 'string
                       (make-string left :initial-element fill)
                       value
                       (make-string right :initial-element fill))))))

(defun py-string-isascii (value)
  (py-bool
   (loop for char across value
         always (<= (char-code char) #x7f))))

(defun py-string-isdecimal (value)
  (py-bool
   (and (> (length value) 0)
        (loop for char across value
              always (digit-char-p char 10)))))

(defun py-unicode-digit-char-p (char)
  (let ((code (char-code char)))
    (or (digit-char-p char 10)
        (member code '(#x00b2 #x00b3 #x00b9)))))

(defun py-unicode-numeric-char-p (char)
  (let ((code (char-code char)))
    (or (py-unicode-digit-char-p char)
        (member code '(#x00bc #x00bd #x00be)))))

(defun py-string-isdigit (value)
  (py-bool
   (and (> (length value) 0)
        (loop for char across value
              always (py-unicode-digit-char-p char)))))

(defun py-string-isnumeric (value)
  (py-bool
   (and (> (length value) 0)
        (loop for char across value
              always (py-unicode-numeric-char-p char)))))

(defun py-string-isalpha (value)
  (py-bool
   (and (> (length value) 0)
        (loop for char across value
              always (alpha-char-p char)))))

(defun py-string-isalnum (value)
  (py-bool
   (and (> (length value) 0)
        (loop for char across value
              always (or (alpha-char-p char)
                         (py-unicode-digit-char-p char))))))

(defun py-string-identifier-start-char-p (char)
  (or (char= char #\_)
      (alpha-char-p char)))

(defun py-unicode-combining-mark-char-p (char)
  (let ((code (char-code char)))
    (or (<= #x0300 code #x036f)
        (<= #x1ab0 code #x1aff)
        (<= #x1dc0 code #x1dff)
        (<= #x20d0 code #x20ff)
        (<= #xfe20 code #xfe2f))))

(defun py-string-identifier-continue-char-p (char)
  (or (py-string-identifier-start-char-p char)
      (digit-char-p char 10)
      (py-unicode-combining-mark-char-p char)))

(defun py-string-isidentifier (value)
  (py-bool
   (and (> (length value) 0)
        (py-string-identifier-start-char-p (char value 0))
        (loop for index from 1 below (length value)
              always (py-string-identifier-continue-char-p
                      (char value index))))))

(defun py-unicode-printable-char-p (char)
  (let ((code (char-code char)))
    (and (or (= code 32)
             (not (or (<= 0 code 31)
                      (<= 127 code 159)
                      (member code '(160 173 5760 8192 8193 8194 8195 8196
                                     8197 8198 8199 8200 8201 8202 8232
                                     8233 8239 8287 12288)))))
         t)))

(defun py-string-isprintable (value)
  (py-bool
   (loop for char across value
         always (py-unicode-printable-char-p char))))

(defun py-string-isspace (value)
  (py-bool
   (and (> (length value) 0)
        (loop for char across value
              always (py-string-default-strip-char-p char)))))

(defun py-string-islower (value)
  (let ((cased nil))
    (loop for char across value
          do (let ((is-cased (py-string-cased-char-p char)))
               (when (and is-cased
                          (char/= char (char-downcase char)))
                 (return-from py-string-islower *py-false*))
               (when is-cased
                 (setf cased t))))
    (py-bool cased)))

(defun py-string-isupper (value)
  (let ((cased nil))
    (loop for char across value
          do (let ((is-cased (py-string-cased-char-p char)))
               (when (and is-cased
                          (char/= char (char-upcase char)))
                 (return-from py-string-isupper *py-false*))
               (when is-cased
                 (setf cased t))))
    (py-bool cased)))

(defun py-string-istitle (value)
  (let ((cased nil)
        (previous-is-cased nil))
    (loop for char across value
          do (let* ((is-cased (py-string-cased-char-p char))
                    (is-uppercase (and is-cased
                                       (char= char (char-upcase char))))
                    (is-lowercase (and is-cased
                                       (char= char (char-downcase char)))))
               (cond
                 (is-uppercase
                  (when previous-is-cased
                    (return-from py-string-istitle *py-false*))
                  (setf previous-is-cased t)
                  (setf cased t))
                 (is-lowercase
                  (unless previous-is-cased
                    (return-from py-string-istitle *py-false*))
                  (setf previous-is-cased t)
                  (setf cased t))
                 (t
                  (setf previous-is-cased nil)))))
    (py-bool cased)))

(defun py-string-swapcase (value)
  (with-output-to-string (stream)
    (loop for char across value
          do (cond
               ((char= char (char-upcase char))
                (write-char (char-downcase char) stream))
               ((char= char (char-downcase char))
                (write-char (char-upcase char) stream))
               (t
                (write-char char stream))))))

(defun py-string-capitalize (value)
  (if (= (length value) 0)
      value
      (concatenate 'string
                   (string-upcase (subseq value 0 1))
                   (string-downcase (subseq value 1)))))

(defun py-string-cased-char-p (char)
  (or (char/= char (char-upcase char))
      (char/= char (char-downcase char))))

(defun py-string-title (value)
  (with-output-to-string (stream)
    (let ((previous-is-cased nil))
      (loop for char across value
            do (progn
                 (write-char
                  (if previous-is-cased
                      (char-downcase char)
                      (char-upcase char))
                  stream)
                 (setf previous-is-cased
                       (py-string-cased-char-p char)))))))

(defun py-string-default-strip-char-p (char)
  (member (char-code char) '(9 10 11 12 13 28 29 30 31 32 133 160 5760 8192 8193 8194 8195 8196 8197 8198 8199 8200 8201 8202 8232 8233 8239 8287 12288)))

(defun py-string-strip-char-p (char chars)
  (if (eq chars *py-none*)
      (py-string-default-strip-char-p char)
      (find char chars :test #'char=)))

(defun py-string-strip (value &optional (chars *py-none*) (direction :both))
  (unless (or (eq chars *py-none*) (stringp chars))
    (error "strip arg must be None or str"))
  (let ((start 0)
        (end (length value)))
    (when (member direction '(:both :left))
      (loop while (and (< start end)
                       (py-string-strip-char-p (char value start) chars))
            do (incf start)))
    (when (member direction '(:both :right))
      (loop while (and (< start end)
                       (py-string-strip-char-p (char value (1- end)) chars))
            do (decf end)))
    (subseq value start end)))

(defun py-string-getitem (value index)
  (if (py-slice-object-p index)
      (py-string-slice value index)
      (let ((normalized-index (py-string-normalized-index value index)))
        (subseq value normalized-index (1+ normalized-index)))))

(setf (py-type-attr *py-str-type* "__len__")
      (lambda (obj)
        (py-len obj)))

(setf (py-type-attr *py-str-type* "__getitem__")
      (lambda (obj index)
        (py-string-getitem obj index)))

(setf (py-type-attr *py-str-type* "__repr__")
      (lambda (obj)
        (with-output-to-string (stream)
          (py-repr obj stream))))

(setf (py-type-attr *py-str-type* "__contains__")
      (lambda (obj value)
        (py-contains value obj)))

(setf (py-type-attr *py-str-type* "__add__")
      (lambda (obj value)
        (py-add obj value)))

(setf (py-type-attr *py-str-type* "__mul__")
      (lambda (obj value)
        (py-mul obj value)))

(setf (py-type-attr *py-str-type* "__rmul__")
      (lambda (obj value)
        (py-mul value obj)))

(setf (py-type-attr *py-str-type* "count")
      (lambda (obj substring &optional
                   (start *py-none*)
                   (end *py-none*))
        (py-string-count obj substring start end)))

(setf (py-type-attr *py-str-type* "find")
      (lambda (obj substring &optional
                   (start *py-none*)
                   (end *py-none*))
        (py-string-find obj substring start end)))

(setf (py-type-attr *py-str-type* "rfind")
      (lambda (obj substring &optional
                   (start *py-none*)
                   (end *py-none*))
        (py-string-rfind obj substring start end)))

(setf (py-type-attr *py-str-type* "index")
      (lambda (obj substring &optional
                   (start *py-none*)
                   (end *py-none*))
        (py-string-index obj substring start end)))

(setf (py-type-attr *py-str-type* "rindex")
      (lambda (obj substring &optional
                   (start *py-none*)
                   (end *py-none*))
        (py-string-rindex obj substring start end)))

(setf (py-type-attr *py-str-type* "startswith")
      (lambda (obj prefix &optional
                   (start *py-none*)
                   (end *py-none*))
        (py-string-startswith obj prefix start end)))

(setf (py-type-attr *py-str-type* "endswith")
      (lambda (obj suffix &optional
                   (start *py-none*)
                   (end *py-none*))
        (py-string-endswith obj suffix start end)))

(setf (py-type-attr *py-str-type* "join")
      (lambda (obj iterable)
        (py-string-join obj iterable)))

(setf (py-type-attr *py-str-type* "expandtabs")
      (lambda (obj &optional (tabsize 8))
        (py-string-expandtabs obj tabsize)))

(setf (py-type-attr *py-str-type* "removeprefix")
      (lambda (obj prefix)
        (py-string-removeprefix obj prefix)))

(setf (py-type-attr *py-str-type* "removesuffix")
      (lambda (obj suffix)
        (py-string-removesuffix obj suffix)))

(setf (py-type-attr *py-str-type* "replace")
      (lambda (obj old new &optional (count -1))
        (py-string-replace obj old new count)))

(setf (py-type-attr *py-str-type* "partition")
      (lambda (obj separator)
        (py-string-partition obj separator)))

(setf (py-type-attr *py-str-type* "rpartition")
      (lambda (obj separator)
        (py-string-partition obj separator t)))

(setf (py-type-attr *py-str-type* "split")
      (lambda (obj &optional (separator *py-none*) (maxsplit -1))
        (py-string-split obj separator maxsplit)))

(setf (py-type-attr *py-str-type* "rsplit")
      (lambda (obj &optional (separator *py-none*) (maxsplit -1))
        (py-string-rsplit obj separator maxsplit)))

(setf (py-type-attr *py-str-type* "splitlines")
      (lambda (obj &optional (keepends *py-false*))
        (py-string-splitlines obj keepends)))

(setf (py-type-attr *py-str-type* "lower")
      (lambda (obj)
        (py-string-lower obj)))

(setf (py-type-attr *py-str-type* "casefold")
      (lambda (obj)
        (py-string-casefold obj)))

(setf (py-type-attr *py-str-type* "upper")
      (lambda (obj)
        (py-string-upper obj)))

(setf (py-type-attr *py-str-type* "capitalize")
      (lambda (obj)
        (py-string-capitalize obj)))

(setf (py-type-attr *py-str-type* "ljust")
      (lambda (obj width &optional (fillchar " "))
        (py-string-ljust obj width fillchar)))

(setf (py-type-attr *py-str-type* "rjust")
      (lambda (obj width &optional (fillchar " "))
        (py-string-rjust obj width fillchar)))

(setf (py-type-attr *py-str-type* "zfill")
      (lambda (obj width)
        (py-string-zfill obj width)))

(setf (py-type-attr *py-str-type* "center")
      (lambda (obj width &optional (fillchar " "))
        (py-string-center obj width fillchar)))

(setf (py-type-attr *py-str-type* "isascii")
      (lambda (obj)
        (py-string-isascii obj)))

(setf (py-type-attr *py-str-type* "isdecimal")
      (lambda (obj)
        (py-string-isdecimal obj)))

(setf (py-type-attr *py-str-type* "isdigit")
      (lambda (obj)
        (py-string-isdigit obj)))

(setf (py-type-attr *py-str-type* "isnumeric")
      (lambda (obj)
        (py-string-isnumeric obj)))

(setf (py-type-attr *py-str-type* "isalpha")
      (lambda (obj)
        (py-string-isalpha obj)))

(setf (py-type-attr *py-str-type* "isalnum")
      (lambda (obj)
        (py-string-isalnum obj)))

(setf (py-type-attr *py-str-type* "isidentifier")
      (lambda (obj)
        (py-string-isidentifier obj)))

(setf (py-type-attr *py-str-type* "isprintable")
      (lambda (obj)
        (py-string-isprintable obj)))

(setf (py-type-attr *py-str-type* "isspace")
      (lambda (obj)
        (py-string-isspace obj)))

(setf (py-type-attr *py-str-type* "islower")
      (lambda (obj)
        (py-string-islower obj)))

(setf (py-type-attr *py-str-type* "isupper")
      (lambda (obj)
        (py-string-isupper obj)))

(setf (py-type-attr *py-str-type* "istitle")
      (lambda (obj)
        (py-string-istitle obj)))

(setf (py-type-attr *py-str-type* "swapcase")
      (lambda (obj)
        (py-string-swapcase obj)))

(setf (py-type-attr *py-str-type* "capitalize")
      (lambda (obj)
        (py-string-capitalize obj)))

(setf (py-type-attr *py-str-type* "title")
      (lambda (obj)
        (py-string-title obj)))

(setf (py-type-attr *py-str-type* "strip")
      (lambda (obj &optional (chars *py-none*))
        (py-string-strip obj chars :both)))

(setf (py-type-attr *py-str-type* "lstrip")
      (lambda (obj &optional (chars *py-none*))
        (py-string-strip obj chars :left)))

(setf (py-type-attr *py-str-type* "rstrip")
      (lambda (obj &optional (chars *py-none*))
        (py-string-strip obj chars :right)))

(setf (py-type-attr *py-int-type* "bit_length")
      (lambda (obj)
        (py-int-bit-length obj)))

(setf (py-type-attr *py-int-type* "bit_count")
      (lambda (obj)
        (py-int-bit-count obj)))

(setf (py-type-attr *py-int-type* "__round__")
      (lambda (obj &optional (ndigits *py-none*))
        (if (eq ndigits *py-none*)
            (py-normalize-bool-number obj)
            (error "int.__round__() with ndigits is not supported by Clamp yet"))))

(setf (py-type-attr *py-float-type* "__round__")
      (lambda (obj &optional (ndigits *py-none*))
        (if (eq ndigits *py-none*)
            (round obj)
            (error "float.__round__() with ndigits is not supported by Clamp yet"))))

(defun py-list-resize (obj new-size)
  (let* ((storage (py-list-storage obj "resize"))
         (old-size (or (py-object-size obj) 0))
         (allocated (py-list-object-allocated obj)))
    (if (and (<= new-size allocated)
             (>= new-size (floor allocated 2)))
        (progn
          (setf (fill-pointer storage) new-size)
          (setf (py-object-size obj) new-size)
          storage)
        (let* ((new-allocated
                 (if (= new-size 0)
                     0
                     (logand (+ new-size (ash new-size -3) 6) (lognot 3))))
               (new-allocated
                 (if (> (- new-size old-size) (- new-allocated new-size))
                     (logand (+ new-size 3) (lognot 3))
                     new-allocated))
               (new-storage (make-array new-allocated
                                        :adjustable t
                                        :fill-pointer new-size)))
          (loop for index from 0 below (min old-size new-size)
                do (setf (aref new-storage index) (aref storage index)))
          (setf (py-object-value obj) new-storage)
          (setf (py-object-size obj) new-size)
          (setf (py-list-object-allocated obj) new-allocated)
          new-storage))))

(setf (py-type-attr *py-list-type* "append")
      (lambda (obj value)
        (let* ((size (or (py-object-size obj) 0))
               (storage (py-list-resize obj (1+ size))))
          (setf (aref storage size) value))
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
        (let* ((size (or (py-object-size obj) 0))
               (normalized-index (py-list-insert-index size index))
               (storage (py-list-resize obj (1+ size))))
          (loop for i downfrom size above normalized-index
                do (setf (aref storage i) (aref storage (1- i))))
          (setf (aref storage normalized-index) value))
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

(defparameter +py-list-basic-size+ 40)
(defparameter +py-tuple-basic-size+ 32)
(defparameter +py-object-pointer-size+ 8)

(setf (py-type-attr *py-list-type* "__sizeof__")
      (lambda (obj)
        (py-list-storage obj "__sizeof__")
        (+ +py-list-basic-size+
           (* (py-list-object-allocated obj) +py-object-pointer-size+))))

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

(defun py-list-set-slice (obj slice value)
  (let* ((storage (py-list-storage obj "__setitem__"))
         (size (or (py-object-size obj) 0))
         (replacement (py-list value))
         (replacement-storage (py-list-storage replacement "__setitem__"))
         (replacement-size (or (py-object-size replacement) 0)))
    (multiple-value-bind (start step slice-length stop)
        (py-list-slice-parameters slice size)
      (if (= step 1)
          (let ((result-storage (make-array 0 :adjustable t :fill-pointer 0)))
            (loop for index from 0 below start
                  do (vector-push-extend (aref storage index) result-storage))
            (loop for index from 0 below replacement-size
                  do (vector-push-extend (aref replacement-storage index)
                                         result-storage))
            (loop for index from stop below size
                  do (vector-push-extend (aref storage index) result-storage))
            (setf (py-object-value obj) result-storage)
            (setf (py-object-size obj) (fill-pointer result-storage))
            (setf (py-list-object-allocated obj)
                  (array-total-size result-storage)))
          (progn
            (unless (= replacement-size slice-length)
              (error "attempt to assign sequence of size ~A to extended slice of size ~A"
                     replacement-size
                     slice-length))
            (loop for offset from 0 below slice-length
                  for index = start then (+ index step)
                  do (setf (aref storage index)
                           (aref replacement-storage offset)))))))
  *py-none*)

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

(setf (py-type-attr *py-dict-type* "__len__")
      (lambda (obj)
        (py-dict-storage obj "__len__")
        (or (py-object-size obj) 0)))

(setf (py-type-attr *py-dict-type* "__contains__")
      (lambda (obj key)
        (multiple-value-bind (value found)
            (gethash key (py-dict-storage obj "__contains__"))
          (declare (ignore value))
          (py-bool found))))

(setf (py-type-attr *py-dict-type* "__getitem__")
      (lambda (obj key)
        (multiple-value-bind (value found)
            (gethash key (py-dict-storage obj "__getitem__"))
          (if found
              value
              (error "~S" key)))))

(setf (py-type-attr *py-dict-type* "__setitem__")
      (lambda (obj key value)
        (py-dict-set-entry obj key value)
        *py-none*))

(setf (py-type-attr *py-dict-type* "__delitem__")
      (lambda (obj key)
        (py-dict-delete-entry obj key)))

(setf (py-type-attr *py-dict-type* "get")
      (lambda (obj key &optional (default *py-none*))
        (multiple-value-bind (value found)
            (gethash key (py-dict-storage obj "get"))
          (if found value default))))

(defparameter +py-dict-pop-missing-default+ (gensym "PY-DICT-POP-MISSING-DEFAULT"))

(setf (py-type-attr *py-dict-type* "pop")
      (lambda (obj key &optional (default +py-dict-pop-missing-default+))
        (multiple-value-bind (value found)
            (gethash key (py-dict-storage obj "pop"))
          (cond
            (found
             (py-dict-delete-entry obj key)
             value)
            ((not (eq default +py-dict-pop-missing-default+))
             default)
            (t
             (error "~S" key))))))

(setf (py-type-attr *py-dict-type* "copy")
      (lambda (obj)
        (py-dict-copy obj)))

(setf (py-type-attr *py-dict-type* "clear")
      (lambda (obj)
        (py-dict-clear obj)))

(setf (py-type-attr *py-dict-type* "__repr__")
      (lambda (obj)
        (with-output-to-string (stream)
          (py-repr obj stream))))

(setf (py-type-attr *py-list-type* "__getitem__")
      (lambda (obj index)
        (if (py-slice-object-p index)
            (py-list-slice obj index)
            (aref (py-list-storage obj "__getitem__")
                  (py-list-normalized-index obj index "list")))))

(setf (py-type-attr *py-list-type* "__setitem__")
      (lambda (obj index value)
        (if (py-slice-object-p index)
            (py-list-set-slice obj index value)
            (setf (aref (py-list-storage obj "__setitem__")
                        (py-list-normalized-index obj index "list"))
                  value))
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

(setf (py-type-attr *py-bytes-type* "__len__")
      (lambda (obj)
        (py-bytes-storage obj "__len__")
        (or (py-object-size obj) 0)))

(setf (py-type-attr *py-bytes-type* "__getitem__")
      (lambda (obj index)
        (if (py-slice-object-p index)
            (py-bytes-slice obj index)
            (aref (py-bytes-storage obj "__getitem__")
                  (py-list-normalized-index obj index "bytes")))))

(setf (py-type-attr *py-bytes-type* "__repr__")
      (lambda (obj)
        (with-output-to-string (stream)
          (py-repr obj stream))))

(setf (py-type-attr *py-bytes-type* "__iter__")
      (lambda (obj)
        (py-iter obj)))

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

(setf (py-type-attr *py-tuple-type* "__sizeof__")
      (lambda (obj)
        (py-tuple-storage obj "__sizeof__")
        (+ +py-tuple-basic-size+
           (* (or (py-object-size obj) 0) +py-object-pointer-size+))))

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

(defvar *py-empty-tuple* nil)

(defun make-py-tuple (&rest values)
  (if (null values)
      (or *py-empty-tuple*
          (setf *py-empty-tuple*
                (make-py-tuple-object :type *py-tuple-type*
                                      :size 0
                                      :value (make-array 0))))
      (let* ((size (length values))
             (storage (make-array size)))
        (loop for value in values
              for index from 0
              do (setf (aref storage index) value))
        (make-py-tuple-object :type *py-tuple-type*
                              :size size
                              :value storage))))

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
    (when (= repeat-count 1)
      (return-from py-tuple-repeat items))
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

(defun py-pow (left right &optional (modulus *py-none*))
  (let ((normalized-left (py-normalize-bool-number left))
        (normalized-right (py-normalize-bool-number right))
        (normalized-modulus (py-normalize-bool-number modulus)))
    (unless (and (numberp normalized-left) (numberp normalized-right))
      (error "Unsupported Python ** or pow() between ~S and ~S" left right))
    (if (eq modulus *py-none*)
        (let ((result (expt normalized-left normalized-right)))
          (if (and (integerp normalized-left)
                   (integerp normalized-right)
                   (< normalized-right 0))
              (float result)
              result))
        (progn
          (unless (and (integerp normalized-left)
                       (integerp normalized-right)
                       (integerp normalized-modulus))
            (error "pow() 3rd argument only supported for integers"))
          (when (= normalized-modulus 0)
            (error "pow() 3rd argument cannot be 0"))
          (mod (expt normalized-left normalized-right)
               normalized-modulus)))))

(defun py-truediv (left right)
  (let ((normalized-left (py-normalize-bool-number left))
        (normalized-right (py-normalize-bool-number right)))
    (cond
      ((py-path-object-p left)
       (py-path-joinpath left right))
      ((and (numberp normalized-left) (numberp normalized-right))
       (float (/ normalized-left normalized-right)))
      (t
       (error "Unsupported Python / between ~S and ~S" left right)))))

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

(defun py-number-to-base (value base prefix)
  (let ((normalized-value (py-normalize-bool-number value)))
    (unless (integerp normalized-value)
      (error "integer argument expected, got ~S" value))
    (if (< normalized-value 0)
        (format nil "-~A~VR" prefix base (- normalized-value))
        (format nil "~A~VR" prefix base normalized-value))))

(defun py-bin (value)
  (py-number-to-base value 2 "0b"))

(defun py-oct (value)
  (py-number-to-base value 8 "0o"))

(defun py-hex (value)
  (string-downcase (py-number-to-base value 16 "0x")))

(defun py-chr (value)
  (let ((normalized-value (py-normalize-bool-number value)))
    (unless (integerp normalized-value)
      (error "chr() arg must be an integer"))
    (unless (<= 0 normalized-value #x10ffff)
      (error "chr() arg not in range(0x110000)"))
    (let ((char (code-char normalized-value)))
      (unless char
        (error "chr() arg not representable as a Clamp character"))
      (string char))))

(defun py-ord (value)
  (unless (and (stringp value) (= (length value) 1))
    (if (stringp value)
        (error "ord() expected a character, but string of length ~A found"
               (length value))
        (error "ord() expected string of length 1, got ~S" value)))
  (char-code (char value 0)))

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
           (eq (py-object-type obj) *py-bytes-iterator-type*)
           (eq (py-object-type obj) *py-tuple-iterator-type*)
           (eq (py-object-type obj) *py-tuple-reverse-iterator-type*)
           (eq (py-object-type obj) *py-enumerate-type*)
           (eq (py-object-type obj) *py-zip-type*)
           (eq (py-object-type obj) *py-filter-type*)
           (eq (py-object-type obj) *py-map-type*)
           (eq (py-object-type obj) *py-range-iterator-type*)
           (eq (py-object-type obj) *py-buffered-reader-type*))))

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

(defun py-bytes-iterator-p (obj)
  (and (py-object-p obj)
       (eq (py-object-type obj) *py-bytes-iterator-type*)))

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

(defun py-map (function &rest iterables)
  (when (null iterables)
    (error "map() must have at least two arguments."))
  (make-py-map-object
   :type *py-map-type*
   :function function
   :iterators (apply #'make-py-tuple
                     (loop for iterable in iterables
                           collect (py-iter iterable)))))

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

(defun py-range-hash (range)
  (let ((length (py-range-object-length range)))
    (py-hash
     (make-py-tuple length
                    (if (= length 0)
                        *py-none*
                        (py-range-object-start range))
                    (if (<= length 1)
                        *py-none*
                        (py-range-object-step range))))))

(defun py-iter (obj)
  (cond
    ((py-iterator-p obj) obj)
    ((stringp obj)
     (make-py-string-iterator-object :type *py-string-iterator-type*
                                     :sequence obj
                                     :index 0))
    ((py-bytes-object-p obj)
     (make-py-bytes-iterator-object :type *py-bytes-iterator-type*
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
                (py-type-name (py-type-of obj))
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
                (py-type-name (py-type-of obj))
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
    ((py-bytes-iterator-p iterator)
     (let* ((sequence (py-bytes-iterator-object-sequence iterator))
            (index (py-bytes-iterator-object-index iterator))
            (size (or (py-object-size sequence) 0)))
       (if (and (>= index 0) (< index size))
           (prog1
               (aref (py-object-value sequence) index)
             (setf (py-bytes-iterator-object-index iterator) (1+ index)))
           (progn
             (setf (py-bytes-iterator-object-index iterator) -1)
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
    ((py-map-object-p iterator)
     (let* ((iterators (py-map-object-iterators iterator))
            (iterator-count (or (py-object-size iterators) 0))
            (items
              (loop for index from 0 below iterator-count
                    collect (py-next (aref (py-object-value iterators) index)))))
       (apply #'py-invoke-callable
              (py-map-object-function iterator)
              items)))
    ((py-range-iterator-p iterator)
     (let* ((range (py-range-iterator-object-range iterator))
            (index (py-range-iterator-object-index iterator))
            (length (py-range-object-length range)))
       (if (< index length)
           (prog1
               (py-range-item range index)
             (setf (py-range-iterator-object-index iterator) (1+ index)))
           (py-raise *py-stop-iteration*))))
    ((py-buffered-reader-object-p iterator)
     (let ((line (py-buffered-reader-readline iterator)))
       (if (> (or (py-object-size line) 0) 0)
           line
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

(defun py-bytes-iterator-length-hint (iterator)
  (let* ((sequence (py-bytes-iterator-object-sequence iterator))
         (index (py-bytes-iterator-object-index iterator))
         (length-remaining (if (>= index 0)
                               (- (or (py-object-size sequence) 0) index)
                               0)))
    (max length-remaining 0)))

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

(setf (py-type-attr *py-str-type* "__iter__")
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

(setf (py-type-attr *py-bytes-iterator-type* "__iter__")
      (lambda (iterator)
        (py-iter iterator)))

(setf (py-type-attr *py-bytes-iterator-type* "__next__")
      (lambda (iterator)
        (py-next iterator)))

(setf (py-type-attr *py-bytes-iterator-type* "__length_hint__")
      (lambda (iterator)
        (py-bytes-iterator-length-hint iterator)))

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

(setf (py-type-attr *py-map-type* "__iter__")
      (lambda (iterator)
        (py-iter iterator)))

(setf (py-type-attr *py-map-type* "__next__")
      (lambda (iterator)
        (py-next iterator)))

(setf (py-type-number-bool-fn *py-range-type*)
      (lambda (obj)
        (if (> (py-range-object-length obj) 0) 1 0)))

(setf (py-type-mapping-length-fn *py-range-type*) #'py-range-object-length)

(setf (py-type-attr *py-range-type* "__iter__")
      (lambda (obj)
        (py-iter obj)))

(setf (py-type-attr *py-range-type* "__len__")
      (lambda (obj)
        (py-range-object-length obj)))

(setf (py-type-attr *py-range-type* "__bool__")
      (lambda (obj)
        (py-bool (py-truthy-p obj))))

(setf (py-type-attr *py-range-type* "__contains__")
      (lambda (obj value)
        (py-range-contains obj value)))

(setf (py-type-attr *py-range-type* "__eq__")
      (lambda (obj value)
        (py-eq obj value)))

(setf (py-type-attr *py-range-type* "__ne__")
      (lambda (obj value)
        (py-ne obj value)))

(setf (py-type-attr *py-range-type* "__hash__")
      (lambda (obj)
        (py-hash obj)))

(setf (py-type-attr *py-range-type* "__reduce__")
      (lambda (obj)
        (make-py-tuple
         (py-type-of obj)
         (make-py-tuple
          (py-range-object-start obj)
          (py-range-object-stop obj)
          (py-range-object-step obj)))))

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

(defvar *py-repr-stack* nil)

(defun py-repr-enter (value)
  (if (member value *py-repr-stack* :test #'eq)
      t
      (progn
        (push value *py-repr-stack*)
        nil)))

(defun py-repr-leave (value)
  (setf *py-repr-stack* (remove value *py-repr-stack* :test #'eq :count 1)))

(defun py-list-repr (value stream)
  (if (py-repr-enter value)
      (princ "[...]" stream)
      (unwind-protect
           (progn
             (princ "[" stream)
             (loop for index from 0 below (or (py-object-size value) 0)
                   do (progn
                        (when (> index 0)
                          (princ ", " stream))
                        (py-repr (aref (py-object-value value) index) stream)))
             (princ "]" stream))
        (py-repr-leave value))))

(defun py-tuple-repr (value stream)
  (if (py-repr-enter value)
      (princ "(...)" stream)
      (unwind-protect
           (progn
             (princ "(" stream)
             (loop for index from 0 below (or (py-object-size value) 0)
                   do (progn
                        (when (> index 0)
                          (princ ", " stream))
                        (py-repr (aref (py-object-value value) index) stream)))
             (when (= (or (py-object-size value) 0) 1)
               (princ "," stream))
             (princ ")" stream))
        (py-repr-leave value))))

(defun py-dict-repr (value stream)
  (if (py-repr-enter value)
      (princ "{...}" stream)
      (unwind-protect
           (let ((storage (py-dict-storage value "__repr__"))
                 (keys (py-dict-object-keys value)))
             (princ "{" stream)
             (loop for index from 0 below (fill-pointer keys)
                   for key = (aref keys index)
                   do (progn
                        (when (> index 0)
                          (princ ", " stream))
                        (py-repr key stream)
                        (princ ": " stream)
                        (py-repr (gethash key storage) stream)))
             (princ "}" stream))
        (py-repr-leave value))))

(defun py-string-contains-p (string char)
  (not (null (position char string))))

(defun py-replace-exponent-marker (string)
  (substitute #\e #\d (substitute #\e #\D string)))

(defun py-float-string (value)
  (cond
    ((sb-ext:float-nan-p (float value 1.0d0)) "nan")
    ((sb-ext:float-infinity-p (float value 1.0d0))
     (if (minusp value) "-inf" "inf"))
    ((typep value 'double-float)
     (let* ((raw (string-downcase (write-to-string value)))
            (d-pos (position #\d raw)))
       (if d-pos
           (let ((mantissa (subseq raw 0 d-pos))
                 (exponent (parse-integer raw :start (1+ d-pos))))
             (if (= exponent 0)
                 mantissa
                 (concatenate 'string mantissa "e" (write-to-string exponent))))
           raw)))
    (t (write-to-string value))))

(defun py-repr (value &optional (stream *standard-output*))
  (cond
    ((py-type-p value)
     (format stream "<class '~A'>" (py-type-name value)))
    ((stringp value) (py-string-repr value stream))
    ((py-list-object-p value)
     (py-list-repr value stream))
    ((py-bytes-object-p value)
     (princ "b" stream)
     (py-string-repr
      (with-output-to-string (bytes-stream)
        (loop for index from 0 below (or (py-object-size value) 0)
              do (write-char (code-char (aref (py-object-value value) index))
                             bytes-stream)))
      stream))
    ((py-tuple-object-p value)
     (py-tuple-repr value stream))
    ((py-dict-object-p value)
     (py-dict-repr value stream))
    ((py-module-spec-object-p value)
     (py-module-spec-repr value stream))
    ((py-source-file-loader-object-p value)
     (princ "<_frozen_importlib_external.SourceFileLoader object>" stream))
    ((py-path-object-p value)
     (princ "PosixPath(" stream)
     (py-string-repr (py-path-string value) stream)
     (princ ")" stream))
    ((py-module-object-p value)
     (if (py-module-object-source-path value)
         (format stream "<module '~A' from '~A'>"
                 (py-module-object-name value)
                 (py-module-object-source-path value))
         (format stream "<module '~A'>" (py-module-object-name value))))
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
    ((eq value *py-not-implemented*) (princ "NotImplemented" stream))
    ((eq value *py-true*) (princ "True" stream))
    ((eq value *py-false*) (princ "False" stream))
    ((floatp value) (princ (py-float-string value) stream))
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
    ((py-map-object-p value) (princ "<map>" stream))
    ((py-range-object-p value) (py-repr value stream))
    ((py-range-iterator-p value) (princ "<range_iterator>" stream))
    ((py-type-p value) (py-repr value stream))
    ((py-list-object-p value) (py-repr value stream))
    ((py-bytes-object-p value) (py-repr value stream))
    ((py-tuple-object-p value) (py-repr value stream))
    ((py-dict-object-p value) (py-repr value stream))
    ((py-module-spec-object-p value) (py-repr value stream))
    ((py-source-file-loader-object-p value) (py-repr value stream))
    ((py-path-object-p value) (princ (py-path-string value) stream))
    ((py-module-object-p value) (py-repr value stream))
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
