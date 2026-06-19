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
   :py-invoke-callable
   :py-bind-args
   :py-module-object
   :py-module-object-name
   :py-module-object-source-path
   :py-module-object-package-name
   :*py-current-module*
   :*py-module-search-paths*
   :*py-module-loader*
   :*py-sys-argv*
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
   :*py-base-exception-type*
   :*py-exception-type*
   :*py-runtime-error-type*
   :*py-type-error-type*
   :*py-value-error-type*
   :*py-lookup-error-type*
   :*py-import-error-type*
   :*py-module-not-found-error-type*
   :*py-attribute-error-type*
   :*py-name-error-type*
   :*py-os-error-type*
   :*py-file-not-found-error-type*
   :*py-timeout-error-type*
   :py-bool
   :py-truthy-p
   :py-and
   :py-or
   :py-len
   :py-length-hint
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
   :py-lisp-error-to-exception
   :py-raise
   :*py-stop-iteration*
   :*py-stop-iteration-type*
   :*py-stop-async-iteration-type*
   :py-stop-iteration-p
   :make-py-coroutine
   :make-py-async-generator
   :py-async-generator-yield
   :py-await
   :py-coroutine-run
   :py-aiter
   :py-anext-item
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
   :py-unpack-sequence
   :make-py-list
   :make-py-dict-from-pairs
   :make-py-tuple
   :make-py-bytes-from-vector
   :py-append
   :py-insert
   :py-pop
   :py-getitem
   :py-setitem
   :py-delitem))

(in-package "CLAMP.__CLAMP_INTERNALS__")


(require :sb-bsd-sockets)

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

(defparameter *py-stop-async-iteration-type*
  (make-py-type :type *py-type-type*
                :name "StopAsyncIteration"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-runtime-error-type*
  (make-py-type :type *py-type-type*
                :name "RuntimeError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-type-error-type*
  (make-py-type :type *py-type-type*
                :name "TypeError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-value-error-type*
  (make-py-type :type *py-type-type*
                :name "ValueError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-lookup-error-type*
  (make-py-type :type *py-type-type*
                :name "LookupError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-import-error-type*
  (make-py-type :type *py-type-type*
                :name "ImportError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-module-not-found-error-type*
  (make-py-type :type *py-type-type*
                :name "ModuleNotFoundError"
                :bases (list *py-import-error-type*)
                :basicsize 1))

(defparameter *py-attribute-error-type*
  (make-py-type :type *py-type-type*
                :name "AttributeError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-name-error-type*
  (make-py-type :type *py-type-type*
                :name "NameError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-os-error-type*
  (make-py-type :type *py-type-type*
                :name "OSError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-file-not-found-error-type*
  (make-py-type :type *py-type-type*
                :name "FileNotFoundError"
                :bases (list *py-os-error-type*)
                :basicsize 1))

(defparameter *py-timeout-error-type*
  (make-py-type :type *py-type-type*
                :name "TimeoutError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-asyncio-cancelled-error-type*
  (make-py-type :type *py-type-type*
                :name "CancelledError"
                :bases (list *py-base-exception-type*)
                :basicsize 1))

(defparameter *py-asyncio-invalid-state-error-type*
  (make-py-type :type *py-type-type*
                :name "InvalidStateError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-asyncio-incomplete-read-error-type*
  (make-py-type :type *py-type-type*
                :name "IncompleteReadError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-asyncio-limit-overrun-error-type*
  (make-py-type :type *py-type-type*
                :name "LimitOverrunError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-asyncio-broken-barrier-error-type*
  (make-py-type :type *py-type-type*
                :name "BrokenBarrierError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-asyncio-queue-full-type*
  (make-py-type :type *py-type-type*
                :name "QueueFull"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-asyncio-queue-empty-type*
  (make-py-type :type *py-type-type*
                :name "QueueEmpty"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-coroutine-type*
  (make-py-type :type *py-type-type*
                :name "coroutine"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-async-generator-type*
  (make-py-type :type *py-type-type*
                :name "async_generator"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-future-type*
  (make-py-type :type *py-type-type*
                :name "Future"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-task-type*
  (make-py-type :type *py-type-type*
                :name "Task"
                :bases (list *py-asyncio-future-type*)
                :basicsize 1))

(defparameter *py-asyncio-task-group-type*
  (make-py-type :type *py-type-type*
                :name "TaskGroup"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-runner-type*
  (make-py-type :type *py-type-type*
                :name "Runner"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-process-type*
  (make-py-type :type *py-type-type*
                :name "Process"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-timeout-type*
  (make-py-type :type *py-type-type*
                :name "Timeout"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-event-loop-policy-type*
  (make-py-type :type *py-type-type*
                :name "_ClampEventLoopPolicy"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-event-loop-type*
  (make-py-type :type *py-type-type*
                :name "_ClampEventLoop"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-handle-type*
  (make-py-type :type *py-type-type*
                :name "Handle"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-timer-handle-type*
  (make-py-type :type *py-type-type*
                :name "TimerHandle"
                :bases (list *py-asyncio-handle-type*)
                :basicsize 1))

(defparameter *py-asyncio-sleep-type*
  (make-py-type :type *py-type-type*
                :name "sleep"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-lock-type*
  (make-py-type :type *py-type-type*
                :name "Lock"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-condition-type*
  (make-py-type :type *py-type-type*
                :name "Condition"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-semaphore-type*
  (make-py-type :type *py-type-type*
                :name "Semaphore"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-bounded-semaphore-type*
  (make-py-type :type *py-type-type*
                :name "BoundedSemaphore"
                :bases (list *py-asyncio-semaphore-type*)
                :basicsize 1))

(defparameter *py-asyncio-barrier-type*
  (make-py-type :type *py-type-type*
                :name "Barrier"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-event-type*
  (make-py-type :type *py-type-type*
                :name "Event"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-queue-type*
  (make-py-type :type *py-type-type*
                :name "Queue"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-priority-queue-type*
  (make-py-type :type *py-type-type*
                :name "PriorityQueue"
                :bases (list *py-asyncio-queue-type*)
                :basicsize 1))

(defparameter *py-asyncio-lifo-queue-type*
  (make-py-type :type *py-type-type*
                :name "LifoQueue"
                :bases (list *py-asyncio-queue-type*)
                :basicsize 1))

(defparameter *py-asyncio-as-completed-type*
  (make-py-type :type *py-type-type*
                :name "_AsCompleted"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-stream-reader-type*
  (make-py-type :type *py-type-type*
                :name "StreamReader"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-stream-writer-type*
  (make-py-type :type *py-type-type*
                :name "StreamWriter"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-asyncio-server-type*
  (make-py-type :type *py-type-type*
                :name "Server"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-contextvars-context-var-type*
  (make-py-type :type *py-type-type*
                :name "ContextVar"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-contextvars-token-type*
  (make-py-type :type *py-type-type*
                :name "Token"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-contextvars-context-type*
  (make-py-type :type *py-type-type*
                :name "Context"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-contextlib-async-generator-context-manager-type*
  (make-py-type :type *py-type-type*
                :name "_AsyncGeneratorContextManager"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-contextlib-aclosing-type*
  (make-py-type :type *py-type-type*
                :name "aclosing"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-contextlib-nullcontext-type*
  (make-py-type :type *py-type-type*
                :name "nullcontext"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-contextlib-async-exit-stack-type*
  (make-py-type :type *py-type-type*
                :name "AsyncExitStack"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-aiohttp-client-session-type*
  (make-py-type :type *py-type-type*
                :name "ClientSession"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-aiohttp-client-timeout-type*
  (make-py-type :type *py-type-type*
                :name "ClientTimeout"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-aiohttp-tcp-connector-type*
  (make-py-type :type *py-type-type*
                :name "TCPConnector"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-aiohttp-basic-auth-type*
  (make-py-type :type *py-type-type*
                :name "BasicAuth"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-aiohttp-form-data-type*
  (make-py-type :type *py-type-type*
                :name "FormData"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-aiohttp-cookie-jar-type*
  (make-py-type :type *py-type-type*
                :name "CookieJar"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-aiohttp-request-context-type*
  (make-py-type :type *py-type-type*
                :name "_RequestContextManager"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-aiohttp-client-response-type*
  (make-py-type :type *py-type-type*
                :name "ClientResponse"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-aiohttp-stream-reader-type*
  (make-py-type :type *py-type-type*
                :name "StreamReader"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-aiohttp-chunk-iterator-type*
  (make-py-type :type *py-type-type*
                :name "AsyncStreamIterator"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-aiohttp-client-websocket-response-type*
  (make-py-type :type *py-type-type*
                :name "ClientWebSocketResponse"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-aiohttp-ws-message-type*
  (make-py-type :type *py-type-type*
                :name "WSMessage"
                :bases (list *py-object-type*)
                :basicsize 1))

(defparameter *py-aiohttp-client-error-type*
  (make-py-type :type *py-type-type*
                :name "ClientError"
                :bases (list *py-exception-type*)
                :basicsize 1))

(defparameter *py-aiohttp-client-response-error-type*
  (make-py-type :type *py-type-type*
                :name "ClientResponseError"
                :bases (list *py-aiohttp-client-error-type*)
                :basicsize 1))

(defparameter *py-aiohttp-content-type-error-type*
  (make-py-type :type *py-type-type*
                :name "ContentTypeError"
                :bases (list *py-aiohttp-client-response-error-type*)
                :basicsize 1))

(defparameter *py-aiohttp-client-connection-error-type*
  (make-py-type :type *py-type-type*
                :name "ClientConnectionError"
                :bases (list *py-aiohttp-client-error-type*)
                :basicsize 1))

(defparameter *py-aiohttp-client-connector-error-type*
  (make-py-type :type *py-type-type*
                :name "ClientConnectorError"
                :bases (list *py-aiohttp-client-connection-error-type*)
                :basicsize 1))

(defparameter *py-aiohttp-client-payload-error-type*
  (make-py-type :type *py-type-type*
                :name "ClientPayloadError"
                :bases (list *py-aiohttp-client-error-type*)
                :basicsize 1))

(defparameter *py-aiohttp-invalid-url-type*
  (make-py-type :type *py-type-type*
                :name "InvalidURL"
                :bases (list *py-aiohttp-client-error-type*)
                :basicsize 1))

(defparameter *py-aiohttp-too-many-redirects-type*
  (make-py-type :type *py-type-type*
                :name "TooManyRedirects"
                :bases (list *py-aiohttp-client-response-error-type*)
                :basicsize 1))

(defparameter *py-aiohttp-server-timeout-error-type*
  (make-py-type :type *py-type-type*
                :name "ServerTimeoutError"
                :bases (list *py-aiohttp-client-connection-error-type* *py-timeout-error-type*)
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

(defparameter *py-dict-key-iterator-type*
  (make-py-type :type *py-type-type*
                :name "dict_keyiterator"
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


(defstruct (py-coroutine-object (:include py-object))
  name
  thunk
  (state :created)
  result
  exception)

(defstruct (py-async-generator-object (:include py-object))
  name
  thunk
  (realized nil)
  (items '())
  (index 0)
  (closed nil))

(defvar *py-async-generator-yields* nil)

(defstruct (py-asyncio-future-object (:include py-object))
  loop
  (state :pending)
  result
  exception
  (callbacks '()))

(defstruct (py-asyncio-task-object (:include py-asyncio-future-object))
  coroutine
  name)

(defstruct (py-asyncio-task-group-object (:include py-object))
  loop
  (entered nil)
  (exiting nil)
  (tasks '()))

(defstruct (py-asyncio-runner-object (:include py-object))
  loop
  debug
  loop-factory
  (entered nil)
  (closed nil))

(defstruct (py-asyncio-process-object (:include py-object))
  args
  returncode
  stdout
  stderr)

(defstruct (py-asyncio-timeout-object (:include py-object))
  loop
  deadline
  (expired nil))

(defstruct (py-asyncio-event-loop-policy-object (:include py-object))
  loop)

(defvar *py-asyncio-event-loop-policy* nil)
(defvar *py-asyncio-default-event-loop* nil)
(defparameter *py-asyncio-subprocess-pipe* -1)
(defparameter *py-asyncio-subprocess-stdout* -2)
(defparameter *py-asyncio-subprocess-devnull* -3)

(defstruct (py-asyncio-event-loop-object (:include py-object))
  (closed nil)
  (running nil)
  (stopping nil)
  (debug nil)
  (ready-callbacks '())
  (tasks '())
  current-task)

(defstruct (py-asyncio-handle-object (:include py-object))
  callback
  args
  (cancelled nil))

(defstruct (py-asyncio-timer-handle-object (:include py-asyncio-handle-object))
  when)

(defstruct (py-asyncio-sleep-object (:include py-object))
  delay
  result)

(defstruct (py-asyncio-lock-object (:include py-object))
  loop
  (locked nil))

(defstruct (py-asyncio-condition-object (:include py-object))
  loop
  lock)

(defstruct (py-asyncio-semaphore-object (:include py-object))
  loop
  (counter 1))

(defstruct (py-asyncio-bounded-semaphore-object (:include py-asyncio-semaphore-object))
  (bound 1))

(defstruct (py-asyncio-barrier-object (:include py-object))
  loop
  parties
  (waiting 0)
  (broken nil))

(defstruct (py-asyncio-event-object (:include py-object))
  loop
  (flag nil))

(defstruct (py-asyncio-queue-object (:include py-object))
  loop
  (maxsize 0)
  (items '())
  (unfinished-tasks 0))

(defstruct (py-asyncio-as-completed-object (:include py-object))
  items
  (index 0))

(defstruct (py-asyncio-stream-reader-object (:include py-object))
  socket
  stream
  (eof nil))

(defstruct (py-asyncio-stream-writer-object (:include py-object))
  socket
  stream
  peername
  (closing nil))

(defstruct (py-asyncio-server-object (:include py-object))
  socket
  thread
  sockets
  callback
  (closed nil)
  (serving t))

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

(defun py-length-hint (value &optional (default 0))
  (let ((normalized-default (py-normalize-bool-number default)))
    (unless (integerp normalized-default)
      (error "'~A' object cannot be interpreted as an integer"
             (py-type-name (py-type-of default))))
    (let ((length
            (cond
              ((py-object-p value)
               (py-type-slot-length (py-object-type value) value))
              ((stringp value)
               (py-type-slot-length *py-str-type* value))
              (t nil))))
      (when length
        (return-from py-length-hint length)))
    (let ((type (handler-case
                    (py-type-of value)
                  (error ()
                    nil))))
      (unless type
        (return-from py-length-hint normalized-default))
      (multiple-value-bind (hint found) (py-find-type-attr type "__length_hint__")
        (unless found
          (return-from py-length-hint normalized-default))
        (let ((result (handler-case
                          (py-invoke-callable hint value)
                        (type-error ()
                          normalized-default))))
          (when (eq result *py-not-implemented*)
            (return-from py-length-hint normalized-default))
          (let ((normalized-result (py-normalize-bool-number result)))
            (unless (integerp normalized-result)
              (error "__length_hint__ must be integer, not ~A"
                     (py-type-name (py-type-of result))))
            (when (< normalized-result 0)
              (error "__length_hint__() should return >= 0"))
            normalized-result))))))

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

(defun py-dict-eq (left right)
  (let ((left-size (or (py-object-size left) 0))
        (right-size (or (py-object-size right) 0)))
    (and (= left-size right-size)
         (let ((left-storage (py-dict-storage left "=="))
               (right-storage (py-dict-storage right "=="))
               (left-keys (py-dict-object-keys left)))
           (loop for index from 0 below (fill-pointer left-keys)
                 for key = (aref left-keys index)
                 always (multiple-value-bind (right-value found)
                            (gethash key right-storage)
                          (and found
                               (py-truthy-p
                                (py-eq (gethash key left-storage)
                                       right-value)))))))))

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
       ((and (py-dict-object-p left) (py-dict-object-p right))
        (py-dict-eq left right))
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
  (let ((exception (make-py-exception-object :type type :value args :args args)))
    (setf (gethash "args" (py-object-attrs exception))
          (if (fboundp 'make-py-tuple)
              (apply #'make-py-tuple args)
              args))
    exception))

(defun py-make-import-error (type message &key name path)
  (let ((exception (make-py-exception type message)))
    (setf (py-object-attr exception "name") (or name *py-none*))
    (setf (py-object-attr exception "path") (or path *py-none*))
    exception))

(defun py-raise-type (type message)
  (py-raise (make-py-exception type message)))

(defun py-raise-import-error (message &key name path (type *py-import-error-type*))
  (py-raise (py-make-import-error type message :name name :path path)))

(defun py-lisp-error-to-exception (condition)
  (cond
    ((typep condition 'unbound-variable)
     (make-py-exception
      *py-name-error-type*
      (format nil "name '~A' is not defined"
              (string-downcase (symbol-name (cell-error-name condition))))))
    ((typep condition 'file-error)
     (make-py-exception *py-file-not-found-error-type* (namestring (file-error-pathname condition))))
    (t
     (make-py-exception *py-runtime-error-type* (princ-to-string condition)))))

(defparameter *py-stop-iteration*
  (make-py-exception *py-stop-iteration-type*))

(define-condition py-exception (error)
  ((value :initarg :value :reader py-exception-value))
  (:report (lambda (condition stream)
             (let ((value (py-exception-value condition)))
               (if (py-exception-object-p value)
                   (let ((message (py-exception-message value)))
                     (princ (py-type-name (py-object-type value)) stream)
                     (unless (string= message "")
                       (princ ": " stream)
                       (princ message stream)))
                   (princ value stream))))))

(defun py-exception-message (exception)
  (let ((args (py-exception-object-args exception)))
    (if args
        (with-output-to-string (stream)
          (loop for arg in args
                for first = t then nil
                do (progn
                     (unless first
                       (princ " " stream))
                     (py-display arg stream))))
        "")))

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
  owner-type
  (coroutine-function nil)
  (async-generator-function nil))

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
      ((string= name "has_location")
       (let ((truth-value (py-bool (py-truthy-p value))))
         (setf (py-module-spec-object-set-fileattr obj)
               (py-truthy-p value))
         (setf (py-module-spec-object-has-location obj)
               (py-truthy-p value))
         (setf (gethash "has_location" (py-object-attrs obj)) truth-value)
         (setf (gethash "_set_fileattr" (py-object-attrs obj)) truth-value)))
      ((string= name "_set_fileattr")
       (setf (py-module-spec-object-set-fileattr obj) value)
       (setf (py-module-spec-object-has-location obj) value)
       (setf (gethash "has_location" (py-object-attrs obj)) value)
       (setf (gethash "_set_fileattr" (py-object-attrs obj)) value)))
    (let ((dict (py-module-spec-object-namespace-dict obj)))
      (when (and dict
                 (not (string= name "__dict__"))
                 (not (py-dict-has-key-p dict name)))
        (vector-push-extend name (py-dict-object-keys dict))
        (setf (py-object-size dict) (hash-table-count (py-object-attrs obj))))))
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
(defvar *py-sys-path* nil)
(defvar *py-module-loader* nil)
(defvar *py-sys-argv* nil)
(defvar *py-sys-modules* (make-hash-table :test #'equal))
(defvar *py-builtin-module-builders* (make-hash-table :test #'equal))

(defun py-ensure-sys-path ()
  (or *py-sys-path*
      (setf *py-sys-path*
            (apply #'make-py-list
                   (or *py-module-search-paths*
                       (list (namestring (uiop:getcwd))))))))

(defun py-current-module-search-paths ()
  (py-list-values (py-ensure-sys-path)))

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
  (uninitialized-submodules '())
  namespace-dict)

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

(defun py-path-suffix (path)
  (let* ((name (py-path-name path))
         (start (loop for index from 0 below (length name)
                      while (char= (char name index) #\.)
                      finally (return index)))
         (trimmed (subseq name start))
         (dot (position #\. trimmed :from-end t)))
    (if dot
        (subseq trimmed dot)
        "")))

(defun py-path-suffixes (path)
  (let* ((name (py-path-name path))
         (start (loop for index from 0 below (length name)
                      while (char= (char name index) #\.)
                      finally (return index)))
         (trimmed (subseq name start))
         (first-dot (position #\. trimmed))
         (suffixes '()))
    (when first-dot
      (let ((segment-start (1+ first-dot)))
        (loop for dot = (position #\. trimmed :start segment-start)
              while dot
              do (progn
                   (push (concatenate 'string
                                      "."
                                      (subseq trimmed segment-start dot))
                         suffixes)
                   (setf segment-start (1+ dot))))
        (push (concatenate 'string "." (subseq trimmed segment-start))
              suffixes)))
    (apply #'make-py-list (nreverse suffixes))))

(defun py-path-stem (path)
  (let* ((name (py-path-name path))
         (dot (position #\. name :from-end t)))
    (if dot
        (let ((stem (subseq name 0 dot)))
          (if (find-if (lambda (char) (not (char= char #\.))) stem)
              stem
              name))
        name)))

(defun py-path-parent-string (path)
  (let* ((path-string (py-path-string path))
         (end (length path-string)))
    (loop while (and (> end 1)
                     (char= (char path-string (1- end)) #\/))
          do (decf end))
    (let* ((normalized (subseq path-string 0 end))
           (slash (position #\/ normalized :from-end t)))
      (cond
        ((string= normalized "/") "/")
        ((null slash) ".")
        ((= slash 0) "/")
        (t (subseq normalized 0 slash))))))

(defun py-path-parent (path)
  (let ((parent-string (py-path-parent-string path)))
    (if (and (py-path-object-p path)
             (string= parent-string (py-path-object-path path)))
        path
        (make-py-path parent-string))))

(defun py-path-with-name (path name)
  (unless (stringp name)
    (error "argument must be a str object, not ~A" (py-type-name (py-type-of name))))
  (when (or (= (length name) 0)
            (string= name ".")
            (position #\/ name))
    (error "Invalid name ~S" name))
  (let ((old-name (py-path-name path)))
    (when (= (length old-name) 0)
      (error "~A has an empty name" (py-path-string path))))
  (let ((parent (py-path-parent-string path)))
    (make-py-path
     (cond
       ((string= parent ".") name)
       ((string= parent "/") (concatenate 'string "/" name))
       (t (concatenate 'string parent "/" name))))))

(defun py-path-with-suffix (path suffix)
  (unless (stringp suffix)
    (error "argument must be a str object, not ~A" (py-type-name (py-type-of suffix))))
  (let ((stem (py-path-stem path)))
    (when (= (length stem) 0)
      (error "~A has an empty name" (py-path-string path)))
    (when (and (> (length suffix) 0)
               (not (char= (char suffix 0) #\.)))
      (error "Invalid suffix ~S" suffix))
    (py-path-with-name path (concatenate 'string stem suffix))))

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
  (py-module-spec-object-cached spec))

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
        (let* ((directory (pathname-directory
                           (uiop:ensure-directory-pathname path)))
               (last-part (first (last directory))))
          (if (stringp last-part) last-part "")))))

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
        (py-raise (make-py-exception *py-os-error-type* "OSError"))))

(setf (py-type-attr *py-source-file-loader-type* "set_data")
      (lambda (loader path data)
        (declare (ignore loader))
        (py-write-file-bytes path data)))

(setf (py-type-attr *py-source-file-loader-type* "_cache_bytecode")
      (lambda (loader source-path bytecode-path data)
        (declare (ignore loader source-path bytecode-path data))
        *py-none*))

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

(setf (py-type-attr *py-path-type* "with_name")
      (lambda (path name)
        (py-path-with-name path name)))

(setf (py-type-attr *py-path-type* "with_suffix")
      (lambda (path suffix)
        (py-path-with-suffix path suffix)))

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

(setf (py-type-attr *py-path-type* "as_posix")
      (lambda (path)
        (py-path-string path)))

(setf (py-type-attr *py-path-type* "__str__")
      (lambda (path)
        (py-path-string path)))

(setf (py-type-attr *py-path-type* "__fspath__")
      (lambda (path)
        (py-path-string path)))

(setf (py-type-attr *py-path-type* "__repr__")
      (lambda (path)
        (with-output-to-string (stream)
          (py-repr path stream))))

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
  (let* ((cached *py-none*)
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
  (declare (ignore source-path))
  *py-none*)

(defun py-set-module-source-path (module source-path)
  (setf (py-module-object-source-path module) source-path)
  (setf (py-object-attr module "__file__") source-path)
  (setf (py-object-attr module "__cached__") *py-none*))

(defun py-find-module-source-in-roots (relative-name roots)
  (let* ((components (py-module-path-components relative-name))
         (relative-file (format nil "~{~A~^/~}.py" components))
         (relative-init (format nil "~{~A~^/~}/__init__.py" components)))
    (loop for root in roots
          for file-path = (merge-pathnames relative-file (uiop:ensure-directory-pathname root))
          for init-path = (merge-pathnames relative-init (uiop:ensure-directory-pathname root))
          for file = (py-probe-file file-path)
          for init = (py-probe-file init-path)
          when file do (return (values file nil))
          when init do (return (values init t))
          finally (return (values nil nil)))))

(defun py-list-values (value)
  (cond
    ((py-list-object-p value)
     (let ((storage (py-object-value value))
           (size (or (py-object-size value) 0)))
       (loop for index from 0 below size collect (aref storage index))))
    (t '())))

(defun py-find-module-source (name &optional parent-module)
  (if parent-module
      (py-find-module-source-in-roots
       (py-module-child-name name)
       (py-list-values (py-object-attr parent-module "__path__")))
      (py-find-module-source-in-roots
       name
       (py-current-module-search-paths))))

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
      (let ((parent-name (py-module-parent-name name)))
        (when parent-name
          (py-load-module parent-name)))
      (let ((module (funcall builder)))
        (setf (gethash name *py-sys-modules*) module)
        (let ((parent-name (py-module-parent-name name)))
          (when parent-name
            (let ((parent (gethash parent-name *py-sys-modules*)))
              (when parent
                (setf (py-object-attr parent (py-module-child-name name)) module)))))
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
          (py-raise-import-error
           (format nil "No module named '~A'; '~A' is not a package" name parent-name)
           :name name
           :type *py-module-not-found-error-type*)))))
  (multiple-value-bind (source-path package-p)
      (py-find-module-source name (and (py-module-parent-name name)
                                       (gethash (py-module-parent-name name)
                                                *py-sys-modules*)))
    (unless source-path
      (py-raise-import-error
       (format nil "No module named '~A'" name)
       :name name
       :type *py-module-not-found-error-type*))
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

(defun py-current-package-name ()
  (unless *py-current-module*
    (py-raise-import-error "attempted relative import with no known parent package"))
  (let ((package (py-object-attr *py-current-module* "__package__")))
    (unless (and (stringp package) (> (length package) 0))
      (py-raise-import-error "attempted relative import with no known parent package"))
    package))

(defun py-resolve-relative-import-name (name level &optional package)
  (let ((base (or package (py-current-package-name))))
    (unless (and (stringp base) (> (length base) 0))
      (py-raise-import-error "attempted relative import with no known parent package"))
    (loop repeat (1- level)
          do (let ((pos (position #\. base :from-end t)))
               (unless pos
                 (py-raise-import-error "attempted relative import beyond top-level package"))
               (setf base (subseq base 0 pos))))
    (if (> (length name) 0)
        (concatenate 'string base "." name)
        base)))

(defun py-import-name (name &optional (fromlist *py-none*) (level 0))
  (let* ((normalized-level (py-normalize-bool-number level))
         (full-name (if (> normalized-level 0)
                        (py-resolve-relative-import-name name normalized-level)
                        name))
         (module (py-import-module full-name)))
    (if (py-truthy-p fromlist)
        module
        (py-import-module (py-module-root-name full-name)))))

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
             (multiple-value-bind (builder builtin-found) (gethash full-name *py-builtin-module-builders*)
               (declare (ignore builder))
               (if builtin-found
                   (py-import-module full-name)
                   (multiple-value-bind (source-path package-p)
                       (py-find-module-source full-name module)
                     (declare (ignore package-p))
                     (when source-path
                       (py-import-module full-name)))))))))))
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

(defun py-import-star (name &optional (level 0))
  (let ((module (py-import-name name (list "*") level)))
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
    (py-raise-type *py-type-error-type* "module name must be a string"))
  (let* ((normalized-level (py-normalize-bool-number level)))
    (unless (integerp normalized-level)
      (py-raise-type *py-type-error-type* "level must be an integer"))
    (when (< normalized-level 0)
      (py-raise-type *py-value-error-type* "level must be >= 0"))
    (when (and (= normalized-level 0) (= (length name) 0))
      (py-raise-type *py-value-error-type* "Empty module name"))
    (let* ((full-name (if (> normalized-level 0)
                          (py-resolve-relative-import-name name normalized-level)
                          name))
           (module (py-import-module full-name)))
      (if (py-truthy-p fromlist)
          (if (nth-value 1 (gethash "__path__" (py-object-attrs module)))
              (py-import-handle-fromlist module (py-import-fromlist-names fromlist))
              module)
          (py-import-module (py-module-root-name full-name))))))

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
      (py-exception ()
        (py-raise-import-error
         (format nil "cannot import name '~A' from '~A'" name (py-module-object-name module))
         :name name)))))

(defun py-register-builtin-module (name builder)
  (setf (gethash name *py-builtin-module-builders*) builder))


(defun make-clamp-sys-module ()
  (let ((module (make-clamp-module "sys")))
    (setf (py-object-attr module "modules")
          (make-py-dict-for-storage *py-sys-modules*))
    (setf (py-object-attr module "path") (py-ensure-sys-path))
    (setf (py-object-attr module "argv")
          (apply #'make-py-list (or *py-sys-argv* '())))
    module))

(defun py-importlib-resolve-name (name package)
  (if (and (> (length name) 0) (char= (char name 0) #\.))
      (let ((level 0)
            (start 0))
        (loop while (and (< start (length name))
                         (char= (char name start) #\.))
              do (incf level)
                 (incf start))
        (py-resolve-relative-import-name (subseq name start) level package))
      name))

(defun py-importlib-import-module (name &optional (package *py-none*))
  (unless (stringp name)
    (py-raise-type *py-type-error-type* "module name must be a string"))
  (let ((full-name (py-importlib-resolve-name
                    name
                    (unless (eq package *py-none*) package))))
    (py-import-module full-name)))

(defun py-importlib-reload (module)
  (unless (py-module-object-p module)
    (py-raise-type *py-type-error-type* "reload() argument must be a module"))
  (let ((name (py-module-object-name module)))
    (remhash name *py-sys-modules*)
    (py-import-module name)))

(defun py-importlib-invalidate-caches ()
  *py-none*)

(defun py-importlib-find-spec (name &optional (package *py-none*))
  (let* ((full-name (if (and (stringp name)
                             (> (length name) 0)
                             (char= (char name 0) #\.))
                        (py-importlib-resolve-name
                         name
                         (unless (eq package *py-none*) package))
                        name)))
    (multiple-value-bind (cached found) (gethash full-name *py-sys-modules*)
      (when found
        (return-from py-importlib-find-spec (py-object-attr cached "__spec__"))))
    (let* ((parent-name (py-module-parent-name full-name))
           (parent (and parent-name (py-import-module parent-name))))
      (when (and parent-name
                 (not (nth-value 1 (gethash "__path__" (py-object-attrs parent)))))
        (return-from py-importlib-find-spec *py-none*))
      (multiple-value-bind (source-path package-p)
          (py-find-module-source full-name parent)
        (if source-path
            (make-clamp-module-spec
             full-name
             source-path
             package-p
             (make-clamp-source-file-loader full-name source-path))
            *py-none*)))))

(defun py-importlib-module-from-spec (spec)
  (unless (py-module-spec-object-p spec)
    (py-raise-type *py-type-error-type* "spec must be a ModuleSpec object"))
  (let* ((name (py-module-spec-object-name spec))
         (source-path (py-module-spec-object-origin spec))
         (package-p (not (eq (py-module-spec-object-submodule-search-locations spec)
                             *py-none*)))
         (module (make-clamp-module name
                                    :source-path source-path
                                    :package-p package-p)))
    (setf (py-object-attr module "__loader__")
          (py-module-spec-object-loader spec))
    (setf (py-object-attr module "__spec__") spec)
    (when package-p
      (setf (py-object-attr module "__path__")
            (py-module-spec-object-submodule-search-locations spec)))
    module))

(defun py-importlib-spec-from-file-location (name location)
  (let* ((path (py-path-string location))
         (package-p (string= (pathname-name path) "__init__"))
         (loader (make-clamp-source-file-loader name path)))
    (make-clamp-module-spec name path package-p loader)))

(defun make-clamp-importlib-module ()
  (let ((module (make-clamp-module "importlib")))
    (setf (py-object-attr module "import_module") #'py-importlib-import-module)
    (setf (py-object-attr module "reload") #'py-importlib-reload)
    (setf (py-object-attr module "invalidate_caches") #'py-importlib-invalidate-caches)
    module))

(defun make-clamp-importlib-util-module ()
  (let ((module (make-clamp-module "importlib.util")))
    (setf (py-object-attr module "find_spec") #'py-importlib-find-spec)
    (setf (py-object-attr module "module_from_spec") #'py-importlib-module-from-spec)
    (setf (py-object-attr module "spec_from_file_location") #'py-importlib-spec-from-file-location)
    module))

(defun make-clamp-importlib-machinery-module ()
  (let ((module (make-clamp-module "importlib.machinery")))
    (setf (py-object-attr module "ModuleSpec") *py-module-spec-type*)
    (setf (py-object-attr module "SourceFileLoader") *py-source-file-loader-type*)
    module))

(defun make-clamp-importlib-resources-module ()
  (let ((module (make-clamp-module "importlib.resources")))
    module))

(defun make-clamp-importlib-resources-readers-module ()
  (let ((module (make-clamp-module "importlib.resources.readers")))
    (setf (py-object-attr module "FileReader") *py-file-reader-type*)
    module))

(py-register-builtin-module "sys" #'make-clamp-sys-module)
(py-register-builtin-module "importlib" #'make-clamp-importlib-module)
(py-register-builtin-module "importlib.util" #'make-clamp-importlib-util-module)
(py-register-builtin-module "importlib.machinery" #'make-clamp-importlib-machinery-module)
(py-register-builtin-module "importlib.resources" #'make-clamp-importlib-resources-module)
(py-register-builtin-module "importlib.resources.readers" #'make-clamp-importlib-resources-readers-module)

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

(defun make-clamp-operator-module ()
  (let ((module (make-clamp-module "operator")))
    (setf (py-object-attr module "__doc__") "Clamp built-in operator module")
    (setf (py-object-attr module "length_hint") #'py-length-hint)
    module))

(py-register-builtin-module "operator" #'make-clamp-operator-module)

(py-register-builtin-module "math" #'make-clamp-math-module)

(defun make-clamp-inspect-module ()
  (let ((module (make-clamp-module "inspect")))
    (setf (py-object-attr module "__doc__") "Clamp built-in inspect compatibility module")
    (setf (py-object-attr module "iscoroutine") #'py-inspect-iscoroutine)
    (setf (py-object-attr module "iscoroutinefunction") #'py-inspect-iscoroutinefunction)
    (setf (py-object-attr module "isawaitable") #'py-inspect-isawaitable)
    (setf (py-object-attr module "isasyncgen") #'py-inspect-isasyncgen)
    (setf (py-object-attr module "isasyncgenfunction") #'py-inspect-isasyncgenfunction)
    module))

(py-register-builtin-module "inspect" #'make-clamp-inspect-module)

(defvar *py-asyncio-running-loop* nil)

(defun make-py-coroutine (name thunk)
  (make-py-coroutine-object :type *py-coroutine-type*
                            :name name
                            :thunk thunk
                            :state :created))

(defun make-py-async-generator (name thunk)
  (make-py-async-generator-object :type *py-async-generator-type*
                                  :name name
                                  :thunk thunk))

(defun py-async-generator-yield (value)
  (unless (consp *py-async-generator-yields*)
    (error "yield outside async generator"))
  (push value (car *py-async-generator-yields*))
  *py-none*)

(defun py-async-generator-realize (generator)
  (unless (py-async-generator-object-realized generator)
    (let ((*py-async-generator-yields* (list '())))
      (funcall (py-async-generator-object-thunk generator))
      (setf (py-async-generator-object-items generator)
            (nreverse (car *py-async-generator-yields*)))
      (setf (py-async-generator-object-realized generator) t)))
  generator)

(defun py-async-generator-anext (generator)
  (make-py-coroutine "async_generator.__anext__"
                     (lambda ()
                       (when (py-async-generator-object-closed generator)
                         (py-raise (make-py-exception *py-stop-async-iteration-type*)))
                       (py-async-generator-realize generator)
                       (let ((index (py-async-generator-object-index generator))
                             (items (py-async-generator-object-items generator)))
                         (if (< index (length items))
                             (prog1 (nth index items)
                               (setf (py-async-generator-object-index generator) (1+ index)))
                             (py-raise (make-py-exception *py-stop-async-iteration-type*)))))))

(defun py-async-generator-asend (generator value)
  (if (eq value *py-none*)
      (py-async-generator-anext generator)
      (make-py-coroutine "async_generator.asend"
                         (lambda ()
                           (py-raise
                            (make-py-exception *py-type-error-type*
                                               "can't send non-None value to a just-started async generator"))))))

(defun py-async-generator-aclose (generator)
  (make-py-coroutine "async_generator.aclose"
                     (lambda ()
                       (setf (py-async-generator-object-closed generator) t)
                       (setf (py-async-generator-object-index generator)
                             (length (py-async-generator-object-items generator)))
                       *py-none*)))

(defun py-async-generator-athrow (generator exception &rest args)
  (declare (ignore args))
  (make-py-coroutine "async_generator.athrow"
                     (lambda ()
                       (setf (py-async-generator-object-closed generator) t)
                       (cond
                         ((py-exception-object-p exception)
                          (py-raise exception))
                         ((py-type-p exception)
                          (py-raise (make-py-exception exception)))
                         (t
                          (py-raise (make-py-exception *py-type-error-type*
                                                       "exceptions must be classes or instances deriving from BaseException")))))))

(defun py-asyncio-invalid-state ()
  (py-raise (make-py-exception *py-asyncio-invalid-state-error-type* "invalid state")))

(defun py-future-done-p (future)
  (not (eq (py-asyncio-future-object-state future) :pending)))

(defun py-asyncio-schedule-callback (loop callback &rest args)
  (let ((handle (make-py-asyncio-handle-object
                 :type *py-asyncio-handle-type*
                 :callback callback
                 :args args)))
    (if (py-asyncio-event-loop-object-p loop)
        (push handle (py-asyncio-event-loop-object-ready-callbacks loop))
        (apply #'py-invoke-callable callback args))
    handle))

(defun py-asyncio-schedule-timer-callback (loop when callback &rest args)
  (let ((handle (make-py-asyncio-timer-handle-object
                 :type *py-asyncio-timer-handle-type*
                 :callback callback
                 :args args
                 :when when)))
    (if (py-asyncio-event-loop-object-p loop)
        (push handle (py-asyncio-event-loop-object-ready-callbacks loop))
        (apply #'py-invoke-callable callback args))
    handle))

(defun py-asyncio-handle-cancel (handle)
  (setf (py-asyncio-handle-object-cancelled handle) t)
  *py-none*)

(defun py-asyncio-handle-cancelled (handle)
  (py-bool (py-asyncio-handle-object-cancelled handle)))

(defun py-asyncio-timer-handle-when (handle)
  (py-asyncio-timer-handle-object-when handle))

(defun py-asyncio-run-ready (loop)
  (when (py-asyncio-event-loop-object-p loop)
    (loop while (py-asyncio-event-loop-object-ready-callbacks loop)
          do (let ((callbacks (nreverse (py-asyncio-event-loop-object-ready-callbacks loop))))
               (setf (py-asyncio-event-loop-object-ready-callbacks loop) '())
               (dolist (handle callbacks)
                 (unless (py-asyncio-handle-object-cancelled handle)
                   (apply #'py-invoke-callable
                          (py-asyncio-handle-object-callback handle)
                          (py-asyncio-handle-object-args handle)))))))
  *py-none*)

(defun py-future-run-callbacks (future)
  (let ((callbacks (nreverse (py-asyncio-future-object-callbacks future)))
        (loop (or (py-asyncio-future-object-loop future) *py-asyncio-running-loop*)))
    (setf (py-asyncio-future-object-callbacks future) '())
    (dolist (callback callbacks)
      (py-asyncio-schedule-callback loop callback future)))
  *py-none*)

(defun py-future-add-done-callback (future callback &rest args)
  (declare (ignore args))
  (if (py-future-done-p future)
      (py-asyncio-schedule-callback (or (py-asyncio-future-object-loop future) *py-asyncio-running-loop*) callback future)
      (push callback (py-asyncio-future-object-callbacks future)))
  *py-none*)

(defun py-future-remove-done-callback (future callback)
  (let ((removed 0)
        (kept '()))
    (dolist (registered (py-asyncio-future-object-callbacks future))
      (if (eq registered callback)
          (incf removed)
          (push registered kept)))
    (setf (py-asyncio-future-object-callbacks future) (nreverse kept))
    removed))

(defun py-future-mark-finished (future state result exception)
  (setf (py-asyncio-future-object-result future) result)
  (setf (py-asyncio-future-object-exception future) exception)
  (setf (py-asyncio-future-object-state future) state)
  (py-future-run-callbacks future)
  result)

(defun py-future-set-result (future result)
  (when (py-future-done-p future)
    (py-asyncio-invalid-state))
  (py-future-mark-finished future :finished result nil))

(defun py-future-set-exception (future exception)
  (when (py-future-done-p future)
    (py-asyncio-invalid-state))
  (py-future-mark-finished future :finished nil exception)
  *py-none*)

(defun py-future-result (future)
  (unless (py-future-done-p future)
    (py-asyncio-invalid-state))
  (let ((exception (py-asyncio-future-object-exception future)))
    (when exception
      (if (py-exception-object-p exception)
          (py-raise exception)
          (error exception))))
  (py-asyncio-future-object-result future))

(defun py-future-exception (future)
  (unless (py-future-done-p future)
    (py-asyncio-invalid-state))
  (when (eq (py-asyncio-future-object-state future) :cancelled)
    (py-raise (make-py-exception *py-asyncio-cancelled-error-type*)))
  (or (py-asyncio-future-object-exception future) *py-none*))

(defun py-future-cancel (future &optional (msg *py-none*))
  (declare (ignore msg))
  (if (py-future-done-p future)
      *py-false*
      (progn
        (py-future-mark-finished future :cancelled nil
                                 (make-py-exception *py-asyncio-cancelled-error-type*))
        *py-true*)))

(defun py-future-cancelled (future)
  (py-bool (eq (py-asyncio-future-object-state future) :cancelled)))

(defun py-future-done (future)
  (py-bool (py-future-done-p future)))

(defun py-future-get-loop (future)
  (py-asyncio-future-object-loop future))

(defun py-task-get-name (task)
  (py-asyncio-task-object-name task))

(defun py-task-set-name (task name)
  (setf (py-asyncio-task-object-name task) name)
  *py-none*)


(defun py-coroutine-run (coroutine)
  (case (py-coroutine-object-state coroutine)
    (:created
     (setf (py-coroutine-object-state coroutine) :running)
     (handler-case
         (let ((result (funcall (py-coroutine-object-thunk coroutine))))
           (setf (py-coroutine-object-result coroutine) result)
           (setf (py-coroutine-object-state coroutine) :closed)
           result)
       (py-exception (condition)
         (setf (py-coroutine-object-exception coroutine) (py-exception-value condition))
         (setf (py-coroutine-object-state coroutine) :closed)
         (error condition))
       (error (condition)
         (setf (py-coroutine-object-exception coroutine) condition)
         (setf (py-coroutine-object-state coroutine) :closed)
         (error condition))))
    (:closed
     (py-raise (make-py-exception *py-runtime-error-type* "cannot reuse already awaited coroutine")))
    (otherwise
     (py-raise (make-py-exception *py-runtime-error-type* "coroutine is already running")))))

(defun py-asyncio-run-task (task)
  (when (eq (py-asyncio-future-object-state task) :pending)
    (let* ((loop (py-asyncio-task-object-loop task))
           (previous-task (and (py-asyncio-event-loop-object-p loop)
                               (py-asyncio-event-loop-object-current-task loop))))
      (when (py-asyncio-event-loop-object-p loop)
        (setf (py-asyncio-event-loop-object-current-task loop) task))
      (unwind-protect
           (handler-case
               (py-future-set-result task (py-coroutine-run (py-asyncio-task-object-coroutine task)))
             (py-exception (condition)
               (py-future-mark-finished task :finished nil (py-exception-value condition))
               (error condition))
             (error (condition)
               (py-future-mark-finished task :finished nil condition)
               (error condition)))
        (when (py-asyncio-event-loop-object-p loop)
          (setf (py-asyncio-event-loop-object-current-task loop) previous-task)))))
  task)

(defun py-await (awaitable)
  (cond
    ((py-coroutine-object-p awaitable)
     (py-coroutine-run awaitable))
    ((py-asyncio-task-object-p awaitable)
     (py-asyncio-run-ready (py-asyncio-task-object-loop awaitable))
     (py-asyncio-run-task awaitable)
     (py-asyncio-run-ready (py-asyncio-task-object-loop awaitable))
     (py-future-result awaitable))
    ((py-asyncio-future-object-p awaitable)
     (py-asyncio-run-ready (py-asyncio-future-object-loop awaitable))
     (py-future-result awaitable))
    ((py-asyncio-sleep-object-p awaitable)
     (py-asyncio-run-ready *py-asyncio-running-loop*)
     (py-asyncio-sleep-object-result awaitable))
    ((py-object-p awaitable)
     (multiple-value-bind (method found) (py-find-type-attr (py-object-type awaitable) "__await__")
       (if found
           (let ((iterator (py-invoke-callable method awaitable)))
             (loop with result = *py-none*
                   do (multiple-value-bind (item found-item) (py-next-item iterator)
                        (unless found-item (return result))
                        (setf result item))))
           (py-raise (make-py-exception *py-type-error-type* "object is not awaitable")))))
    (t
     (py-raise (make-py-exception *py-type-error-type* "object is not awaitable")))))

(defun py-asyncio-make-event-loop-policy ()
  (make-py-asyncio-event-loop-policy-object
   :type *py-asyncio-event-loop-policy-type*))

(defun py-asyncio-get-event-loop-policy ()
  (or *py-asyncio-event-loop-policy*
      (setf *py-asyncio-event-loop-policy*
            (py-asyncio-make-event-loop-policy))))

(defun py-asyncio-set-event-loop-policy (policy)
  (setf *py-asyncio-event-loop-policy*
        (if (eq policy *py-none*)
            (py-asyncio-make-event-loop-policy)
            policy))
  *py-none*)

(defun py-asyncio-policy-new-event-loop (policy)
  (declare (ignore policy))
  (py-asyncio-new-event-loop))

(defun py-asyncio-policy-get-event-loop (policy)
  (or *py-asyncio-running-loop*
      (py-asyncio-event-loop-policy-object-loop policy)
      *py-asyncio-default-event-loop*
      (let ((loop (py-asyncio-new-event-loop)))
        (setf (py-asyncio-event-loop-policy-object-loop policy) loop)
        (setf *py-asyncio-default-event-loop* loop)
        loop)))

(defun py-asyncio-policy-set-event-loop (policy loop)
  (setf (py-asyncio-event-loop-policy-object-loop policy) loop)
  (setf *py-asyncio-default-event-loop* loop)
  *py-none*)

(defun py-asyncio-new-event-loop ()
  (make-py-asyncio-event-loop-object :type *py-asyncio-event-loop-type*))

(defun py-asyncio-create-future (loop)
  (make-py-asyncio-future-object :type *py-asyncio-future-type* :loop loop))

(defun py-asyncio-keyword-value (args keyword default)
  (let ((remaining args)
        (value default)
        (found nil)
        (positional '()))
    (loop while remaining
          do (let ((item (pop remaining)))
               (if (keywordp item)
                   (progn
                     (unless remaining
                       (error "keyword argument ~A has no value" item))
                     (let ((argument-value (pop remaining)))
                       (when (eq item keyword)
                         (setf value argument-value)
                         (setf found t))))
                   (push item positional))))
    (values value found (nreverse positional))))

(defun py-asyncio-create-task (loop coroutine &rest args)
  (multiple-value-bind (keyword-name keyword-name-supplied-p positional)
      (py-asyncio-keyword-value args :name *py-none*)
    (let ((name (cond
                  (keyword-name-supplied-p keyword-name)
                  (positional (first positional))
                  (t *py-none*))))
      (unless (py-coroutine-object-p coroutine)
        (py-raise (make-py-exception *py-type-error-type* "a coroutine was expected")))
      (let ((task (make-py-asyncio-task-object :type *py-asyncio-task-type*
                                               :loop loop
                                               :coroutine coroutine
                                               :name name)))
        (when (py-asyncio-event-loop-object-p loop)
          (pushnew task (py-asyncio-event-loop-object-tasks loop) :test #'eq))
        task))))

(defun py-asyncio-default-loop (loop)
  (if (eq loop *py-none*)
      (or *py-asyncio-running-loop*
          (py-asyncio-new-event-loop))
      loop))

(defun py-asyncio-future-constructor (&rest args)
  (multiple-value-bind (keyword-loop keyword-loop-supplied-p positional)
      (py-asyncio-keyword-value args :loop *py-none*)
    (let ((loop (cond
                  (keyword-loop-supplied-p keyword-loop)
                  (positional (first positional))
                  (t *py-none*))))
      (py-asyncio-create-future (py-asyncio-default-loop loop)))))

(defun py-asyncio-task-constructor (coroutine &rest args)
  (multiple-value-bind (keyword-loop keyword-loop-supplied-p positional)
      (py-asyncio-keyword-value args :loop *py-none*)
    (multiple-value-bind (keyword-name keyword-name-supplied-p ignored-positional)
        (py-asyncio-keyword-value args :name *py-none*)
      (declare (ignore ignored-positional))
      (let ((loop (cond
                    (keyword-loop-supplied-p keyword-loop)
                    (positional (first positional))
                    (t *py-none*)))
            (name (if keyword-name-supplied-p keyword-name *py-none*)))
        (py-asyncio-create-task (py-asyncio-default-loop loop) coroutine :name name)))))

(defun py-asyncio-run-until-complete (loop awaitable)
  (when (py-asyncio-event-loop-object-running loop)
    (py-raise (make-py-exception *py-runtime-error-type* "This event loop is already running")))
  (let ((*py-asyncio-running-loop* loop))
    (setf (py-asyncio-event-loop-object-running loop) t)
    (unwind-protect
         (py-await (if (py-coroutine-object-p awaitable)
                       (py-asyncio-create-task loop awaitable)
                       awaitable))
      (setf (py-asyncio-event-loop-object-running loop) nil))))

(defun py-asyncio-run-forever (loop)
  (when (py-asyncio-event-loop-object-running loop)
    (py-raise (make-py-exception *py-runtime-error-type* "This event loop is already running")))
  (let ((*py-asyncio-running-loop* loop))
    (setf (py-asyncio-event-loop-object-running loop) t)
    (setf (py-asyncio-event-loop-object-stopping loop) nil)
    (unwind-protect
         (loop while (and (not (py-asyncio-event-loop-object-stopping loop))
                          (py-asyncio-event-loop-object-ready-callbacks loop))
               do (py-asyncio-run-ready loop))
      (setf (py-asyncio-event-loop-object-running loop) nil)
      (setf (py-asyncio-event-loop-object-stopping loop) nil)))
  *py-none*)

(defun py-asyncio-loop-stop (loop)
  (setf (py-asyncio-event-loop-object-stopping loop) t)
  *py-none*)

(defun py-asyncio-run (awaitable &key (debug *py-none*) (loop_factory *py-none*))
  (declare (ignore debug loop_factory))
  (when *py-asyncio-running-loop*
    (py-raise (make-py-exception *py-runtime-error-type* "asyncio.run() cannot be called from a running event loop")))
  (let ((loop (py-asyncio-new-event-loop)))
    (unwind-protect
         (py-asyncio-run-until-complete loop awaitable)
      (setf (py-asyncio-event-loop-object-closed loop) t))))

(defun py-asyncio-runner (&rest args)
  (multiple-value-bind (keyword-debug keyword-debug-supplied-p positional)
      (py-asyncio-keyword-value args :debug *py-none*)
    (declare (ignore positional))
    (multiple-value-bind (keyword-loop-factory keyword-loop-factory-supplied-p ignored-positional)
        (py-asyncio-keyword-value args :loop_factory *py-none*)
      (declare (ignore ignored-positional))
      (let ((runner (make-py-asyncio-runner-object
                     :type *py-asyncio-runner-type*
                     :debug (if keyword-debug-supplied-p keyword-debug *py-none*)
                     :loop-factory (if keyword-loop-factory-supplied-p keyword-loop-factory *py-none*))))
        (setf (py-object-attr runner "closed") *py-false*)
        runner))))

(defun py-asyncio-runner-get-or-create-loop (runner)
  (when (py-asyncio-runner-object-closed runner)
    (py-raise (make-py-exception *py-runtime-error-type* "Runner is closed")))
  (or (py-asyncio-runner-object-loop runner)
      (let* ((factory (py-asyncio-runner-object-loop-factory runner))
             (loop (if (and factory (not (eq factory *py-none*)))
                       (py-invoke-callable factory)
                       (py-asyncio-new-event-loop))))
        (setf (py-asyncio-runner-object-loop runner) loop)
        loop)))

(defun py-asyncio-runner-enter (runner)
  (py-asyncio-runner-get-or-create-loop runner)
  (setf (py-asyncio-runner-object-entered runner) t)
  runner)

(defun py-asyncio-runner-exit (runner exc-type exc-value traceback)
  (declare (ignore exc-type exc-value traceback))
  (py-asyncio-runner-close runner)
  *py-false*)

(defun py-asyncio-runner-close (runner)
  (unless (py-asyncio-runner-object-closed runner)
    (let ((loop (py-asyncio-runner-object-loop runner)))
      (when (py-asyncio-event-loop-object-p loop)
        (setf (py-asyncio-event-loop-object-closed loop) t)))
    (setf (py-asyncio-runner-object-closed runner) t)
    (setf (py-object-attr runner "closed") *py-true*))
  *py-none*)

(defun py-asyncio-runner-get-loop (runner)
  (py-asyncio-runner-get-or-create-loop runner))

(defun py-asyncio-runner-run (runner awaitable &rest args)
  (declare (ignore args))
  (when *py-asyncio-running-loop*
    (py-raise (make-py-exception *py-runtime-error-type* "Runner.run() cannot be called from a running event loop")))
  (py-asyncio-run-until-complete (py-asyncio-runner-get-or-create-loop runner) awaitable))

(setf (py-type-attr *py-asyncio-runner-type* "__enter__") #'py-asyncio-runner-enter)
(setf (py-type-attr *py-asyncio-runner-type* "__exit__") #'py-asyncio-runner-exit)
(setf (py-type-attr *py-asyncio-runner-type* "close") #'py-asyncio-runner-close)
(setf (py-type-attr *py-asyncio-runner-type* "get_loop") #'py-asyncio-runner-get-loop)

(setf (py-type-attr *py-asyncio-runner-type* "run") #'py-asyncio-runner-run)

(defun py-asyncio-stream-text (stream)
  (if stream
      (with-output-to-string (out)
        (loop for char = (read-char stream nil nil)
              while char
              do (write-char char out)))
      ""))

(defun py-asyncio-subprocess-bytes (text)
  (make-py-bytes-from-vector
   (sb-ext:string-to-octets (or text "") :external-format :utf-8)))

(defun py-asyncio-run-program-capture (program arguments stdout-mode stderr-mode shell-p)
  (let* ((output-stream-p (eq stdout-mode *py-asyncio-subprocess-pipe*))
         (error-stream-p (eq stderr-mode *py-asyncio-subprocess-pipe*))
         (error-to-output-p (eq stderr-mode *py-asyncio-subprocess-stdout*))
         (process (if shell-p
                      (sb-ext:run-program "/bin/sh"
                                          (list "-c" program)
                                          :search nil
                                          :wait t
                                          :output (if output-stream-p :stream nil)
                                          :error (cond
                                                   (error-stream-p :stream)
                                                   (error-to-output-p :output)
                                                   (t nil)))
                      (sb-ext:run-program program
                                          arguments
                                          :search t
                                          :wait t
                                          :output (if output-stream-p :stream nil)
                                          :error (cond
                                                   (error-stream-p :stream)
                                                   (error-to-output-p :output)
                                                   (t nil))))))
    (let ((stdout-text (if output-stream-p
                           (py-asyncio-stream-text (sb-ext:process-output process))
                           ""))
          (stderr-text (if error-stream-p
                           (py-asyncio-stream-text (sb-ext:process-error process))
                           ""))
          (returncode (or (ignore-errors (sb-ext:process-exit-code process)) 0)))
      (values returncode stdout-text stderr-text))))

(defun py-asyncio-make-process (args returncode stdout-text stderr-text stdout-mode stderr-mode)
  (let ((process (make-py-asyncio-process-object
                  :type *py-asyncio-process-type*
                  :args args
                  :returncode returncode
                  :stdout (if (eq stdout-mode *py-asyncio-subprocess-pipe*)
                              (py-asyncio-subprocess-bytes stdout-text)
                              *py-none*)
                  :stderr (if (eq stderr-mode *py-asyncio-subprocess-pipe*)
                              (py-asyncio-subprocess-bytes stderr-text)
                              *py-none*))))
    (setf (py-object-attr process "args") args)
    (setf (py-object-attr process "returncode") returncode)
    (setf (py-object-attr process "stdout") (py-asyncio-process-object-stdout process))
    (setf (py-object-attr process "stderr") (py-asyncio-process-object-stderr process))
    process))

(defun py-asyncio-subprocess-keyword (args keyword default)
  (multiple-value-bind (value supplied-p positional)
      (py-asyncio-keyword-value args keyword default)
    (values (if supplied-p value default) positional)))

(defun py-asyncio-create-subprocess-exec (program &rest args)
  (multiple-value-bind (stdout-mode positional-after-stdout)
      (py-asyncio-subprocess-keyword args :stdout *py-none*)
    (declare (ignore positional-after-stdout))
    (multiple-value-bind (stderr-mode positional)
        (py-asyncio-subprocess-keyword args :stderr *py-none*)
      (make-py-coroutine "create_subprocess_exec"
                         (lambda ()
                           (let ((arguments (mapcar #'py-str positional)))
                             (multiple-value-bind (returncode stdout-text stderr-text)
                                 (py-asyncio-run-program-capture (py-str program) arguments stdout-mode stderr-mode nil)
                               (py-asyncio-make-process
                                (apply #'make-py-tuple program positional)
                                returncode
                                stdout-text
                                stderr-text
                                stdout-mode
                                stderr-mode))))))))

(defun py-asyncio-create-subprocess-shell (cmd &rest args)
  (multiple-value-bind (stdout-mode positional-after-stdout)
      (py-asyncio-subprocess-keyword args :stdout *py-none*)
    (declare (ignore positional-after-stdout))
    (multiple-value-bind (stderr-mode positional)
        (py-asyncio-subprocess-keyword args :stderr *py-none*)
      (declare (ignore positional))
      (make-py-coroutine "create_subprocess_shell"
                         (lambda ()
                           (multiple-value-bind (returncode stdout-text stderr-text)
                               (py-asyncio-run-program-capture (py-str cmd) '() stdout-mode stderr-mode t)
                             (py-asyncio-make-process
                              (make-py-tuple cmd)
                              returncode
                              stdout-text
                              stderr-text
                              stdout-mode
                              stderr-mode)))))))

(defun py-asyncio-process-communicate (process &optional (input *py-none*))
  (declare (ignore input))
  (make-py-coroutine "Process.communicate"
                     (lambda ()
                       (make-py-tuple
                        (py-asyncio-process-object-stdout process)
                        (py-asyncio-process-object-stderr process)))))

(defun py-asyncio-process-wait (process)
  (make-py-coroutine "Process.wait"
                     (lambda ()
                       (py-asyncio-process-object-returncode process))))

(defun py-asyncio-process-send-signal (process signal)
  (declare (ignore process signal))
  *py-none*)

(defun py-asyncio-process-terminate (process)
  (declare (ignore process))
  *py-none*)

(defun py-asyncio-process-kill (process)
  (declare (ignore process))
  *py-none*)

(setf (py-type-attr *py-asyncio-process-type* "communicate") #'py-asyncio-process-communicate)
(setf (py-type-attr *py-asyncio-process-type* "wait") #'py-asyncio-process-wait)
(setf (py-type-attr *py-asyncio-process-type* "send_signal") #'py-asyncio-process-send-signal)
(setf (py-type-attr *py-asyncio-process-type* "terminate") #'py-asyncio-process-terminate)
(setf (py-type-attr *py-asyncio-process-type* "kill") #'py-asyncio-process-kill)

(defun py-asyncio-get-running-loop ()
  (or *py-asyncio-running-loop*
      (py-raise (make-py-exception *py-runtime-error-type* "no running event loop"))))

(defun py-asyncio-get-event-loop ()
  (py-asyncio-policy-get-event-loop (py-asyncio-get-event-loop-policy)))

(defun py-asyncio-set-event-loop (loop)
    (py-asyncio-policy-set-event-loop (py-asyncio-get-event-loop-policy) loop))

(defun py-asyncio-ensure-future (awaitable &rest args)
  (multiple-value-bind (keyword-loop keyword-loop-supplied-p positional)
      (py-asyncio-keyword-value args :loop *py-none*)
    (declare (ignore positional))
    (let ((loop (if keyword-loop-supplied-p
                    keyword-loop
                    (or *py-asyncio-running-loop* (py-asyncio-new-event-loop)))))
      (cond
        ((or (py-asyncio-future-object-p awaitable)
             (py-asyncio-task-object-p awaitable))
         awaitable)
        ((py-coroutine-object-p awaitable)
         (py-asyncio-create-task loop awaitable))
        (t
         (py-raise (make-py-exception *py-type-error-type* "An asyncio.Future, a coroutine or an awaitable is required")))))))

(defun py-asyncio-shield (awaitable &rest args)
  (apply #'py-asyncio-ensure-future awaitable args))

(defun py-asyncio-timeout-expired-p (timeout)
  (and (not (eq timeout *py-none*))
       (numberp timeout)
       (<= timeout 0)))

(defun py-asyncio-wait-for (awaitable &rest args)
  (multiple-value-bind (keyword-timeout keyword-timeout-supplied-p positional)
      (py-asyncio-keyword-value args :timeout *py-none*)
    (let ((timeout (cond
                     (keyword-timeout-supplied-p keyword-timeout)
                     (positional (first positional))
                     (t *py-none*))))
      (make-py-coroutine "wait_for"
                         (lambda ()
                           (let ((future (py-asyncio-ensure-future awaitable)))
                             (when (and (py-asyncio-timeout-expired-p timeout)
                                        (not (py-future-done-p future)))
                               (py-future-cancel future)
                               (py-raise (make-py-exception *py-timeout-error-type*)))
                             (py-await future)))))))

(defun py-asyncio-current-time ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun py-asyncio-timeout-deadline-expired-p (timeout)
  (let ((deadline (py-asyncio-timeout-object-deadline timeout)))
    (and (not (eq deadline *py-none*))
         (numberp deadline)
         (<= deadline (py-asyncio-current-time)))))

(defun py-asyncio-make-timeout (deadline)
  (make-py-asyncio-timeout-object
   :type *py-asyncio-timeout-type*
   :loop (or *py-asyncio-running-loop*
             (py-asyncio-new-event-loop))
   :deadline deadline))

(defun py-asyncio-timeout (&rest args)
  (multiple-value-bind (keyword-delay keyword-delay-supplied-p positional)
      (py-asyncio-keyword-value args :delay *py-none*)
    (let* ((delay (cond
                    (keyword-delay-supplied-p keyword-delay)
                    (positional (first positional))
                    (t *py-none*)))
           (deadline (if (eq delay *py-none*)
                         *py-none*
                         (+ (py-asyncio-current-time) delay))))
      (py-asyncio-make-timeout deadline))))

(defun py-asyncio-timeout-at (&rest args)
  (multiple-value-bind (keyword-when keyword-when-supplied-p positional)
      (py-asyncio-keyword-value args :when *py-none*)
    (let ((deadline (cond
                      (keyword-when-supplied-p keyword-when)
                      (positional (first positional))
                      (t *py-none*))))
      (py-asyncio-make-timeout deadline))))

(defun py-asyncio-timeout-when (timeout)
  (py-asyncio-timeout-object-deadline timeout))

(defun py-asyncio-timeout-reschedule (timeout deadline)
  (setf (py-asyncio-timeout-object-deadline timeout) deadline)
  (setf (py-asyncio-timeout-object-expired timeout) nil)
  *py-none*)

(defun py-asyncio-timeout-expired (timeout)
  (when (py-asyncio-timeout-deadline-expired-p timeout)
    (setf (py-asyncio-timeout-object-expired timeout) t))
  (py-bool (py-asyncio-timeout-object-expired timeout)))

(defun py-asyncio-timeout-aenter (timeout)
  (make-py-coroutine "Timeout.__aenter__"
                     (lambda ()
                       (when (py-asyncio-timeout-deadline-expired-p timeout)
                         (setf (py-asyncio-timeout-object-expired timeout) t)
                         (py-raise (make-py-exception *py-timeout-error-type*)))
                       timeout)))

(defun py-asyncio-timeout-aexit (timeout exc-type exc-value traceback)
  (declare (ignore exc-type exc-value traceback))
  (make-py-coroutine "Timeout.__aexit__"
                     (lambda ()
                       (when (py-asyncio-timeout-deadline-expired-p timeout)
                         (setf (py-asyncio-timeout-object-expired timeout) t)
                         (py-raise (make-py-exception *py-timeout-error-type*)))
                       *py-false*)))

(setf (py-type-attr *py-asyncio-timeout-type* "when") #'py-asyncio-timeout-when)
(setf (py-type-attr *py-asyncio-timeout-type* "reschedule") #'py-asyncio-timeout-reschedule)
(setf (py-type-attr *py-asyncio-timeout-type* "expired") #'py-asyncio-timeout-expired)
(setf (py-type-attr *py-asyncio-timeout-type* "__aenter__") #'py-asyncio-timeout-aenter)
(setf (py-type-attr *py-asyncio-timeout-type* "__aexit__") #'py-asyncio-timeout-aexit)

(defun py-asyncio-current-task (&optional (loop *py-none*))
  (let ((selected-loop (if (eq loop *py-none*) *py-asyncio-running-loop* loop)))
    (if (py-asyncio-event-loop-object-p selected-loop)
        (or (py-asyncio-event-loop-object-current-task selected-loop) *py-none*)
        *py-none*)))

(defun py-asyncio-all-tasks (&optional (loop *py-none*))
  (let ((selected-loop (if (eq loop *py-none*) *py-asyncio-running-loop* loop)))
    (if (py-asyncio-event-loop-object-p selected-loop)
        (apply #'make-py-list
               (remove-if #'py-future-done-p
                          (py-asyncio-event-loop-object-tasks selected-loop)))
        (make-py-list))))

(defun py-asyncio-module-create-task (coroutine &rest args)
  (apply #'py-asyncio-create-task (py-asyncio-get-running-loop) coroutine args))

(defun py-asyncio-isfuture (obj)
  (py-bool (py-asyncio-future-object-p obj)))

(defun py-asyncio-iscoroutine (obj)
  (py-bool (or (py-coroutine-object-p obj)
               (py-async-generator-object-p obj))))

(defun py-asyncio-iscoroutinefunction (obj)
  (py-bool
   (and (py-callable-p obj)
        (py-callable-coroutine-function obj)
        (not (py-callable-async-generator-function obj)))))

(defun py-inspect-iscoroutine (obj)
  (py-bool (py-coroutine-object-p obj)))

(defun py-inspect-iscoroutinefunction (obj)
  (py-asyncio-iscoroutinefunction obj))

(defun py-inspect-isasyncgen (obj)
  (py-bool (py-async-generator-object-p obj)))

(defun py-inspect-isasyncgenfunction (obj)
  (py-bool
   (and (py-callable-p obj)
        (py-callable-async-generator-function obj))))

(defun py-inspect-isawaitable (obj)
  (py-bool
   (or (py-coroutine-object-p obj)
       (py-asyncio-future-object-p obj)
       (and (py-object-p obj)
            (multiple-value-bind (attr found) (gethash "__await__" (py-object-attrs obj))
              (declare (ignore attr))
              found))
       (and (or (py-object-p obj)
                (integerp obj)
                (floatp obj)
                (stringp obj))
            (multiple-value-bind (attr found) (py-find-type-attr (py-type-of obj) "__await__")
              (declare (ignore attr))
              found)))))

(defun py-asyncio-run-coroutine-threadsafe (coroutine loop)
  (unless (py-coroutine-object-p coroutine)
    (py-raise (make-py-exception *py-type-error-type* "A coroutine object is required")))
  (unless (py-asyncio-event-loop-object-p loop)
    (py-raise (make-py-exception *py-type-error-type* "loop must be an event loop")))
  (let ((task (py-asyncio-create-task loop coroutine)))
    (py-asyncio-run-ready loop)
    (py-asyncio-run-task task)
    (py-asyncio-run-ready loop)
    task))

(defun py-asyncio-to-thread (callable &rest args)
  (make-py-coroutine "to_thread"
                     (lambda ()
                       (apply #'py-invoke-callable callable args))))

(defun py-asyncio-task-group ()
  (make-py-asyncio-task-group-object
   :type *py-asyncio-task-group-type*
   :loop (or *py-asyncio-running-loop*
             (py-asyncio-new-event-loop))))

(defun py-asyncio-task-group-aenter (group)
  (make-py-coroutine "TaskGroup.__aenter__"
                     (lambda ()
                       (setf (py-asyncio-task-group-object-entered group) t)
                       (setf (py-asyncio-task-group-object-exiting group) nil)
                       group)))

(defun py-asyncio-task-group-create-task (group coroutine &rest args)
  (unless (py-asyncio-task-group-object-entered group)
    (py-raise (make-py-exception *py-runtime-error-type*
                                 "TaskGroup has not been entered")))
  (when (py-asyncio-task-group-object-exiting group)
    (py-raise (make-py-exception *py-runtime-error-type*
                                 "TaskGroup is finished")))
  (let* ((loop (or *py-asyncio-running-loop*
                   (py-asyncio-task-group-object-loop group)
                   (py-asyncio-new-event-loop)))
         (task (apply #'py-asyncio-create-task loop coroutine args)))
    (push task (py-asyncio-task-group-object-tasks group))
    task))

(defun py-asyncio-task-group-aexit (group exc-type exc-value traceback)
  (declare (ignore exc-type traceback))
  (make-py-coroutine "TaskGroup.__aexit__"
                     (lambda ()
                       (setf (py-asyncio-task-group-object-exiting group) t)
                       (when (not (eq exc-value *py-none*))
                         (dolist (task (py-asyncio-task-group-object-tasks group))
                           (unless (py-future-done-p task)
                             (py-future-cancel task))))
                       (unwind-protect
                            (dolist (task (nreverse (py-asyncio-task-group-object-tasks group)))
                              (py-await task))
                         (setf (py-asyncio-task-group-object-entered group) nil)
                         (setf (py-asyncio-task-group-object-exiting group) t))
                       *py-false*)))

(setf (py-type-attr *py-asyncio-task-group-type* "__aenter__") #'py-asyncio-task-group-aenter)
(setf (py-type-attr *py-asyncio-task-group-type* "__aexit__") #'py-asyncio-task-group-aexit)
(setf (py-type-attr *py-asyncio-task-group-type* "create_task") #'py-asyncio-task-group-create-task)

(defun py-asyncio-sleep (delay &rest args)
  (multiple-value-bind (keyword-result keyword-result-supplied-p positional)
      (py-asyncio-keyword-value args :result *py-none*)
    (let ((result (cond
                    (keyword-result-supplied-p keyword-result)
                    (positional (first positional))
                    (t *py-none*))))
      (make-py-asyncio-sleep-object :type *py-asyncio-sleep-type*
                                    :delay delay
                                    :result result))))

(defun py-asyncio-gather (&rest awaitables)
  (let ((filtered-awaitables '())
        (return-exceptions *py-false*)
        (rest awaitables))
    (loop while rest
          do (let ((item (pop rest)))
               (if (eq item :return_exceptions)
                   (setf return-exceptions (if rest (pop rest) *py-false*))
                   (push item filtered-awaitables))))
    (setf awaitables (nreverse filtered-awaitables))
    (let ((future (py-asyncio-create-future (or *py-asyncio-running-loop*
                                               (py-asyncio-new-event-loop))))
          (result (make-py-list)))
      (dolist (awaitable awaitables)
        (handler-case
            (py-append result (py-await awaitable))
          (py-exception (condition)
            (if (py-truthy-p return-exceptions)
                (py-append result (py-exception-value condition))
                (error condition)))
          (error (condition)
            (if (py-truthy-p return-exceptions)
                (py-append result condition)
                (error condition)))))
      (py-future-set-result future result)
      future)))

(defparameter *py-asyncio-first-completed* "FIRST_COMPLETED")
(defparameter *py-asyncio-first-exception* "FIRST_EXCEPTION")
(defparameter *py-asyncio-all-completed* "ALL_COMPLETED")

(defun py-asyncio-wait-normalize-awaitables (awaitables loop)
  (let ((items '())
        (iterator (py-iter awaitables)))
    (loop
      (multiple-value-bind (item found) (py-next-item iterator)
        (unless found (return))
        (push (if (or (py-asyncio-future-object-p item)
                      (py-asyncio-task-object-p item))
                  item
                  (py-asyncio-create-task loop item))
              items)))
    (nreverse items)))

(defun py-asyncio-wait (awaitables &rest args)
  (multiple-value-bind (keyword-timeout keyword-timeout-supplied-p positional-after-timeout)
      (py-asyncio-keyword-value args :timeout *py-none*)
    (let ((timeout (cond
                     (keyword-timeout-supplied-p keyword-timeout)
                     (positional-after-timeout (first positional-after-timeout))
                     (t *py-none*))))
      (multiple-value-bind (keyword-return-when keyword-return-when-supplied-p positional)
          (py-asyncio-keyword-value args :return_when *py-asyncio-all-completed*)
        (declare (ignore positional))
        (let ((return-when (if keyword-return-when-supplied-p
                               keyword-return-when
                               *py-asyncio-all-completed*)))
          (make-py-coroutine "wait"
                             (lambda ()
                               (let* ((loop (or *py-asyncio-running-loop*
                                                (py-asyncio-new-event-loop)))
                                      (items (py-asyncio-wait-normalize-awaitables awaitables loop))
                                      (done (make-py-list))
                                      (pending (make-py-list)))
                                 (if (py-asyncio-timeout-expired-p timeout)
                                     (dolist (item items)
                                       (if (py-future-done-p item)
                                           (py-append done item)
                                           (py-append pending item)))
                                     (progn
                                       (dolist (item items)
                                         (let ((had-exception nil))
                                           (unless (py-future-done-p item)
                                             (handler-case
                                                 (py-await item)
                                               (py-exception (condition)
                                                 (declare (ignore condition))
                                                 (setf had-exception t))
                                               (error (condition)
                                                 (declare (ignore condition))
                                                 (setf had-exception t))))
                                           (when (py-future-done-p item)
                                             (py-append done item))
                                           (when (or (and (string= return-when *py-asyncio-first-completed*)
                                                          (> (or (py-object-size done) 0) 0))
                                                     (and (string= return-when *py-asyncio-first-exception*)
                                                          had-exception))
                                             (return))))
                                       (dolist (item items)
                                         (unless (py-future-done-p item)
                                           (py-append pending item)))))
                                 (make-py-tuple done pending)))))))))

(defun py-asyncio-loop-time (loop)
  (declare (ignore loop))
  (py-asyncio-current-time))

(defun py-asyncio-loop-call-soon (loop callback &rest args)
  (apply #'py-asyncio-schedule-callback loop callback args))

(defun py-asyncio-loop-call-later (loop delay callback &rest args)
  (apply #'py-asyncio-schedule-timer-callback
         loop
         (+ (py-asyncio-loop-time loop) delay)
         callback
         args))

(defun py-asyncio-loop-call-at (loop when callback &rest args)
  (apply #'py-asyncio-schedule-timer-callback loop when callback args))

(defun py-asyncio-loop-call-soon-threadsafe (loop callback &rest args)
  (apply #'py-asyncio-loop-call-soon loop callback args))

(defun py-asyncio-loop-get-debug (loop)
  (py-bool (py-asyncio-event-loop-object-debug loop)))

(defun py-asyncio-loop-set-debug (loop enabled)
  (setf (py-asyncio-event-loop-object-debug loop) (py-truthy-p enabled))
  *py-none*)

(defun py-asyncio-loop-shutdown-asyncgens (loop)
  (declare (ignore loop))
  (make-py-coroutine "shutdown_asyncgens"
                     (lambda () *py-none*)))

(defun py-asyncio-loop-shutdown-default-executor (loop &rest args)
  (declare (ignore loop args))
  (make-py-coroutine "shutdown_default_executor"
                     (lambda () *py-none*)))

(defun py-asyncio-loop-run-in-executor (loop executor callable &rest args)
  (declare (ignore executor))
  (let ((future (py-asyncio-create-future loop)))
    (handler-case
        (py-future-set-result future (apply #'py-invoke-callable callable args))
      (py-exception (condition)
        (py-future-set-exception future (py-exception-value condition)))
      (error (condition)
        (py-future-set-exception future condition)))
    future))


(defun py-asyncio-stream-string-bytes (text)
  (make-py-bytes-from-vector
   (sb-ext:string-to-octets text :external-format :utf-8)))

(defun py-asyncio-stream-bytes-string (data)
  (cond
    ((stringp data) data)
    ((py-bytes-object-p data)
     (sb-ext:octets-to-string (py-bytes-storage data "StreamWriter.write") :external-format :utf-8))
    (t (py-str data))))

(defun py-asyncio-stream-reader-read (reader &optional (n -1))
  (make-py-coroutine "StreamReader.read"
                     (lambda ()
                       (let ((stream (py-asyncio-stream-reader-object-stream reader)))
                         (cond
                           ((and (integerp n) (= n 0))
                            (py-asyncio-stream-string-bytes ""))
                           ((and (integerp n) (> n 0))
                            (let ((buffer (make-string n)))
                              (let ((count (read-sequence buffer stream)))
                                (when (< count n)
                                  (setf (py-asyncio-stream-reader-object-eof reader) t))
                                (py-asyncio-stream-string-bytes (subseq buffer 0 count)))))
                           (t
                            (let ((text (with-output-to-string (out)
                                          (loop for char = (read-char stream nil nil)
                                                while char
                                                do (write-char char out)))))
                              (setf (py-asyncio-stream-reader-object-eof reader) t)
                              (py-asyncio-stream-string-bytes text))))))))

(defun py-asyncio-stream-reader-readline (reader)
  (make-py-coroutine "StreamReader.readline"
                     (lambda ()
                       (multiple-value-bind (line missing-newline-p)
                           (read-line (py-asyncio-stream-reader-object-stream reader) nil nil)
                         (if line
                             (progn
                               (when missing-newline-p
                                 (setf (py-asyncio-stream-reader-object-eof reader) t))
                               (py-asyncio-stream-string-bytes
                                (if missing-newline-p
                                    line
                                    (concatenate 'string line (string #\Newline)))))
                             (progn
                               (setf (py-asyncio-stream-reader-object-eof reader) t)
                               (py-asyncio-stream-string-bytes "")))))))

(defun py-asyncio-stream-incomplete-read-error (partial expected)
  (let ((exception (make-py-exception *py-asyncio-incomplete-read-error-type* partial expected)))
    (setf (py-object-attr exception "partial") partial)
    (setf (py-object-attr exception "expected") expected)
    exception))

(defun py-asyncio-stream-limit-overrun-error (message consumed)
  (let ((exception (make-py-exception *py-asyncio-limit-overrun-error-type* message)))
    (setf (py-object-attr exception "consumed") consumed)
    exception))

(defun py-asyncio-stream-separator-string (separator)
  (cond
    ((or (eq separator *py-none*) (null separator)) (string #\Newline))
    ((stringp separator) separator)
    ((py-bytes-object-p separator)
     (sb-ext:octets-to-string (py-bytes-storage separator "StreamReader.readuntil") :external-format :utf-8))
    (t (py-raise (make-py-exception *py-type-error-type* "separator must be bytes or str")))))

(defun py-asyncio-string-suffix-p (value suffix)
  (let ((value-length (length value))
        (suffix-length (length suffix)))
    (and (<= suffix-length value-length)
         (string= suffix value :start1 0 :end1 suffix-length
                  :start2 (- value-length suffix-length) :end2 value-length))))

(defun py-asyncio-stream-reader-readuntil (reader &optional (separator *py-none*))
  (make-py-coroutine "StreamReader.readuntil"
                     (lambda ()
                       (let ((separator-text (py-asyncio-stream-separator-string separator)))
                         (when (= (length separator-text) 0)
                           (py-raise (make-py-exception *py-value-error-type* "Separator should be at least one-byte string")))
                         (let ((stream (py-asyncio-stream-reader-object-stream reader))
                               (buffer ""))
                           (loop for char = (read-char stream nil nil)
                                 while char
                                 do (setf buffer (concatenate 'string buffer (string char)))
                                    (when (py-asyncio-string-suffix-p buffer separator-text)
                                      (return (py-asyncio-stream-string-bytes buffer)))
                                 finally
                                    (progn
                                      (setf (py-asyncio-stream-reader-object-eof reader) t)
                                      (py-raise (py-asyncio-stream-incomplete-read-error
                                                 (py-asyncio-stream-string-bytes buffer)
                                                 *py-none*)))))))))

(defun py-asyncio-stream-reader-readexactly (reader n)
  (make-py-coroutine "StreamReader.readexactly"
                     (lambda ()
                       (let ((buffer (make-string n))
                             (stream (py-asyncio-stream-reader-object-stream reader)))
                         (let ((count (read-sequence buffer stream)))
                           (when (< count n)
                             (setf (py-asyncio-stream-reader-object-eof reader) t)
                             (py-raise
                              (py-asyncio-stream-incomplete-read-error
                               (py-asyncio-stream-string-bytes (subseq buffer 0 count))
                               n)))
                           (py-asyncio-stream-string-bytes buffer))))))

(defun py-asyncio-stream-reader-at-eof (reader)
  (py-bool (py-asyncio-stream-reader-object-eof reader)))

(defun py-asyncio-stream-reader-aiter (reader)
  reader)

(defun py-asyncio-stream-reader-anext (reader)
  (make-py-coroutine "StreamReader.__anext__"
                     (lambda ()
                       (let ((line (py-await (py-asyncio-stream-reader-readline reader))))
                         (if (= (or (py-object-size line) 0) 0)
                             (py-raise (make-py-exception *py-stop-async-iteration-type*))
                             line)))))

(defun py-asyncio-stream-writer-write (writer data)
  (unless (py-asyncio-stream-writer-object-closing writer)
    (write-string (py-asyncio-stream-bytes-string data)
                  (py-asyncio-stream-writer-object-stream writer)))
  *py-none*)

(defun py-asyncio-stream-writer-writelines (writer data)
  (let ((iterator (py-iter data)))
    (loop
      (multiple-value-bind (item found) (py-next-item iterator)
        (unless found (return))
        (py-asyncio-stream-writer-write writer item))))
  *py-none*)

(defun py-asyncio-stream-writer-can-write-eof (writer)
  (declare (ignore writer))
  *py-true*)

(defun py-asyncio-stream-writer-write-eof (writer)
  (unless (py-asyncio-stream-writer-object-closing writer)
    (finish-output (py-asyncio-stream-writer-object-stream writer)))
  *py-none*)

(defun py-asyncio-stream-writer-drain (writer)
  (make-py-coroutine "StreamWriter.drain"
                     (lambda ()
                       (unless (py-asyncio-stream-writer-object-closing writer)
                         (finish-output (py-asyncio-stream-writer-object-stream writer)))
                       *py-none*)))

(defun py-asyncio-stream-writer-close (writer)
  (setf (py-asyncio-stream-writer-object-closing writer) t)
  (ignore-errors (close (py-asyncio-stream-writer-object-stream writer)))
  (ignore-errors (sb-bsd-sockets:socket-close (py-asyncio-stream-writer-object-socket writer)))
  *py-none*)

(defun py-asyncio-stream-writer-wait-closed (writer)
  (make-py-coroutine "StreamWriter.wait_closed"
                     (lambda ()
                       (py-asyncio-stream-writer-close writer)
                       *py-none*)))

(defun py-asyncio-stream-writer-is-closing (writer)
  (py-bool (py-asyncio-stream-writer-object-closing writer)))

(defun py-asyncio-stream-writer-get-extra-info (writer name &optional (default *py-none*))
  (cond
    ((string= name "peername") (py-asyncio-stream-writer-object-peername writer))
    ((string= name "sockname") (py-asyncio-socket-name-tuple
                                (py-asyncio-stream-writer-object-socket writer)))
    (t default)))

(defun py-asyncio-open-connection (&rest args)
  (multiple-value-bind (keyword-host keyword-host-supplied-p positional)
      (py-asyncio-keyword-value args :host *py-none*)
    (multiple-value-bind (keyword-port keyword-port-supplied-p ignored-positional)
        (py-asyncio-keyword-value args :port *py-none*)
      (declare (ignore ignored-positional))
      (let ((host (cond
                    (keyword-host-supplied-p keyword-host)
                    (positional (first positional))
                    (t *py-none*)))
            (port (cond
                    (keyword-port-supplied-p keyword-port)
                    ((rest positional) (second positional))
                    (t *py-none*))))
        (make-py-coroutine "open_connection"
                           (lambda ()
                             (unless (and (stringp host) (integerp port))
                               (py-raise (make-py-exception *py-type-error-type* "open_connection requires host and port")))
                             (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
                               (handler-case
                                   (progn
                                     (sb-bsd-sockets:socket-connect
                                      socket
                                      (car (sb-bsd-sockets:host-ent-addresses
                                            (sb-bsd-sockets:get-host-by-name host)))
                                      port)
                                     (let* ((stream (sb-bsd-sockets:socket-make-stream
                                                     socket
                                                     :input t
                                                     :output t
                                                     :element-type 'character
                                                     :external-format :utf-8
                                                     :buffering :none))
                                            (peername (make-py-tuple host port))
                                            (reader (make-py-asyncio-stream-reader-object
                                                     :type *py-asyncio-stream-reader-type*
                                                     :socket socket
                                                     :stream stream))
                                            (writer (make-py-asyncio-stream-writer-object
                                                     :type *py-asyncio-stream-writer-type*
                                                     :socket socket
                                                     :stream stream
                                                     :peername peername)))
                                       (make-py-tuple reader writer)))
                                 (error (condition)
                                   (ignore-errors (sb-bsd-sockets:socket-close socket))
                                   (error condition))))))))))


(defun py-asyncio-socket-address (host)
  (car (sb-bsd-sockets:host-ent-addresses
        (sb-bsd-sockets:get-host-by-name host))))

(defun py-asyncio-address-string (address)
  (if (and (vectorp address) (= (length address) 4))
      (format nil "~D.~D.~D.~D"
              (aref address 0) (aref address 1) (aref address 2) (aref address 3))
      (py-str address)))

(defun py-asyncio-socket-name-tuple (socket)
  (multiple-value-bind (address port) (sb-bsd-sockets:socket-name socket)
    (make-py-tuple (py-asyncio-address-string address) port)))

(defun py-asyncio-make-stream-pair (socket host port)
  (let* ((stream (sb-bsd-sockets:socket-make-stream
                  socket
                  :input t
                  :output t
                  :element-type 'character
                  :external-format :utf-8
                  :buffering :none))
         (peername (make-py-tuple host port))
         (reader (make-py-asyncio-stream-reader-object
                  :type *py-asyncio-stream-reader-type*
                  :socket socket
                  :stream stream))
         (writer (make-py-asyncio-stream-writer-object
                  :type *py-asyncio-stream-writer-type*
                  :socket socket
                  :stream stream
                  :peername peername)))
    (values reader writer)))

(defun py-asyncio-server-handle-client (server client-socket host port)
  (handler-case
      (multiple-value-bind (reader writer)
          (py-asyncio-make-stream-pair client-socket host port)
        (let ((result (py-invoke-callable
                       (py-asyncio-server-object-callback server)
                       reader
                       writer)))
          (when (py-coroutine-object-p result)
            (py-await result))))
    (error ()
      (ignore-errors (sb-bsd-sockets:socket-close client-socket)))))

(defun py-asyncio-server-accept-loop (server host port)
  (loop while (and (not (py-asyncio-server-object-closed server))
                   (py-asyncio-server-object-serving server))
        do (handler-case
               (let ((client-socket (sb-bsd-sockets:socket-accept
                                     (py-asyncio-server-object-socket server))))
                 (py-asyncio-server-handle-client server client-socket host port))
             (error ()
               (when (and (not (py-asyncio-server-object-closed server))
                          (py-asyncio-server-object-serving server))
                 (sleep 0.01))))))

(defun py-asyncio-server-close (server)
  (setf (py-asyncio-server-object-closed server) t)
  (setf (py-asyncio-server-object-serving server) nil)
  (setf (py-object-attr server "closed") *py-true*)
  (ignore-errors (sb-bsd-sockets:socket-close (py-asyncio-server-object-socket server)))
  *py-none*)

(defun py-asyncio-server-wait-closed (server)
  (make-py-coroutine "Server.wait_closed"
                     (lambda ()
                       (let ((thread (py-asyncio-server-object-thread server)))
                         (when thread
                           (ignore-errors (sb-thread:join-thread thread :timeout 0.1))))
                       *py-none*)))

(defun py-asyncio-server-is-serving (server)
  (py-bool (and (py-asyncio-server-object-serving server)
                (not (py-asyncio-server-object-closed server)))))

(defun py-asyncio-server-serve-forever (server)
  (make-py-coroutine "Server.serve_forever"
                     (lambda ()
                       (if (py-asyncio-server-is-serving server)
                           *py-none*
                           (py-raise (make-py-exception *py-runtime-error-type* "server is closed"))))))

(defun py-asyncio-server-aenter (server)
  (make-py-coroutine "Server.__aenter__"
                     (lambda () server)))

(defun py-asyncio-server-aexit (server exc-type exc-value traceback)
  (declare (ignore exc-type exc-value traceback))
  (make-py-coroutine "Server.__aexit__"
                     (lambda ()
                       (py-asyncio-server-close server)
                       (py-await (py-asyncio-server-wait-closed server))
                       *py-false*)))

(defun py-asyncio-start-server (client-connected-cb &rest args)
  (multiple-value-bind (keyword-host keyword-host-supplied-p positional)
      (py-asyncio-keyword-value args :host *py-none*)
    (multiple-value-bind (keyword-port keyword-port-supplied-p ignored-positional)
        (py-asyncio-keyword-value args :port *py-none*)
      (declare (ignore ignored-positional))
      (let ((host (cond
                    (keyword-host-supplied-p keyword-host)
                    (positional (first positional))
                    (t "127.0.0.1")))
            (port (cond
                    (keyword-port-supplied-p keyword-port)
                    ((rest positional) (second positional))
                    (t *py-none*))))
        (make-py-coroutine "start_server"
                           (lambda ()
                             (unless (and (stringp host) (integerp port))
                               (py-raise (make-py-exception *py-type-error-type* "start_server requires host and port")))
                             (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
                               (handler-case
                                   (progn
                                     (sb-bsd-sockets:socket-bind socket (py-asyncio-socket-address host) port)
                                     (sb-bsd-sockets:socket-listen socket 5)
                                     (let ((server (make-py-asyncio-server-object
                                                    :type *py-asyncio-server-type*
                                                    :socket socket
                                                    :callback client-connected-cb
                                                    :sockets (make-py-tuple
                                                              (py-asyncio-socket-name-tuple socket)))))
                                       (setf (py-object-attr server "sockets")
                                             (py-asyncio-server-object-sockets server))
                                       (setf (py-object-attr server "closed") *py-false*)
                                       (setf (py-asyncio-server-object-thread server)
                                             (sb-thread:make-thread
                                              (lambda ()
                                                (py-asyncio-server-accept-loop server host port))
                                              :name "clamp-asyncio-server"))
                                       server))
                                 (error (condition)
                                   (ignore-errors (sb-bsd-sockets:socket-close socket))
                                   (error condition))))))))))

(setf (py-type-attr *py-asyncio-server-type* "close") #'py-asyncio-server-close)
(setf (py-type-attr *py-asyncio-server-type* "wait_closed") #'py-asyncio-server-wait-closed)
(setf (py-type-attr *py-asyncio-server-type* "is_serving") #'py-asyncio-server-is-serving)
(setf (py-type-attr *py-asyncio-server-type* "serve_forever") #'py-asyncio-server-serve-forever)
(setf (py-type-attr *py-asyncio-server-type* "__aenter__") #'py-asyncio-server-aenter)
(setf (py-type-attr *py-asyncio-server-type* "__aexit__") #'py-asyncio-server-aexit)

(setf (py-type-attr *py-asyncio-stream-reader-type* "read") #'py-asyncio-stream-reader-read)
(setf (py-type-attr *py-asyncio-stream-reader-type* "readline") #'py-asyncio-stream-reader-readline)
(setf (py-type-attr *py-asyncio-stream-reader-type* "readuntil") #'py-asyncio-stream-reader-readuntil)
(setf (py-type-attr *py-asyncio-stream-reader-type* "readexactly") #'py-asyncio-stream-reader-readexactly)
(setf (py-type-attr *py-asyncio-stream-reader-type* "at_eof") #'py-asyncio-stream-reader-at-eof)
(setf (py-type-attr *py-asyncio-stream-reader-type* "__aiter__") #'py-asyncio-stream-reader-aiter)
(setf (py-type-attr *py-asyncio-stream-reader-type* "__anext__") #'py-asyncio-stream-reader-anext)
(setf (py-type-attr *py-asyncio-stream-writer-type* "write") #'py-asyncio-stream-writer-write)
(setf (py-type-attr *py-asyncio-stream-writer-type* "writelines") #'py-asyncio-stream-writer-writelines)
(setf (py-type-attr *py-asyncio-stream-writer-type* "can_write_eof") #'py-asyncio-stream-writer-can-write-eof)
(setf (py-type-attr *py-asyncio-stream-writer-type* "write_eof") #'py-asyncio-stream-writer-write-eof)
(setf (py-type-attr *py-asyncio-stream-writer-type* "drain") #'py-asyncio-stream-writer-drain)
(setf (py-type-attr *py-asyncio-stream-writer-type* "close") #'py-asyncio-stream-writer-close)
(setf (py-type-attr *py-asyncio-stream-writer-type* "wait_closed") #'py-asyncio-stream-writer-wait-closed)
(setf (py-type-attr *py-asyncio-stream-writer-type* "is_closing") #'py-asyncio-stream-writer-is-closing)
(setf (py-type-attr *py-asyncio-stream-writer-type* "get_extra_info") #'py-asyncio-stream-writer-get-extra-info)

(defun py-stop-async-iteration-p (value)
  (cond
    ((typep value 'py-exception)
     (py-stop-async-iteration-p (py-exception-value value)))
    ((py-exception-object-p value)
     (eq (py-object-type value) *py-stop-async-iteration-type*))
    (t nil)))

(defun py-aiter (obj)
  (let ((iterator (py-call-attr obj "__aiter__")))
    iterator))

(defun py-anext (async-iterator)
  (py-call-attr async-iterator "__anext__"))

(defun py-anext-item (async-iterator)
  (handler-case
      (values (py-await (py-anext async-iterator)) t)
    (py-exception (condition)
      (if (py-stop-async-iteration-p condition)
          (values nil nil)
          (error condition)))))

(defun py-asyncio-lock ()
  (make-py-asyncio-lock-object :type *py-asyncio-lock-type*
                               :loop (or *py-asyncio-running-loop*
                                         (py-asyncio-new-event-loop))))

(defun py-asyncio-lock-acquire (lock)
  (make-py-coroutine "Lock.acquire"
                     (lambda ()
                       (setf (py-asyncio-lock-object-locked lock) t)
                       *py-true*)))

(defun py-asyncio-lock-release (lock)
  (unless (py-asyncio-lock-object-locked lock)
    (py-raise (make-py-exception *py-runtime-error-type* "Lock is not acquired.")))
  (setf (py-asyncio-lock-object-locked lock) nil)
  *py-none*)

(defun py-asyncio-lock-aenter (lock)
  (make-py-coroutine "Lock.__aenter__"
                     (lambda ()
                       (py-await (py-asyncio-lock-acquire lock))
                       *py-none*)))

(defun py-asyncio-lock-aexit (lock exc-type exc-value traceback)
  (declare (ignore exc-type exc-value traceback))
  (make-py-coroutine "Lock.__aexit__"
                     (lambda ()
                       (py-asyncio-lock-release lock)
                       *py-false*)))

(defun py-asyncio-as-completed (awaitables)
  (let ((items (make-py-list)))
    (let ((iterator (py-iter awaitables)))
      (loop
        (multiple-value-bind (item found) (py-next-item iterator)
          (unless found (return))
          (py-append items
                     (if (py-coroutine-object-p item)
                         (py-asyncio-create-task (or *py-asyncio-running-loop*
                                                     (py-asyncio-new-event-loop))
                                                 item)
                         item)))))
    (make-py-asyncio-as-completed-object :type *py-asyncio-as-completed-type*
                                         :items items
                                         :index 0)))

(defun py-asyncio-as-completed-next-item (iterator stop-exception)
  (let* ((items (py-asyncio-as-completed-object-items iterator))
         (index (py-asyncio-as-completed-object-index iterator))
         (size (or (py-object-size items) 0)))
    (if (< index size)
        (prog1
            (aref (py-object-value items) index)
          (setf (py-asyncio-as-completed-object-index iterator) (1+ index)))
        (py-raise stop-exception))))

(defun py-asyncio-as-completed-anext (iterator)
  (make-py-coroutine "as_completed.__anext__"
                     (lambda ()
                       (py-asyncio-as-completed-next-item
                        iterator
                        (make-py-exception *py-stop-async-iteration-type*)))))

(defun py-asyncio-as-completed-next (iterator)
  (py-asyncio-as-completed-next-item iterator *py-stop-iteration*))

(setf (py-type-attr *py-coroutine-type* "__await__")
      (lambda (coroutine) coroutine))

(setf (py-type-attr *py-async-generator-type* "__aiter__")
      (lambda (generator) generator))
(setf (py-type-attr *py-async-generator-type* "__anext__") #'py-async-generator-anext)
(setf (py-type-attr *py-async-generator-type* "asend") #'py-async-generator-asend)
(setf (py-type-attr *py-async-generator-type* "aclose") #'py-async-generator-aclose)
(setf (py-type-attr *py-async-generator-type* "athrow") #'py-async-generator-athrow)

(defun py-contextlib-async-generator-context-manager (generator)
  (let ((manager (make-py-contextlib-async-generator-context-manager-object
                  :type *py-contextlib-async-generator-context-manager-type*
                  :generator generator)))
    manager))

(defun py-contextlib-async-generator-context-manager-aenter (manager)
  (make-py-coroutine "_AsyncGeneratorContextManager.__aenter__"
                     (lambda ()
                       (py-await
                        (py-async-generator-anext
                         (py-contextlib-async-generator-context-manager-object-generator manager))))))

(defun py-contextlib-async-generator-context-manager-aexit (manager exc-type exc-value traceback)
  (declare (ignore exc-type exc-value traceback))
  (make-py-coroutine "_AsyncGeneratorContextManager.__aexit__"
                     (lambda ()
                       (py-await
                        (py-async-generator-aclose
                         (py-contextlib-async-generator-context-manager-object-generator manager)))
                       *py-false*)))

(defun py-contextlib-asynccontextmanager (function)
  (make-py-callable
   :name "asynccontextmanager"
   :fn (lambda (&rest args)
         (let ((generator (apply #'py-invoke-callable function args)))
           (unless (py-async-generator-object-p generator)
             (py-raise (make-py-exception *py-type-error-type*
                                          "asynccontextmanager function must return an async generator")))
           (py-contextlib-async-generator-context-manager generator)))))

(setf (py-type-attr *py-contextlib-async-generator-context-manager-type* "__aenter__")
      #'py-contextlib-async-generator-context-manager-aenter)
(setf (py-type-attr *py-contextlib-async-generator-context-manager-type* "__aexit__")
      #'py-contextlib-async-generator-context-manager-aexit)

(defun py-contextlib-aclosing (thing)
  (make-py-contextlib-aclosing-object :type *py-contextlib-aclosing-type* :thing thing))

(defun py-contextlib-aclosing-aenter (manager)
  (make-py-coroutine "aclosing.__aenter__"
                     (lambda ()
                       (py-contextlib-aclosing-object-thing manager))))

(defun py-contextlib-aclosing-aexit (manager exc-type exc-value traceback)
  (declare (ignore exc-type exc-value traceback))
  (make-py-coroutine "aclosing.__aexit__"
                     (lambda ()
                       (py-await
                        (py-call-attr (py-contextlib-aclosing-object-thing manager) "aclose"))
                       *py-false*)))

(defun py-contextlib-nullcontext (&optional (enter-result *py-none*))
  (make-py-contextlib-nullcontext-object :type *py-contextlib-nullcontext-type*
                                         :enter-result enter-result))

(defun py-contextlib-nullcontext-enter (manager)
  (py-contextlib-nullcontext-object-enter-result manager))

(defun py-contextlib-nullcontext-exit (manager exc-type exc-value traceback)
  (declare (ignore manager exc-type exc-value traceback))
  *py-false*)

(defun py-contextlib-nullcontext-aenter (manager)
  (make-py-coroutine "nullcontext.__aenter__"
                     (lambda ()
                       (py-contextlib-nullcontext-object-enter-result manager))))

(defun py-contextlib-nullcontext-aexit (manager exc-type exc-value traceback)
  (declare (ignore manager exc-type exc-value traceback))
  (make-py-coroutine "nullcontext.__aexit__"
                     (lambda () *py-false*)))

(defun py-contextlib-async-exit-stack ()
  (make-py-contextlib-async-exit-stack-object
   :type *py-contextlib-async-exit-stack-type*))

(defun py-contextlib-async-exit-stack-push-async-exit (stack exit)
  (push exit (py-contextlib-async-exit-stack-object-exit-callbacks stack))
  exit)

(defun py-contextlib-async-exit-stack-enter-async-context (stack manager)
  (make-py-coroutine "AsyncExitStack.enter_async_context"
                     (lambda ()
                       (let ((value (py-await (py-call-attr manager "__aenter__")))
                             (exit-callback
                              (make-py-callable
                               :name "AsyncExitStack.__aexit__ callback"
                               :fn (lambda (exc-type exc-value traceback)
                                     (py-call-attr manager "__aexit__" exc-type exc-value traceback)))))
                         (py-contextlib-async-exit-stack-push-async-exit stack exit-callback)
                         value))))

(defun py-contextlib-async-exit-stack-aenter (stack)
  (make-py-coroutine "AsyncExitStack.__aenter__"
                     (lambda () stack)))

(defun py-contextlib-async-exit-stack-aexit (stack exc-type exc-value traceback)
  (make-py-coroutine "AsyncExitStack.__aexit__"
                     (lambda ()
                       (let ((suppressed nil))
                         (loop while (py-contextlib-async-exit-stack-object-exit-callbacks stack)
                               for exit = (pop (py-contextlib-async-exit-stack-object-exit-callbacks stack))
                               do (let ((result (py-await (py-invoke-callable exit exc-type exc-value traceback))))
                                    (when (py-truthy-p result)
                                      (setf suppressed t))))
                         (py-bool suppressed)))))

(defun py-contextlib-async-exit-stack-aclose (stack)
  (py-contextlib-async-exit-stack-aexit stack *py-none* *py-none* *py-none*))

(setf (py-type-attr *py-contextlib-aclosing-type* "__aenter__") #'py-contextlib-aclosing-aenter)
(setf (py-type-attr *py-contextlib-aclosing-type* "__aexit__") #'py-contextlib-aclosing-aexit)
(setf (py-type-attr *py-contextlib-nullcontext-type* "__enter__") #'py-contextlib-nullcontext-enter)
(setf (py-type-attr *py-contextlib-nullcontext-type* "__exit__") #'py-contextlib-nullcontext-exit)
(setf (py-type-attr *py-contextlib-nullcontext-type* "__aenter__") #'py-contextlib-nullcontext-aenter)
(setf (py-type-attr *py-contextlib-nullcontext-type* "__aexit__") #'py-contextlib-nullcontext-aexit)
(setf (py-type-attr *py-contextlib-async-exit-stack-type* "__aenter__") #'py-contextlib-async-exit-stack-aenter)
(setf (py-type-attr *py-contextlib-async-exit-stack-type* "__aexit__") #'py-contextlib-async-exit-stack-aexit)
(setf (py-type-attr *py-contextlib-async-exit-stack-type* "enter_async_context") #'py-contextlib-async-exit-stack-enter-async-context)
(setf (py-type-attr *py-contextlib-async-exit-stack-type* "push_async_exit") #'py-contextlib-async-exit-stack-push-async-exit)
(setf (py-type-attr *py-contextlib-async-exit-stack-type* "aclose") #'py-contextlib-async-exit-stack-aclose)

(defun make-clamp-contextlib-module ()
  (let ((module (make-clamp-module "contextlib")))
    (setf (py-object-attr module "__doc__") "Clamp built-in contextlib compatibility module")
    (setf (py-object-attr module "asynccontextmanager") #'py-contextlib-asynccontextmanager)
    (setf (py-object-attr module "aclosing") #'py-contextlib-aclosing)
    (setf (py-object-attr module "nullcontext") #'py-contextlib-nullcontext)
    (setf (py-object-attr module "AsyncExitStack") #'py-contextlib-async-exit-stack)
    (setf (py-object-attr module "_AsyncGeneratorContextManager")
          *py-contextlib-async-generator-context-manager-type*)
    module))

(py-register-builtin-module "contextlib" #'make-clamp-contextlib-module)

(setf (py-type-attr *py-asyncio-future-type* "result") #'py-future-result)
(setf (py-type-attr *py-asyncio-future-type* "exception") #'py-future-exception)
(setf (py-type-attr *py-asyncio-future-type* "set_result") #'py-future-set-result)
(setf (py-type-attr *py-asyncio-future-type* "set_exception") #'py-future-set-exception)
(setf (py-type-attr *py-asyncio-future-type* "done") #'py-future-done)
(setf (py-type-attr *py-asyncio-future-type* "cancel") #'py-future-cancel)
(setf (py-type-attr *py-asyncio-future-type* "cancelled") #'py-future-cancelled)
(setf (py-type-attr *py-asyncio-future-type* "add_done_callback") #'py-future-add-done-callback)
(setf (py-type-attr *py-asyncio-future-type* "remove_done_callback") #'py-future-remove-done-callback)
(setf (py-type-attr *py-asyncio-future-type* "get_loop") #'py-future-get-loop)
(setf (py-type-attr *py-asyncio-future-type* "__await__")
      (lambda (future) future))

(setf (py-type-attr *py-asyncio-task-type* "result") #'py-future-result)
(setf (py-type-attr *py-asyncio-task-type* "exception") #'py-future-exception)
(setf (py-type-attr *py-asyncio-task-type* "done") #'py-future-done)
(setf (py-type-attr *py-asyncio-task-type* "cancel") #'py-future-cancel)
(setf (py-type-attr *py-asyncio-task-type* "cancelled") #'py-future-cancelled)
(setf (py-type-attr *py-asyncio-task-type* "add_done_callback") #'py-future-add-done-callback)
(setf (py-type-attr *py-asyncio-task-type* "remove_done_callback") #'py-future-remove-done-callback)
(setf (py-type-attr *py-asyncio-task-type* "get_loop") #'py-future-get-loop)
(setf (py-type-attr *py-asyncio-task-type* "get_name") #'py-task-get-name)
(setf (py-type-attr *py-asyncio-task-type* "set_name") #'py-task-set-name)
(setf (py-type-attr *py-asyncio-task-type* "get_coro")
      (lambda (task) (py-asyncio-task-object-coroutine task)))
(setf (py-type-attr *py-asyncio-task-type* "__await__")
      (lambda (task) task))

(setf (py-type-attr *py-asyncio-event-loop-policy-type* "get_event_loop") #'py-asyncio-policy-get-event-loop)
(setf (py-type-attr *py-asyncio-event-loop-policy-type* "set_event_loop") #'py-asyncio-policy-set-event-loop)
(setf (py-type-attr *py-asyncio-event-loop-policy-type* "new_event_loop") #'py-asyncio-policy-new-event-loop)

(setf (py-type-attr *py-asyncio-event-loop-type* "time") #'py-asyncio-loop-time)
(setf (py-type-attr *py-asyncio-event-loop-type* "create_future") #'py-asyncio-create-future)
(setf (py-type-attr *py-asyncio-event-loop-type* "create_task") #'py-asyncio-create-task)
(setf (py-type-attr *py-asyncio-event-loop-type* "run_until_complete") #'py-asyncio-run-until-complete)
(setf (py-type-attr *py-asyncio-event-loop-type* "run_forever") #'py-asyncio-run-forever)
(setf (py-type-attr *py-asyncio-event-loop-type* "stop") #'py-asyncio-loop-stop)
(setf (py-type-attr *py-asyncio-event-loop-type* "call_soon") #'py-asyncio-loop-call-soon)
(setf (py-type-attr *py-asyncio-event-loop-type* "call_soon_threadsafe") #'py-asyncio-loop-call-soon-threadsafe)
(setf (py-type-attr *py-asyncio-event-loop-type* "call_later") #'py-asyncio-loop-call-later)
(setf (py-type-attr *py-asyncio-event-loop-type* "call_at") #'py-asyncio-loop-call-at)
(setf (py-type-attr *py-asyncio-event-loop-type* "get_debug") #'py-asyncio-loop-get-debug)
(setf (py-type-attr *py-asyncio-event-loop-type* "set_debug") #'py-asyncio-loop-set-debug)
(setf (py-type-attr *py-asyncio-event-loop-type* "shutdown_asyncgens") #'py-asyncio-loop-shutdown-asyncgens)
(setf (py-type-attr *py-asyncio-event-loop-type* "shutdown_default_executor") #'py-asyncio-loop-shutdown-default-executor)
(setf (py-type-attr *py-asyncio-event-loop-type* "run_in_executor") #'py-asyncio-loop-run-in-executor)
(setf (py-type-attr *py-asyncio-event-loop-type* "is_running")
      (lambda (loop) (py-bool (py-asyncio-event-loop-object-running loop))))
(setf (py-type-attr *py-asyncio-event-loop-type* "is_closed")
      (lambda (loop) (py-bool (py-asyncio-event-loop-object-closed loop))))
(setf (py-type-attr *py-asyncio-event-loop-type* "close")
      (lambda (loop) (setf (py-asyncio-event-loop-object-closed loop) t) *py-none*))

(setf (py-type-attr *py-asyncio-handle-type* "cancel") #'py-asyncio-handle-cancel)
(setf (py-type-attr *py-asyncio-handle-type* "cancelled") #'py-asyncio-handle-cancelled)
(setf (py-type-attr *py-asyncio-timer-handle-type* "cancel") #'py-asyncio-handle-cancel)
(setf (py-type-attr *py-asyncio-timer-handle-type* "cancelled") #'py-asyncio-handle-cancelled)
(setf (py-type-attr *py-asyncio-timer-handle-type* "when") #'py-asyncio-timer-handle-when)


(setf (py-type-attr *py-asyncio-lock-type* "acquire") #'py-asyncio-lock-acquire)
(setf (py-type-attr *py-asyncio-lock-type* "release") #'py-asyncio-lock-release)
(setf (py-type-attr *py-asyncio-lock-type* "locked")
      (lambda (lock) (py-bool (py-asyncio-lock-object-locked lock))))
(setf (py-type-attr *py-asyncio-lock-type* "__aenter__") #'py-asyncio-lock-aenter)
(setf (py-type-attr *py-asyncio-lock-type* "__aexit__") #'py-asyncio-lock-aexit)

(defun py-asyncio-condition (&rest args)
  (multiple-value-bind (keyword-lock keyword-lock-supplied-p positional)
      (py-asyncio-keyword-value args :lock *py-none*)
    (let ((lock (cond
                  (keyword-lock-supplied-p keyword-lock)
                  (positional (first positional))
                  (t *py-none*))))
      (when (eq lock *py-none*)
        (setf lock (py-asyncio-lock)))
      (make-py-asyncio-condition-object
       :type *py-asyncio-condition-type*
       :loop (or *py-asyncio-running-loop*
                 (py-asyncio-new-event-loop))
       :lock lock))))

(defun py-asyncio-condition-lock (condition)
  (py-asyncio-condition-object-lock condition))

(defun py-asyncio-condition-acquire (condition)
  (py-asyncio-lock-acquire (py-asyncio-condition-lock condition)))

(defun py-asyncio-condition-release (condition)
  (py-asyncio-lock-release (py-asyncio-condition-lock condition)))

(defun py-asyncio-condition-locked (condition)
  (py-bool (py-asyncio-lock-object-locked (py-asyncio-condition-lock condition))))

(defun py-asyncio-condition-wait (condition)
  (make-py-coroutine "Condition.wait"
                     (lambda ()
                       (unless (py-asyncio-lock-object-locked (py-asyncio-condition-lock condition))
                         (py-raise (make-py-exception *py-runtime-error-type*
                                                      "cannot wait on un-acquired lock")))
                       (py-asyncio-lock-release (py-asyncio-condition-lock condition))
                       (py-await (py-asyncio-lock-acquire (py-asyncio-condition-lock condition)))
                       *py-true*)))

(defun py-asyncio-condition-wait-for (condition predicate)
  (make-py-coroutine "Condition.wait_for"
                     (lambda ()
                       (let ((result (py-invoke-callable predicate)))
                         (unless (py-truthy-p result)
                           (py-await (py-asyncio-condition-wait condition))
                           (setf result (py-invoke-callable predicate)))
                         result))))

(defun py-asyncio-condition-notify (condition &optional (n 1))
  (declare (ignore n))
  (unless (py-asyncio-lock-object-locked (py-asyncio-condition-lock condition))
    (py-raise (make-py-exception *py-runtime-error-type*
                                 "cannot notify on un-acquired lock")))
  *py-none*)

(defun py-asyncio-condition-notify-all (condition)
  (py-asyncio-condition-notify condition nil))

(defun py-asyncio-condition-aenter (condition)
  (make-py-coroutine "Condition.__aenter__"
                     (lambda ()
                       (py-await (py-asyncio-condition-acquire condition))
                       *py-none*)))

(defun py-asyncio-condition-aexit (condition exc-type exc-value traceback)
  (declare (ignore exc-type exc-value traceback))
  (make-py-coroutine "Condition.__aexit__"
                     (lambda ()
                       (py-asyncio-condition-release condition)
                       *py-false*)))

(setf (py-type-attr *py-asyncio-condition-type* "acquire") #'py-asyncio-condition-acquire)
(setf (py-type-attr *py-asyncio-condition-type* "release") #'py-asyncio-condition-release)
(setf (py-type-attr *py-asyncio-condition-type* "locked") #'py-asyncio-condition-locked)
(setf (py-type-attr *py-asyncio-condition-type* "wait") #'py-asyncio-condition-wait)
(setf (py-type-attr *py-asyncio-condition-type* "wait_for") #'py-asyncio-condition-wait-for)
(setf (py-type-attr *py-asyncio-condition-type* "notify") #'py-asyncio-condition-notify)
(setf (py-type-attr *py-asyncio-condition-type* "notify_all") #'py-asyncio-condition-notify-all)
(setf (py-type-attr *py-asyncio-condition-type* "__aenter__") #'py-asyncio-condition-aenter)
(setf (py-type-attr *py-asyncio-condition-type* "__aexit__") #'py-asyncio-condition-aexit)

(defun py-asyncio-semaphore (&rest args)
  (multiple-value-bind (keyword-value keyword-value-supplied-p positional)
      (py-asyncio-keyword-value args :value 1)
    (let ((value (cond
                   (keyword-value-supplied-p keyword-value)
                   (positional (first positional))
                   (t 1))))
      (when (and (numberp value) (< value 0))
        (py-raise (make-py-exception *py-runtime-error-type* "Semaphore initial value must be >= 0")))
      (make-py-asyncio-semaphore-object :type *py-asyncio-semaphore-type*
                                        :loop (or *py-asyncio-running-loop*
                                                  (py-asyncio-new-event-loop))
                                        :counter value))))

(defun py-asyncio-semaphore-locked (semaphore)
  (py-bool (<= (py-asyncio-semaphore-object-counter semaphore) 0)))

(defun py-asyncio-semaphore-acquire (semaphore)
  (make-py-coroutine "Semaphore.acquire"
                     (lambda ()
                       (if (> (py-asyncio-semaphore-object-counter semaphore) 0)
                           (progn
                             (decf (py-asyncio-semaphore-object-counter semaphore))
                             *py-true*)
                           *py-false*))))

(defun py-asyncio-semaphore-release (semaphore)
  (incf (py-asyncio-semaphore-object-counter semaphore))
  *py-none*)

(defun py-asyncio-bounded-semaphore (&rest args)
  (multiple-value-bind (keyword-value keyword-value-supplied-p positional)
      (py-asyncio-keyword-value args :value 1)
    (let ((value (cond
                   (keyword-value-supplied-p keyword-value)
                   (positional (first positional))
                   (t 1))))
      (when (and (numberp value) (< value 0))
        (py-raise (make-py-exception *py-value-error-type*
                                     "Semaphore initial value must be >= 0")))
      (make-py-asyncio-bounded-semaphore-object
       :type *py-asyncio-bounded-semaphore-type*
       :loop (or *py-asyncio-running-loop*
                 (py-asyncio-new-event-loop))
       :counter value
       :bound value))))

(defun py-asyncio-bounded-semaphore-release (semaphore)
  (when (>= (py-asyncio-semaphore-object-counter semaphore)
            (py-asyncio-bounded-semaphore-object-bound semaphore))
    (py-raise (make-py-exception *py-value-error-type*
                                 "BoundedSemaphore released too many times")))
  (py-asyncio-semaphore-release semaphore))

(defun py-asyncio-semaphore-aenter (semaphore)
  (make-py-coroutine "Semaphore.__aenter__"
                     (lambda ()
                       (py-await (py-asyncio-semaphore-acquire semaphore))
                       *py-none*)))

(defun py-asyncio-semaphore-aexit (semaphore exc-type exc-value traceback)
  (declare (ignore exc-type exc-value traceback))
  (make-py-coroutine "Semaphore.__aexit__"
                     (lambda ()
                       (py-asyncio-semaphore-release semaphore)
                       *py-false*)))

(defun py-asyncio-bounded-semaphore-aexit (semaphore exc-type exc-value traceback)
  (declare (ignore exc-type exc-value traceback))
  (make-py-coroutine "BoundedSemaphore.__aexit__"
                     (lambda ()
                       (py-asyncio-bounded-semaphore-release semaphore)
                       *py-false*)))

(setf (py-type-attr *py-asyncio-semaphore-type* "acquire") #'py-asyncio-semaphore-acquire)
(setf (py-type-attr *py-asyncio-semaphore-type* "release") #'py-asyncio-semaphore-release)
(setf (py-type-attr *py-asyncio-semaphore-type* "locked") #'py-asyncio-semaphore-locked)
(setf (py-type-attr *py-asyncio-semaphore-type* "__aenter__") #'py-asyncio-semaphore-aenter)
(setf (py-type-attr *py-asyncio-semaphore-type* "__aexit__") #'py-asyncio-semaphore-aexit)
(setf (py-type-attr *py-asyncio-bounded-semaphore-type* "acquire") #'py-asyncio-semaphore-acquire)
(setf (py-type-attr *py-asyncio-bounded-semaphore-type* "release") #'py-asyncio-bounded-semaphore-release)
(setf (py-type-attr *py-asyncio-bounded-semaphore-type* "locked") #'py-asyncio-semaphore-locked)
(setf (py-type-attr *py-asyncio-bounded-semaphore-type* "__aenter__") #'py-asyncio-semaphore-aenter)
(setf (py-type-attr *py-asyncio-bounded-semaphore-type* "__aexit__") #'py-asyncio-bounded-semaphore-aexit)

(defun py-asyncio-barrier-refresh-attrs (barrier)
  (setf (py-object-attr barrier "parties") (py-asyncio-barrier-object-parties barrier))
  (setf (py-object-attr barrier "n_waiting") (py-asyncio-barrier-object-waiting barrier))
  (setf (py-object-attr barrier "broken") (py-bool (py-asyncio-barrier-object-broken barrier)))
  barrier)

(defun py-asyncio-barrier (&rest args)
  (multiple-value-bind (keyword-parties keyword-parties-supplied-p positional)
      (py-asyncio-keyword-value args :parties *py-none*)
    (let ((parties (cond
                     (keyword-parties-supplied-p keyword-parties)
                     (positional (first positional))
                     (t *py-none*))))
      (when (or (eq parties *py-none*) (and (numberp parties) (< parties 1)))
        (py-raise (make-py-exception *py-value-error-type* "parties must be >= 1")))
      (py-asyncio-barrier-refresh-attrs
       (make-py-asyncio-barrier-object
        :type *py-asyncio-barrier-type*
        :loop (or *py-asyncio-running-loop*
                  (py-asyncio-new-event-loop))
        :parties parties)))))

(defun py-asyncio-barrier-wait (barrier)
  (make-py-coroutine "Barrier.wait"
                     (lambda ()
                       (when (py-asyncio-barrier-object-broken barrier)
                         (py-raise (make-py-exception *py-asyncio-broken-barrier-error-type*
                                                      "Barrier is broken")))
                       (let ((index (py-asyncio-barrier-object-waiting barrier)))
                         (incf (py-asyncio-barrier-object-waiting barrier))
                         (when (>= (py-asyncio-barrier-object-waiting barrier)
                                   (py-asyncio-barrier-object-parties barrier))
                           (setf (py-asyncio-barrier-object-waiting barrier) 0))
                         (py-asyncio-barrier-refresh-attrs barrier)
                         index))))

(defun py-asyncio-barrier-reset (barrier)
  (make-py-coroutine "Barrier.reset"
                     (lambda ()
                       (setf (py-asyncio-barrier-object-waiting barrier) 0)
                       (setf (py-asyncio-barrier-object-broken barrier) nil)
                       (py-asyncio-barrier-refresh-attrs barrier)
                       *py-none*)))

(defun py-asyncio-barrier-abort (barrier)
  (make-py-coroutine "Barrier.abort"
                     (lambda ()
                       (setf (py-asyncio-barrier-object-broken barrier) t)
                       (py-asyncio-barrier-refresh-attrs barrier)
                       *py-none*)))

(defun py-asyncio-barrier-aenter (barrier)
  (make-py-coroutine "Barrier.__aenter__"
                     (lambda ()
                       (py-await (py-asyncio-barrier-wait barrier))
                       *py-none*)))

(defun py-asyncio-barrier-aexit (barrier exc-type exc-value traceback)
  (declare (ignore barrier exc-type exc-value traceback))
  (make-py-coroutine "Barrier.__aexit__"
                     (lambda () *py-false*)))

(setf (py-type-attr *py-asyncio-barrier-type* "wait") #'py-asyncio-barrier-wait)
(setf (py-type-attr *py-asyncio-barrier-type* "reset") #'py-asyncio-barrier-reset)
(setf (py-type-attr *py-asyncio-barrier-type* "abort") #'py-asyncio-barrier-abort)
(setf (py-type-attr *py-asyncio-barrier-type* "__aenter__") #'py-asyncio-barrier-aenter)
(setf (py-type-attr *py-asyncio-barrier-type* "__aexit__") #'py-asyncio-barrier-aexit)

(defun py-asyncio-event ()
  (make-py-asyncio-event-object :type *py-asyncio-event-type*
                                :loop (or *py-asyncio-running-loop*
                                          (py-asyncio-new-event-loop))))

(defun py-asyncio-event-is-set (event)
  (py-bool (py-asyncio-event-object-flag event)))

(defun py-asyncio-event-set (event)
  (setf (py-asyncio-event-object-flag event) t)
  *py-none*)

(defun py-asyncio-event-clear (event)
  (setf (py-asyncio-event-object-flag event) nil)
  *py-none*)

(defun py-asyncio-event-wait (event)
  (make-py-coroutine "Event.wait"
                     (lambda ()
                       (py-bool (py-asyncio-event-object-flag event)))))

(defun py-asyncio-make-queue (type args)
  (multiple-value-bind (keyword-maxsize keyword-maxsize-supplied-p positional)
      (py-asyncio-keyword-value args :maxsize 0)
    (let ((maxsize (cond
                     (keyword-maxsize-supplied-p keyword-maxsize)
                     (positional (first positional))
                     (t 0))))
      (make-py-asyncio-queue-object :type type
                                    :loop (or *py-asyncio-running-loop*
                                              (py-asyncio-new-event-loop))
                                    :maxsize maxsize))))

(defun py-asyncio-queue (&rest args)
  (py-asyncio-make-queue *py-asyncio-queue-type* args))

(defun py-asyncio-priority-queue (&rest args)
  (py-asyncio-make-queue *py-asyncio-priority-queue-type* args))

(defun py-asyncio-lifo-queue (&rest args)
  (py-asyncio-make-queue *py-asyncio-lifo-queue-type* args))

(defun py-asyncio-queue-size (queue)
  (length (py-asyncio-queue-object-items queue)))

(defun py-asyncio-queue-full-p (queue)
  (let ((maxsize (py-asyncio-queue-object-maxsize queue)))
    (and (numberp maxsize) (> maxsize 0)
         (>= (py-asyncio-queue-size queue) maxsize))))

(defun py-asyncio-queue-empty (queue)
  (py-bool (= (py-asyncio-queue-size queue) 0)))

(defun py-asyncio-queue-full (queue)
  (py-bool (py-asyncio-queue-full-p queue)))

(defun py-asyncio-queue-qsize (queue)
  (py-asyncio-queue-size queue))

(defun py-asyncio-queue-put-nowait (queue item)
  (when (py-asyncio-queue-full-p queue)
    (py-raise (make-py-exception *py-asyncio-queue-full-type* "Queue full")))
  (setf (py-asyncio-queue-object-items queue)
        (append (py-asyncio-queue-object-items queue) (list item)))
  (incf (py-asyncio-queue-object-unfinished-tasks queue))
  *py-none*)

(defun py-asyncio-priority-queue-put-nowait (queue item)
  (py-asyncio-queue-put-nowait queue item)
  (setf (py-asyncio-queue-object-items queue)
        (stable-sort (copy-list (py-asyncio-queue-object-items queue))
                     (lambda (left right)
                       (py-truthy-p (py-lt left right)))))
  *py-none*)

(defun py-asyncio-queue-put (queue item)
  (make-py-coroutine "Queue.put"
                     (lambda ()
                       (py-asyncio-queue-put-nowait queue item))))

(defun py-asyncio-priority-queue-put (queue item)
  (make-py-coroutine "PriorityQueue.put"
                     (lambda ()
                       (py-asyncio-priority-queue-put-nowait queue item))))

(defun py-asyncio-queue-get-nowait (queue)
  (let ((items (py-asyncio-queue-object-items queue)))
    (unless items
      (py-raise (make-py-exception *py-asyncio-queue-empty-type* "Queue empty")))
    (let ((item (first items)))
      (setf (py-asyncio-queue-object-items queue) (rest items))
      item)))

(defun py-asyncio-lifo-queue-get-nowait (queue)
  (let ((items (py-asyncio-queue-object-items queue)))
    (unless items
      (py-raise (make-py-exception *py-asyncio-queue-empty-type* "Queue empty")))
    (let ((item (car (last items))))
      (setf (py-asyncio-queue-object-items queue) (butlast items))
      item)))

(defun py-asyncio-queue-get (queue)
  (make-py-coroutine "Queue.get"
                     (lambda ()
                       (py-asyncio-queue-get-nowait queue))))

(defun py-asyncio-lifo-queue-get (queue)
  (make-py-coroutine "LifoQueue.get"
                     (lambda ()
                       (py-asyncio-lifo-queue-get-nowait queue))))

(defun py-asyncio-queue-task-done (queue)
  (when (<= (py-asyncio-queue-object-unfinished-tasks queue) 0)
    (py-raise (make-py-exception *py-runtime-error-type* "task_done() called too many times")))
  (decf (py-asyncio-queue-object-unfinished-tasks queue))
  *py-none*)

(defun py-asyncio-queue-join (queue)
  (make-py-coroutine "Queue.join"
                     (lambda ()
                       (if (= (py-asyncio-queue-object-unfinished-tasks queue) 0)
                           *py-none*
                           (py-raise (make-py-exception *py-runtime-error-type*
                                                        "Queue join would block"))))))

(setf (py-type-attr *py-asyncio-event-type* "is_set") #'py-asyncio-event-is-set)
(setf (py-type-attr *py-asyncio-event-type* "set") #'py-asyncio-event-set)
(setf (py-type-attr *py-asyncio-event-type* "clear") #'py-asyncio-event-clear)
(setf (py-type-attr *py-asyncio-event-type* "wait") #'py-asyncio-event-wait)

(setf (py-type-attr *py-asyncio-queue-type* "qsize") #'py-asyncio-queue-qsize)
(setf (py-type-attr *py-asyncio-queue-type* "empty") #'py-asyncio-queue-empty)
(setf (py-type-attr *py-asyncio-queue-type* "full") #'py-asyncio-queue-full)
(setf (py-type-attr *py-asyncio-queue-type* "put") #'py-asyncio-queue-put)
(setf (py-type-attr *py-asyncio-queue-type* "put_nowait") #'py-asyncio-queue-put-nowait)
(setf (py-type-attr *py-asyncio-queue-type* "get") #'py-asyncio-queue-get)
(setf (py-type-attr *py-asyncio-queue-type* "get_nowait") #'py-asyncio-queue-get-nowait)
(setf (py-type-attr *py-asyncio-queue-type* "task_done") #'py-asyncio-queue-task-done)
(setf (py-type-attr *py-asyncio-queue-type* "join") #'py-asyncio-queue-join)
(setf (py-type-attr *py-asyncio-priority-queue-type* "put") #'py-asyncio-priority-queue-put)
(setf (py-type-attr *py-asyncio-priority-queue-type* "put_nowait") #'py-asyncio-priority-queue-put-nowait)
(setf (py-type-attr *py-asyncio-lifo-queue-type* "get") #'py-asyncio-lifo-queue-get)
(setf (py-type-attr *py-asyncio-lifo-queue-type* "get_nowait") #'py-asyncio-lifo-queue-get-nowait)

(setf (py-type-attr *py-asyncio-as-completed-type* "__iter__")
      (lambda (iterator) iterator))
(setf (py-type-attr *py-asyncio-as-completed-type* "__next__") #'py-asyncio-as-completed-next)
(setf (py-type-attr *py-asyncio-as-completed-type* "__aiter__")
      (lambda (iterator) iterator))
(setf (py-type-attr *py-asyncio-as-completed-type* "__anext__") #'py-asyncio-as-completed-anext)

(defun make-clamp-asyncio-module ()
  (let ((module (make-clamp-module "asyncio")))
    (setf (py-object-attr module "__doc__") "Clamp built-in asyncio core module")
    (setf (py-object-attr module "run") #'py-asyncio-run)
    (setf (py-object-attr module "Runner") #'py-asyncio-runner)
    (setf (py-object-attr module "sleep") #'py-asyncio-sleep)
    (setf (py-object-attr module "gather") #'py-asyncio-gather)
    (setf (py-object-attr module "wait") #'py-asyncio-wait)
    (setf (py-object-attr module "FIRST_COMPLETED") *py-asyncio-first-completed*)
    (setf (py-object-attr module "FIRST_EXCEPTION") *py-asyncio-first-exception*)
    (setf (py-object-attr module "ALL_COMPLETED") *py-asyncio-all-completed*)
    (setf (py-object-attr module "get_running_loop") #'py-asyncio-get-running-loop)
    (setf (py-object-attr module "get_event_loop") #'py-asyncio-get-event-loop)
    (setf (py-object-attr module "set_event_loop") #'py-asyncio-set-event-loop)
    (setf (py-object-attr module "get_event_loop_policy") #'py-asyncio-get-event-loop-policy)
    (setf (py-object-attr module "set_event_loop_policy") #'py-asyncio-set-event-loop-policy)
    (setf (py-object-attr module "create_task") #'py-asyncio-module-create-task)
    (setf (py-object-attr module "run_coroutine_threadsafe") #'py-asyncio-run-coroutine-threadsafe)
    (setf (py-object-attr module "isfuture") #'py-asyncio-isfuture)
    (setf (py-object-attr module "iscoroutine") #'py-asyncio-iscoroutine)
    (setf (py-object-attr module "iscoroutinefunction") #'py-asyncio-iscoroutinefunction)
    (setf (py-object-attr module "to_thread") #'py-asyncio-to-thread)
    (setf (py-object-attr module "TaskGroup") #'py-asyncio-task-group)
    (setf (py-object-attr module "ensure_future") #'py-asyncio-ensure-future)
    (setf (py-object-attr module "shield") #'py-asyncio-shield)
    (setf (py-object-attr module "wait_for") #'py-asyncio-wait-for)
    (setf (py-object-attr module "timeout") #'py-asyncio-timeout)
    (setf (py-object-attr module "timeout_at") #'py-asyncio-timeout-at)
    (setf (py-object-attr module "Timeout") *py-asyncio-timeout-type*)
    (setf (py-object-attr module "current_task") #'py-asyncio-current-task)
    (setf (py-object-attr module "all_tasks") #'py-asyncio-all-tasks)
    (setf (py-object-attr module "new_event_loop") #'py-asyncio-new-event-loop)
    (setf (py-object-attr module "Lock") #'py-asyncio-lock)
    (setf (py-object-attr module "Condition") #'py-asyncio-condition)
    (setf (py-object-attr module "Semaphore") #'py-asyncio-semaphore)
    (setf (py-object-attr module "BoundedSemaphore") #'py-asyncio-bounded-semaphore)
    (setf (py-object-attr module "Barrier") #'py-asyncio-barrier)
    (setf (py-object-attr module "Event") #'py-asyncio-event)
    (setf (py-object-attr module "Queue") #'py-asyncio-queue)
    (setf (py-object-attr module "PriorityQueue") #'py-asyncio-priority-queue)
    (setf (py-object-attr module "LifoQueue") #'py-asyncio-lifo-queue)
    (setf (py-object-attr module "as_completed") #'py-asyncio-as-completed)
    (setf (py-object-attr module "open_connection") #'py-asyncio-open-connection)
    (setf (py-object-attr module "start_server") #'py-asyncio-start-server)
    (setf (py-object-attr module "StreamReader") *py-asyncio-stream-reader-type*)
    (setf (py-object-attr module "StreamWriter") *py-asyncio-stream-writer-type*)
    (setf (py-object-attr module "Server") *py-asyncio-server-type*)
    (setf (py-object-attr module "create_subprocess_exec") #'py-asyncio-create-subprocess-exec)
    (setf (py-object-attr module "create_subprocess_shell") #'py-asyncio-create-subprocess-shell)
    (setf (py-object-attr module "Process") *py-asyncio-process-type*)
    (setf (py-object-attr module "PIPE") *py-asyncio-subprocess-pipe*)
    (setf (py-object-attr module "STDOUT") *py-asyncio-subprocess-stdout*)
    (setf (py-object-attr module "DEVNULL") *py-asyncio-subprocess-devnull*)
    (setf (py-object-attr module "Future") #'py-asyncio-future-constructor)
    (setf (py-object-attr module "Task") #'py-asyncio-task-constructor)
    (setf (py-object-attr module "CancelledError") *py-asyncio-cancelled-error-type*)
    (setf (py-object-attr module "TimeoutError") *py-timeout-error-type*)
    (setf (py-object-attr module "InvalidStateError") *py-asyncio-invalid-state-error-type*)
    (setf (py-object-attr module "IncompleteReadError") *py-asyncio-incomplete-read-error-type*)
    (setf (py-object-attr module "LimitOverrunError") *py-asyncio-limit-overrun-error-type*)
    (setf (py-object-attr module "QueueFull") *py-asyncio-queue-full-type*)
    (setf (py-object-attr module "QueueEmpty") *py-asyncio-queue-empty-type*)
    (setf (py-object-attr module "BrokenBarrierError") *py-asyncio-broken-barrier-error-type*)
    module))

(py-register-builtin-module "asyncio" #'make-clamp-asyncio-module)

(defun make-clamp-asyncio-exceptions-module ()
  (let ((module (make-clamp-module "asyncio.exceptions")))
    (setf (py-object-attr module "__doc__") "Clamp built-in asyncio exceptions module")
    (setf (py-object-attr module "CancelledError") *py-asyncio-cancelled-error-type*)
    (setf (py-object-attr module "TimeoutError") *py-timeout-error-type*)
    (setf (py-object-attr module "InvalidStateError") *py-asyncio-invalid-state-error-type*)
    (setf (py-object-attr module "IncompleteReadError") *py-asyncio-incomplete-read-error-type*)
    (setf (py-object-attr module "LimitOverrunError") *py-asyncio-limit-overrun-error-type*)
    module))

(py-register-builtin-module "asyncio.exceptions" #'make-clamp-asyncio-exceptions-module)

(defun make-clamp-asyncio-futures-module ()
  (let ((module (make-clamp-module "asyncio.futures")))
    (setf (py-object-attr module "__doc__") "Clamp built-in asyncio futures module")
    (setf (py-object-attr module "Future") #'py-asyncio-future-constructor)
    (setf (py-object-attr module "isfuture") #'py-asyncio-isfuture)
    (setf (py-object-attr module "InvalidStateError") *py-asyncio-invalid-state-error-type*)
    module))

(py-register-builtin-module "asyncio.futures" #'make-clamp-asyncio-futures-module)

(defun make-clamp-asyncio-tasks-module ()
  (let ((module (make-clamp-module "asyncio.tasks")))
    (setf (py-object-attr module "__doc__") "Clamp built-in asyncio tasks module")
    (setf (py-object-attr module "Task") #'py-asyncio-task-constructor)
    (setf (py-object-attr module "create_task") #'py-asyncio-module-create-task)
    (setf (py-object-attr module "ensure_future") #'py-asyncio-ensure-future)
    (setf (py-object-attr module "shield") #'py-asyncio-shield)
    (setf (py-object-attr module "wait_for") #'py-asyncio-wait-for)
    (setf (py-object-attr module "wait") #'py-asyncio-wait)
    (setf (py-object-attr module "gather") #'py-asyncio-gather)
    (setf (py-object-attr module "as_completed") #'py-asyncio-as-completed)
    (setf (py-object-attr module "current_task") #'py-asyncio-current-task)
    (setf (py-object-attr module "all_tasks") #'py-asyncio-all-tasks)
    (setf (py-object-attr module "FIRST_COMPLETED") *py-asyncio-first-completed*)
    (setf (py-object-attr module "FIRST_EXCEPTION") *py-asyncio-first-exception*)
    (setf (py-object-attr module "ALL_COMPLETED") *py-asyncio-all-completed*)
    module))

(py-register-builtin-module "asyncio.tasks" #'make-clamp-asyncio-tasks-module)

(defun make-clamp-asyncio-locks-module ()
  (let ((module (make-clamp-module "asyncio.locks")))
    (setf (py-object-attr module "__doc__") "Clamp built-in asyncio locks module")
    (setf (py-object-attr module "Lock") #'py-asyncio-lock)
    (setf (py-object-attr module "Event") #'py-asyncio-event)
    (setf (py-object-attr module "Condition") #'py-asyncio-condition)
    (setf (py-object-attr module "Semaphore") #'py-asyncio-semaphore)
    (setf (py-object-attr module "BoundedSemaphore") #'py-asyncio-bounded-semaphore)
    (setf (py-object-attr module "Barrier") #'py-asyncio-barrier)
    (setf (py-object-attr module "BrokenBarrierError") *py-asyncio-broken-barrier-error-type*)
    module))

(py-register-builtin-module "asyncio.locks" #'make-clamp-asyncio-locks-module)

(defun make-clamp-asyncio-queues-module ()
  (let ((module (make-clamp-module "asyncio.queues")))
    (setf (py-object-attr module "__doc__") "Clamp built-in asyncio queues module")
    (setf (py-object-attr module "Queue") #'py-asyncio-queue)
    (setf (py-object-attr module "PriorityQueue") #'py-asyncio-priority-queue)
    (setf (py-object-attr module "LifoQueue") #'py-asyncio-lifo-queue)
    (setf (py-object-attr module "QueueFull") *py-asyncio-queue-full-type*)
    (setf (py-object-attr module "QueueEmpty") *py-asyncio-queue-empty-type*)
    module))

(py-register-builtin-module "asyncio.queues" #'make-clamp-asyncio-queues-module)

(defun make-clamp-asyncio-runners-module ()
  (let ((module (make-clamp-module "asyncio.runners")))
    (setf (py-object-attr module "__doc__") "Clamp built-in asyncio runners module")
    (setf (py-object-attr module "run") #'py-asyncio-run)
    (setf (py-object-attr module "Runner") #'py-asyncio-runner)
    module))

(py-register-builtin-module "asyncio.runners" #'make-clamp-asyncio-runners-module)

(defun make-clamp-asyncio-timeouts-module ()
  (let ((module (make-clamp-module "asyncio.timeouts")))
    (setf (py-object-attr module "__doc__") "Clamp built-in asyncio timeouts module")
    (setf (py-object-attr module "Timeout") *py-asyncio-timeout-type*)
    (setf (py-object-attr module "timeout") #'py-asyncio-timeout)
    (setf (py-object-attr module "timeout_at") #'py-asyncio-timeout-at)
    module))

(py-register-builtin-module "asyncio.timeouts" #'make-clamp-asyncio-timeouts-module)

(defun make-clamp-asyncio-taskgroups-module ()
  (let ((module (make-clamp-module "asyncio.taskgroups")))
    (setf (py-object-attr module "__doc__") "Clamp built-in asyncio taskgroups module")
    (setf (py-object-attr module "TaskGroup") #'py-asyncio-task-group)
    module))

(py-register-builtin-module "asyncio.taskgroups" #'make-clamp-asyncio-taskgroups-module)

(defun make-clamp-asyncio-events-module ()
  (let ((module (make-clamp-module "asyncio.events")))
    (setf (py-object-attr module "__doc__") "Clamp built-in asyncio events module")
    (setf (py-object-attr module "get_running_loop") #'py-asyncio-get-running-loop)
    (setf (py-object-attr module "get_event_loop") #'py-asyncio-get-event-loop)
    (setf (py-object-attr module "set_event_loop") #'py-asyncio-set-event-loop)
    (setf (py-object-attr module "new_event_loop") #'py-asyncio-new-event-loop)
    (setf (py-object-attr module "get_event_loop_policy") #'py-asyncio-get-event-loop-policy)
    (setf (py-object-attr module "set_event_loop_policy") #'py-asyncio-set-event-loop-policy)
    module))

(py-register-builtin-module "asyncio.events" #'make-clamp-asyncio-events-module)

(defun make-clamp-asyncio-coroutines-module ()
  (let ((module (make-clamp-module "asyncio.coroutines")))
    (setf (py-object-attr module "__doc__") "Clamp built-in asyncio coroutines module")
    (setf (py-object-attr module "iscoroutine") #'py-asyncio-iscoroutine)
    (setf (py-object-attr module "iscoroutinefunction") #'py-asyncio-iscoroutinefunction)
    module))

(py-register-builtin-module "asyncio.coroutines" #'make-clamp-asyncio-coroutines-module)

(defun make-clamp-asyncio-streams-module ()
  (let ((module (make-clamp-module "asyncio.streams")))
    (setf (py-object-attr module "__doc__") "Clamp built-in asyncio streams module")
    (setf (py-object-attr module "open_connection") #'py-asyncio-open-connection)
    (setf (py-object-attr module "start_server") #'py-asyncio-start-server)
    (setf (py-object-attr module "StreamReader") *py-asyncio-stream-reader-type*)
    (setf (py-object-attr module "StreamWriter") *py-asyncio-stream-writer-type*)
    (setf (py-object-attr module "Server") *py-asyncio-server-type*)
    (setf (py-object-attr module "IncompleteReadError") *py-asyncio-incomplete-read-error-type*)
    (setf (py-object-attr module "LimitOverrunError") *py-asyncio-limit-overrun-error-type*)
    module))

(py-register-builtin-module "asyncio.streams" #'make-clamp-asyncio-streams-module)

(defun make-clamp-asyncio-subprocess-module ()
  (let ((module (make-clamp-module "asyncio.subprocess")))
    (setf (py-object-attr module "__doc__") "Clamp built-in asyncio subprocess module")
    (setf (py-object-attr module "create_subprocess_exec") #'py-asyncio-create-subprocess-exec)
    (setf (py-object-attr module "create_subprocess_shell") #'py-asyncio-create-subprocess-shell)
    (setf (py-object-attr module "Process") *py-asyncio-process-type*)
    (setf (py-object-attr module "PIPE") *py-asyncio-subprocess-pipe*)
    (setf (py-object-attr module "STDOUT") *py-asyncio-subprocess-stdout*)
    (setf (py-object-attr module "DEVNULL") *py-asyncio-subprocess-devnull*)
    module))

(py-register-builtin-module "asyncio.subprocess" #'make-clamp-asyncio-subprocess-module)


(defstruct (py-contextvars-context-var-object (:include py-object))
  name
  default
  (has-default nil))

(defstruct (py-contextvars-token-object (:include py-object))
  var
  old-value
  (had-value nil)
  (used nil))

(defstruct (py-contextvars-context-object (:include py-object))
  values)

(defstruct (py-contextlib-async-generator-context-manager-object (:include py-object))
  generator)

(defstruct (py-contextlib-aclosing-object (:include py-object))
  thing)

(defstruct (py-contextlib-nullcontext-object (:include py-object))
  enter-result)

(defstruct (py-contextlib-async-exit-stack-object (:include py-object))
  (exit-callbacks '()))

(defvar *py-contextvars-current-values* (make-hash-table :test #'eq))
(defparameter *py-contextvars-token-missing*
  (make-py-object :type *py-object-type*))

(defun py-contextvars-copy-values (values)
  (let ((copy (make-hash-table :test #'eq)))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             values)
    copy))

(defun py-contextvars-context-var (&rest args)
  (multiple-value-bind (keyword-default keyword-default-supplied-p positional)
      (py-asyncio-keyword-value args :default *py-none*)
    (let ((name (first positional)))
      (unless (stringp name)
        (py-raise (make-py-exception *py-type-error-type* "ContextVar name must be a str")))
      (let ((var (make-py-contextvars-context-var-object
                  :type *py-contextvars-context-var-type*
                  :name name
                  :default keyword-default
                  :has-default keyword-default-supplied-p)))
        (setf (py-object-attr var "name") name)
        var))))

(defun py-contextvars-context-var-get (var &optional (default *py-contextvars-token-missing*))
  (multiple-value-bind (value found) (gethash var *py-contextvars-current-values*)
    (cond
      (found value)
      ((not (eq default *py-contextvars-token-missing*)) default)
      ((py-contextvars-context-var-object-has-default var)
       (py-contextvars-context-var-object-default var))
      (t
       (py-raise (make-py-exception *py-lookup-error-type*))))))

(defun py-contextvars-context-var-set (var value)
  (multiple-value-bind (old-value found) (gethash var *py-contextvars-current-values*)
    (setf (gethash var *py-contextvars-current-values*) value)
    (let ((token (make-py-contextvars-token-object
                  :type *py-contextvars-token-type*
                  :var var
                  :old-value old-value
                  :had-value found)))
      (setf (py-object-attr token "var") var)
      (setf (py-object-attr token "old_value")
            (if found old-value *py-contextvars-token-missing*))
      token)))

(defun py-contextvars-context-var-reset (var token)
  (unless (and (py-contextvars-token-object-p token)
               (eq (py-contextvars-token-object-var token) var))
    (py-raise (make-py-exception *py-value-error-type* "Token was created by a different ContextVar")))
  (when (py-contextvars-token-object-used token)
    (py-raise (make-py-exception *py-runtime-error-type* "Token has already been used once")))
  (setf (py-contextvars-token-object-used token) t)
  (if (py-contextvars-token-object-had-value token)
      (setf (gethash var *py-contextvars-current-values*)
            (py-contextvars-token-object-old-value token))
      (remhash var *py-contextvars-current-values*))
  *py-none*)

(defun py-contextvars-context ()
  (make-py-contextvars-context-object
   :type *py-contextvars-context-type*
   :values (make-hash-table :test #'eq)))

(defun py-contextvars-copy-context ()
  (make-py-contextvars-context-object
   :type *py-contextvars-context-type*
   :values (py-contextvars-copy-values *py-contextvars-current-values*)))

(defun py-contextvars-context-copy (context)
  (make-py-contextvars-context-object
   :type *py-contextvars-context-type*
   :values (py-contextvars-copy-values (py-contextvars-context-object-values context))))

(defun py-contextvars-context-run (context callable &rest args)
  (let ((*py-contextvars-current-values*
          (py-contextvars-copy-values (py-contextvars-context-object-values context))))
    (unwind-protect
         (apply #'py-invoke-callable callable args)
      (setf (py-contextvars-context-object-values context)
            (py-contextvars-copy-values *py-contextvars-current-values*)))))

(defun py-contextvars-context-get (context var &optional (default *py-none*))
  (multiple-value-bind (value found)
      (gethash var (py-contextvars-context-object-values context))
    (if found value default)))

(setf (py-type-attr *py-contextvars-context-var-type* "get") #'py-contextvars-context-var-get)
(setf (py-type-attr *py-contextvars-context-var-type* "set") #'py-contextvars-context-var-set)
(setf (py-type-attr *py-contextvars-context-var-type* "reset") #'py-contextvars-context-var-reset)
(setf (py-type-attr *py-contextvars-context-type* "run") #'py-contextvars-context-run)
(setf (py-type-attr *py-contextvars-context-type* "copy") #'py-contextvars-context-copy)
(setf (py-type-attr *py-contextvars-context-type* "get") #'py-contextvars-context-get)

(defun make-clamp-contextvars-module ()
  (let ((module (make-clamp-module "contextvars")))
    (setf (py-object-attr module "__doc__") "Clamp built-in contextvars module")
    (setf (py-object-attr module "ContextVar") #'py-contextvars-context-var)
    (setf (py-object-attr module "Token") *py-contextvars-token-type*)
    (setf (py-object-attr module "Context") #'py-contextvars-context)
    (setf (py-object-attr module "copy_context") #'py-contextvars-copy-context)
    (setf (py-object-attr *py-contextvars-token-type* "MISSING") *py-contextvars-token-missing*)
    module))

(py-register-builtin-module "contextvars" #'make-clamp-contextvars-module)


(defstruct (py-aiohttp-client-session-object (:include py-object))
  headers
  auth
  base-url
  timeout
  connector
  connector-owner
  cookie-jar
  raise-for-status
  (closed nil))

(defstruct (py-aiohttp-client-timeout-object (:include py-object))
  total
  connect
  sock-read
  sock-connect
  ceil-threshold)

(defstruct (py-aiohttp-tcp-connector-object (:include py-object))
  ssl
  limit
  limit-per-host
  force-close
  (closed nil))

(defstruct (py-aiohttp-basic-auth-object (:include py-object))
  login
  password
  encoding)

(defstruct (py-aiohttp-form-data-object (:include py-object))
  (fields '()))

(defstruct (py-aiohttp-cookie-jar-object (:include py-object))
  cookies)

(defstruct (py-aiohttp-request-context-object (:include py-object))
  session
  method
  url
  headers
  body
  timeout
  raise-for-status
  allow-redirects
  max-redirects
  response)

(defstruct (py-aiohttp-client-response-object (:include py-object))
  url
  method
  status
  reason
  body
  headers
  content
  (closed nil))

(defstruct (py-aiohttp-stream-reader-object (:include py-object))
  response
  body
  (index 0))

(defstruct (py-aiohttp-chunk-iterator-object (:include py-object))
  reader
  chunk-size
  (tuple-mode nil))

(defstruct (py-aiohttp-client-websocket-response-object (:include py-object))
  url
  messages
  sent
  (closed nil))

(defstruct (py-aiohttp-ws-message-object (:include py-object))
  message-type
  data
  extra)

(defun py-aiohttp-client-session (&rest args)
  (multiple-value-bind (keyword-headers keyword-headers-supplied-p positional)
      (py-asyncio-keyword-value args :headers *py-none*)
    (declare (ignore positional))
    (multiple-value-bind (keyword-auth keyword-auth-supplied-p ignored-auth-positional)
        (py-asyncio-keyword-value args :auth *py-none*)
      (declare (ignore ignored-auth-positional))
      (multiple-value-bind (keyword-base-url keyword-base-url-supplied-p ignored-base-url-positional)
          (py-asyncio-keyword-value args :base_url *py-none*)
        (declare (ignore ignored-base-url-positional))
        (multiple-value-bind (keyword-timeout keyword-timeout-supplied-p ignored-timeout-positional)
            (py-asyncio-keyword-value args :timeout *py-none*)
          (declare (ignore ignored-timeout-positional))
          (multiple-value-bind (keyword-connector keyword-connector-supplied-p ignored-connector-positional)
              (py-asyncio-keyword-value args :connector *py-none*)
            (declare (ignore ignored-connector-positional))
            (multiple-value-bind (keyword-connector-owner keyword-connector-owner-supplied-p ignored-connector-owner-positional)
                (py-asyncio-keyword-value args :connector_owner *py-true*)
              (declare (ignore ignored-connector-owner-positional))
            (multiple-value-bind (keyword-cookie-jar keyword-cookie-jar-supplied-p ignored-cookie-jar-positional)
                (py-asyncio-keyword-value args :cookie_jar *py-none*)
              (declare (ignore ignored-cookie-jar-positional))
              (multiple-value-bind (keyword-cookies keyword-cookies-supplied-p ignored-cookies-positional)
                  (py-asyncio-keyword-value args :cookies *py-none*)
                (declare (ignore ignored-cookies-positional))
                (multiple-value-bind (keyword-raise-for-status keyword-raise-for-status-supplied-p ignored-raise-for-status-positional)
                    (py-asyncio-keyword-value args :raise_for_status *py-false*)
                  (declare (ignore ignored-raise-for-status-positional))
                  (let* ((headers (if keyword-headers-supplied-p keyword-headers *py-none*))
                         (auth (if keyword-auth-supplied-p keyword-auth *py-none*))
                         (base-url (if keyword-base-url-supplied-p keyword-base-url *py-none*))
                         (timeout (if keyword-timeout-supplied-p keyword-timeout *py-none*))
                         (connector (if keyword-connector-supplied-p keyword-connector *py-none*))
                         (connector-owner (if keyword-connector-owner-supplied-p keyword-connector-owner *py-true*))
                         (raise-for-status (if keyword-raise-for-status-supplied-p keyword-raise-for-status *py-false*))
                         (cookie-jar (if (and keyword-cookie-jar-supplied-p
                                              (not (eq keyword-cookie-jar *py-none*)))
                                         keyword-cookie-jar
                                         (py-aiohttp-cookie-jar)))
                         (session (make-py-aiohttp-client-session-object
                                   :type *py-aiohttp-client-session-type*
                                   :headers headers
                                   :auth auth
                                   :base-url base-url
                                   :timeout timeout
                                   :connector connector
                                   :connector-owner connector-owner
                                   :cookie-jar cookie-jar
                                   :raise-for-status raise-for-status)))
                    (when keyword-cookies-supplied-p
                      (py-aiohttp-cookie-jar-update-cookies cookie-jar keyword-cookies))
                    (setf (py-object-attr session "closed") *py-false*)
                    (setf (py-object-attr session "headers") headers)
                    (setf (py-object-attr session "auth") auth)
                    (setf (py-object-attr session "base_url") base-url)
                    (setf (py-object-attr session "timeout") timeout)
                    (setf (py-object-attr session "connector") connector)
                    (setf (py-object-attr session "connector_owner") connector-owner)
                    (setf (py-object-attr session "cookie_jar") cookie-jar)
                    (setf (py-object-attr session "raise_for_status") raise-for-status)
                    session)))))))))))
(defun py-aiohttp-client-timeout (&rest args)
  (multiple-value-bind (keyword-total keyword-total-supplied-p positional)
      (py-asyncio-keyword-value args :total *py-none*)
    (multiple-value-bind (keyword-connect keyword-connect-supplied-p ignored-positional)
        (py-asyncio-keyword-value args :connect *py-none*)
      (declare (ignore ignored-positional))
      (multiple-value-bind (keyword-sock-read keyword-sock-read-supplied-p ignored-positional)
          (py-asyncio-keyword-value args :sock_read *py-none*)
        (declare (ignore ignored-positional))
        (multiple-value-bind (keyword-sock-connect keyword-sock-connect-supplied-p ignored-positional)
            (py-asyncio-keyword-value args :sock_connect *py-none*)
          (declare (ignore ignored-positional))
          (multiple-value-bind (keyword-ceil-threshold keyword-ceil-threshold-supplied-p ignored-positional)
              (py-asyncio-keyword-value args :ceil_threshold 5)
            (declare (ignore ignored-positional))
            (let* ((total (cond
                            (keyword-total-supplied-p keyword-total)
                            (positional (first positional))
                            (t *py-none*)))
                   (connect (if keyword-connect-supplied-p keyword-connect *py-none*))
                   (sock-read (if keyword-sock-read-supplied-p keyword-sock-read *py-none*))
                   (sock-connect (if keyword-sock-connect-supplied-p keyword-sock-connect *py-none*))
                   (ceil-threshold (if keyword-ceil-threshold-supplied-p keyword-ceil-threshold 5))
                   (timeout (make-py-aiohttp-client-timeout-object
                             :type *py-aiohttp-client-timeout-type*
                             :total total
                             :connect connect
                             :sock-read sock-read
                             :sock-connect sock-connect
                             :ceil-threshold ceil-threshold)))
              (setf (py-object-attr timeout "total") total)
              (setf (py-object-attr timeout "connect") connect)
              (setf (py-object-attr timeout "sock_read") sock-read)
              (setf (py-object-attr timeout "sock_connect") sock-connect)
              (setf (py-object-attr timeout "ceil_threshold") ceil-threshold)
              timeout)))))))

(defun py-aiohttp-tcp-connector (&rest args)
  (multiple-value-bind (keyword-ssl keyword-ssl-supplied-p positional)
      (py-asyncio-keyword-value args :ssl *py-none*)
    (declare (ignore positional))
    (multiple-value-bind (keyword-limit keyword-limit-supplied-p ignored-positional)
        (py-asyncio-keyword-value args :limit 100)
      (declare (ignore ignored-positional))
      (multiple-value-bind (keyword-limit-per-host keyword-limit-per-host-supplied-p ignored-positional)
          (py-asyncio-keyword-value args :limit_per_host 0)
        (declare (ignore ignored-positional))
        (multiple-value-bind (keyword-force-close keyword-force-close-supplied-p ignored-positional)
            (py-asyncio-keyword-value args :force_close *py-false*)
          (declare (ignore ignored-positional))
          (let* ((ssl (if keyword-ssl-supplied-p keyword-ssl *py-none*))
                 (limit (if keyword-limit-supplied-p keyword-limit 100))
                 (limit-per-host (if keyword-limit-per-host-supplied-p keyword-limit-per-host 0))
                 (force-close (if keyword-force-close-supplied-p keyword-force-close *py-false*))
                 (connector (make-py-aiohttp-tcp-connector-object
                             :type *py-aiohttp-tcp-connector-type*
                             :ssl ssl
                             :limit limit
                             :limit-per-host limit-per-host
                             :force-close force-close)))
            (setf (py-object-attr connector "ssl") ssl)
            (setf (py-object-attr connector "limit") limit)
            (setf (py-object-attr connector "limit_per_host") limit-per-host)
            (setf (py-object-attr connector "force_close") force-close)
            (setf (py-object-attr connector "closed") *py-false*)
            connector))))))

(defun py-aiohttp-tcp-connector-close (connector)
  (setf (py-aiohttp-tcp-connector-object-closed connector) t)
  (setf (py-object-attr connector "closed") *py-true*)
  *py-none*)

(defun py-aiohttp-base64-encode (text)
  (let* ((octets (sb-ext:string-to-octets text :external-format :utf-8))
         (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (with-output-to-string (stream)
      (loop for index from 0 below (length octets) by 3
            for remaining = (- (length octets) index)
            for b1 = (aref octets index)
            for b2 = (if (> remaining 1) (aref octets (1+ index)) 0)
            for b3 = (if (> remaining 2) (aref octets (+ index 2)) 0)
            for triple = (+ (ash b1 16) (ash b2 8) b3)
            do (progn
                 (write-char (char alphabet (ldb (byte 6 18) triple)) stream)
                 (write-char (char alphabet (ldb (byte 6 12) triple)) stream)
                 (write-char (if (> remaining 1)
                                 (char alphabet (ldb (byte 6 6) triple))
                                 #\=)
                             stream)
                 (write-char (if (> remaining 2)
                                 (char alphabet (ldb (byte 6 0) triple))
                                 #\=)
                             stream))))))

(defun py-aiohttp-basic-auth (&rest args)
  (multiple-value-bind (keyword-encoding keyword-encoding-supplied-p positional)
      (py-asyncio-keyword-value args :encoding "latin1")
    (multiple-value-bind (keyword-login keyword-login-supplied-p ignored-login-positional)
        (py-asyncio-keyword-value args :login *py-none*)
      (declare (ignore ignored-login-positional))
      (multiple-value-bind (keyword-password keyword-password-supplied-p ignored-password-positional)
          (py-asyncio-keyword-value args :password "")
        (declare (ignore ignored-password-positional))
        (let* ((login (cond
                        (keyword-login-supplied-p keyword-login)
                        (positional (first positional))
                        (t (py-raise (make-py-exception *py-type-error-type* "BasicAuth requires login")))))
               (password (cond
                           (keyword-password-supplied-p keyword-password)
                           ((rest positional) (second positional))
                           (t "")))
               (encoding (if keyword-encoding-supplied-p keyword-encoding "latin1"))
               (auth (make-py-aiohttp-basic-auth-object
                      :type *py-aiohttp-basic-auth-type*
                      :login login
                      :password password
                      :encoding encoding)))
          (setf (py-object-attr auth "login") login)
          (setf (py-object-attr auth "password") password)
          (setf (py-object-attr auth "encoding") encoding)
          auth)))))
(defun py-aiohttp-basic-auth-encode (auth)
  (concatenate 'string
               "Basic "
               (py-aiohttp-base64-encode
                (concatenate 'string
                             (py-str (py-aiohttp-basic-auth-object-login auth))
                             ":"
                             (py-str (py-aiohttp-basic-auth-object-password auth))))))

(defun py-aiohttp-form-data-add-pair (form name value)
  (push (cons name value) (py-aiohttp-form-data-object-fields form))
  *py-none*)

(defun py-aiohttp-form-data-add-one (form item)
  (cond
    ((py-dict-object-p item)
     (let ((storage (py-dict-storage item "FormData"))
           (keys (py-dict-object-keys item)))
       (loop for index from 0 below (fill-pointer keys)
             for key = (aref keys index)
             do (py-aiohttp-form-data-add-pair form key (gethash key storage)))))
    ((py-list-object-p item)
     (let ((storage (py-object-value item))
           (size (or (py-object-size item) 0)))
       (loop for index from 0 below size
             for pair = (aref storage index)
             do (let ((items (py-unpack-sequence pair 2)))
                  (py-aiohttp-form-data-add-pair form (first items) (second items))))))
    ((py-tuple-object-p item)
     (let ((storage (py-object-value item))
           (size (or (py-object-size item) 0)))
       (if (and (= size 2)
                (not (or (py-list-object-p (aref storage 0))
                         (py-tuple-object-p (aref storage 0)))))
           (py-aiohttp-form-data-add-pair form (aref storage 0) (aref storage 1))
           (loop for index from 0 below size
                 for pair = (aref storage index)
                 do (let ((items (py-unpack-sequence pair 2)))
                      (py-aiohttp-form-data-add-pair form (first items) (second items)))))))
    (t
     (let ((items (py-unpack-sequence item 2)))
       (py-aiohttp-form-data-add-pair form (first items) (second items)))))
  *py-none*)

(defun py-aiohttp-form-data (&rest args)
  (let ((form (make-py-aiohttp-form-data-object :type *py-aiohttp-form-data-type*)))
    (dolist (item args)
      (py-aiohttp-form-data-add-one form item))
    (setf (py-object-attr form "is_multipart") *py-false*)
    form))

(defun py-aiohttp-form-data-add-field (form name value &rest args)
  (declare (ignore args))
  (py-aiohttp-form-data-add-pair form name value)
  *py-none*)

(defun py-aiohttp-form-data-add-fields (form &rest fields)
  (dolist (field fields)
    (py-aiohttp-form-data-add-one form field))
  *py-none*)

(defun py-aiohttp-cookie-jar (&rest args)
  (declare (ignore args))
  (let ((jar (make-py-aiohttp-cookie-jar-object
              :type *py-aiohttp-cookie-jar-type*
              :cookies (make-hash-table :test #'equal))))
    jar))

(defun py-aiohttp-cookie-jar-update-cookies (jar cookies &optional (response-url *py-none*))
  (declare (ignore response-url))
  (when (and cookies (not (eq cookies *py-none*)))
    (cond
      ((py-dict-object-p cookies)
       (let ((storage (py-dict-storage cookies "CookieJar.update_cookies"))
             (keys (py-dict-object-keys cookies)))
         (loop for index from 0 below (fill-pointer keys)
               for key = (aref keys index)
               do (setf (gethash (py-str key) (py-aiohttp-cookie-jar-object-cookies jar))
                        (py-str (gethash key storage))))))
      ((stringp cookies)
       (dolist (part (split-string-on-char cookies #\;))
         (let* ((trimmed (string-trim '(#\Space #\Tab) part))
                (equals (position #\= trimmed)))
           (when equals
             (setf (gethash (subseq trimmed 0 equals) (py-aiohttp-cookie-jar-object-cookies jar))
                   (subseq trimmed (1+ equals)))))))
      (t
       (py-raise (make-py-exception *py-type-error-type* "cookies must be a dict or cookie header string")))))
  *py-none*)

(defun py-aiohttp-cookie-jar-filter-cookies (jar &optional (request-url *py-none*))
  (declare (ignore request-url))
  (let ((pairs '()))
    (maphash (lambda (key value)
               (push (list key value) pairs))
             (py-aiohttp-cookie-jar-object-cookies jar))
    (apply #'make-py-dict-from-pairs (nreverse pairs))))

(defun py-aiohttp-cookie-jar-clear (jar)
  (clrhash (py-aiohttp-cookie-jar-object-cookies jar))
  *py-none*)

(defun py-aiohttp-cookie-header-from-table (table)
  (let ((pairs '()))
    (maphash (lambda (key value)
               (push (cons key value) pairs))
             table)
    (when pairs
      (with-output-to-string (stream)
        (loop for pair in (nreverse pairs)
              for first = t then nil
              do (progn
                   (unless first (princ "; " stream))
                   (princ (car pair) stream)
                   (write-char #\= stream)
                   (princ (cdr pair) stream)))))))

(defun py-aiohttp-cookie-header (jar extra-cookies)
  (let ((combined (make-hash-table :test #'equal)))
    (when (and jar (not (eq jar *py-none*)))
      (maphash (lambda (key value)
                 (setf (gethash key combined) value))
               (py-aiohttp-cookie-jar-object-cookies jar)))
    (when (and extra-cookies (not (eq extra-cookies *py-none*)))
      (cond
        ((py-dict-object-p extra-cookies)
         (let ((storage (py-dict-storage extra-cookies "request cookies"))
               (keys (py-dict-object-keys extra-cookies)))
           (loop for index from 0 below (fill-pointer keys)
                 for key = (aref keys index)
                 do (setf (gethash (py-str key) combined) (py-str (gethash key storage))))))
        ((stringp extra-cookies)
         (py-aiohttp-cookie-jar-update-cookies
          (make-py-aiohttp-cookie-jar-object :type *py-aiohttp-cookie-jar-type* :cookies combined)
          extra-cookies))
        (t
         (py-raise (make-py-exception *py-type-error-type* "cookies must be a dict or cookie header string")))))
    (py-aiohttp-cookie-header-from-table combined)))

(defun py-aiohttp-add-cookie-header (headers jar cookies)
  (let ((header (py-aiohttp-cookie-header jar cookies)))
    (when (and header (not (py-aiohttp-headers-have-p headers "cookie")))
      (py-dict-set-entry headers "Cookie" header)))
  headers)

(defun py-aiohttp-parse-set-cookie (set-cookie)
  (when (and set-cookie (not (eq set-cookie *py-none*)))
    (let* ((first-part (first (split-string-on-char (py-str set-cookie) #\;)))
           (equals (and first-part (position #\= first-part))))
      (when equals
        (list (subseq first-part 0 equals)
              (subseq first-part (1+ equals)))))))

(defun py-aiohttp-cookie-jar-update-set-cookie (jar set-cookie)
  (let ((pair (py-aiohttp-parse-set-cookie set-cookie)))
    (when (and jar (not (eq jar *py-none*)) pair)
      (setf (gethash (first pair) (py-aiohttp-cookie-jar-object-cookies jar))
            (second pair))))
  *py-none*)

(defun py-aiohttp-response-cookies (headers)
  (let ((pair (and (py-dict-object-p headers)
                   (py-aiohttp-parse-set-cookie
                    (gethash "set-cookie" (py-dict-storage headers "aiohttp response cookies"))))))
    (if pair
        (make-py-dict-from-pairs pair)
        (make-py-dict-from-pairs))))

(defun py-aiohttp-form-data-urlencode-pairs (pairs)
  (with-output-to-string (stream)
    (loop for pair in pairs
          for first = t then nil
          do (progn
               (unless first (write-char #\& stream))
               (princ (py-aiohttp-query-encode (car pair)) stream)
               (write-char #\= stream)
               (princ (py-aiohttp-query-encode (cdr pair)) stream)))))

(defun py-aiohttp-form-data-body (form)
  (py-aiohttp-form-data-urlencode-pairs
   (nreverse (copy-list (py-aiohttp-form-data-object-fields form)))))

(defun py-aiohttp-dict-form-body (data)
  (let ((pairs '())
        (storage (py-dict-storage data "aiohttp form data"))
        (keys (py-dict-object-keys data)))
    (loop for index from 0 below (fill-pointer keys)
          for key = (aref keys index)
          do (push (cons key (gethash key storage)) pairs))
    (py-aiohttp-form-data-urlencode-pairs (nreverse pairs))))

(defun py-aiohttp-form-content-type-p (body)
  (or (py-aiohttp-form-data-object-p body)
      (py-dict-object-p body)))

(defun py-aiohttp-body-content-type (body)
  (if (py-aiohttp-form-content-type-p body)
      "application/x-www-form-urlencoded"
      *py-none*))

(defun py-aiohttp-auth-header (auth)
  (cond
    ((or (null auth) (eq auth *py-none*)) *py-none*)
    ((py-aiohttp-basic-auth-object-p auth)
     (py-aiohttp-basic-auth-encode auth))
    ((or (py-list-object-p auth) (py-tuple-object-p auth))
     (let ((items (py-unpack-sequence auth 2)))
       (py-aiohttp-basic-auth-encode
        (py-aiohttp-basic-auth (first items) (second items)))))
    (t
     (py-raise (make-py-exception *py-type-error-type* "auth must be aiohttp.BasicAuth or a (login, password) pair")))))

(defun py-aiohttp-request-headers (headers body auth)
  (let ((copy (py-aiohttp-copy-headers headers))
        (body-content-type (py-aiohttp-body-content-type body))
        (auth-header (py-aiohttp-auth-header auth)))
    (when (and (not (eq body-content-type *py-none*))
               (not (py-aiohttp-headers-have-p copy "content-type")))
      (py-dict-set-entry copy "Content-Type" body-content-type))
    (when (and (not (or (null body) (eq body *py-none*)))
               (not (py-aiohttp-headers-have-p copy "content-length")))
      (py-dict-set-entry copy "Content-Length" (length (py-aiohttp-normalize-body body))))
    (when (and (not (eq auth-header *py-none*))
               (not (py-aiohttp-headers-have-p copy "authorization")))
      (py-dict-set-entry copy "Authorization" auth-header))
    copy))

(defun py-aiohttp-session-aenter (session)
  (make-py-coroutine "ClientSession.__aenter__"
                     (lambda () session)))

(defun py-aiohttp-session-close-now (session)
  (when (and (py-truthy-p (py-aiohttp-client-session-object-connector-owner session))
             (py-aiohttp-tcp-connector-object-p (py-aiohttp-client-session-object-connector session)))
    (py-aiohttp-tcp-connector-close (py-aiohttp-client-session-object-connector session)))
  (setf (py-aiohttp-client-session-object-closed session) t)
  (setf (py-object-attr session "closed") *py-true*)
  *py-none*)

(defun py-aiohttp-session-detach (session)
  (setf (py-aiohttp-client-session-object-connector session) *py-none*)
  (setf (py-object-attr session "connector") *py-none*)
  (setf (py-aiohttp-client-session-object-closed session) t)
  (setf (py-object-attr session "closed") *py-true*)
  *py-none*)

(defun py-aiohttp-session-close (session)
  (py-aiohttp-session-close-now session)
  (make-py-coroutine "ClientSession.close"
                     (lambda () *py-none*)))

(defun py-aiohttp-session-aexit (session exc-type exc-value traceback)
  (declare (ignore exc-type exc-value traceback))
  (make-py-coroutine "ClientSession.__aexit__"
                     (lambda ()
                       (py-aiohttp-session-close-now session)
                       *py-false*)))

(defun py-aiohttp-strip-query-fragment (url)
  (let ((end (length url)))
    (loop for marker across "?#"
          do (let ((pos (position marker url)))
               (when pos
                 (setf end (min end pos)))))
    (subseq url 0 end)))

(defun py-aiohttp-percent-decode (text)
  (with-output-to-string (out)
    (loop for index from 0 below (length text)
          do (let ((ch (char text index)))
               (cond
                 ((and (char= ch #\%)
                       (<= (+ index 2) (1- (length text))))
                  (let ((hex (subseq text (1+ index) (+ index 3))))
                    (write-char (code-char (parse-integer hex :radix 16)) out)
                    (incf index 2)))
                 ((char= ch #\+)
                  (write-char #\Space out))
                 (t
                  (write-char ch out)))))))

(defun py-aiohttp-data-url-parts (url)
  (let* ((comma (position #\, url))
         (metadata (if comma (subseq url (length "data:") comma) ""))
         (payload (if comma (subseq url (1+ comma)) ""))
         (media-type (if (string= metadata "")
                         "text/plain;charset=US-ASCII"
                         (first (split-string-on-char metadata #\;)))))
    (values media-type (py-aiohttp-percent-decode payload))))

(defun py-aiohttp-file-url-path (url)
  (py-aiohttp-percent-decode (py-aiohttp-strip-query-fragment (subseq url (length "file://")))))

(defun py-aiohttp-http-url-p (url)
  (and (stringp url)
       (>= (length url) (length "http://"))
       (string= url "http://" :end1 (length "http://") :end2 (length "http://"))))

(defun py-aiohttp-parse-http-url (url)
  (let* ((rest (subseq url (length "http://")))
         (slash (position #\/ rest))
         (authority (if slash (subseq rest 0 slash) rest))
         (path (if slash (subseq rest slash) "/"))
         (colon (position #\: authority :from-end t))
         (host (if colon (subseq authority 0 colon) authority))
         (port (if colon (parse-integer (subseq authority (1+ colon))) 80)))
    (values host port path)))

(defun py-aiohttp-normalize-body (body)
  (cond
    ((or (null body) (eq body *py-none*)) "")
    ((stringp body) body)
    ((py-bytes-object-p body)
     (sb-ext:octets-to-string (py-bytes-storage body "aiohttp body") :external-format :utf-8))
    ((py-aiohttp-form-data-object-p body)
     (py-aiohttp-form-data-body body))
    ((py-dict-object-p body)
     (py-aiohttp-dict-form-body body))
    (t (py-str body))))

(defun py-aiohttp-json-escape-string (value)
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for char across value
          do (case char
               (#\" (princ "\\\"" stream))
               (#\\ (princ "\\\\" stream))
               (#\Newline (princ "\\n" stream))
               (#\Return (princ "\\r" stream))
               (#\Tab (princ "\\t" stream))
               (otherwise (write-char char stream))))
    (write-char #\" stream)))

(defun py-aiohttp-json-dumps (value)
  (cond
    ((eq value *py-none*) "null")
    ((eq value *py-true*) "true")
    ((eq value *py-false*) "false")
    ((stringp value) (py-aiohttp-json-escape-string value))
    ((numberp value) (write-to-string value))
    ((py-list-object-p value)
     (with-output-to-string (stream)
       (write-char #\[ stream)
       (loop for index from 0 below (or (py-object-size value) 0)
             do (progn
                  (when (> index 0) (write-char #\, stream))
                  (princ (py-aiohttp-json-dumps (aref (py-object-value value) index)) stream)))
       (write-char #\] stream)))
    ((py-tuple-object-p value)
     (with-output-to-string (stream)
       (write-char #\[ stream)
       (loop for index from 0 below (or (py-object-size value) 0)
             do (progn
                  (when (> index 0) (write-char #\, stream))
                  (princ (py-aiohttp-json-dumps (aref (py-object-value value) index)) stream)))
       (write-char #\] stream)))
    ((py-dict-object-p value)
     (with-output-to-string (stream)
       (let ((storage (py-dict-storage value "aiohttp json"))
             (keys (py-dict-object-keys value)))
         (write-char #\{ stream)
         (loop for index from 0 below (fill-pointer keys)
               for key = (aref keys index)
               do (progn
                    (when (> index 0) (write-char #\, stream))
                    (princ (py-aiohttp-json-escape-string (py-str key)) stream)
                    (write-char #\: stream)
                    (princ (py-aiohttp-json-dumps (gethash key storage)) stream)))
         (write-char #\} stream))))
    (t (py-aiohttp-json-escape-string (py-str value)))))

(defun py-aiohttp-query-safe-char-p (char)
  (or (alphanumericp char)
      (member char '(#\- #\_ #\. #\~))))

(defun py-aiohttp-query-encode (value)
  (with-output-to-string (stream)
    (loop for char across (py-str value)
          do (cond
               ((py-aiohttp-query-safe-char-p char)
                (write-char char stream))
               ((char= char #\Space)
                (write-char #\+ stream))
               (t
                (format stream "%~2,'0X" (char-code char)))))))

(defun py-aiohttp-params-string (params)
  (cond
    ((or (null params) (eq params *py-none*)) "")
    ((stringp params) params)
    ((py-dict-object-p params)
     (with-output-to-string (stream)
       (let ((storage (py-dict-storage params "aiohttp params"))
             (keys (py-dict-object-keys params)))
         (loop for index from 0 below (fill-pointer keys)
               for key = (aref keys index)
               do (progn
                    (when (> index 0) (write-char #\& stream))
                    (princ (py-aiohttp-query-encode key) stream)
                    (write-char #\= stream)
                    (princ (py-aiohttp-query-encode (gethash key storage)) stream))))))
    (t (py-str params))))

(defun py-aiohttp-url-with-params (url params)
  (let ((query (py-aiohttp-params-string params)))
    (if (= (length query) 0)
        url
        (concatenate 'string url (if (position #\? url) "&" "?") query))))

(defun py-aiohttp-absolute-url-p (url)
  (or (and (stringp url) (>= (length url) (length "http://"))
           (string= url "http://" :end1 (length "http://") :end2 (length "http://")))
      (and (stringp url) (>= (length url) (length "data:"))
           (string= url "data:" :end1 (length "data:") :end2 (length "data:")))
      (and (stringp url) (>= (length url) (length "file://"))
           (string= url "file://" :end1 (length "file://") :end2 (length "file://")))))

(defun py-aiohttp-join-url (base url)
  (if (or (eq base *py-none*) (py-aiohttp-absolute-url-p url))
      url
      (let* ((base-text (py-str base))
             (url-text (py-str url))
             (base-has-slash (and (> (length base-text) 0)
                                  (char= (char base-text (1- (length base-text))) #\/)))
             (url-has-slash (and (> (length url-text) 0)
                                 (char= (char url-text 0) #\/))))
        (cond
          ((and base-has-slash url-has-slash)
           (concatenate 'string base-text (subseq url-text 1)))
          ((or base-has-slash url-has-slash)
           (concatenate 'string base-text url-text))
          (t
           (concatenate 'string base-text "/" url-text))))))

(defun py-aiohttp-merge-headers (session-headers request-headers)
  (let ((merged (py-aiohttp-copy-headers session-headers)))
    (when (and request-headers (not (eq request-headers *py-none*)))
      (unless (py-dict-object-p request-headers)
        (error "aiohttp headers must be a dict"))
      (let ((storage (py-dict-storage request-headers "aiohttp headers"))
            (keys (py-dict-object-keys request-headers)))
        (loop for index from 0 below (fill-pointer keys)
              for key = (aref keys index)
              do (py-dict-set-entry merged key (gethash key storage)))))
    merged))

(defun py-aiohttp-headers-have-p (headers name)
  (and (py-dict-object-p headers)
       (let ((keys (py-dict-object-keys headers)))
         (loop for index from 0 below (fill-pointer keys)
               for key = (aref keys index)
               thereis (string= (string-downcase (py-str key)) name)))))

(defun py-aiohttp-copy-headers (headers)
  (if (and headers (not (eq headers *py-none*)))
      (let ((copy (make-py-dict-from-pairs))
            (storage (py-dict-storage headers "aiohttp headers"))
            (keys (py-dict-object-keys headers)))
        (loop for index from 0 below (fill-pointer keys)
              for key = (aref keys index)
              do (py-dict-set-entry copy key (gethash key storage)))
        copy)
      (make-py-dict-from-pairs)))

(defun py-aiohttp-json-headers (headers)
  (let ((copy (py-aiohttp-copy-headers headers)))
    (unless (py-aiohttp-headers-have-p copy "content-type")
      (py-dict-set-entry copy "Content-Type" "application/json"))
    copy))

(defun py-aiohttp-read-http-body (stream headers)
  (let ((content-length (gethash "content-length" headers)))
    (if content-length
        (let* ((size (parse-integer content-length :junk-allowed t))
               (body (make-string (or size 0))))
          (when (> (length body) 0)
            (read-sequence body stream))
          body)
        (with-output-to-string (out)
          (loop for char = (read-char stream nil nil)
                while char
                do (write-char char out))))))

(defun py-aiohttp-headers-dict-from-table (headers-table)
  (let ((pairs '()))
    (maphash (lambda (key value)
               (push (list key value) pairs))
             headers-table)
    (apply #'make-py-dict-from-pairs (nreverse pairs))))

(defun py-aiohttp-request-header-pairs (headers)
  (let ((pairs '()))
    (when (and headers (not (eq headers *py-none*)))
      (unless (py-dict-object-p headers)
        (error "aiohttp headers must be a dict"))
      (let ((storage (py-dict-storage headers "aiohttp headers"))
            (keys (py-dict-object-keys headers)))
        (loop for index from 0 below (fill-pointer keys)
              for key = (aref keys index)
              for value = (gethash key storage)
              do (push (cons (py-str key) (py-str value)) pairs))))
    (nreverse pairs)))

(defun py-aiohttp-http-request (method url body request-headers)
  (multiple-value-bind (host port path) (py-aiohttp-parse-http-url url)
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (multiple-value-prog1
          (handler-case
              (progn
                (sb-bsd-sockets:socket-connect
                 socket
                 (car (sb-bsd-sockets:host-ent-addresses
                       (sb-bsd-sockets:get-host-by-name host)))
                 port)
                (let ((stream (sb-bsd-sockets:socket-make-stream
                               socket
                               :input t
                               :output t
                               :element-type 'character
                               :external-format :utf-8
                               :buffering :none))
                      (request-body (py-aiohttp-normalize-body body))
                      (header-pairs (py-aiohttp-request-header-pairs request-headers)))
                  (format stream "~A ~A HTTP/1.0~C~CHost: ~A~C~C" method path #\Return #\Linefeed host #\Return #\Linefeed)
                  (dolist (pair header-pairs)
                    (format stream "~A: ~A~C~C" (car pair) (cdr pair) #\Return #\Linefeed))
                  (format stream "Connection: close~C~C" #\Return #\Linefeed)
                  (when (> (length request-body) 0)
                    (unless (find "content-length" header-pairs :key (lambda (pair) (string-downcase (car pair))) :test #'string=)
                      (format stream "Content-Length: ~A~C~C" (length request-body) #\Return #\Linefeed)))
                  (format stream "~C~C" #\Return #\Linefeed)
                  (when (> (length request-body) 0)
                    (write-string request-body stream))
                  (finish-output stream)
                  (let* ((status-line (or (read-line stream nil nil) "HTTP/1.0 599 Network Unavailable"))
                         (first-space (position #\Space status-line))
                         (second-space (and first-space (position #\Space status-line :start (1+ first-space))))
                         (status (if first-space
                                     (parse-integer status-line :start (1+ first-space)
                                                                :end (or second-space (length status-line))
                                                                :junk-allowed t)
                                     599))
                         (reason (if second-space (subseq status-line (1+ second-space)) ""))
                         (headers-table (make-hash-table :test #'equal)))
                    (loop for raw-line = (read-line stream nil nil)
                          for line = (and raw-line (string-trim '(#\Return) raw-line))
                          while (and line (not (string= line "")))
                          do (let ((colon (position #\: line)))
                               (when colon
                                 (setf (gethash (string-downcase (subseq line 0 colon)) headers-table)
                                       (string-trim '(#\Space #\Tab #\Return) (subseq line (1+ colon)))))))
                    (let ((body-text (py-aiohttp-read-http-body stream headers-table))
                          (headers-dict (py-aiohttp-headers-dict-from-table headers-table)))
                      (values status
                              reason
                              body-text
                              (or (gethash "content-type" headers-table) "application/octet-stream")
                              headers-dict)))))
            (error ()
              (values 599 "Network Unavailable" "" "text/plain" (make-py-dict-from-pairs))))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun py-aiohttp-load-url-body (method url body request-headers)
  (cond
    ((and (stringp url) (>= (length url) (length "data:"))
          (string= url "data:" :end1 (length "data:") :end2 (length "data:")))
     (multiple-value-bind (content-type response-body) (py-aiohttp-data-url-parts url)
       (values 200 "OK" response-body content-type
               (make-py-dict-from-pairs (list "content-type" content-type)))))
    ((and (stringp url) (>= (length url) (length "file://"))
          (string= url "file://" :end1 (length "file://") :end2 (length "file://")))
     (let ((path (py-aiohttp-file-url-path url))
           (content-type "text/plain"))
       (if (probe-file path)
           (values 200 "OK"
                   (with-open-file (stream path :direction :input :external-format :utf-8)
                     (let ((contents (make-string (file-length stream))))
                       (read-sequence contents stream)
                       contents))
                   content-type
                   (make-py-dict-from-pairs (list "content-type" content-type)))
           (values 404 "Not Found" "" content-type
                   (make-py-dict-from-pairs (list "content-type" content-type))))))
    ((py-aiohttp-http-url-p url)
     (py-aiohttp-http-request method url body request-headers))
    (t
     (values 599 "Network Unavailable"
             "Clamp aiohttp supports http://, data:, and file:// URLs"
             "text/plain"
             (make-py-dict-from-pairs (list "content-type" "text/plain"))))))

(defun py-aiohttp-redirect-status-p (status)
  (member status '(301 302 303 307 308)))

(defun py-aiohttp-header-value (headers name)
  (and (py-dict-object-p headers)
       (gethash (string-downcase name) (py-dict-storage headers "aiohttp response headers"))))

(defun py-aiohttp-content-length (headers)
  (let ((value (py-aiohttp-header-value headers "content-length")))
    (if value
        (or (parse-integer (py-str value) :junk-allowed t) *py-none*)
        *py-none*)))

(defun py-aiohttp-content-charset (content-type)
  (when content-type
    (dolist (part (rest (split-string-on-char (py-str content-type) #\;)))
      (let* ((trimmed (string-trim '(#\Space #\Tab) part))
             (equals (position #\= trimmed)))
        (when (and equals
                   (string= (string-downcase (subseq trimmed 0 equals)) "charset"))
          (return (string-trim '(#\Space #\Tab #\") (subseq trimmed (1+ equals)))))))))

(defun py-aiohttp-response-encoding (response)
  (let ((charset (py-object-attr response "charset")))
    (if (eq charset *py-none*) "utf-8" charset)))

(defun py-aiohttp-timeout-total (timeout)
  (cond
    ((or (null timeout) (eq timeout *py-none*)) *py-none*)
    ((numberp timeout) timeout)
    ((py-aiohttp-client-timeout-object-p timeout)
     (py-aiohttp-client-timeout-object-total timeout))
    (t *py-none*)))

(defun py-aiohttp-timeout-expired-p (timeout)
  (let ((total (py-aiohttp-timeout-total timeout)))
    (and (not (eq total *py-none*))
         (numberp total)
         (<= total 0))))

(defun py-aiohttp-raise-timeout ()
  (py-raise (make-py-exception *py-aiohttp-server-timeout-error-type* "Request timed out")))

(defun py-aiohttp-origin (url)
  (when (py-aiohttp-http-url-p url)
    (let* ((rest (subseq url (length "http://")))
           (slash (position #\/ rest))
           (authority (if slash (subseq rest 0 slash) rest)))
      (concatenate 'string "http://" authority))))

(defun py-aiohttp-resolve-location (url location)
  (cond
    ((py-aiohttp-absolute-url-p location) location)
    ((not (py-aiohttp-http-url-p url)) location)
    ((and (> (length location) 0) (char= (char location 0) #\/))
     (concatenate 'string (py-aiohttp-origin url) location))
    (t
     (let* ((origin (py-aiohttp-origin url))
            (rest (subseq url (length origin)))
            (path (py-aiohttp-strip-query-fragment (if (> (length rest) 0) rest "/")))
            (last-slash (position #\/ path :from-end t))
            (directory (if last-slash (subseq path 0 (1+ last-slash)) "/")))
       (concatenate 'string origin directory location)))))

(defun py-aiohttp-response-request-info (url method headers)
  (make-py-dict-from-pairs
   (list "url" url)
   (list "method" method)
   (list "headers" headers)))

(defun py-aiohttp-build-response (request url status reason body content-type headers history)
  (let ((response (make-py-aiohttp-client-response-object
                   :type *py-aiohttp-client-response-type*
                   :url url
                   :method (py-aiohttp-request-context-object-method request)
                   :status status
                   :reason reason
                   :body body
                   :headers headers)))
    (setf (py-aiohttp-client-response-object-content response)
          (py-aiohttp-make-stream-reader response body))
    (py-aiohttp-cookie-jar-update-set-cookie
     (py-aiohttp-client-session-object-cookie-jar
      (py-aiohttp-request-context-object-session request))
     (py-aiohttp-header-value headers "set-cookie"))
    (setf (py-object-attr response "status") status)
    (setf (py-object-attr response "reason") reason)
    (setf (py-object-attr response "url") url)
    (setf (py-object-attr response "real_url") url)
    (setf (py-object-attr response "method") (py-aiohttp-request-context-object-method request))
    (setf (py-object-attr response "request_info")
          (py-aiohttp-response-request-info url
                                            (py-aiohttp-request-context-object-method request)
                                            (py-aiohttp-request-context-object-headers request)))
    (setf (py-object-attr response "history") (apply #'make-py-tuple history))
    (setf (py-object-attr response "cookies") (py-aiohttp-response-cookies headers))
    (setf (py-object-attr response "content_type") content-type)
    (setf (py-object-attr response "content_length") (py-aiohttp-content-length headers))
    (setf (py-object-attr response "charset") (or (py-aiohttp-content-charset content-type) *py-none*))
    (setf (py-object-attr response "ok") (py-bool (< status 400)))
    (setf (py-object-attr response "closed") *py-false*)
    (setf (py-object-attr response "headers") (py-aiohttp-client-response-object-headers response))
    (setf (py-object-attr response "content") (py-aiohttp-client-response-object-content response))
    response))

(defun py-aiohttp-string-bytes (text)
  (make-py-bytes-from-vector
   (sb-ext:string-to-octets text :external-format :utf-8)))

(defun py-aiohttp-stream-reader-remaining (reader)
  (let ((body (py-aiohttp-stream-reader-object-body reader))
        (index (py-aiohttp-stream-reader-object-index reader)))
    (max 0 (- (length body) index))))

(defun py-aiohttp-stream-reader-at-eof (reader)
  (py-bool (= (py-aiohttp-stream-reader-remaining reader) 0)))

(defun py-aiohttp-stream-reader-feed-eof (reader)
  (setf (py-aiohttp-stream-reader-object-index reader)
        (length (py-aiohttp-stream-reader-object-body reader)))
  *py-none*)

(defun py-aiohttp-stream-reader-read-now (reader n)
  (let* ((body (py-aiohttp-stream-reader-object-body reader))
         (index (py-aiohttp-stream-reader-object-index reader))
         (remaining (- (length body) index))
         (requested (if (or (eq n *py-none*) (< n 0)) remaining (min n remaining)))
         (end (+ index requested))
         (chunk (subseq body index end)))
    (setf (py-aiohttp-stream-reader-object-index reader) end)
    (py-aiohttp-string-bytes chunk)))

(defun py-aiohttp-stream-reader-read (reader &optional (n -1))
  (make-py-coroutine "StreamReader.read"
                     (lambda ()
                       (py-aiohttp-stream-reader-read-now reader n))))

(defun py-aiohttp-stream-reader-readany (reader)
  (py-aiohttp-stream-reader-read reader -1))

(defun py-aiohttp-stream-reader-read-nowait (reader &optional (n -1))
  (py-aiohttp-stream-reader-read-now reader n))

(defun py-aiohttp-stream-reader-readchunk (reader)
  (make-py-coroutine "StreamReader.readchunk"
                     (lambda ()
                       (make-py-tuple
                        (py-aiohttp-stream-reader-read-now reader -1)
                        *py-false*))))

(defun py-aiohttp-stream-reader-is-eof (reader)
  (py-aiohttp-stream-reader-at-eof reader))

(defun py-aiohttp-stream-reader-exception (reader)
  (declare (ignore reader))
  *py-none*)

(defun py-aiohttp-stream-reader-readexactly (reader n)
  (make-py-coroutine "StreamReader.readexactly"
                     (lambda ()
                       (let ((remaining (py-aiohttp-stream-reader-remaining reader)))
                         (when (< remaining n)
                           (let ((partial (py-aiohttp-stream-reader-read-now reader remaining))
                                 (exception (make-py-exception *py-asyncio-incomplete-read-error-type*
                                                               "not enough bytes available")))
                             (setf (py-object-attr exception "partial") partial)
                             (setf (py-object-attr exception "expected") n)
                             (py-raise exception)))
                         (py-aiohttp-stream-reader-read-now reader n)))))

(defun py-aiohttp-stream-reader-readline (reader)
  (make-py-coroutine "StreamReader.readline"
                     (lambda ()
                       (let* ((body (py-aiohttp-stream-reader-object-body reader))
                              (index (py-aiohttp-stream-reader-object-index reader))
                              (newline (position #\Newline body :start index))
                              (end (if newline (1+ newline) (length body))))
                         (setf (py-aiohttp-stream-reader-object-index reader) end)
                         (py-aiohttp-string-bytes (subseq body index end))))))

(defun py-aiohttp-stream-reader-aiter (reader)
  reader)

(defun py-aiohttp-stream-reader-anext (reader)
  (make-py-coroutine "StreamReader.__anext__"
                     (lambda ()
                       (let ((line (py-await (py-aiohttp-stream-reader-readline reader))))
                         (if (= (or (py-object-size line) 0) 0)
                             (py-raise (make-py-exception *py-stop-async-iteration-type*))
                             line)))))

(defun py-aiohttp-stream-reader-iter-chunked (reader n)
  (make-py-aiohttp-chunk-iterator-object :type *py-aiohttp-chunk-iterator-type*
                                         :reader reader
                                         :chunk-size n))

(defun py-aiohttp-stream-reader-iter-any (reader)
  (make-py-aiohttp-chunk-iterator-object :type *py-aiohttp-chunk-iterator-type*
                                         :reader reader
                                         :chunk-size -1))

(defun py-aiohttp-stream-reader-iter-chunks (reader)
  (make-py-aiohttp-chunk-iterator-object :type *py-aiohttp-chunk-iterator-type*
                                         :reader reader
                                         :chunk-size -1
                                         :tuple-mode t))

(defun py-aiohttp-chunk-iterator-anext (iterator)
  (make-py-coroutine "AsyncStreamIterator.__anext__"
                     (lambda ()
                       (let ((reader (py-aiohttp-chunk-iterator-object-reader iterator)))
                         (when (= (py-aiohttp-stream-reader-remaining reader) 0)
                           (py-raise (make-py-exception *py-stop-async-iteration-type*)))
                         (let ((chunk (py-aiohttp-stream-reader-read-now
                                      reader
                                      (py-aiohttp-chunk-iterator-object-chunk-size iterator))))
                           (if (py-aiohttp-chunk-iterator-object-tuple-mode iterator)
                               (make-py-tuple chunk *py-false*)
                               chunk))))))

(defun py-aiohttp-make-stream-reader (response body)
  (make-py-aiohttp-stream-reader-object :type *py-aiohttp-stream-reader-type*
                                        :response response
                                        :body body))

(defun py-aiohttp-json-whitespace-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return)))

(defun py-aiohttp-json-skip-ws (text index)
  (loop while (and (< index (length text))
                   (py-aiohttp-json-whitespace-p (char text index)))
        do (incf index))
  index)

(defun py-aiohttp-json-parse-string (text index)
  (unless (and (< index (length text)) (char= (char text index) #\"))
    (error "expected JSON string"))
  (incf index)
  (let ((stream (make-string-output-stream)))
    (loop while (< index (length text))
          for char = (char text index)
          do (cond
               ((char= char #\")
                (return-from py-aiohttp-json-parse-string
                  (values (get-output-stream-string stream) (1+ index))))
               ((char= char #\\)
                (incf index)
                (when (>= index (length text))
                  (error "unterminated JSON escape"))
                (let ((escaped (char text index)))
                  (write-char
                   (case escaped
                     (#\" #\")
                     (#\\ #\\)
                     (#\/ #\/)
                     (#\b #\Backspace)
                     (#\f #\Page)
                     (#\n #\Newline)
                     (#\r #\Return)
                     (#\t #\Tab)
                     (otherwise escaped))
                   stream)))
               (t
                (write-char char stream)))
          do (incf index)))
  (error "unterminated JSON string"))

(defun py-aiohttp-json-parse-number (text index)
  (let ((start index))
    (when (and (< index (length text)) (char= (char text index) #\-))
      (incf index))
    (loop while (and (< index (length text)) (digit-char-p (char text index)))
          do (incf index))
    (if (and (< index (length text)) (char= (char text index) #\.))
        (progn
          (incf index)
          (loop while (and (< index (length text)) (digit-char-p (char text index)))
                do (incf index))
          (values (read-from-string (subseq text start index)) index))
        (values (parse-integer (subseq text start index)) index))))

(defun py-aiohttp-json-parse-literal (text index literal value)
  (let ((end (+ index (length literal))))
    (unless (and (<= end (length text))
                 (string= text literal :start1 index :end1 end))
      (error "invalid JSON literal"))
    (values value end)))

(defun py-aiohttp-json-parse-array (text index)
  (let ((items '()))
    (incf index)
    (setf index (py-aiohttp-json-skip-ws text index))
    (when (and (< index (length text)) (char= (char text index) #\]))
      (return-from py-aiohttp-json-parse-array
        (values (make-py-list :size 0 :value (make-array 0 :adjustable t :fill-pointer 0)) (1+ index))))
    (loop
      (multiple-value-bind (value next-index) (py-aiohttp-json-parse-value text index)
        (push value items)
        (setf index (py-aiohttp-json-skip-ws text next-index)))
      (cond
        ((and (< index (length text)) (char= (char text index) #\,))
         (setf index (py-aiohttp-json-skip-ws text (1+ index))))
        ((and (< index (length text)) (char= (char text index) #\]))
         (return (values (apply #'make-py-list (nreverse items)) (1+ index))))
        (t
         (error "expected JSON array delimiter"))))))

(defun py-aiohttp-json-parse-object (text index)
  (let ((pairs '()))
    (incf index)
    (setf index (py-aiohttp-json-skip-ws text index))
    (when (and (< index (length text)) (char= (char text index) #\}))
      (return-from py-aiohttp-json-parse-object
        (values (make-py-dict-from-pairs) (1+ index))))
    (loop
      (multiple-value-bind (key next-index) (py-aiohttp-json-parse-string text index)
        (setf index (py-aiohttp-json-skip-ws text next-index))
        (unless (and (< index (length text)) (char= (char text index) #\:))
          (error "expected JSON object colon"))
        (multiple-value-bind (value value-index)
            (py-aiohttp-json-parse-value text (py-aiohttp-json-skip-ws text (1+ index)))
          (push (list key value) pairs)
          (setf index (py-aiohttp-json-skip-ws text value-index))))
      (cond
        ((and (< index (length text)) (char= (char text index) #\,))
         (setf index (py-aiohttp-json-skip-ws text (1+ index))))
        ((and (< index (length text)) (char= (char text index) #\}))
         (return (values (apply #'make-py-dict-from-pairs (nreverse pairs)) (1+ index))))
        (t
         (error "expected JSON object delimiter"))))))

(defun py-aiohttp-json-parse-value (text index)
  (setf index (py-aiohttp-json-skip-ws text index))
  (when (>= index (length text))
    (error "unexpected end of JSON input"))
  (let ((char (char text index)))
    (cond
      ((char= char #\") (py-aiohttp-json-parse-string text index))
      ((char= char #\{) (py-aiohttp-json-parse-object text index))
      ((char= char #\[) (py-aiohttp-json-parse-array text index))
      ((or (char= char #\-) (digit-char-p char)) (py-aiohttp-json-parse-number text index))
      ((char= char #\t) (py-aiohttp-json-parse-literal text index "true" *py-true*))
      ((char= char #\f) (py-aiohttp-json-parse-literal text index "false" *py-false*))
      ((char= char #\n) (py-aiohttp-json-parse-literal text index "null" *py-none*))
      (t (error "unexpected JSON character: ~A" char)))))

(defun py-aiohttp-json-loads (text)
  (multiple-value-bind (value index) (py-aiohttp-json-parse-value text 0)
    (let ((end (py-aiohttp-json-skip-ws text index)))
      (unless (= end (length text))
        (error "extra data after JSON value"))
      value)))

(defun py-aiohttp-make-response (request)
  (when (py-aiohttp-timeout-expired-p (py-aiohttp-request-context-object-timeout request))
    (py-aiohttp-raise-timeout))
  (let ((url (py-aiohttp-request-context-object-url request))
        (history '())
        (remaining (py-aiohttp-request-context-object-max-redirects request)))
    (loop
      (multiple-value-bind (status reason body content-type headers)
          (py-aiohttp-load-url-body (py-aiohttp-request-context-object-method request)
                                    url
                                    (py-aiohttp-request-context-object-body request)
                                    (py-aiohttp-request-context-object-headers request))
        (let ((location (py-aiohttp-header-value headers "location")))
          (if (and (py-truthy-p (py-aiohttp-request-context-object-allow-redirects request))
                   (py-aiohttp-redirect-status-p status)
                   location)
              (progn
                (let ((redirect-response (py-aiohttp-build-response request url status reason body content-type headers (nreverse history))))
                  (push redirect-response history))
                (decf remaining)
                (when (< remaining 0)
                  (py-raise
                   (py-aiohttp-make-response-error
                    *py-aiohttp-too-many-redirects-type*
                    (first history)
                    "Too many redirects")))
                (setf url (py-aiohttp-resolve-location url location)))
              (let ((response (py-aiohttp-build-response request url status reason body content-type headers (nreverse history))))
                (when (py-truthy-p (py-aiohttp-request-context-object-raise-for-status request))
                  (py-aiohttp-response-raise-for-status response))
                (return response))))))))

(defun py-aiohttp-request-aenter (request)
  (make-py-coroutine "_RequestContextManager.__aenter__"
                     (lambda ()
                       (let ((response (py-aiohttp-make-response request)))
                         (setf (py-aiohttp-request-context-object-response request) response)
                         response))))

(defun py-aiohttp-request-aexit (request exc-type exc-value traceback)
  (declare (ignore exc-type exc-value traceback))
  (make-py-coroutine "_RequestContextManager.__aexit__"
                     (lambda ()
                       (let ((response (py-aiohttp-request-context-object-response request)))
                         (when response
                           (py-aiohttp-response-release response)))
                       *py-false*)))

(defun py-aiohttp-request-await (request)
  (let ((response (py-aiohttp-make-response request)))
    (setf (py-aiohttp-request-context-object-response request) response)
    (py-iter (make-py-list response))))

(defparameter *py-aiohttp-ws-msg-text* 1)
(defparameter *py-aiohttp-ws-msg-binary* 2)
(defparameter *py-aiohttp-ws-msg-close* 8)
(defparameter *py-aiohttp-ws-msg-ping* 9)
(defparameter *py-aiohttp-ws-msg-pong* 10)
(defparameter *py-aiohttp-ws-msg-closed* 257)
(defparameter *py-aiohttp-ws-msg-error* 258)

(defun py-aiohttp-ws-message (message-type data &optional (extra *py-none*))
  (let ((message (make-py-aiohttp-ws-message-object
                  :type *py-aiohttp-ws-message-type*
                  :message-type message-type
                  :data data
                  :extra extra)))
    (setf (py-object-attr message "type") message-type)
    (setf (py-object-attr message "data") data)
    (setf (py-object-attr message "extra") extra)
    message))

(defun py-aiohttp-websocket-sync-closed (ws)
  (setf (py-object-attr ws "closed") (py-bool (py-aiohttp-client-websocket-response-object-closed ws)))
  ws)

(defun py-aiohttp-make-websocket-response (url messages)
  (let ((ws (make-py-aiohttp-client-websocket-response-object
             :type *py-aiohttp-client-websocket-response-type*
             :url url
             :messages messages
             :sent '())))
    (setf (py-object-attr ws "url") url)
    (setf (py-object-attr ws "closed") *py-false*)
    ws))

(defun py-aiohttp-websocket-from-url (url timeout)
  (when (py-aiohttp-timeout-expired-p timeout)
    (py-aiohttp-raise-timeout))
  (multiple-value-bind (status reason body content-type headers)
      (py-aiohttp-load-url-body "GET" url *py-none* (make-py-dict-from-pairs))
    (declare (ignore reason content-type headers))
    (when (>= status 400)
      (py-raise (make-py-exception *py-aiohttp-client-response-error-type* (format nil "HTTP ~A" status))))
    (py-aiohttp-make-websocket-response
     url
     (if (> (length body) 0)
         (list (py-aiohttp-ws-message *py-aiohttp-ws-msg-text* body))
         '()))))

(defun py-aiohttp-websocket-aenter (ws)
  (make-py-coroutine "ClientWebSocketResponse.__aenter__"
                     (lambda () ws)))

(defun py-aiohttp-websocket-aexit (ws exc-type exc-value traceback)
  (declare (ignore exc-type exc-value traceback))
  (make-py-coroutine "ClientWebSocketResponse.__aexit__"
                     (lambda ()
                       (py-aiohttp-websocket-close-now ws)
                       *py-false*)))

(defun py-aiohttp-websocket-await (ws)
  (py-iter (make-py-list ws)))

(defun py-aiohttp-websocket-close-now (ws)
  (setf (py-aiohttp-client-websocket-response-object-closed ws) t)
  (py-aiohttp-websocket-sync-closed ws)
  *py-true*)

(defun py-aiohttp-websocket-close (ws &rest args)
  (declare (ignore args))
  (make-py-coroutine "ClientWebSocketResponse.close"
                     (lambda ()
                       (py-aiohttp-websocket-close-now ws))))

(defun py-aiohttp-websocket-receive-now (ws)
  (let ((messages (py-aiohttp-client-websocket-response-object-messages ws)))
    (cond
      (messages
       (let ((message (first messages)))
         (setf (py-aiohttp-client-websocket-response-object-messages ws) (rest messages))
         message))
      ((py-aiohttp-client-websocket-response-object-closed ws)
       (py-aiohttp-ws-message *py-aiohttp-ws-msg-closed* *py-none*))
      (t
       (py-aiohttp-websocket-close-now ws)
       (py-aiohttp-ws-message *py-aiohttp-ws-msg-closed* *py-none*)))))

(defun py-aiohttp-websocket-receive (ws &rest args)
  (declare (ignore args))
  (make-py-coroutine "ClientWebSocketResponse.receive"
                     (lambda () (py-aiohttp-websocket-receive-now ws))))

(defun py-aiohttp-websocket-aiter (ws)
  ws)

(defun py-aiohttp-websocket-anext (ws)
  (make-py-coroutine "ClientWebSocketResponse.__anext__"
                     (lambda ()
                       (let ((message (py-aiohttp-websocket-receive-now ws)))
                         (if (member (py-aiohttp-ws-message-object-message-type message)
                                     (list *py-aiohttp-ws-msg-close*
                                           *py-aiohttp-ws-msg-closed*
                                           *py-aiohttp-ws-msg-error*))
                             (py-raise (make-py-exception *py-stop-async-iteration-type*))
                             message)))))

(defun py-aiohttp-websocket-receive-str (ws &rest args)
  (declare (ignore args))
  (make-py-coroutine "ClientWebSocketResponse.receive_str"
                     (lambda ()
                       (let ((message (py-aiohttp-websocket-receive-now ws)))
                         (if (= (py-aiohttp-ws-message-object-message-type message) *py-aiohttp-ws-msg-text*)
                             (py-aiohttp-ws-message-object-data message)
                             (py-raise (make-py-exception *py-type-error-type* "Received message is not str")))))))

(defun py-aiohttp-websocket-receive-bytes (ws &rest args)
  (declare (ignore args))
  (make-py-coroutine "ClientWebSocketResponse.receive_bytes"
                     (lambda ()
                       (let ((message (py-aiohttp-websocket-receive-now ws)))
                         (if (= (py-aiohttp-ws-message-object-message-type message) *py-aiohttp-ws-msg-binary*)
                             (py-aiohttp-ws-message-object-data message)
                             (py-raise (make-py-exception *py-type-error-type* "Received message is not bytes")))))))

(defun py-aiohttp-websocket-send-str (ws data &rest args)
  (declare (ignore args))
  (make-py-coroutine "ClientWebSocketResponse.send_str"
                     (lambda ()
                       (let ((text (py-str data)))
                         (push text (py-aiohttp-client-websocket-response-object-sent ws))
                         (setf (py-aiohttp-client-websocket-response-object-messages ws)
                               (append (py-aiohttp-client-websocket-response-object-messages ws)
                                       (list (py-aiohttp-ws-message *py-aiohttp-ws-msg-text* text)))))
                       *py-none*)))

(defun py-aiohttp-websocket-send-bytes (ws data &rest args)
  (declare (ignore args))
  (make-py-coroutine "ClientWebSocketResponse.send_bytes"
                     (lambda ()
                       (unless (py-bytes-object-p data)
                         (py-raise (make-py-exception *py-type-error-type* "data argument must be bytes-like")))
                       (push data (py-aiohttp-client-websocket-response-object-sent ws))
                       (setf (py-aiohttp-client-websocket-response-object-messages ws)
                             (append (py-aiohttp-client-websocket-response-object-messages ws)
                                     (list (py-aiohttp-ws-message *py-aiohttp-ws-msg-binary* data))))
                       *py-none*)))

(defun py-aiohttp-websocket-send-json (ws data &rest args)
  (declare (ignore args))
  (py-aiohttp-websocket-send-str ws (py-aiohttp-json-dumps data)))

(defun py-aiohttp-websocket-receive-json (ws &rest args)
  (declare (ignore args))
  (make-py-coroutine "ClientWebSocketResponse.receive_json"
                     (lambda ()
                       (py-aiohttp-json-loads
                        (py-await (py-aiohttp-websocket-receive-str ws))))))

(defun py-aiohttp-response-aenter (response)
  (make-py-coroutine "ClientResponse.__aenter__"
                     (lambda () response)))

(defun py-aiohttp-response-release (response)
  (setf (py-aiohttp-client-response-object-closed response) t)
  (setf (py-object-attr response "closed") *py-true*)
  *py-none*)

(defun py-aiohttp-response-close (response)
  (py-aiohttp-response-release response))

(defun py-aiohttp-response-wait-for-close (response)
  (make-py-coroutine "ClientResponse.wait_for_close"
                     (lambda ()
                       (py-aiohttp-response-release response))))

(defun py-aiohttp-response-aexit (response exc-type exc-value traceback)
  (declare (ignore exc-type exc-value traceback))
  (make-py-coroutine "ClientResponse.__aexit__"
                     (lambda ()
                       (py-aiohttp-response-release response)
                       *py-false*)))

(defun py-aiohttp-response-text (response &rest args)
  (declare (ignore args))
  (make-py-coroutine "ClientResponse.text"
                     (lambda () (py-aiohttp-client-response-object-body response))))

(defun py-aiohttp-response-read (response)
  (make-py-coroutine "ClientResponse.read"
                     (lambda ()
                       (py-aiohttp-string-bytes
                        (py-aiohttp-client-response-object-body response)))))

(defun py-aiohttp-response-get-encoding (response)
  (py-aiohttp-response-encoding response))

(defun py-aiohttp-make-response-error (type response message)
  (let ((exception (make-py-exception type message)))
    (setf (py-object-attr exception "status") (py-aiohttp-client-response-object-status response))
    (setf (py-object-attr exception "message") message)
    (setf (py-object-attr exception "headers")
          (py-aiohttp-client-response-object-headers response))
    (setf (py-object-attr exception "request_info")
          (make-py-dict-from-pairs
           (list "url" (py-aiohttp-client-response-object-url response))
           (list "method" (py-aiohttp-client-response-object-method response))))
    exception))

(defun py-aiohttp-content-type-matches-p (actual expected)
  (or (eq expected *py-none*)
      (let ((actual-text (string-downcase (or actual "")))
            (expected-text (string-downcase (py-str expected))))
        (or (string= expected-text "")
            (search expected-text actual-text)
            (and (string= expected-text "application/json")
                 (search "+json" actual-text))))))

(defun py-aiohttp-response-json (response &rest args)
  (multiple-value-bind (content-type content-type-supplied-p positional)
      (py-asyncio-keyword-value args :content_type "application/json")
    (declare (ignore positional))
    (let ((expected-content-type (if content-type-supplied-p content-type "application/json")))
      (make-py-coroutine "ClientResponse.json"
                         (lambda ()
                           (let ((actual (py-object-attr response "content_type")))
                             (unless (py-aiohttp-content-type-matches-p actual expected-content-type)
                               (py-raise
                                (py-aiohttp-make-response-error
                                 *py-aiohttp-content-type-error-type*
                                 response
                                 (format nil "Attempt to decode JSON with unexpected mimetype: ~A" actual))))
                             (py-aiohttp-json-loads
                              (py-aiohttp-client-response-object-body response))))))))

(defun py-aiohttp-response-raise-for-status (response)
  (let ((status (py-aiohttp-client-response-object-status response)))
    (when (>= status 400)
      (let* ((reason (py-aiohttp-client-response-object-reason response))
             (message (if (and (stringp reason) (> (length reason) 0))
                          reason
                          (format nil "HTTP ~A" status))))
        (py-raise
         (py-aiohttp-make-response-error
          *py-aiohttp-client-response-error-type*
          response
          message)))))
  *py-none*)

(defun py-aiohttp-session-request (session method url &rest args)
  (multiple-value-bind (keyword-data keyword-data-supplied-p positional)
      (py-asyncio-keyword-value args :data *py-none*)
    (declare (ignore positional))
    (multiple-value-bind (keyword-json keyword-json-supplied-p ignored-json-positional)
        (py-asyncio-keyword-value args :json *py-none*)
      (declare (ignore ignored-json-positional))
      (multiple-value-bind (keyword-params keyword-params-supplied-p ignored-params-positional)
          (py-asyncio-keyword-value args :params *py-none*)
        (declare (ignore ignored-params-positional))
        (multiple-value-bind (keyword-headers keyword-headers-supplied-p ignored-headers-positional)
            (py-asyncio-keyword-value args :headers *py-none*)
          (declare (ignore ignored-headers-positional))
          (multiple-value-bind (keyword-auth keyword-auth-supplied-p ignored-auth-positional)
              (py-asyncio-keyword-value args :auth *py-none*)
            (declare (ignore ignored-auth-positional))
            (multiple-value-bind (keyword-cookies keyword-cookies-supplied-p ignored-cookies-positional)
                (py-asyncio-keyword-value args :cookies *py-none*)
              (declare (ignore ignored-cookies-positional))
              (multiple-value-bind (keyword-raise-for-status keyword-raise-for-status-supplied-p ignored-raise-for-status-positional)
                  (py-asyncio-keyword-value args :raise_for_status *py-none*)
                (declare (ignore ignored-raise-for-status-positional))
                (multiple-value-bind (keyword-allow-redirects keyword-allow-redirects-supplied-p ignored-allow-redirects-positional)
                    (py-asyncio-keyword-value args :allow_redirects *py-true*)
                  (declare (ignore ignored-allow-redirects-positional))
                  (multiple-value-bind (keyword-max-redirects keyword-max-redirects-supplied-p ignored-max-redirects-positional)
                      (py-asyncio-keyword-value args :max_redirects 10)
                    (declare (ignore ignored-max-redirects-positional))
                    (multiple-value-bind (keyword-timeout keyword-timeout-supplied-p ignored-timeout-positional)
                        (py-asyncio-keyword-value args :timeout *py-none*)
                      (declare (ignore ignored-timeout-positional))
                (when (py-aiohttp-client-session-object-closed session)
                  (py-raise (make-py-exception *py-runtime-error-type* "Session is closed")))
                (when (and keyword-data-supplied-p keyword-json-supplied-p)
                  (py-raise (make-py-exception *py-value-error-type* "data and json parameters can not be used at the same time")))
                (let* ((joined-url (py-aiohttp-join-url
                                    (py-aiohttp-client-session-object-base-url session)
                                    url))
                       (request-url (if keyword-params-supplied-p
                                        (py-aiohttp-url-with-params joined-url keyword-params)
                                        joined-url))
                       (request-body (cond
                                       (keyword-json-supplied-p (py-aiohttp-json-dumps keyword-json))
                                       (keyword-data-supplied-p keyword-data)
                                       (t *py-none*)))
                       (request-headers-arg (if keyword-headers-supplied-p keyword-headers *py-none*))
                       (merged-headers (py-aiohttp-merge-headers
                                        (py-aiohttp-client-session-object-headers session)
                                        request-headers-arg))
                       (base-headers (if keyword-json-supplied-p
                                         (py-aiohttp-json-headers merged-headers)
                                         merged-headers))
                       (request-auth (if keyword-auth-supplied-p
                                         keyword-auth
                                         (py-aiohttp-client-session-object-auth session)))
                       (request-timeout (if keyword-timeout-supplied-p
                                            keyword-timeout
                                            (py-aiohttp-client-session-object-timeout session)))
                       (raise-for-status (if keyword-raise-for-status-supplied-p
                                             keyword-raise-for-status
                                             (py-aiohttp-client-session-object-raise-for-status session)))
                       (allow-redirects (if keyword-allow-redirects-supplied-p keyword-allow-redirects *py-true*))
                       (max-redirects (if keyword-max-redirects-supplied-p keyword-max-redirects 10))
                       (request-headers (py-aiohttp-request-headers
                                         base-headers
                                         request-body
                                         request-auth)))
                  (py-aiohttp-add-cookie-header
                   request-headers
                   (py-aiohttp-client-session-object-cookie-jar session)
                   (if keyword-cookies-supplied-p keyword-cookies *py-none*))
                  (make-py-aiohttp-request-context-object :type *py-aiohttp-request-context-type*
                                                          :session session
                                                          :method method
                                                          :url request-url
                                                          :headers request-headers
                                                          :body request-body
                                                          :timeout request-timeout
                                                          :raise-for-status raise-for-status
                                                          :allow-redirects allow-redirects
                                                          :max-redirects max-redirects)))))))))))))
(defun py-aiohttp-session-get (session url &rest args)
  (apply #'py-aiohttp-session-request session "GET" url args))

(defun py-aiohttp-session-post (session url &rest args)
  (apply #'py-aiohttp-session-request session "POST" url args))

(defun py-aiohttp-session-put (session url &rest args)
  (apply #'py-aiohttp-session-request session "PUT" url args))

(defun py-aiohttp-session-delete (session url &rest args)
  (apply #'py-aiohttp-session-request session "DELETE" url args))

(defun py-aiohttp-session-patch (session url &rest args)
  (apply #'py-aiohttp-session-request session "PATCH" url args))

(defun py-aiohttp-session-head (session url &rest args)
  (apply #'py-aiohttp-session-request session "HEAD" url args))

(defun py-aiohttp-session-options (session url &rest args)
  (apply #'py-aiohttp-session-request session "OPTIONS" url args))

(defun py-aiohttp-session-ws-connect (session url &rest args)
  (multiple-value-bind (keyword-timeout keyword-timeout-supplied-p positional)
      (py-asyncio-keyword-value args :timeout *py-none*)
    (declare (ignore positional))
    (when (py-aiohttp-client-session-object-closed session)
      (py-raise (make-py-exception *py-runtime-error-type* "Session is closed")))
    (let* ((request-url (py-aiohttp-join-url
                         (py-aiohttp-client-session-object-base-url session)
                         url))
           (timeout (if keyword-timeout-supplied-p
                        keyword-timeout
                        (py-aiohttp-client-session-object-timeout session))))
      (py-aiohttp-websocket-from-url request-url timeout))))

(defun py-aiohttp-module-request (method url &rest args)
  (apply #'py-aiohttp-session-request (py-aiohttp-client-session) method url args))

(defun py-aiohttp-module-get (url &rest args)
  (apply #'py-aiohttp-module-request "GET" url args))

(defun py-aiohttp-module-post (url &rest args)
  (apply #'py-aiohttp-module-request "POST" url args))

(defun py-aiohttp-module-put (url &rest args)
  (apply #'py-aiohttp-module-request "PUT" url args))

(defun py-aiohttp-module-delete (url &rest args)
  (apply #'py-aiohttp-module-request "DELETE" url args))

(defun py-aiohttp-module-patch (url &rest args)
  (apply #'py-aiohttp-module-request "PATCH" url args))

(defun py-aiohttp-module-head (url &rest args)
  (apply #'py-aiohttp-module-request "HEAD" url args))

(defun py-aiohttp-module-options (url &rest args)
  (apply #'py-aiohttp-module-request "OPTIONS" url args))

(defun py-aiohttp-module-ws-connect (url &rest args)
  (apply #'py-aiohttp-session-ws-connect (py-aiohttp-client-session) url args))

(setf (py-type-attr *py-aiohttp-client-session-type* "__aenter__") #'py-aiohttp-session-aenter)
(setf (py-type-attr *py-aiohttp-client-session-type* "__aexit__") #'py-aiohttp-session-aexit)
(setf (py-type-attr *py-aiohttp-client-session-type* "close") #'py-aiohttp-session-close)
(setf (py-type-attr *py-aiohttp-client-session-type* "detach") #'py-aiohttp-session-detach)
(setf (py-type-attr *py-aiohttp-client-session-type* "request") #'py-aiohttp-session-request)
(setf (py-type-attr *py-aiohttp-client-session-type* "get") #'py-aiohttp-session-get)
(setf (py-type-attr *py-aiohttp-client-session-type* "post") #'py-aiohttp-session-post)
(setf (py-type-attr *py-aiohttp-client-session-type* "put") #'py-aiohttp-session-put)
(setf (py-type-attr *py-aiohttp-client-session-type* "delete") #'py-aiohttp-session-delete)
(setf (py-type-attr *py-aiohttp-client-session-type* "patch") #'py-aiohttp-session-patch)
(setf (py-type-attr *py-aiohttp-client-session-type* "head") #'py-aiohttp-session-head)
(setf (py-type-attr *py-aiohttp-client-session-type* "options") #'py-aiohttp-session-options)
(setf (py-type-attr *py-aiohttp-client-session-type* "ws_connect") #'py-aiohttp-session-ws-connect)

(setf (py-type-attr *py-aiohttp-tcp-connector-type* "close") #'py-aiohttp-tcp-connector-close)

(setf (py-type-attr *py-aiohttp-basic-auth-type* "encode") #'py-aiohttp-basic-auth-encode)
(setf (py-type-attr *py-aiohttp-form-data-type* "add_field") #'py-aiohttp-form-data-add-field)
(setf (py-type-attr *py-aiohttp-form-data-type* "add_fields") #'py-aiohttp-form-data-add-fields)

(setf (py-type-attr *py-aiohttp-cookie-jar-type* "update_cookies") #'py-aiohttp-cookie-jar-update-cookies)
(setf (py-type-attr *py-aiohttp-cookie-jar-type* "filter_cookies") #'py-aiohttp-cookie-jar-filter-cookies)
(setf (py-type-attr *py-aiohttp-cookie-jar-type* "clear") #'py-aiohttp-cookie-jar-clear)

(setf (py-type-attr *py-aiohttp-request-context-type* "__aenter__") #'py-aiohttp-request-aenter)
(setf (py-type-attr *py-aiohttp-request-context-type* "__aexit__") #'py-aiohttp-request-aexit)
(setf (py-type-attr *py-aiohttp-request-context-type* "__await__") #'py-aiohttp-request-await)

(setf (py-type-attr *py-aiohttp-client-websocket-response-type* "__aenter__") #'py-aiohttp-websocket-aenter)
(setf (py-type-attr *py-aiohttp-client-websocket-response-type* "__aexit__") #'py-aiohttp-websocket-aexit)
(setf (py-type-attr *py-aiohttp-client-websocket-response-type* "__await__") #'py-aiohttp-websocket-await)
(setf (py-type-attr *py-aiohttp-client-websocket-response-type* "__aiter__") #'py-aiohttp-websocket-aiter)
(setf (py-type-attr *py-aiohttp-client-websocket-response-type* "__anext__") #'py-aiohttp-websocket-anext)
(setf (py-type-attr *py-aiohttp-client-websocket-response-type* "close") #'py-aiohttp-websocket-close)
(setf (py-type-attr *py-aiohttp-client-websocket-response-type* "receive") #'py-aiohttp-websocket-receive)
(setf (py-type-attr *py-aiohttp-client-websocket-response-type* "receive_str") #'py-aiohttp-websocket-receive-str)
(setf (py-type-attr *py-aiohttp-client-websocket-response-type* "receive_bytes") #'py-aiohttp-websocket-receive-bytes)
(setf (py-type-attr *py-aiohttp-client-websocket-response-type* "receive_json") #'py-aiohttp-websocket-receive-json)
(setf (py-type-attr *py-aiohttp-client-websocket-response-type* "send_str") #'py-aiohttp-websocket-send-str)
(setf (py-type-attr *py-aiohttp-client-websocket-response-type* "send_bytes") #'py-aiohttp-websocket-send-bytes)
(setf (py-type-attr *py-aiohttp-client-websocket-response-type* "send_json") #'py-aiohttp-websocket-send-json)

(setf (py-type-attr *py-aiohttp-client-response-type* "__aenter__") #'py-aiohttp-response-aenter)
(setf (py-type-attr *py-aiohttp-client-response-type* "__aexit__") #'py-aiohttp-response-aexit)
(setf (py-type-attr *py-aiohttp-client-response-type* "text") #'py-aiohttp-response-text)
(setf (py-type-attr *py-aiohttp-client-response-type* "read") #'py-aiohttp-response-read)
(setf (py-type-attr *py-aiohttp-client-response-type* "json") #'py-aiohttp-response-json)
(setf (py-type-attr *py-aiohttp-client-response-type* "get_encoding") #'py-aiohttp-response-get-encoding)
(setf (py-type-attr *py-aiohttp-client-response-type* "raise_for_status") #'py-aiohttp-response-raise-for-status)
(setf (py-type-attr *py-aiohttp-client-response-type* "release") #'py-aiohttp-response-release)
(setf (py-type-attr *py-aiohttp-client-response-type* "close") #'py-aiohttp-response-close)
(setf (py-type-attr *py-aiohttp-client-response-type* "wait_for_close") #'py-aiohttp-response-wait-for-close)

(setf (py-type-attr *py-aiohttp-stream-reader-type* "read") #'py-aiohttp-stream-reader-read)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "readany") #'py-aiohttp-stream-reader-readany)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "read_nowait") #'py-aiohttp-stream-reader-read-nowait)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "readchunk") #'py-aiohttp-stream-reader-readchunk)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "readexactly") #'py-aiohttp-stream-reader-readexactly)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "readline") #'py-aiohttp-stream-reader-readline)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "iter_chunked") #'py-aiohttp-stream-reader-iter-chunked)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "iter_any") #'py-aiohttp-stream-reader-iter-any)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "iter_chunks") #'py-aiohttp-stream-reader-iter-chunks)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "at_eof") #'py-aiohttp-stream-reader-at-eof)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "is_eof") #'py-aiohttp-stream-reader-is-eof)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "exception") #'py-aiohttp-stream-reader-exception)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "feed_eof") #'py-aiohttp-stream-reader-feed-eof)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "__aiter__") #'py-aiohttp-stream-reader-aiter)
(setf (py-type-attr *py-aiohttp-stream-reader-type* "__anext__") #'py-aiohttp-stream-reader-anext)

(setf (py-type-attr *py-aiohttp-chunk-iterator-type* "__aiter__")
      (lambda (iterator) iterator))
(setf (py-type-attr *py-aiohttp-chunk-iterator-type* "__anext__") #'py-aiohttp-chunk-iterator-anext)

(defun py-aiohttp-export-client-api (module)
  (setf (py-object-attr module "ClientSession") #'py-aiohttp-client-session)
  (setf (py-object-attr module "request") #'py-aiohttp-module-request)
  (setf (py-object-attr module "get") #'py-aiohttp-module-get)
  (setf (py-object-attr module "post") #'py-aiohttp-module-post)
  (setf (py-object-attr module "put") #'py-aiohttp-module-put)
  (setf (py-object-attr module "delete") #'py-aiohttp-module-delete)
  (setf (py-object-attr module "patch") #'py-aiohttp-module-patch)
  (setf (py-object-attr module "head") #'py-aiohttp-module-head)
  (setf (py-object-attr module "options") #'py-aiohttp-module-options)
  (setf (py-object-attr module "ws_connect") #'py-aiohttp-module-ws-connect)
  (setf (py-object-attr module "ClientTimeout") #'py-aiohttp-client-timeout)
  (setf (py-object-attr module "TCPConnector") #'py-aiohttp-tcp-connector)
  (setf (py-object-attr module "BasicAuth") #'py-aiohttp-basic-auth)
  (setf (py-object-attr module "FormData") #'py-aiohttp-form-data)
  (setf (py-object-attr module "CookieJar") #'py-aiohttp-cookie-jar)
  (setf (py-object-attr module "ClientResponse") *py-aiohttp-client-response-type*)
  (setf (py-object-attr module "ClientWebSocketResponse") *py-aiohttp-client-websocket-response-type*)
  (setf (py-object-attr module "WSMessage") *py-aiohttp-ws-message-type*)
  (setf (py-object-attr module "WSMsgType") (py-aiohttp-ws-msg-type-object))
  module)

(defun py-aiohttp-ws-msg-type-object ()
  (let ((obj (make-py-object :type *py-object-type*)))
    (setf (py-object-attr obj "TEXT") *py-aiohttp-ws-msg-text*)
    (setf (py-object-attr obj "BINARY") *py-aiohttp-ws-msg-binary*)
    (setf (py-object-attr obj "CLOSE") *py-aiohttp-ws-msg-close*)
    (setf (py-object-attr obj "PING") *py-aiohttp-ws-msg-ping*)
    (setf (py-object-attr obj "PONG") *py-aiohttp-ws-msg-pong*)
    (setf (py-object-attr obj "CLOSED") *py-aiohttp-ws-msg-closed*)
    (setf (py-object-attr obj "ERROR") *py-aiohttp-ws-msg-error*)
    obj))

(defun py-aiohttp-export-exceptions (module)
  (setf (py-object-attr module "ClientError") *py-aiohttp-client-error-type*)
  (setf (py-object-attr module "ClientResponseError") *py-aiohttp-client-response-error-type*)
  (setf (py-object-attr module "ContentTypeError") *py-aiohttp-content-type-error-type*)
  (setf (py-object-attr module "ClientConnectionError") *py-aiohttp-client-connection-error-type*)
  (setf (py-object-attr module "ClientConnectorError") *py-aiohttp-client-connector-error-type*)
  (setf (py-object-attr module "ClientPayloadError") *py-aiohttp-client-payload-error-type*)
  (setf (py-object-attr module "InvalidURL") *py-aiohttp-invalid-url-type*)
  (setf (py-object-attr module "TooManyRedirects") *py-aiohttp-too-many-redirects-type*)
  (setf (py-object-attr module "ServerTimeoutError") *py-aiohttp-server-timeout-error-type*)
  module)

(defun py-aiohttp-export-all (module)
  (py-aiohttp-export-client-api module)
  (py-aiohttp-export-exceptions module)
  module)

(defun make-clamp-aiohttp-module ()
  (let ((module (make-clamp-module "aiohttp")))
    (setf (py-object-attr module "__doc__") "Clamp built-in aiohttp-compatible offline module")
    (setf (py-object-attr module "__all__")
          (make-py-list
           "ClientSession" "request" "get" "post" "put" "delete" "patch" "head" "options" "ws_connect"
           "ClientTimeout" "TCPConnector" "BasicAuth" "FormData" "CookieJar" "ClientResponse" "ClientWebSocketResponse" "WSMessage" "WSMsgType"
           "ClientError" "ClientResponseError" "ContentTypeError" "ClientConnectionError"
           "ClientConnectorError" "ClientPayloadError" "InvalidURL" "TooManyRedirects" "ServerTimeoutError"))
    (py-aiohttp-export-all module)))

(defun make-clamp-aiohttp-client-module ()
  (let ((module (make-clamp-module "aiohttp.client")))
    (setf (py-object-attr module "__doc__") "Clamp built-in aiohttp.client compatibility module")
    (setf (py-object-attr module "__all__")
          (make-py-list
           "ClientSession" "request" "get" "post" "put" "delete" "patch" "head" "options" "ws_connect"
           "ClientTimeout" "TCPConnector" "BasicAuth" "FormData" "CookieJar" "ClientResponse" "ClientWebSocketResponse" "WSMessage" "WSMsgType"))
    (py-aiohttp-export-client-api module)))

(defun make-clamp-aiohttp-client-exceptions-module ()
  (let ((module (make-clamp-module "aiohttp.client_exceptions")))
    (setf (py-object-attr module "__doc__") "Clamp built-in aiohttp.client_exceptions compatibility module")
    (setf (py-object-attr module "__all__")
          (make-py-list
           "ClientError" "ClientResponseError" "ContentTypeError" "ClientConnectionError"
           "ClientConnectorError" "ClientPayloadError" "InvalidURL" "TooManyRedirects" "ServerTimeoutError"))
    (py-aiohttp-export-exceptions module)))

(defun make-clamp-aiohttp-connector-module ()
  (let ((module (make-clamp-module "aiohttp.connector")))
    (setf (py-object-attr module "__doc__") "Clamp built-in aiohttp.connector compatibility module")
    (setf (py-object-attr module "__all__") (make-py-list "TCPConnector"))
    (setf (py-object-attr module "TCPConnector") #'py-aiohttp-tcp-connector)
    module))

(defun make-clamp-aiohttp-client-reqrep-module ()
  (let ((module (make-clamp-module "aiohttp.client_reqrep")))
    (setf (py-object-attr module "__doc__") "Clamp built-in aiohttp.client_reqrep compatibility module")
    (setf (py-object-attr module "__all__") (make-py-list "ClientResponse" "ClientWebSocketResponse" "WSMessage"))
    (setf (py-object-attr module "ClientResponse") *py-aiohttp-client-response-type*)
    (setf (py-object-attr module "ClientWebSocketResponse") *py-aiohttp-client-websocket-response-type*)
    (setf (py-object-attr module "WSMessage") *py-aiohttp-ws-message-type*)
    module))

(defun py-aiohttp-set-header-constants (module)
  (setf (py-object-attr module "METH_ANY") "*")
  (setf (py-object-attr module "METH_CONNECT") "CONNECT")
  (setf (py-object-attr module "METH_DELETE") "DELETE")
  (setf (py-object-attr module "METH_GET") "GET")
  (setf (py-object-attr module "METH_HEAD") "HEAD")
  (setf (py-object-attr module "METH_OPTIONS") "OPTIONS")
  (setf (py-object-attr module "METH_PATCH") "PATCH")
  (setf (py-object-attr module "METH_POST") "POST")
  (setf (py-object-attr module "METH_PUT") "PUT")
  (setf (py-object-attr module "METH_TRACE") "TRACE")
  (setf (py-object-attr module "ACCEPT") "ACCEPT")
  (setf (py-object-attr module "ACCEPT_ENCODING") "ACCEPT-ENCODING")
  (setf (py-object-attr module "AUTHORIZATION") "AUTHORIZATION")
  (setf (py-object-attr module "CONTENT_LENGTH") "CONTENT-LENGTH")
  (setf (py-object-attr module "CONTENT_TYPE") "CONTENT-TYPE")
  (setf (py-object-attr module "COOKIE") "COOKIE")
  (setf (py-object-attr module "HOST") "HOST")
  (setf (py-object-attr module "LOCATION") "LOCATION")
  (setf (py-object-attr module "SET_COOKIE") "SET-COOKIE")
  (setf (py-object-attr module "USER_AGENT") "USER-AGENT")
  module)

(defun make-clamp-aiohttp-hdrs-module ()
  (let ((module (make-clamp-module "aiohttp.hdrs")))
    (setf (py-object-attr module "__doc__") "Clamp built-in aiohttp.hdrs compatibility module")
    (setf (py-object-attr module "__all__")
          (make-py-list
           "METH_ANY" "METH_CONNECT" "METH_DELETE" "METH_GET" "METH_HEAD" "METH_OPTIONS"
           "METH_PATCH" "METH_POST" "METH_PUT" "METH_TRACE" "ACCEPT" "ACCEPT_ENCODING"
           "AUTHORIZATION" "CONTENT_LENGTH" "CONTENT_TYPE" "COOKIE" "HOST" "LOCATION"
           "SET_COOKIE" "USER_AGENT"))
    (py-aiohttp-set-header-constants module)))

(defun make-clamp-aiohttp-helpers-module ()
  (let ((module (make-clamp-module "aiohttp.helpers")))
    (setf (py-object-attr module "__doc__") "Clamp built-in aiohttp.helpers compatibility module")
    (setf (py-object-attr module "__all__") (make-py-list "BasicAuth"))
    (setf (py-object-attr module "BasicAuth") #'py-aiohttp-basic-auth)
    module))

(py-register-builtin-module "aiohttp" #'make-clamp-aiohttp-module)
(py-register-builtin-module "aiohttp.client" #'make-clamp-aiohttp-client-module)
(py-register-builtin-module "aiohttp.client_exceptions" #'make-clamp-aiohttp-client-exceptions-module)
(py-register-builtin-module "aiohttp.connector" #'make-clamp-aiohttp-connector-module)
(py-register-builtin-module "aiohttp.client_reqrep" #'make-clamp-aiohttp-client-reqrep-module)
(py-register-builtin-module "aiohttp.hdrs" #'make-clamp-aiohttp-hdrs-module)
(py-register-builtin-module "aiohttp.helpers" #'make-clamp-aiohttp-helpers-module)


(defun py-keyword-argument-name (keyword)
  (string-downcase (symbol-name keyword)))

(defun py-bind-args (function-name param-names required-count defaults call-args)
  (let* ((param-count (length param-names))
         (values (make-array param-count :initial-element nil))
         (supplied (make-array param-count :initial-element nil))
         (pos-index 0)
         (seen-keyword nil)
         (remaining call-args))
    (labels ((param-index (name)
               (position name param-names :test #'string=))
             (mark-value (index value source-name)
               (when (aref supplied index)
                 (error "~A() got multiple values for argument '~A'"
                        function-name source-name))
               (setf (aref values index) value)
               (setf (aref supplied index) t)))
      (loop while remaining
            do (let ((item (pop remaining)))
                 (cond
                   ((keywordp item)
                    (setf seen-keyword t)
                    (unless remaining
                      (error "~A() keyword argument ~A has no value"
                             function-name item))
                    (let* ((name (py-keyword-argument-name item))
                           (index (param-index name)))
                      (unless index
                        (error "~A() got an unexpected keyword argument '~A'"
                               function-name name))
                      (mark-value index (pop remaining) name)))
                   (seen-keyword
                    (error "~A() positional argument follows keyword argument"
                           function-name))
                   (t
                    (when (>= pos-index param-count)
                      (error "~A() takes ~A positional arguments but more were given"
                             function-name param-count))
                    (mark-value pos-index item (nth pos-index param-names))
                    (incf pos-index))))))
    (loop for index from 0 below param-count
          collect (cond
                    ((aref supplied index)
                     (aref values index))
                    ((< index required-count)
                     (error "~A() missing required argument: '~A'"
                            function-name (nth index param-names)))
                    (t
                     (nth (- index required-count) defaults))))))

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
  (when (and (py-module-spec-object-p obj) (string= name "__dict__"))
    (return-from py-lookup-attr (py-module-spec-dict obj)))
  (when (and (py-source-file-loader-object-p obj) (string= name "__dict__"))
    (return-from py-lookup-attr (py-source-file-loader-dict obj)))
  (when (and (py-file-reader-object-p obj) (string= name "__dict__"))
    (return-from py-lookup-attr (py-file-reader-dict obj)))
  (when (and (py-path-object-p obj) (string= name "parent"))
    (return-from py-lookup-attr (py-path-parent obj)))
  (when (and (py-path-object-p obj) (string= name "suffix"))
    (return-from py-lookup-attr (py-path-suffix obj)))
  (when (and (py-path-object-p obj) (string= name "suffixes"))
    (return-from py-lookup-attr (py-path-suffixes obj)))
  (when (and (py-path-object-p obj) (string= name "stem"))
    (return-from py-lookup-attr (py-path-stem obj)))
  (when (py-object-p obj)
    (multiple-value-bind (attr found) (gethash name (py-object-attrs obj))
      (when found
        (return-from py-lookup-attr attr))))
  (multiple-value-bind (attr found) (py-find-type-attr (py-type-of obj) name)
    (when found
      (return-from py-lookup-attr attr)))
  (py-raise-type
   *py-attribute-error-type*
   (format nil "Python object of type ~A has no attribute ~S"
           (py-type-name (py-type-of obj))
           name)))

(defun py-instantiate-type (type &rest args)
  (when (py-type-subtype-p type *py-base-exception-type*)
    (return-from py-instantiate-type (apply #'make-py-exception type args)))
  (when (eq type *py-source-file-loader-type*)
    (destructuring-bind (fullname path) args
      (return-from py-instantiate-type
        (make-clamp-source-file-loader fullname path))))
  (when (eq type *py-file-reader-type*)
    (let ((reader (make-py-file-reader-object :type *py-file-reader-type*)))
      (when args
        (apply (py-type-attr *py-file-reader-type* "__init__") reader args))
      (return-from py-instantiate-type reader)))
  (let ((instance (make-py-instance type)))
    (multiple-value-bind (initializer found) (py-find-type-attr type "__init__")
      (when found
        (let ((result (apply #'py-invoke-callable initializer instance args)))
          (unless (eq result *py-none*)
            (error "__init__() should return None, not ~A"
                   (py-type-name (py-type-of result)))))))
    instance))

(defun py-invoke-callable (callable &rest args)
  (cond
    ((py-callable-p callable)
     (apply (py-callable-fn callable) args))
    ((functionp callable)
     (apply callable args))
    ((py-type-p callable)
     (apply #'py-instantiate-type callable args))
    (t
     (error "Python attribute is not callable: ~S" callable))))

(defun py-callable (value)
  (py-bool
   (or (functionp value)
       (py-callable-p value)
       (py-type-p value))))

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

(defstruct (py-dict-key-iterator-object (:include py-object))
  dict
  (index 0)
  (used 0)
  (remaining 0))

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

(defun py-dict-setdefault (obj key default)
  (let ((storage (py-dict-storage obj "setdefault")))
    (multiple-value-bind (value found)
        (gethash key storage)
      (if found
          value
          (py-dict-set-entry obj key default)))))

(defun py-dict-update (obj &optional (other *py-none*))
  (unless (eq other *py-none*)
    (let ((other-storage (py-dict-storage other "update"))
          (other-keys (py-dict-object-keys other)))
      (loop for index from 0 below (fill-pointer other-keys)
            for key = (aref other-keys index)
            do (py-dict-set-entry obj key (gethash key other-storage)))))
  *py-none*)

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

(defun py-module-spec-dict (spec)
  (or (py-module-spec-object-namespace-dict spec)
      (setf (py-module-spec-object-namespace-dict spec)
            (make-py-dict-for-storage (py-object-attrs spec) spec))))

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
  (let ((path-string (py-path-string path)))
    (handler-case
        (with-open-file (stream path-string :direction :input
                                     :element-type (quote (unsigned-byte 8)))
          (let* ((size (file-length stream))
                 (storage (make-array size :element-type (quote (unsigned-byte 8)))))
            (read-sequence storage stream)
            (make-py-bytes-from-vector storage)))
      (file-error ()
        (py-raise (make-py-exception *py-file-not-found-error-type* path-string)))
      (sb-int:simple-file-error ()
        (py-raise (make-py-exception *py-file-not-found-error-type* path-string))))))

(defun py-write-file-bytes (path data)
  (let ((path-string (py-path-string path))
        (storage (py-bytes-storage data "set_data")))
    (handler-case
        (progn
          (ensure-directories-exist path-string)
          (with-open-file (stream path-string :direction :output
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
  (with-open-file (stream (py-path-string path) :direction :input
                               :element-type (quote (unsigned-byte 8)))
    (file-length stream)))

(defun py-path-mtime (path)
  (- (coerce (file-write-date (py-path-string path)) 'double-float)
     2208988800.0d0))

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

(setf (py-type-attr *py-dict-type* "setdefault")
      (lambda (obj key &optional (default *py-none*))
        (py-dict-setdefault obj key default)))

(setf (py-type-attr *py-dict-type* "update")
      (lambda (obj &optional (other *py-none*))
        (py-dict-update obj other)))

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
           (eq (py-object-type obj) *py-dict-key-iterator-type*)
           (eq (py-object-type obj) *py-buffered-reader-type*)
           (eq (py-object-type obj) *py-asyncio-as-completed-type*))))

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

(defun py-dict-key-iterator-p (obj)
  (and (py-object-p obj)
       (eq (py-object-type obj) *py-dict-key-iterator-type*)))

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
    ((eq (py-object-type obj) *py-dict-type*)
     (make-py-dict-key-iterator-object :type *py-dict-key-iterator-type*
                                       :dict obj
                                       :index 0
                                       :used (or (py-object-size obj) 0)
                                       :remaining (or (py-object-size obj) 0)))
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
    ((py-dict-key-iterator-p iterator)
     (let* ((dict (py-dict-key-iterator-object-dict iterator))
            (used (py-dict-key-iterator-object-used iterator))
            (current-size (and dict (or (py-object-size dict) 0))))
       (unless dict
         (py-raise *py-stop-iteration*))
       (unless (= used current-size)
         (setf (py-dict-key-iterator-object-used iterator) -1)
         (error "dictionary changed size during iteration"))
       (let* ((keys (py-dict-object-keys dict))
              (index (py-dict-key-iterator-object-index iterator))
              (remaining (py-dict-key-iterator-object-remaining iterator)))
         (if (and (< index (fill-pointer keys)) (> remaining 0))
             (prog1
                 (aref keys index)
               (setf (py-dict-key-iterator-object-index iterator) (1+ index))
               (setf (py-dict-key-iterator-object-remaining iterator) (1- remaining)))
             (progn
               (setf (py-dict-key-iterator-object-dict iterator) nil)
               (py-raise *py-stop-iteration*))))))
    ((py-buffered-reader-object-p iterator)
     (let ((line (py-buffered-reader-readline iterator)))
       (if (> (or (py-object-size line) 0) 0)
           line
           (py-raise *py-stop-iteration*))))
    ((py-asyncio-as-completed-object-p iterator)
     (py-asyncio-as-completed-next iterator))
    (t
     (error "Expected Python iterator, got ~S" iterator))))

(defun py-next-item (iterator)
  (handler-case
      (values (py-next iterator) t)
    (py-exception (condition)
      (if (py-stop-iteration-p condition)
          (values nil nil)
          (error condition)))))

(defun py-unpack-sequence (value expected-count)
  (let ((iterator (py-iter value))
        (items '()))
    (loop for index from 0 below expected-count
          do (multiple-value-bind (item found) (py-next-item iterator)
               (unless found
                 (error "not enough values to unpack (expected ~A, got ~A)"
                        expected-count index))
               (push item items)))
    (multiple-value-bind (extra found) (py-next-item iterator)
      (declare (ignore extra))
      (when found
        (error "too many values to unpack (expected ~A)" expected-count)))
    (nreverse items)))

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

(defun py-dict-key-iterator-length-hint (iterator)
  (let ((dict (py-dict-key-iterator-object-dict iterator)))
    (if (and dict
             (= (py-dict-key-iterator-object-used iterator)
                (or (py-object-size dict) 0)))
        (max (py-dict-key-iterator-object-remaining iterator) 0)
        0)))

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

(setf (py-type-attr *py-dict-type* "__iter__")
      (lambda (obj)
        (py-iter obj)))

(setf (py-type-attr *py-dict-key-iterator-type* "__iter__")
      (lambda (iterator)
        (py-iter iterator)))

(setf (py-type-attr *py-dict-key-iterator-type* "__next__")
      (lambda (iterator)
        (py-next iterator)))

(setf (py-type-attr *py-dict-key-iterator-type* "__length_hint__")
      (lambda (iterator)
        (py-dict-key-iterator-length-hint iterator)))

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
    ((py-exception-object-p value) (princ (py-exception-message value) stream))
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
    ((py-dict-key-iterator-p value) (princ "<dict_keyiterator>" stream))
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
