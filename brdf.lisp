;;;; BRDF - Bitcoin chain data represented as RDF

;; Fail here if the Lisp image is not an AllegroCL one.
#-allegro
(error "Direct AllegroGraph interface only works in AllegroCL.")

;; Load `agraph.fasl' dependency unless it's already present.
#-allegrograph
(load "agraph.fasl")

;; Finally, load external dependencies `jsown' and `bp' using Quicklisp.
(ql:quickload "jsown")
(ql:quickload "bp")

(defpackage :brdf
  (:use :cl :bp :db.agraph)
  (:import-from :bp.core.script
                #:*print-script-as-assembly*)
  (:import-from :db.agraph.triple-store-spec
                #:parse-triple-store-specification
                #:ground-store-address-p
                #:ground-store-address-scheme
                #:ground-store-address-host
                #:ground-store-address-port
                #:ground-store-address-user
                #:ground-store-address-password
                #:ground-store-address-catalog
                #:ground-store-address-repo)
  (:import-from :db.agraph.part-constants
                #:+rdf-type-uri+
                #:+xsd-string-uri+
                #:+xsd-integer-upi+)
  (:import-from :db.agraph.http.store
                #:@client)
  (:import-from :db.agraph.http.client
                #:define-namespace
                #:repository-data)
  (:import-from :st-json
                #:jso
                #:write-json-to-string)
  (:export #:prepare-repository
           #:start-load
           #:stop-load
           #:*status-check-period*))

(in-package :brdf)


;;; ----------------------------------------------------------------------------
;;; Model

(defconstant +brdf-vocabulary+ (merge-pathnames "brdf.ttl" *load-truename*))

(defconstant +brdf-namespace+ "http://github.com/rodentrabies/brdf#")

;; Function that converts a Bitcoin block with hash `block-hash' at
;; height `block-height' into triples according to the model given by
;; `graph'. Assumes a triple store is avaliable in `db.agraph:*db*'
;; and a chain supplier is available in `bp:*chain-supplier*'.
(defgeneric load-block (graph block-height block-hash))

(defun type-to-rdf-datatype (type)
  (ecase type
    (:integer  +xsd-integer-upi+)
    (:string   +xsd-string-uri+)
    (:datetime "http://www.w3.org/2001/XMLSchema#dateTime")))

(defun unix-to-date-time (timestamp)
  (excl:universal-time-to-string (excl.osi:unix-to-universal-time timestamp) :format :iso8601))

(defmacro %r (name)
  `(resource ,name "bp"))

(defmacro %l (value type)
  `(literal (format nil "~a" ,value) :datatype (type-to-rdf-datatype ,type)))

(defmethod load-block ((graph (eql :chain)) block-height block-hash)
  (let* ((block-hex (get-block block-hash :encoded t))
         (block (decode 'cblock block-hex))
         (block-version (block-version block))
         (block-size (/ (length block-hex) 2))
         (block-time (unix-to-date-time (block-timestamp block)))
         (block-transactions (block-transactions block))
         (block-transactions-count (length block-transactions))
         (block-resource (%r (format nil "~a" block-height))))
    ;; Add block information triples.
    (add-triple block-resource +rdf-type-uri+      (%r "Block"))
    (add-triple block-resource (%r "blockHeight")  (%l block-height :integer))
    (add-triple block-resource (%r "blockHash")    (%l block-hash :string))
    (add-triple block-resource (%r "blockVersion") (%l block-version :integer))
    (add-triple block-resource (%r "blockSize")    (%l block-size :integer))
    (add-triple block-resource (%r "blockTime")    (%l block-time :datetime))
    (add-triple block-resource (%r "blockTxCount") (%l block-transactions-count :integer))
    ;; Link previous/next blocks.
    (when (plusp block-height)
      (let ((previous-block-resource (%r (format nil "~a" (1- block-height)))))
        (add-triple block-resource          (%r "blockPreviousBlock") previous-block-resource)
        (add-triple previous-block-resource (%r "blockNextBlock")     block-resource)))
    ;; Load block's transactions.
    (loop :for tx-index :below block-transactions-count :do
      (let* ((tx (aref block-transactions tx-index))
             (tx-id (tx-id tx))
             (tx-version (tx-version tx))
             (tx-size (/ (length (encode tx)) 2))
             (tx-locktime (tx-locktime tx))
             (tx-inputs (tx-inputs tx))
             (tx-inputs-count (length tx-inputs))
             (tx-outputs (tx-outputs tx))
             (tx-outputs-count (length tx-outputs))
             (tx-resource (%r tx-id)))
        ;; Link transaction with the block.
        (add-triple block-resource (%r "blockTx") tx-resource)
        (add-triple tx-resource    (%r "txIndex") (%l tx-index :integer))
        ;; Add transaction data.
        (add-triple tx-resource +rdf-type-uri+       (%r "Tx"))
        (add-triple tx-resource (%r "txID")          (%l tx-id :string))
        (add-triple tx-resource (%r "txVersion")     (%l tx-version :integer))
        (add-triple tx-resource (%r "txSize")        (%l tx-size :integer))
        (add-triple tx-resource (%r "txLockTime")    (%l tx-locktime :integer))
        (add-triple tx-resource (%r "txInputCount")  (%l tx-inputs-count :integer))
        (add-triple tx-resource (%r "txOutputCount") (%l tx-outputs-count :integer))
        ;; Load transaction inputs.
        (loop :for input-index :below tx-inputs-count :do
          (let* ((input (aref tx-inputs input-index))
                 (input-prevout-id (txin-previous-tx-id input))
                 (input-prevout-index (txin-previous-tx-index input))
                 (input-sequence (txin-sequence input))
                 (input-script-sig (txin-script-sig input))
                 (input-resource (new-blank-node)))
            ;; Link input with the transaction.
            (add-triple tx-resource    (%r "txInput")    input-resource)
            (add-triple input-resource (%r "inputIndex") (%l input-index :integer))
            ;; Add input data.
            (add-triple input-resource +rdf-type-uri+       (%r "Input"))
            (add-triple input-resource (%r "inputSequence") (%l input-sequence :integer))
            (add-triple input-resource (%r "inputScript")   (%l input-script-sig :string))
            ;; Link input with the output that it spends unless this is a coinbase transaction.
            (when (not (every #'zerop input-prevout-id))
              (let* ((input-prevout-id-hex (hex-encode (reverse input-prevout-id)))
                     (input-previous-output (format nil "~a:~a" input-prevout-id-hex input-prevout-index)))
                (add-triple input-resource (%r "inputPreviousOutput") (%r input-previous-output))))))
        ;; Load transaction outputs.
        (loop :for output-index :below tx-outputs-count :do
          (let* ((output (aref tx-outputs output-index))
                 (output-amount (txout-amount output))
                 (output-script-pubkey (txout-script-pubkey output))
                 (output-resource (%r (format nil "~a:~a" tx-id output-index))))
            ;; Link output with the transaction.
            (add-triple tx-resource     (%r "txOutput")    output-resource)
            (add-triple output-resource (%r "outputIndex") (%l output-index :integer))
            ;; Add output data.
            (add-triple output-resource +rdf-type-uri+      (%r "Output"))
            (add-triple output-resource (%r "outputAmount") (%l output-amount :integer))
            (add-triple output-resource (%r "outputScript") (%l output-script-pubkey :string))
            ;; Output type will be non-NIL only for standard scripts.
            (multiple-value-bind (output-type output-address)
                (script-standard-p output-script-pubkey :network (network))
              (when output-type
                (add-triple output-resource (%r "outputType") (%r (format nil "~a" output-type))))
              (when output-address
                (add-triple output-resource (%r "outputAddress") (%l output-address :string))))))))))

(defmethod load-block ((graph (eql :transactions)) block-height block-hash)
  (let* ((block (decode 'cblock (get-block block-hash :encoded t)))
         (block-resource (%r (format nil "~a" block-height)))
         (block-time (unix-to-date-time (block-timestamp block))))
    ;; Create block resource.
    (add-triple block-resource +rdf-type-uri+ (%r "Block"))
    (add-triple block-resource (%r "blockTime") (%l block-time :datetime))
    (loop :for tx-index :below (length (block-transactions block)) :do
      (let* ((tx (aref (block-transactions block) tx-index))
             (tx-id (tx-id tx))
             (tx-resource (%r tx-id))
             (tx-input-count (length (tx-inputs tx)))
             (tx-output-count (length (tx-outputs tx))))
        ;; Create transaction resource.
        (add-triple block-resource (%r "blockTx") tx-resource)
        (add-triple tx-resource +rdf-type-uri+ (%r "Tx"))
        (add-triple tx-resource (%r "txInputCount") (%l tx-input-count :integer))
        (add-triple tx-resource (%r "txOutputCount") (%l tx-output-count :integer))
        ;; Link transaction with input outpoints.
        (loop :for input-index :below tx-input-count :do
          (let* ((input (aref (tx-inputs tx) input-index))
                 (input-prevout-id (txin-previous-tx-id input))
                 (input-prevout-index (txin-previous-tx-index input)))
            (when (not (every #'zerop input-prevout-id))
              ;; We expect outputs to be described by their generating transactions,
              ;; so only a link here.
              (let* ((input-prevout-id-hex (hex-encode (reverse input-prevout-id)))
                     (input-previous-output (format nil "~a:~a" input-prevout-id-hex input-prevout-index)))
                (add-triple (%r input-previous-output) (%r "outputInputTx") tx-resource)))))
        ;; Link transaction with outputs.
        (loop :for output-index :below tx-output-count :do
          (let* ((output (aref (tx-outputs tx) output-index))
                 (output-amount (txout-amount output))
                 (output-script (txout-script-pubkey output))
                 (output-resource (%r (format nil "~a:~a" tx-id output-index))))
            ;; Link output with the transaction.
            (add-triple tx-resource (%r "txOutput") output-resource)
            (add-triple output-resource +rdf-type-uri+ (%r "Output"))
            (add-triple output-resource (%r "outputAmount") (%l output-amount :integer))
            ;; Output address will be non-NIL only for standard scripts.
            (multiple-value-bind (_ output-address)
                (script-standard-p output-script :network (network))
              (declare (ignore _))
              (if output-address
                  (add-triple output-resource (%r "outputAddress") (%l output-address :string))
                  (add-triple output-resource (%r "outputScript") (%l output-script :string))))))))))


;;; ----------------------------------------------------------------------------
;;; Loader scaffolding

(defvar *workers* nil
  "A place to store loader processes to be able to conveniently stop
them at once.")

(defvar *status-check-period* 10)

(define-condition stop-worker () ())

(defmacro with-open-remote-triple-store ((var spec &rest args) &body body)
  "Hack-macro that forces a triple store specified by SPEC to be
opened as REMOTE-TRIPLE-STORE."
  (let ((parsed-spec (gensym "parsed-spec")))
    `(let ((,parsed-spec (parse-triple-store-specification ,spec)))
       (assert (ground-store-address-p ,parsed-spec))
       (with-open-triple-store (,var (ground-store-address-repo ,parsed-spec)
                                     :triple-store-class 'remote-triple-store
                                     :scheme (ground-store-address-scheme ,parsed-spec)
                                     :server (ground-store-address-host ,parsed-spec)
                                     :port (ground-store-address-port ,parsed-spec)
                                     :user (ground-store-address-user ,parsed-spec)
                                     :password (ground-store-address-password ,parsed-spec)
                                     :catalog (ground-store-address-catalog ,parsed-spec)
                                     ,@args)
         ,@body))))

(defun prepare-repository (dst &optional queries-directory)
  ;; First create/open repository as remote one and make some things
  ;; persistent.
  (with-open-remote-triple-store (db dst :if-does-not-exist :create)
    (let ((client (@client db)))
      ;; Persistently set the main namespace.
      (define-namespace client "bp" +brdf-namespace+ :type :repository)
      ;; Add a bunch of useful saved queries.
      (when queries-directory
        (flet ((save-query (file)
                 (let* ((name (pathname-name file))
                        (query (excl:file-contents file))
                        (saved-query (jso "title" name "query" query "language" "SPARQL")))
                   (setf (repository-data client (format nil "wv.query.~a" name))
                         (write-json-to-string saved-query))))
               (query-file-p (file)
                 (equal (pathname-type file) "rq")))
          (excl:map-over-directory #'save-query queries-directory :filter #'query-file-p)))))
  ;; Perform reinitialization *only* if no triples in the triple
  ;; store. Shot myself in the foot too many times to expose the
  ;; CLEANP argument.
  (with-open-triple-store (db dst :if-does-not-exist :error)
    (when (= (triple-count :db db) 0)
      (drop-index :gposi)
      (drop-index :gspoi)
      (load-turtle +brdf-vocabulary+ :db db :commit t))))

(defun start-load (graph src dst &key workers from-height to-height)
  (when *workers*
    (error "Load process with ~a workers is already running." (length *workers*)))
  ;; Prepare repository for load.
  (prepare-repository dst)
  ;; Set local namespace abbreviation.
  (register-namespace "bp" +brdf-namespace+)
  ;; Now open repository for operation.
  (with-open-triple-store (db dst :if-does-not-exist :error)
    (let ((block-queue (make-instance 'mp:queue :name "BRDF block queue lock")))
      ;; Print starting heights and number of workers.
      (format t "[~a] Starting load with ~a worker~p~%"
              (excl:universal-time-to-string (get-universal-time))
              workers
              workers)
      ;; Start planner/status thread.
      (push (mp:process-run-function
             "BRDF planner"
             #'start-planner
             src dst block-queue from-height to-height workers)
            *workers*)
      ;; Start workers.
      (dotimes (i workers)
        (push (mp:process-run-function
               (format nil "BRDF worker ~a" i)
               #'start-worker
               graph src dst block-queue)
              *workers*)))))

(defun stop-load ()
  (dolist (worker *workers*)
    (ignore-errors
     (mp:process-interrupt worker (lambda () (signal 'stop-worker))))
    (mp:process-join worker))
  (setf *workers* nil)
  (remove-namespace "bp"))

(defun start-worker (graph src dst block-queue)
  (handler-case
      (with-open-triple-store (db dst :if-does-not-exist :error)
        (unwind-protect
             (with-chain-supplier (bprpc:node-rpc-connection :url src)
               (with-buffered-triple-adds (db)
                 (load-blocks graph block-queue)))
          (rollback-triple-store :db db)))
    (stop-worker ())))

(defun load-blocks (graph block-queue)
  (loop
    :for block-height := (mp:dequeue block-queue :wait t)
    :while block-height
    :do
       (tagbody retry-block
          ;; Load block at a height `HEIGHT'.
          (let ((*print-script-as-assembly* t)
                (block-hash (get-block-hash block-height)))
            (when (null block-hash) ;; no known block at that height
              (go retry-block))
            (load-block graph block-height block-hash)))
       ;; Commit all the triples for current block and continue to the
       ;; next one. It will also flush the buffered triples.
       (commit-triple-store)))

(defun loaded-blocks-table ()
  (let ((table (make-hash-table :test '=)))
    (dolist (result (sparql:run-sparql "SELECT ?b { ?b a bp:Block }" :output-format :lists))
      (let* ((block (part->value (first result)))
             (height (parse-integer (subseq block (length +brdf-namespace+)))))
        (setf (gethash height table) t)))
    table))

(defun start-planner (src dst block-queue from-height to-height workers)
  (with-open-triple-store (db dst :if-does-not-exist :error)
    (let* ((node-connection (make-instance 'bprpc:node-rpc-connection :url src))
           (initial-chain-stats (bprpc:getchaintxstats node-connection))
           (highest-known-block (jsown:val initial-chain-stats "window_final_block_height"))
           (loaded-blocks-table (loaded-blocks-table))
           (max-height to-height)
           (from-height (or from-height 0))
           (to-height (or to-height highest-known-block))
           (initially-enqueued-blocks 0))
      ;; Populate the block queue with all blocks in the specified
      ;; range unless they are already in the DB.
      (loop :for i :from from-height :to to-height
            :do (unless (gethash i loaded-blocks-table)
                  (incf initially-enqueued-blocks)
                  (mp:enqueue block-queue i)))
      (format t "[~a] Initially enqueued blocks: ~a~%"
              (excl:universal-time-to-string (get-universal-time))
              initially-enqueued-blocks)
      ;; Start the status check loop.
      (handler-case
          (loop
            (let* ((chain-stats (bprpc:getchaintxstats node-connection))
                   (chain-blocks (jsown:val chain-stats "window_final_block_height"))
                   (chain-txs (jsown:val chain-stats "txcount"))
                   (blocks (sparql:run-sparql "SELECT DISTINCT ?b { ?b a bp:Block }"
                                              :output-format :count))
                   (txs (sparql:run-sparql "SELECT DISTINCT ?b { ?b a bp:Tx }"
                                           :output-format :count))
                   (progress (/ txs chain-txs)))
              (when (> chain-blocks highest-known-block)
                (loop
                  :for i :from (1+ highest-known-block) :to chain-blocks
                  :if (or (null max-height) (<= i max-height))
                    :do (mp:enqueue block-queue i))
                (let ((new-blocks (- chain-blocks highest-known-block)))
                  (format t "[~a] Adding ~a block~p to the queue~%"
                          (excl:universal-time-to-string (get-universal-time))
                          new-blocks
                          new-blocks))
                (setf highest-known-block chain-blocks))
              (when (> highest-known-block to-height)
                (loop :repeat workers :do (mp:enqueue block-queue nil)))
              (format t "[~a] Blocks: ~a/~a, txs: ~a/~a, progress: ~5$~%"
                      (excl:universal-time-to-string (get-universal-time))
                      blocks chain-blocks txs chain-txs progress)
              (sleep *status-check-period*)
              (rollback-triple-store)))
        (stop-worker ())))))


;;; ----------------------------------------------------------------------------
;;; Tests and examples

#+test
;; On first invocation, choose the number of workers that will utilize
;; the underlying machine to the optimum.
(brdf:start-load :chain
                 "http://user:password@127.0.0.1:8332"
                 "http://user:password@127.0.0.1:10035/repositories/brdf"
                 :workers 4)

#+test
;; In order to stop the load, do:
(brdf:stop-load)
