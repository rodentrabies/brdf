;;;; explorer.lisp - Bitcoin transaction graph visualization server
;;;; Uses AllegroServe (aserve / portableaserve).

(uiop:define-package #:brdf.explorer
  (:nicknames #:brdf/explorer)
  (:use #:cl)
  (:import-from #:brdf.etl
                #:+brdf-namespace+)
  (:export #:start #:stop))

(in-package #:brdf.explorer)

(defparameter *default-server-port* 6102)

(defparameter *static-files-location* (asdf:system-relative-pathname "brdf" "static/"))

(defvar *server* nil)

(defun static-file (name)
  (namestring (merge-pathnames name *static-files-location*)))

(defun parse-allegrograph-url (url)
  ;; Return (values sparql-endpoint username password).
  (let* ((proto-end  (+ (search "://" url) 3))
         (at-pos     (position #\@ url :start proto-end))
         (host-start (if at-pos (1+ at-pos) proto-end))
         (cred-str   (when at-pos (subseq url proto-end at-pos))))
    (let* ((endpoint (format nil "~A~A" (subseq url 0 proto-end) (subseq url host-start)))
           (username  (when cred-str
                        (let ((colon (position #\: cred-str)))
                          (if colon (subseq cred-str 0 colon) cred-str))))
           (password  (when (and cred-str (position #\: cred-str))
                        (subseq cred-str (1+ (position #\: cred-str))))))
      (values endpoint username password))))

(defun run-sparql (src sparql)
  ;; POST a SPARQL query to AllegroGraph; return the parsed jsown object.
  (multiple-value-bind (endpoint username password)
      (parse-allegrograph-url src)
    (let* ((sparql-url (format nil "~A/sparql" endpoint))
           (auth       (when username (cons username (or password "")))))
      (multiple-value-bind (body code)
          (net.aserve.client:do-http-request sparql-url
            :method :post
            :content sparql
            :content-type "application/sparql-query"
            :basic-authorization auth
            :accept "application/sparql-results+json")
        (unless (= code 200)
          (error "SPARQL request failed with HTTP ~A: ~A" code body))
        (jsown:parse body)))))

(defun sparql-bindings (result)
  (jsown:val (jsown:val result "results") "bindings"))

(defun binding-value (row var)
  (jsown:val (jsown:val row var) "value"))

(defun binding-value-or-nil (row var)
  (handler-case (binding-value row var)
    (error () nil)))

(defun uri-fragment (uri)
  (let ((pos (position #\# uri)))
    (when pos (subseq uri (1+ pos)))))

(defun txid-from-output-uri (uri)
  (let* ((frag      (uri-fragment uri))
         (colon-pos (when frag (position #\: frag))))
    (when colon-pos (subseq frag 0 colon-pos))))

(defun output-index-from-uri (uri)
  (let* ((frag      (uri-fragment uri))
         (colon-pos (when frag (position #\: frag))))
    (when colon-pos
      (parse-integer (subseq frag (1+ colon-pos)) :junk-allowed t))))

(defun make-tx-node (txid)
  `(:obj ("id"          . ,txid)
         ("type"        . "tx")
         ("label"       . ,(subseq txid 0 (min 8 (length txid))))
         ("inputCount"  . 0)
         ("outputCount" . 0)
         ("totalOutput" . 0)))

(defun make-output-node (out-uri &key (amount 0) (address ""))
  (let* ((frag (uri-fragment out-uri))
         (idx  (output-index-from-uri out-uri)))
    `(:obj ("id"      . ,frag)
           ("type"    . "output")
           ("label"   . ,(format nil "~A" (or idx "")))
           ("amount"  . ,amount)
           ("address" . ,address))))

(defun make-block-node (height-str)
  `(:obj ("id"     . ,(format nil "block:~A" height-str))
         ("type"   . "block")
         ("label"  . ,height-str)
         ("height" . ,(parse-integer height-str :junk-allowed t))))

(defun make-edge (source target)
  `(:obj ("source" . ,source)
         ("target" . ,target)))

(defun send-json (req ent data)
  (net.aserve:with-http-response (req ent :content-type "application/json")
    (net.aserve:with-http-body (req ent)
      (write-string (jsown:to-json data) net.aserve::*html-stream*))))

(defun send-error (req ent msg &optional (response net.aserve:*response-internal-server-error*))
  (net.aserve:with-http-response (req ent
                                  :response response
                                  :content-type "application/json")
    (net.aserve:with-http-body (req ent)
      (write-string (jsown:to-json `(:obj ("error" . ,msg)))
                    net.aserve::*html-stream*))))

(defun handle-root-request (req ent)
  (net.aserve:with-http-response (req ent :response net.aserve:*response-moved-permanently*)
    (setf (net.aserve:reply-header-slot-value req :location) "/ui/index.html")
    (net.aserve:with-http-body (req ent))))

(defun handle-random-tx-request (req ent src)
  (handler-case
      (let* ((sparql (format nil "PREFIX bp: <~A>
                                  SELECT ?tx {
                                    ?tx bp:txInputCount ?ic
                                  }
                                  LIMIT 200"
                             +brdf-namespace+))
             (result (run-sparql src sparql))
             (bindings (sparql-bindings result)))
        (when (null bindings)
          (error "SPARQL returned no transactions"))
        (let* ((row    (nth (random (length bindings)) bindings))
               (tx-uri (binding-value row "tx"))
               (txid   (uri-fragment tx-uri)))
          (send-json req ent `(:obj ("txid" . ,txid)))))
    (error (e)
      (send-error req ent (format nil "~A" e)))))

(defun handle-tx-request (req ent src)
  (handler-case
      (let ((txid (net.aserve:request-query-value "txid" req)))
        (unless txid
          (return-from handle-tx-request
            (send-error req ent "txid parameter required"
                        net.aserve:*response-bad-request*)))
        (let* ((nodes-ht (make-hash-table :test #'equal))
               (edges    '()))

          ;; Seed tx node - counts filled from stored triples
          (let* ((sparql (format nil "PREFIX bp: <~A>
                                      SELECT ?ic ?oc {
                                        bp:~A bp:txInputCount ?ic; bp:txOutputCount ?oc.
                                      }"
                                 +brdf-namespace+
                                 txid
                                 txid))
                 (row (first (sparql-bindings (run-sparql src sparql))))
                 (ic (when row (binding-value-or-nil row "ic")))
                 (oc (when row (binding-value-or-nil row "oc")))
                 (seed (make-tx-node txid)))
            (when ic (setf (jsown:val seed "inputCount")
                           (parse-integer ic :junk-allowed t)))
            (when oc (setf (jsown:val seed "outputCount")
                           (parse-integer oc :junk-allowed t)))
            (setf (gethash txid nodes-ht) seed))

          ;; Query 1: outputs this tx produces
          (let* ((sparql (format nil "PREFIX bp: <~A>
                                      SELECT ?out ?outAmount ?outAddr ?spendTx {
                                        bp:~A bp:txOutput ?out.
                                        ?out bp:outputAmount ?outAmount.
                                        ?out bp:outputAddress ?outAddr.
                                        OPTIONAL { ?out bp:outputInputTx ?spendTx }
                                      }"
                                 +brdf-namespace+
                                 txid))
                 (bindings (sparql-bindings (run-sparql src sparql))))
            (dolist (row bindings)
              (let* ((out-uri        (binding-value-or-nil row "out"))
                     (out-amount     (binding-value-or-nil row "outAmount"))
                     (out-addr       (binding-value-or-nil row "outAddr"))
                     (spend-tx       (binding-value-or-nil row "spendTx"))
                     (out-id         (when out-uri (uri-fragment out-uri)))
                     (out-amount-int (if out-amount
                                         (parse-integer out-amount :junk-allowed t) 0))
                     (seed           (gethash txid nodes-ht)))
                (setf (jsown:val seed "totalOutput")
                      (+ (jsown:val seed "totalOutput") out-amount-int))
                (when out-id
                  (unless (gethash out-id nodes-ht)
                    (setf (gethash out-id nodes-ht)
                          (make-output-node out-uri :amount  out-amount-int
                                                    :address (or out-addr ""))))
                  (push (make-edge txid out-id) edges)
                  (when spend-tx
                    (let ((spend-txid (uri-fragment spend-tx)))
                      (when spend-txid
                        (unless (gethash spend-txid nodes-ht)
                          (setf (gethash spend-txid nodes-ht) (make-tx-node spend-txid)))
                        (push (make-edge out-id spend-txid) edges))))))))

          ;; Query 2: outputs from other txs that feed into this tx
          (let* ((sparql (format nil "PREFIX bp: <~A>
                                      SELECT ?parentOut ?parentAmount ?parentAddr {
                                        ?parentOut bp:outputInputTx bp:~A .
                                        OPTIONAL {
                                          ?parentOut bp:outputAmount ?parentAmount;
                                                     bp:outputAddress ?parentAddr
                                        }
                                      }"
                                 +brdf-namespace+
                                 txid))
                 (bindings (sparql-bindings (run-sparql src sparql))))
            (dolist (row bindings)
              (let* ((parent-uri    (binding-value-or-nil row "parentOut"))
                     (parent-amt    (binding-value-or-nil row "parentAmount"))
                     (parent-addr   (binding-value-or-nil row "parentAddr"))
                     (parent-id     (when parent-uri (uri-fragment parent-uri)))
                     (parent-txid   (when parent-uri (txid-from-output-uri parent-uri)))
                     (parent-amount (if parent-amt (parse-integer parent-amt :junk-allowed t) 0)))
                (when parent-id
                  (unless (gethash parent-id nodes-ht)
                    (setf (gethash parent-id nodes-ht)
                          (make-output-node parent-uri :amount  parent-amount
                                                       :address (or parent-addr ""))))
                  (push (make-edge parent-id txid) edges))
                (when parent-txid
                  (unless (gethash parent-txid nodes-ht)
                    (setf (gethash parent-txid nodes-ht) (make-tx-node parent-txid)))
                  (when parent-id
                    (push (make-edge parent-txid parent-id) edges))))))

          (let ((nodes-list '()))
            (maphash (lambda (k v) (declare (ignore k)) (push v nodes-list)) nodes-ht)
            (send-json req ent `(:obj ("nodes" . ,nodes-list)
                                      ("edges" . ,edges))))))
    (error (e)
      (send-error req ent (format nil "~A" e)))))

(defun handle-block-request (req ent src)
  (handler-case
      (let ((height (net.aserve:request-query-value "height" req)))
        (unless height
          (return-from handle-block-request
            (send-error req ent "height parameter required"
                        net.aserve:*response-bad-request*)))
        (let* ((sparql (format nil "PREFIX bp: <~A>
                                    SELECT ?tx ?ic ?oc WHERE {
                                       bp:~A bp:blockTx ?tx .
                                       ?tx bp:txInputCount ?ic;
                                           bp:txOutputCount ?oc
                                    }"
                               +brdf-namespace+
                               height))
               (result (run-sparql src sparql))
               (bindings (sparql-bindings result))
               (block-id (format nil "block:~A" height))
               (block-node (make-block-node height))
               (tx-nodes (mapcar
                          (lambda (row)
                            (let* ((tx-uri (binding-value row "tx"))
                                   (txid   (uri-fragment tx-uri))
                                   (ic     (let ((s (binding-value-or-nil row "ic")))
                                             (if s (parse-integer s :junk-allowed t) 0)))
                                   (oc     (let ((s (binding-value-or-nil row "oc")))
                                             (if s (parse-integer s :junk-allowed t) 0)))
                                   (node   (make-tx-node txid)))
                              (setf (jsown:val node "inputCount")  ic)
                              (setf (jsown:val node "outputCount") oc)
                              node))
                          bindings))
               (edges     (mapcar (lambda (n)
                                    (make-edge block-id (jsown:val n "id")))
                                  tx-nodes))
               (nodes     (cons block-node tx-nodes)))
          (send-json req ent `(:obj ("nodes" . ,nodes) ("edges" . ,edges)))))
    (error (e)
      (send-error req ent (format nil "~A" e)))))

(defun start (src &key (port *default-server-port*))
  ;; Start the BRDF Explorer HTTP server, but make sure to stop an
  ;; existing server, if any.
  (stop)
  (setf *server* (net.aserve:start :port port))
  ;; UI
  (net.aserve:publish      :path "/"              :function #'handle-root-request  :server *server*)
  (net.aserve:publish-file :path "/ui/index.html" :file (static-file "index.html") :server *server*)
  (net.aserve:publish-file :path "/ui/app.js"     :file (static-file "app.js")     :server *server*)
  (net.aserve:publish-file :path "/ui/style.css"  :file (static-file "style.css")  :server *server*)
  ;; API
  (net.aserve:publish :path "/random-tx"
                      :function (lambda (req ent) (handle-random-tx-request req ent src))
                      :server *server*)
  (net.aserve:publish :path "/tx"
                      :function (lambda (req ent) (handle-tx-request req ent src))
                      :server *server*)
  (net.aserve:publish :path "/block"
                      :function (lambda (req ent) (handle-block-request req ent src))
                      :server *server*)
  (format t "~&BRDF Explorer listening on http://localhost:~A/~%" port)
  (values))

(defun stop ()
  ;; Stop the running BRDF Explorer HTTP server, if any.
  (when *server*
    (net.aserve:shutdown :server *server*)
    (setf *server* nil)
    (format t "~&BRDF Explorer stopped.~%"))
  (values))
