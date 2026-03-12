(defsystem "db.agraph"
  :description "AllegroGraph CL client (loaded from pre-compiled .fasl)"
  :perform (asdf:load-op (o c)
              (load (merge-pathnames "agraph.fasl" (asdf:system-source-directory c)))))

(register-system-packages "db.agraph"
  '(:db.agraph
    :db.agraph.triple-store-spec
    :db.agraph.part-constants
    :db.agraph.http.store
    :db.agraph.http.client))

(defsystem "brdf"
    :description "Explore Bitcoin transaction graph with RDF/SPARQL"
  :version "0.0.1"
  :author "rodentrabies <rodentrabies@pm.me>"
  :license "MIT"
  :class :package-inferred-system
  :depends-on
  ("db.agraph"
   "aserve"
   "jsown"
   "brdf/etl"
   "brdf/explorer"))

(register-system-packages "brdf/etl"      '(:brdf.etl))
(register-system-packages "brdf/explorer" '(:brdf.explorer))
