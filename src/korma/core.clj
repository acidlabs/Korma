(ns korma.core
  "Core querying and entity functions"
  (:require [korma.sql.engine :as eng]
            [korma.sql.fns :as sfns]
            [korma.sql.utils :as utils]
            [clojure.set :as set]
            [korma.db :as db])
  (:use [korma.sql.engine :only [bind-query bind-params]]
        korma.validation))

(def ^{:dynamic true} *exec-mode* false)
(def ^{:dynamic true} *pg-schema* nil)
(def ^{:dynamic true} *schema* nil)
(declare get-rel set-pg-schema save insert-or-update-with-rels)

;;*****************************************************
;; Query types
;;*****************************************************

(defn- check-ent [ent]
  (when-not (or (string? ent)
                (map? ent))
    (throw (Exception. (str "Invalid entity provided for the query: " ent)))))

(defn empty-query [ent]
  (let [ent (if (keyword? ent)
              (name ent)
              ent)
        [ent table alias db opts] (if (string? ent)
                                    [{:table ent} ent nil nil nil]
                                    [ent (:table ent) (:alias ent)
                                     (:db ent) (get-in ent [:db :options])])]
    {:ent ent
     :table table
     :db db
     :options opts
     :alias alias}))

(defn select*
  "Create an empty select query. Ent can either be an entity defined by defentity,
  or a string of the table name"
  [ent]
  (let [ent (set-pg-schema ent)]
    (if (:type ent)
      ent
      (let [q (empty-query ent)]
        (merge q {:type :select
                  :fields [::*]
                  :from [(:ent q)]
                  :modifiers []
                  :joins []
                  :where []
                  :order []
                  :aliases #{}
                  :group []
                  :results :results})))))

(defn update*
  "Create an empty update query. Ent can either be an entity defined by defentity,
  or a string of the table name."
  [ent]
  (let [ent (set-pg-schema ent)]
    (if (:type ent)
      ent
      (let [q (empty-query ent)]
        (merge q {:type :update
                  :fields {}
                  :where []
                  :results :keys})))))

(defn delete*
  "Create an empty delete query. Ent can either be an entity defined by defentity,
  or a string of the table name"
  [ent]
  (let [ent (set-pg-schema ent)]
    (if (:type ent)
      ent
      (let [q (empty-query ent)]
        (merge q {:type :delete
                  :where []
                  :results :keys})))))

(defn insert*
  "Create an empty insert query. Ent can either be an entity defined by defentity,
  or a string of the table name"
  [ent]
  (let [ent (set-pg-schema ent)]
    (if (:type ent)
      ent
      (let [q (empty-query ent)]
        (merge q {:type :insert
                  :values []
                  :results :keys})))))

;;*****************************************************
;; Query macros
;;*****************************************************

(defmacro select
  "Creates a select query, applies any modifying functions in the body and then
  executes it. `ent` is either a string or an entity created by defentity.

  ex: (select user
        (fields :name :email)
        (where {:id 2}))"
  [ent & body]
  `(let [query# (-> (select* ~ent)
                    ~@body)]
     (exec query#)))

(defmacro update
  "Creates an update query, applies any modifying functions in the body and then
  executes it. `ent` is either a string or an entity created by defentity.

  ex: (update user
        (set-fields {:name \"chris\"})
        (where {:id 4}))"
  [ent & body]
  `(let [query# (-> (update* ~ent)
                    ~@body)]
     (exec query#)))

(defmacro delete
  "Creates a delete query, applies any modifying functions in the body and then
  executes it. `ent` is either a string or an entity created by defentity.

  ex: (delete user
        (where {:id 7}))"
  [ent & body]
  `(let [query# (-> (delete* ~ent)
                    ~@body)]
     (exec query#)))

(defmacro insert
  "Creates an insert query, applies any modifying functions in the body and then
  executes it. `ent` is either a string or an entity created by defentity. Inserts
  return the last inserted id.

  ex: (insert user
        (values [{:name \"chris\"} {:name \"john\"}]))"
  [ent & body]
  `(let [query# (-> (insert* ~ent)
                    ~@body)]
     (exec query#)))

;;*****************************************************
;; Query parts
;;*****************************************************

(defn- add-aliases [query as]
  (update-in query [:aliases] set/union as))

(defn- update-fields [query fs]
  (let [[first-cur] (:fields query)]
    (if (= first-cur ::*)
      (assoc query :fields fs)
      (update-in query [:fields] concat fs))))

(defn fields
  "Set the fields to be selected in a query. Fields can either be a keyword
  or a vector of two keywords [field alias]:

  (fields query :name [:firstname :first])"
  [query & vs]
  (let [aliases (set (map second (filter coll? vs)))]
    (-> query
        (add-aliases aliases)
        (update-fields vs))))

(defn set-fields
  "Set the fields and values for an update query."
  [query fields-map]
  (update-in query [:set-fields] merge fields-map))

(defn from
  "Add tables to the from clause."
  [query table]
  (update-in query [:from] conj table))

(defn where*
  "Add a where clause to the query. Clause can be either a map or a string, and
  will be AND'ed to the other clauses."
  [query clause]
  (update-in query [:where] conj clause))

(defmacro where
  "Add a where clause to the query, expressing the clause in clojure expressions
  with keywords used to reference fields.
  e.g. (where query (or (= :hits 1) (> :hits 5)))

  Available predicates: and, or, =, not=, <, >, <=, >=, in, like, not

  Where can also take a map at any point and will create a clause that compares keys
  to values. The value can be a vector with one of the above predicate functions
  describing how the key is related to the value: (where query {:name [like \"chris\"})"
  [query form]
  `(let [q# ~query]
     (where* q#
             (bind-query q#
                         (eng/pred-map
                          ~(eng/parse-where `~form))))))

(defn order
  "Add an ORDER BY clause to a select query. field should be a keyword of the field name, dir
  is ASC by default.

  (order query :created :asc)"
  [query field & [dir]]
  (update-in query [:order] conj [field (or dir :ASC)]))

(defn values
  "Add records to an insert clause. values can either be a vector of maps or a single
  map.

  (values query [{:name \"john\"} {:name \"ed\"}])"
  [query values]
  (update-in query [:values] concat (if (map? values)
                                      [values]
                                      values)))

(defn join* [query type table clause]
  (update-in query [:joins] conj [type table clause]))

(defmacro join
  "Add a join clause to a select query, specifying the table name to join and the predicate
  to join on.

  (join query addresses)
  (join query addresses (= :addres.users_id :users.id))
  (join query :right addresses (= :address.users_id :users.id))"
  ([query ent]
     `(let [q# ~query
            e# ~ent
            rel# (get-rel (:ent q#) e#)]
        (join* q# :left e# (sfns/pred-= (:pk rel#) (:fk rel#)))))
  ([query table clause]
     `(join* ~query :left ~table (eng/pred-map ~(eng/parse-where clause))))
  ([query type table clause]
     `(join* ~query ~type ~table (eng/pred-map ~(eng/parse-where clause)))))

(defn post-query
  "Add a function representing a query that should be executed for each result in a select.
  This is done lazily over the result set."
  [query post]
  (update-in query [:post-queries] conj post))

(defn limit
  "Add a limit clause to a select query."
  [query v]
  (assoc query :limit v))

(defn offset
  "Add an offset clause to a select query."
  [query v]
  (assoc query :offset v))

(defn group
  "Add a group-by clause to a select query"
  [query & fields]
  (update-in query [:group] concat fields))

(defmacro aggregate
  "Use a SQL aggregator function, aliasing the results, and optionally grouping by
  a field:

  (select users
    (aggregate (count :*) :cnt :status))

  Aggregates available: count, sum, avg, min, max, first, last"
  [query agg alias & [group-by]]
  `(let [q# ~query]
     (bind-query q#
                 (let [res# (fields q# [~(eng/parse-aggregate agg) ~alias])]
                   (if ~group-by
                     (group res# ~group-by)
                     res#)))))

;;*****************************************************
;; Other sql
;;*****************************************************

(defn sqlfn*
  "Call an arbitrary SQL function by providing the name of the function
  and its params"
  [fn-name & params]
  (apply eng/sql-func (name fn-name) params))

(defmacro sqlfn
  "Call an arbitrary SQL function by providing func as a symbol or keyword
  and its params"
  [func & params]
  `(sqlfn* (quote ~func) ~@params))

(defmacro subselect
  "Create a subselect clause to be used in queries. This works exactly like (select ...)
  execept it will wrap the query in ( .. ) and make sure it can be used in any current
  query:

  (select users
    (where {:id [in (subselect users2 (fields :id))]}))"
  [& parts]
  `(utils/sub-query (query-only (select ~@parts))))

(defn modifier
  "Add a modifer to the beginning of a query:

  (select orders
    (modifier \"DISTINCT\"))"
  [query & modifiers]
  (update-in query [:modifiers] conj (reduce str modifiers)))

(defn raw
  "Embed a raw string of SQL in a query. This is used when Korma doesn't
  provide some specific functionality you're looking for:

  (select users
    (fields (raw \"PERIOD(NOW(), NOW())\")))"
  [s]
  (utils/generated s))

;;*****************************************************
;; Query exec
;;*****************************************************

(defmacro sql-only
  "Wrap around a set of queries so that instead of executing, each will return a string of the SQL
  that would be used."
  [& body]
  `(binding [*exec-mode* :sql]
     ~@body))

(defmacro dry-run
  "Wrap around a set of queries to print to the console all SQL that would
  be run and return dummy values instead of executing them."
  [& body]
  `(binding [*exec-mode* :dry-run]
     ~@body))

(defmacro query-only
  "Wrap around a set of queries to force them to return their query objects."
  [& body]
  `(binding [*exec-mode* :query]
     ~@body))

(defn as-sql
  "Force a query to return a string of SQL when (exec) is called."
  [query]
  (bind-query query (:sql-str (eng/->sql query))))

(defn- apply-posts
  [query results]
  (if-let [posts (seq (:post-queries query))]
    (let [post-fn (apply comp posts)]
      (post-fn results))
    results))

(defn- apply-transforms
  [query results]
  (if (not= (:type query) :select)
    results
    (if-let [trans (seq (-> query :ent :transforms))]
      (let [trans-fn (apply comp trans)]
        (map trans-fn results))
      results)))

(defn- apply-prepares
  [query]
  (if-let [preps (seq (-> query :ent :prepares))]
    (let [preps (apply comp preps)]
      (condp = (:type query)
        :insert (let [values (:values query)]
                  (assoc query :values (map preps values)))
        :update (let [value (:set-fields query)]
                  (assoc query :set-fields (preps value)))
        query))
    query))

(defn exec
  "Execute a query map and return the results."
  [query]
  (let [query (apply-prepares query)
        query (bind-query query (eng/->sql query))
        sql (:sql-str query)
        params (:params query)]
    (cond
     (:sql query) sql
     (= *exec-mode* :sql) sql
     (= *exec-mode* :query) query
     (= *exec-mode* :dry-run) (do
                                (println "dry run ::" sql "::" (vec params))
                                (let [pk (-> query :ent :pk)
                                      results (apply-posts query [{pk 1}])]
                                  (first results)
                                  results))
     :else (let [results (db/do-query query)]
             (apply-transforms query (apply-posts query results))))))

(defn exec-raw
  "Execute a raw SQL string, supplying whether results should be returned. `sql` can either be
  a string or a vector of the sql string and its params. You can also optionally
  provide the connection to execute against as the first parameter.

  (exec-raw [\"SELECT * FROM users WHERE age > ?\" [5]] :results)"
  [conn? & [sql with-results?]]
  (let [sql-vec (fn [v] (if (vector? v) v [v nil]))
        [conn? [sql-str params] with-results?] (if (or (string? conn?)
                                                       (vector? conn?))
                                                 [nil (sql-vec conn?) sql]
                                                 [conn? (sql-vec sql) with-results?])]
    (db/do-query {:db conn? :results with-results? :sql-str sql-str :params params})))

;;*****************************************************
;; Entities
;;*****************************************************

(defn create-entity
  "Create an entity representing a table in a database."
  [table]
  {:table table
   :name table
   :pk :id
   :db nil
   :constraints {}
   :indexes []
   :transforms '()
   :prepares '()
   :fields []
   :rel {}})

(defn create-relation
  "Create a relation map describing how two entities are related."
  [ent sub-ent type opts]
  (cond
   (#{:has-many-to-many :belongs-to-many-to-many} type)
   (let [d-fk (keyword (str (:table ent) "_id"))
         d-sub-fk (keyword (str (:table sub-ent) "_id"))
         d-map-table (condp = type
                       :has-many-to-many (str (:table ent) "2" (:table sub-ent))
                       :belongs-to-many-to-many (str (:table sub-ent) "2" (:table ent)))
         opts (merge {:fk d-fk
                      :sub-fk d-sub-fk
                      :map-table d-map-table}
                     opts)
         [pk fk sub-pk sub-fk] [(raw (eng/prefix ent (:pk ent)))
                                (raw (eng/prefix (name (:map-table opts)) (:fk opts)))
                                (raw (eng/prefix sub-ent (:pk ent)))
                                (raw (eng/prefix (name (:map-table opts)) (:sub-fk opts)))]]
     {:table (:table sub-ent)
      :alias (:alias sub-ent)
      :ent sub-ent
      :rel-type type
      :pk pk
      :sub-pk sub-pk
      :fk fk
      :sub-fk sub-fk
      :map-table (:map-table opts)})
   :else (let [[pk fk foreign-ent] (condp = type
                                     :has-one [(raw (eng/prefix ent (:pk ent)))
                                               (raw (eng/prefix sub-ent (keyword (str (:table ent) "_id"))))
                                               sub-ent]
                                     :belongs-to [(raw (eng/prefix sub-ent (:pk sub-ent)))
                                                  (raw (eng/prefix ent (keyword (str (:table sub-ent) "_id"))))
                                                  ent]
                                     :has-many [(raw (eng/prefix ent (:pk ent)))
                                                (raw (eng/prefix sub-ent (keyword (str (:table ent) "_id"))))
                                                sub-ent])
               opts (when (:fk opts)
                      {:fk (raw (eng/prefix foreign-ent (:fk opts)))})]
           (merge {:ent sub-ent
                   :table (:table sub-ent)
                   :alias (:alias sub-ent)
                   :rel-type type
                   :pk pk
                   :fk fk}
                  opts))))

(defn rel
  [ent sub-ent type opts]
  (let [[rel-name sub-ent] (if (vector? sub-ent)
                             sub-ent
                             [(-> sub-ent meta :name) sub-ent])
        var-name (-> sub-ent meta :name)
        cur-ns *ns*]
    (assoc-in ent [:rel (name rel-name)]
              (delay
               (let [resolved (ns-resolve cur-ns var-name)
                     sub-ent (when resolved
                               (deref sub-ent))]
                 (when-not (map? sub-ent)
                   (throw (Exception. (format "Entity used in relationship does not exist: %s" (name var-name)))))
                 (create-relation ent sub-ent type opts))))))

(defn get-rel [ent sub-ent]
  (let [sub-name (if (map? sub-ent)
                   (:name sub-ent)
                   (name sub-ent))]
    (force (get-in ent [:rel sub-name]))))

(defn- prepare-sub-entity
  [sub-ent]
  (if (vector? sub-ent)
    [(first sub-ent) `(var ~(second sub-ent))]
    `(var ~sub-ent)))

(defmacro has-one
  "Add a has-one relationship for the given entity. It is assumed that the foreign key
  is on the sub-entity with the format table_id: user.id = address.user_id
  Opts can include a key for :fk to explicitly set the foreign key.

  (has-one users address {:fk :addressID})

  Sub-Entity can also be a vector with the name of the relationship and the entity.

  (has-one users [:parent users])"
  [ent sub-ent & [opts]]
  (let [sub-ent (prepare-sub-entity sub-ent)]
    `(rel ~ent ~sub-ent :has-one ~opts)))

(defmacro belongs-to
  "Add a belongs-to relationship for the given entity. It is assumed that the foreign key
  is on the current entity with the format sub-ent-table_id: email.user_id = user.id.
  Opts can include a key for :fk to explicitly set the foreign key.

  (belongs-to users email {:fk :emailID})

  Sub-Entity can also be a vector with the name of the relationship and the entity.

  (has-one users [:parent users])"
  [ent sub-ent & [opts]]
  (let [sub-ent (prepare-sub-entity sub-ent)]
    `(rel ~ent ~sub-ent :belongs-to ~opts)))

(defmacro has-many
  "Add a has-many relation for the given entity. It is assumed that the foreign key
  is on the sub-entity with the format table_id: user.id = email.user_id
  Opts can include a key for :fk to explicitly set the foreign key.

  (has-many users email {:fk :emailID})

  Sub-Entity can also be a vector with the name of the relationship and the entity.

  (has-one users [:parent users])"
  [ent sub-ent & [opts]]
  (let [sub-ent (prepare-sub-entity sub-ent)]
    `(rel ~ent ~sub-ent :has-many ~opts)))

(defmacro has-many-to-many
  "Adds a many-many relation for given entity. It is assumed that mapping table
  exists between entity and sub-entity with name entity2sub-entity: user2email
  and this table has columns table_id and sub-ent-table_id: user_id, email_id.
  Opts can include a key for :fk to explicitly set the entity foreign key in
  mapping table, a key :sub-fk to explicitly set the sub-entity foreign key in mapping
  table and finally also a key :map-table to explicity set the name of mapping table.

  (has-many-many users email {:fk :userID :sub-fk :emailID :map-table 'users2emails'})"
  [ent sub-ent & [opts]]
  (let [sub-ent (prepare-sub-entity sub-ent)]
    `(rel ~ent ~sub-ent :has-many-to-many ~opts)))

(defmacro belongs-to-many-to-many
  "Adds a many-many relation for given entity. It is assumed that mapping table
  exists between entity and sub-entity with name sub-entity2entity: email2user
  and this table has columns table_id and sub-ent-table_id: user_id, email_id.
  Opts can include a key for :fk to explicitly set the entity foreign key in
  mapping table, a key :sub-fk to explicitly set the sub-entity foreign key in mapping
  table and finally also a key :map-table to explicity set the name of mapping table.

  (has-many-many users email {:fk :userID :sub-fk :emailID :map-table 'emails2users'})"
  [ent sub-ent & [opts]]
  (let [sub-ent (prepare-sub-entity sub-ent)]
    `(rel ~ent ~sub-ent :belongs-to-many-to-many ~opts)))

(defn entity-fields
  "Set the fields to be retrieved by default in select queries for the
  entity."
  [ent & fields]
  (update-in ent [:fields] concat (map #(eng/prefix ent %) fields)))

(defn table
  "Set the name of the table and an optional alias to be used for the entity.
  By default the table is the name of entity's symbol."
  [ent t & [alias]]
  (let [tname (if (or (keyword? t)
                      (string? t))
                (name t)
                (if alias
                  t
                  (throw (Exception. "Generated tables must have aliases."))))
        ent (assoc ent :table tname)]
    (if alias
      (assoc ent :alias (name alias))
      ent)))

(defn pk
  "Set the primary key used for an entity. :id by default."
  [ent pk]
  (assoc ent :pk (keyword pk)))

(defn database
  "Set the database connection to be used for this entity."
  [ent db]
  (assoc ent :db db))

(defn transform
  "Add a function to be applied to results coming from the database"
  [ent func]
  (update-in ent [:transforms] conj func))

(defn prepare
  "Add a function to be applied to records/values going into the database"
  [ent func]
  (update-in ent [:prepares] conj func))

(defn nest
  "Add "
  [ent & rels]
  (update-in ent
             [:accepted-rels]
             concat (map (fn [x] (cond
                                 (map? x) (:name x)
                                 (keyword? x) (name x))) rels)))

(defmacro defentity
  "Define an entity representing a table in the database, applying any modifications in
  the body."
  [ent & body]
  `(let [e# (-> (create-entity ~(name ent))
                ~@body)]
     (def ~ent e#)))

;;*****************************************************
;; With
;;*****************************************************

(defn- force-prefix [ent fields]
  (for [field fields]
    (if (vector? field)
      [(utils/generated (eng/prefix ent (first field))) (second field)]
      (eng/prefix ent field))))

(defn merge-part [query neue k]
  (update-in query [k] #(if-let [vs (k neue)]
                          (vec (concat % vs))
                          %)))

(defn- merge-query [query neue]
  (let [merged (reduce #(merge-part % neue %2)
                       query
                       [:fields :group :order :where :params :joins :post-queries])]
    (-> merged
        (add-aliases (:aliases neue)))))

(defn- sub-query [query sub-ent func]
  (let [neue (select* sub-ent)
        neue (bind-query neue (func neue))
        neue (-> neue
                 (update-in [:fields] #(force-prefix sub-ent %))
                 (update-in [:order] #(force-prefix sub-ent %))
                 (update-in [:group] #(force-prefix sub-ent %)))]
    (merge-query query neue)))

(defn- with-many [rel query ent func rel-name]
  (let [fk (:fk rel)
        pk (get-in query [:ent :pk])
        table (keyword (eng/table-alias ent))]
    (post-query query
                (partial map
                         #(assoc % rel-name
                                 (select ent
                                         (func)
                                         (where {fk (get % pk)})))))))

(defn- extract-field-keyword
  "Extracts the field keyword from a generated field name.  This method is
   broken in that it will only work with the default naming strategy.
   Fortunately, we're using the default naming strategy."
  [field]
  (let [{:keys [delimiters]} (or eng/*bound-options* @korma.config/options)
        [begin end] delimiters
        quoted-name (last (clojure.string/split (get field :korma.sql.utils/generated) #"[.]"))
        regex (re-pattern (str "^" begin "|" end "$"))]
    (keyword (clojure.string/replace quoted-name regex ""))))

(defn- with-many-to-many [rel query ent func rel-name]
  (let [fk (:fk rel)
        pk (get-in query [:ent :pk])
        table (keyword (eng/table-alias ent))]
    (post-query query
                (partial map
                         (let [pg-schema *pg-schema*]
                           #(binding [*pg-schema* pg-schema]
                              (assoc % rel-name
                                     (select ent
                                             (join :inner (:map-table rel) (= (:sub-pk rel) (:sub-fk rel)))
                                             (func)
                                             (where {fk (get % pk)})))))))))

(defn- with-direct
  [rel query ent func rel-name]
  (let  [fk (extract-field-keyword (:fk rel))
         pk (:pk rel)
         table (keyword (eng/table-alias ent))]
    (post-query (fields query fk)
                (partial map
                         (let [pg-schema *pg-schema*]
                           #(binding [*pg-schema* pg-schema]
                              (dissoc (assoc % rel-name
                                             (first (select ent
                                                            (func)
                                                            (where {pk (get % fk)})))) fk)))))))

(defn with* [query sub-ent func]
  (let [rel (get-rel (:ent query) sub-ent)
        rel-name (if (map? sub-ent)
                   (keyword (:name sub-ent))
                   sub-ent)
        sub-ent (:ent rel)]
    (cond
     (not rel) (throw (Exception. (str "No relationship defined for table: " (:table sub-ent))))
     (#{:has-one :belongs-to} (:rel-type rel)) (with-direct rel query sub-ent func rel-name)
     (#{:has-many-to-many :belongs-to-many-to-many} (:rel-type rel)) (with-many-to-many rel query sub-ent func rel-name)
     :else (with-many rel query sub-ent func rel-name))))

(defmacro with
  "Add a related entity to the given select query. If the entity has a relationship
  type of :belongs-to or :has-one, the requested fields will be returned directly in
  the result map. If the entity is a :has-many, a second query will be executed lazily
  and a key of the entity name will be assoc'd with a vector of the results.

  (defentity email (entity-fields :email))
  (defentity user (has-many email))
  (select user
    (with email) => [{:name \"chris\" :email [{email: \"c@c.com\"}]} ...

  With can also take a body that will further refine the relation:
  (select user
     (with address
        (with state)
        (fields :address.city :state.state)
        (where {:address.zip x})))

  Entity can also be a keyword with the name of the relationship.

  (belongs-to users [:parent users])
  (select users
    (with :parent))"
  [query ent & body]
  `(with* ~query ~ent (fn [q#]
                        (-> q#
                            ~@body))))

;;*****************************************************
;; Postgres Schemas
;;*****************************************************

(defn- set-pg-schema
  "Creates a entity with proper table name in a postgres schema"
  [{tb :table sc :schema :as ent}]
  (let [schema (if *pg-schema* *pg-schema* sc)]
    (table ent (if schema (keyword (str (name schema) "." tb)) tb))))

(defn pg-schema [ent schema]
  (assoc ent :schema schema))

(defmacro with-pg-schema
  "Binds postgres schema for tables in this scope."
  [schema & body]
  `(binding [*pg-schema* ~schema]
     ~@body))

;;*****************************************************
;; Other
;;*****************************************************

(defn page
  "Limits the query's result for a given page and page-size"
  [query page page-size]
  (-> query
      (limit page-size)
      (offset (* page page-size))))

(defn- get-key
  [rel k]
  (keyword (last (clojure.string/split
                  (val (first (k rel))) #"\""))))

(defn rel-values
  [rel]
  [(get-key rel :pk) (get-key rel :fk) (:ent rel)])

(defn- insert-or-update-many-rels
  [many-rels rels-map]
  (doseq [[k rel] many-rels]
    (let [[pk fk ent] (rel-values rel)]
      (doseq [record (map (fn [val] (assoc val fk (pk rels-map))) (rels-map k))]
        (insert-or-update-with-rels ent record)))))

(defn- insert-or-update-one-rels
  [one-rels rels-map]
  (if one-rels
    (reduce
     (fn [value [k v]]
       (assoc value k v))
     rels-map
     (map (fn [[k rel]]
            (let [[pk fk ent] (rel-values rel)
                  id (pk (insert-or-update-with-rels ent (rels-map k)))]
              [fk id]))
          one-rels))
    rels-map))

(defn- get-rels
  [{:keys [rel]} type]
  (reduce
   (fn [rels [k v]]
     (if (= (:rel-type @(rel k)) type)
       (assoc rels (keyword k) @v)
       rels))
   {}
   rel))

(defn insert-or-update-with-rels
  "Inserts a single value with its relationships."
  [{id :pk rel :rel :as ent} record]
  (let [many-rels  (get-rels ent :has-many)
        one-rels   (get-rels ent :belongs-to)
        query      (if (id record)
                     #(update ent (set-fields %) (where {id (id %)}))
                     #(insert ent (values %)))
        rels-keys  (concat (keys many-rels) (keys one-rels))
        new-record (query
                    (apply dissoc
                           (insert-or-update-one-rels one-rels record)
                           rels-keys))]
    (insert-or-update-many-rels many-rels
                                (assoc record id (id new-record)))
    new-record))

(defn save
  "Inserts a single value including its relationships.
   Returns a vector like [valid? output], with valid? true if validation is passed else false,
   and output with the returned record from the insert if valid, else a map with errors."
  [ent record]
  (if-let [errors (get-errors ent record)]
    [false errors]
    [true (insert-or-update-with-rels ent record)]))

(defn create
  [ent [rec & recs]]
  (korma.db/transaction
   (if recs
     (let [[valid?] (save ent rec)]
       (if valid?
         (create recs)
         (korma.db/rollback))))))
