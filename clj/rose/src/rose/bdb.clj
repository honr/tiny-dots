(ns rose.bdb
  (:refer-clojure :exclude [open get put])
  (:import [com.sleepycat.db
	    CacheFilePriority
	    DatabaseException
	    Database
	    DatabaseConfig
	    DatabaseType
	    DatabaseEntry
	    LockMode
	    OperationStatus
	    Environment
	    EnvironmentConfig]))


(defn- config-opts [[k v]]
  ({:read-only #(.setReadOnly % v)
    :set-priority #(.setPriority % (clojure.core/get
				    {:default CacheFilePriority/DEFAULT
				     :high CacheFilePriority/HIGH
				     :low CacheFilePriority/LOW
				     :very-high CacheFilePriority/VERY_HIGH
				     :very-low CacheFilePriority/VERY_LOW} v)) 

    ;; :error-handler .setErrorHandler(com.sleepycat.db.ErrorHandler )
    :allow-create #(.setAllowCreate % v)
    :btree-comparator #(.setBtreeComparator % v)
    :btree-min-key #(.setBtreeMinKey % v)
    :byte-order #(.setByteOrder % v)
    ;; :btree-compressor .setBtreeCompressor(com.sleepycat.db.BtreeCompressor)
    ;; :btree-prefix-calculator .setBtreePrefixCalculator(com.sleepycat.db.BtreePrefixCalculator)
    :cache-size #(.setCacheSize % v)
    :create-dir #(.setCreateDir % v)
    :cache-count #(.setCacheCount % v)
    :checksum #(.setChecksum % v)
    :read-uncommitted #(.setReadUncommitted % v)
    :dirty-read #(.setDirtyRead % v)
    :duplicate-comparator #(.setDuplicateComparator % v)
    :encrypted #(.setEncrypted % v)
    :error-prefix #(.setErrorPrefix % v)
    :error-stream #(.setErrorStream % v)
    :exclusive-create #(.setExclusiveCreate % v)
    ;; :feedback-handler .setFeedbackHandler(com.sleepycat.db.FeedbackHandler)
    :hash-fill-factor #(.setHashFillFactor % v)
    :hash-comparator #(.setHashComparator % v)
    ;; :hasher .setHasher(com.sleepycat.db.Hasher)
    :hash-num-elements #(.setHashNumElements % v)
    ;; :message-handler .setMessageHandler(com.sleepycat.db.MessageHandler)
    :message-stream #(.setMessageStream % v)
    :mode #(.setMode % v)
    :multiversion #(.setMultiversion % v)
    :no-map #(.setNoMMap % v)
    :page-size #(.setPageSize % v)
    ;; :panic-handler .setPanicHandler(com.sleepycat.db.PanicHandler)
    ;; :partition-by-callback .setPartitionByCallback(int,com.sleepycat.db.PartitionHandler)
    ;; :partition-by-range .setPartitionByRange(int,com.sleepycat.db.MultipleDataEntry)
    ;; :partition-dirs .setPartitionDirs(java.io.File[])
    :queue-extent-size #(.setQueueExtentSize % v)
    :queue-in-order #(.setQueueInOrder % v)
    ;; :record-number-appender .setRecordNumberAppender(com.sleepycat.db.RecordNumberAppender)
    :record-delimiter #(.setRecordDelimiter % v)
    :record-length #(.setRecordLength % v)
    :btree-record-numbers #(.setBtreeRecordNumbers % v)
    :record-pad #(.setRecordPad % v)
    :record-source #(.setRecordSource % v)
    :renumbering #(.setRenumbering % v)
    :reverse-split-off #(.setReverseSplitOff % v)
    :sort-duplicates #(.setSortedDuplicates % v)
    :unsorted-duplicates #(.setUnsortedDuplicates % v)
    :snapshot #(.setSnapshot % v)
    :transactional #(.setTransactional % v)
    :transaction-not-durable #(.setTransactionNotDurable % v)
    :truncate #(.setTruncate % v)
    :type #(.setType % (clojure.core/get
			{:hash    DatabaseType/HASH
			 :btree   DatabaseType/BTREE
			 :queue   DatabaseType/QUEUE
			 :recno   DatabaseType/RECNO
			 :unknown DatabaseType/UNKNOWN} v))}
   k))

(defn config [opts]
  (let [x (DatabaseConfig.)]
    (doseq [f (map config-opts opts)]
      (f x))
    x))

(defn open [path-to-db opts]
  (Database. path-to-db nil
	     (config opts)))
;; {:allow-create true :type :hash}

(def ^{:private true} entry-encoding "UTF-8")

(defn put [db k v]
  (let [db-entry-k (DatabaseEntry. (.getBytes k entry-encoding))
	db-entry-v (DatabaseEntry. (.getBytes v entry-encoding))]
    (.put db nil db-entry-k db-entry-v)))

(defn get [db k]
  (let [db-entry-k (DatabaseEntry. (.getBytes k entry-encoding))
	db-entry-v (DatabaseEntry.)]
    (when (= OperationStatus/SUCCESS
	     (.get db nil db-entry-k db-entry-v LockMode/DEFAULT))
      (String. (.getData db-entry-v) entry-encoding))))



(comment
  ;; TODO: consider using db environments
  (let [db-env-config (doto (EnvironmentConfig.)
			(.setAllowCreate true))
	db-env (doto (Environment. (File. "/tmp") db-env-config))]
    (.close db-env))

  )
