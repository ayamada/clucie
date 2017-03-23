(ns clucie.queryparser
  (:import [org.apache.lucene.queryparser.classic QueryParser]
           [org.apache.lucene.analysis Analyzer]
           [org.apache.lucene.search Query]
           [org.apache.lucene.util Version]
           ))

;;; See http://d.hatena.ne.jp/Kazuhira/20130622/1371901567 for syntax

(defn ^Query parse-query
  [^Analyzer analyzer ^String default-field-name ^String query-string]
  (let [^QueryParser qp (QueryParser. default-field-name analyzer)
        query (.parse qp query-string)]
    ;; dump query for debug
    (prn :debug (.toString query))
    query))




