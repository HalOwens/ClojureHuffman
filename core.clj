(ns clojure-noob.core
  (:gen-class))
(use 'clojure.walk)

;; Might use clojure.string/includes? when traversing tree

(def compressionMap (atom {}))
(defrecord charCount[character amount left right])

(defn drop-nth [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll)  
)

(defn countChar [string]
  (loop [final [] 
        string_t string
        i 0]
    (if (< 0 (count string_t))
      (if (> (count final) i)
        (if (= (first string_t) (:character (nth final i)))
          (recur (update-in final [i :amount] inc) (drop 1 string_t) 0)
          (recur final string_t (inc i))  
        )
        (recur (conj final (->charCount (first string_t) 1 nil nil)) (drop 1 string_t) 0)
      )
      final 
    ) 
  )
)

(defn findTwoLowest [coll]
  (loop [coll_t coll
         lowest (map->charCount {:amount Integer/MAX_VALUE})
         secondLowest (map->charCount {:amount Integer/MAX_VALUE})]
    (if (> (count coll_t) 0)
      (if (<= (:amount (first coll_t)) (:amount lowest))
        (recur (drop 1 coll_t) (first coll_t) lowest)
        (if (<= (:amount (first coll_t)) (:amount secondLowest))
          (recur (drop 1 coll_t) lowest (first coll_t))
          (recur (drop 1 coll_t) lowest secondLowest)
        )
      )
      (list lowest secondLowest)
    )
  )
)

(defn generateTree [coll]
  (loop [coll_t coll
         smallest (findTwoLowest coll)]
    (if (= 1 (count coll_t))
      (first coll_t)
      (do
        (def left (first smallest))
        (def right (last smallest))
        (def newNode (->charCount (str (:character left) (:character right)) (+ (:amount left) (:amount right)) left right))
        (def newColl (remove #(= % right) (remove #(= % left) (conj coll_t newNode))))
        (recur newColl (findTwoLowest newColl))
      )
    )
  )
)

(defn buildCompressionMap [tree path]
  (if (and (= (:right tree) nil) (= (:left tree) nil))
    (do
      (swap! compressionMap #(conj {(keyword (str (:character tree))) path} %1))
      (apply str (drop-last path))
    )
    (do
      (buildCompressionMap (:left tree) (str path "1"))
      (buildCompressionMap (:right tree) (str path "0"))
    )
  )
)


(defn compressText [text]
  (loop [text_t text
         compressed ""]
    (if (< 0 (count text_t))
      (recur (apply str (drop 1 text_t)) (str compressed ((keyword (str (first text_t))) @compressionMap)))
      compressed
    )
  )
)

(defn -main []
  (def testString (slurp "test.txt"))
  (def result (countChar testString))
  ;(println (generateTree result))
  (buildCompressionMap (generateTree result) "")
  (println @compressionMap)
  (println (str "File contains " (count testString) " bytes"))
  (println (str "Compressed result contains " (float (/ (count (compressText testString)) 8)) " bytes"))
)



