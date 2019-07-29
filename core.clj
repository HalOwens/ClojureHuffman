;;
;;File.Io.InputeStream
;;
(ns core)
(import java.io.FileOutputStream)

(def compressionMap (atom {}))
(defrecord charCount [character amount left right])

(defn drop-nth [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))


(defn toBinary [number mapLength]
  (loop [binary ""
         number_t number]
    (if (= number_t 0)
      (str (apply str (repeat (- mapLength (count binary)) 0)) binary)
      (recur (str (mod number_t 2) binary) (int (/ number_t 2))))))




(defn countChar [string]
  (loop [final []
         string_t string
         i 0]
    (if (< 0 (count string_t))
      (if (> (count final) i)
        (if (= (first string_t) (:character (nth final i)))
          (recur (update-in final [i :amount] inc) (drop 1 string_t) 0)
          (recur final string_t (inc i)))

        (recur (conj final (->charCount (first string_t) 1 nil nil)) (drop 1 string_t) 0))

      final)))




(defn findTwoLowest [coll]
  (loop [coll_t coll
         lowest (map->charCount {:amount Integer/MAX_VALUE})
         secondLowest (map->charCount {:amount Integer/MAX_VALUE})]
    (if (> (count coll_t) 0)
      (if (<= (:amount (first coll_t)) (:amount lowest))
        (recur (drop 1 coll_t) (first coll_t) lowest)
        (if (<= (:amount (first coll_t)) (:amount secondLowest))
          (recur (drop 1 coll_t) lowest (first coll_t))
          (recur (drop 1 coll_t) lowest secondLowest)))


      (list lowest secondLowest))))


(defn generateTree [coll]
  (loop [coll_t coll
         smallest (findTwoLowest coll)]
    (if (= 1 (count coll_t))
      (first coll_t)
      (do
        (let [left (first smallest)
              right (last smallest)
              ]
          (def newNode (->charCount (str (:character left) (:character right)) (+ (:amount left) (:amount right)) left right))
          (def newColl (remove #(= % right) (remove #(= % left) (conj coll_t newNode))))
          (recur newColl (findTwoLowest newColl))
          )
        )
      )
    )
  )


(defn buildCompressionMap [tree path]
  (if (and (= (:right tree) nil) (= (:left tree) nil))
    (do
      (swap! compressionMap #(conj {(keyword (str (:character tree))) path} %1))
      (apply str (drop-last path)))

    (do
      (buildCompressionMap (:left tree) (str path "1"))
      (buildCompressionMap (:right tree) (str path "0")))))


(defn compressText [text]
  (loop [text_t text
         compressed ""]
    (if (< 0 (count text_t))
      (recur (apply str (drop 1 text_t)) (str compressed ((keyword (str (first text_t))) @compressionMap)))
      compressed)))


(defn bitifyMap []
  (loop [coll_t (seq @compressionMap)
         total 0
         mapStr ""]
    (if (> (count coll_t) 0)
      (recur (drop 1 coll_t)
             (+ total 12 (count (second (first coll_t))))
             (str mapStr (toBinary (int (first (name (first (first coll_t))))) 7) (toBinary (count (second (first coll_t))) 5) (second (first coll_t))))
      (do
        (str (toBinary total 16) mapStr)))))

(defn pow [n x]
  (reduce * (repeat n x))
  )


(defn byteStringtoChar [bytes]
  (loop [bytes_t bytes
         byteArr []]
    (if (> (count bytes_t) 0)
      (recur (drop 8 bytes_t)
             (conj byteArr
                   (loop [binary (map #(- (int %1) 48) (take 8 bytes_t))
                          sum 0
                          power 7]
                     (if (> (count binary) 0)
                       (recur (drop 1 binary) (+ sum (* (first binary) (pow power 2))) (dec power))
                       sum
                       )
                     )
                   )
             )
      byteArr
      )
    )
  )


(defn makeByteSize [binary]
  (def length (count binary))
  (str binary (apply str (repeat (- 8 (mod length 8)) 0)))
  )


(try
  (def file (slurp "testFile.txt"))
  (catch Exception e (str "Caught exception: " (.getMessage e)))
  )
(def out (FileOutputStream. "testFileOut.txt"))
(def result (countChar file))
(buildCompressionMap (generateTree result) "")
(println @compressionMap)
(println (str "File contains " (count file) " bytes"))
(def compressedFile (compressText file))
(def finalBinary (makeByteSize (str (bitifyMap) (toBinary (count compressedFile) 16) compressedFile)))
(println (str "Final result contains " (float (/ (count finalBinary) 8)) " bytes"))
(def byteArray (byteStringtoChar finalBinary))
(println finalBinary)
(.write out (byte-array (map #(+ 128 (byte (- % 128))) byteArray)))
(println (map #(+ 128 (byte (- % 128))) byteArray))

(println (count (byteStringtoChar finalBinary)))
