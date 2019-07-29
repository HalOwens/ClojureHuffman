(ns decode)
(import java.io.FileInputStream)


(defn toBinary [number]
  (loop [binary ""
         number_t number]
    (if (= number_t 0)
      (str (apply str (repeat (- 8 (count binary)) 0)) binary)
      (recur (str (mod number_t 2) binary) (int (/ number_t 2)))
      )
    )
  )


(defn readInFile []
  (def in (FileInputStream. "testFileOut.txt"))
  (loop [chars (.read in)
         file []]
    (if (= chars -1)
      file
      (recur (.read in) (conj file chars))
      )
    )
  )

(defn byteArrayToString [byteArr]
  (loop [byteArr_t byteArr
         string ""]
    (if (> (count byteArr_t) 0)
      (recur (drop 1 byteArr_t) (str string (toBinary (first byteArr_t))))
      string
      )
    )
  )

(defn pow [n x]
  (reduce * (repeat n x))
  )

(defn toDecimal [binary]
  (loop [length (count binary)
         bits (map #(- (int %1) 48) binary)
         total 0]
    (if (< length 1)
      total
      (recur (dec length) (drop 1 bits) (+ total (* (first bits) (pow (dec length) 2))))
      )
    )
  )

(defn getMapLength [binaryStr]
  (toDecimal (take 16 binaryStr))
  )

(defn takeStr [n strs]
  (apply str (take n strs))
  )

(defn buildMap [binaryStr]
  (let [mapLength (getMapLength binaryStr)
        mapBinary (takeStr mapLength (drop 16 binaryStr))]
    (loop [mapBinary_t mapBinary
           key (toDecimal (takeStr 7 mapBinary))
           codeLength (toDecimal (takeStr 5 (drop 7 mapBinary)))
           keyVal (takeStr codeLength (drop 12 mapBinary))
           finalMap {}
           ]
      (if (= 0 (count mapBinary_t))
        finalMap
        (let [nextBinary (drop (+ 12 (count keyVal)) mapBinary_t)
              nextCodeLength (toDecimal (takeStr 5 (drop 7 nextBinary)))]
          (recur nextBinary
                 (toDecimal (takeStr 7 nextBinary))
                  nextCodeLength
                 (takeStr nextCodeLength (drop 12 nextBinary))
                 (merge finalMap (sorted-map (keyword keyVal) (str (char key)))))
          )
        )
      )
    )
  )

(println (readInFile))
(println (byteArrayToString (readInFile)))
(println (buildMap (byteArrayToString (readInFile))))
;(def file (slurp "testFileOut.txt" :encoding "UTF-8"))
;(print (int (first file)))
