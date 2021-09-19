(ns bangfe.nand-assembler
  (:require [clojure.java.io]
            [clojure.string :refer [trim]]
            [tupelo.string :as ts])
  (:gen-class))

(def r-address-map (apply merge (for [x (range 16)]
                                  {(str "R" x) x})))

(def additional-symbol-map {"SP" 0
                            "LCL" 1
                            "ARG" 2
                            "THIS" 3
                            "THAT" 4
                            "SCREEN" 16384
                            "KBD" 24576})

(def calc-code-map {"0"   "0101010"
                    "1"   "0111111"
                    "-1"  "0111010"
                    "D"   "0001100"
                    "A"   "0110000"
                    "M"   "1110000"
                    "!D"  "0001101"
                    "!A"  "0110001"
                    "!M"  "1110001"
                    "-D"  "0001111"
                    "-A"  "0110011"
                    "-M"  "1110011"
                    "D+1" "0011111"
                    "A+1" "0110111"
                    "M+1" "1110111"
                    "D-1" "0001110"
                    "A-1" "0110010"
                    "M-1" "1110010"
                    "D+A" "0000010"
                    "D+M" "1000010"
                    "D-A" "0010011"
                    "D-M" "1010011"
                    "A-D" "0000111"
                    "M-D" "1000111"
                    "D&A" "0000000"
                    "D&M" "1000000"
                    "D|A" "0010101"
                    "D|M" "1010101"})

(def dest-code-map {nil   "000"
                    "M"   "001"
                    "D"   "010"
                    "MD"  "011"
                    "A"   "100"
                    "AM"  "101"
                    "AD"  "110"
                    "AMD" "111"})

(def jump-code-map {nil "000"
                    "JGT" "001"
                    "JEQ" "010"
                    "JGE" "011"
                    "JLT" "100"
                    "JNE" "101"
                    "JLE" "110"
                    "JMP" "111"})

(def base-symbol-map (merge r-address-map additional-symbol-map))

(defn to-a-binary
  [num]
  (ts/pad-left (Integer/toBinaryString num) 16 "0"))

(defn load-assembly
  [path]
  (with-open [rdr (clojure.java.io/reader path)]
    (doall (line-seq rdr))))

(defn strip-whitespace
  [lines]
  (->> lines
       (map (fn [line] (clojure.string/replace line #"//.+" "")))
       (map trim)
       (filter seq)))

(defn op-type
  [[op-bit]]
  (if (= \@ op-bit)
    :a
    :c))

(defn is-op-a?
  [line]
  (= :a (op-type line)))

(defn is-op-c?
  [line]
  (= :c (op-type line)))

(defn is-jump-op?
  [line]
  (re-find #";" line))

(defn is-calc-op?
  [line]
  (re-find #"=" line))

(defn is-jump-destination?
  [[first-bit]]
  (= \( first-bit))

(defn get-a-sym
  "Get everything after the @"
  [line]
  (-> line
      rest
      clojure.string/join))

(defn a-op-address
  "Get the int address of this line without the @"
  [line]
  (let [sym (get-a-sym line)]
    (try
      (Integer/parseInt sym)
      (catch Exception _ sym))))

(defn is-direct-a?
  [line]
  (let [address (a-op-address line)]
    (number? address)))

(defn is-base-a?
  [line]
  (let [sym (get-a-sym line)]
    (get base-symbol-map sym)))

(defn parse-jump-declaration
  [line]
  (-> line
      rest
      drop-last
      clojure.string/join))

(defn create-jump-code-map
  ([lines] (create-jump-code-map lines {} 0))
  ([lines mp idx]
   (loop [lines lines
          mp mp
          idx idx]

     (let [line (first lines)
           remainder (rest lines)
           has-more? (seq remainder)
           is-jump-destination? (is-jump-destination? line)]
       (if is-jump-destination?

         (let [ref (parse-jump-declaration line)
               new-mp (assoc mp ref {:og-line idx :projected-line (- idx (count mp))})]
           (if has-more?
             (recur remainder new-mp (inc idx))
             new-mp))

         (if has-more?
           (recur remainder mp (inc idx))
           mp))))))

(defn create-ref-map
  ([lines jump-code-map] (create-ref-map lines jump-code-map {}))
  ([lines jump-code-map mp]
   (loop [lines lines
          mp mp]
     (let [line (first lines)
           remainder (rest lines)
           has-more? (seq remainder)
           is-op-a? (is-op-a? line)]
       (if is-op-a?
         (let [ref (a-op-address line)
               total-refs (+ 16 (count mp))
               is-in-ref-map? (get jump-code-map ref)
               is-direct-a? (is-direct-a? line)
               is-base-a? (is-base-a? line)

               new-mp (if (or is-base-a? is-in-ref-map? is-direct-a?)
                        mp
                        (merge {ref total-refs} mp))]

           (if has-more?
             (recur remainder new-mp)
             (merge
              new-mp
              base-symbol-map)))

         (if has-more?
           (recur remainder mp)
           (merge
            mp
            base-symbol-map)))))))

(defn prepped-asm
  [lines]
  (-> lines
      strip-whitespace))

(defn remove-jump-refs
  [asm]
  (filter
   (fn
     [line]
     (not
      (re-find #"\(" line))) asm))

(defn deref-asm
  [lines]
  (let [asm (prepped-asm lines)
        jump-code-map (create-jump-code-map asm)
        ref-map (create-ref-map asm jump-code-map)
        converted (map (fn [line]
                         (let [is-op-a? (is-op-a? line)]
                           (if is-op-a?
                             (let [address (a-op-address line)
                                   is-direct? (number? address)]
                               (if is-direct?
                                 line
                                 (let [direct-address (or (get-in jump-code-map [address :projected-line])
                                                          (get ref-map address))]
                                   (str "@" direct-address))))

                             line)))
                       asm)
        dereffed (remove-jump-refs converted)]

    {:dereffed dereffed
     :jump-map jump-code-map
     :ref-map ref-map
     :with-jump-refs converted}))

(defn build-calc-binary
  [line]
  (let [[dest calc] (clojure.string/split line #"=")
        calc-code (get calc-code-map calc)
        dest-code (get dest-code-map dest)
        jump-code (get jump-code-map nil)]

    (str "111" calc-code dest-code jump-code)))

(defn build-jump-binary
  [line]
  (let [[calc jump] (clojure.string/split line #";")
        calc-code (get calc-code-map calc)
        dest-code (get dest-code-map nil)
        jump-code (get jump-code-map jump)]

    (str "111" calc-code dest-code jump-code)))

(defn to-binary
  [lines]
  (let [deref-asm (deref-asm lines)
        lines (:dereffed deref-asm)]
    (for [line lines]
      (let [is-a? (is-op-a? line)]
        (if is-a?
          (to-a-binary (a-op-address line))
          (let [is-calc-op? (is-calc-op? line)]
            (if is-calc-op?
              (build-calc-binary line)
              (build-jump-binary line))))))))

(defn to-file
  "Write a lazy seq to a file"
  [seq path]
  (->>
   seq
   (interpose \newline)
   (apply str)
   (spit path)))

(defn assemble
  [lines out]
  (to-file (to-binary lines) out))

#_(to-file (:dereffed (deref-asm (load-assembly "resources/Pong.asm"))) "output/Pong.asm")

(doseq [asm ["Add" "Max" "Pong" "Rect"]]
  (assemble (load-assembly (format "resources/%s.asm" asm)) (format "output/%s.hack" asm)))

#_(assemble (load-assembly "resources/Pong.asm") "output/Pong.hack")


