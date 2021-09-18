(ns bangfe.nand-assembler
  (:require [clojure.java.io]
            [clojure.string :refer [trim]])
  (:gen-class))

(def r-address-map (apply merge (for [x (range 17)]
                                  {(str "R" x) x})))

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
  [line]
  (let [sym (get-a-sym line)]
    (try
      (Integer/parseInt sym)
      (catch Exception _ sym))))

(defn parse-jump-declaration
  [line]
  (-> line
      rest
      drop-last
      clojure.string/join))

(defn create-jump-map
  ([lines] (create-jump-map lines {} 0))
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
  ([lines] (create-ref-map lines {} 16))
  ([lines mp start-reg]
   (loop [lines lines
          mp mp
          start-reg start-reg]

     (let [line (first lines)
           remainder (rest lines)
           has-more? (seq remainder)
           is-op-a? (is-op-a? line)]
       (if is-op-a?
         (let [ref (a-op-address line)
               new-mp (merge {ref (+ start-reg (inc (count mp)))} mp)]
           (if has-more?
             (recur remainder new-mp start-reg)
             (merge
              new-mp
              r-address-map)))

         (if has-more?
           (recur remainder mp start-reg)
           (merge mp
                  r-address-map)))))))

(defn prepped-asm
  [path]
  (-> (load-assembly path)
      strip-whitespace))

(defn remove-jump-refs
  [asm]
  (filter
   (fn
     [line]
     (not
      (re-find #"\(" line))) asm))

(defn deref-asm
  [path]
  (let [asm (prepped-asm path)
        jump-map (create-jump-map asm)
        ref-map (create-ref-map asm)
        converted (map (fn [line]
                         (let [is-op-a? (is-op-a? line)]
                           (if is-op-a?
                             (let [address (a-op-address line)
                                   is-direct? (number? address)]
                               (if is-direct?
                                 line
                                 (let [direct-address (or (get-in jump-map [address :projected-line])
                                                          (get ref-map address))]
                                   (str "@" direct-address))))

                             line)))
                       asm)
        dereffed (remove-jump-refs converted)]

    [dereffed
     jump-map
     ref-map]
    #_(loop [lines asm
             remainder (rest lines)
             has-more? (seq remainder)]
        (let []

          (if has-more?
            (recur))))))
;; => #'bangfe.nand-assembler/deref-asm


(deref-asm "resources/Pong.asm")
;; => [("@256"
;;      "D=A"
;;      "@18"
;;      "M=D"
;;      "@133"
;;      "0;JMP"
;;      "@15"
;;      "M=D"
;;      "@18"
;;      "AM=M-1"
;;      "D=M"
;;      "A=A-1"
;;      "D=M-D"
;;      "M=0"
;;      "@19"
;;      "D;JNE"
;;      "@18"
;;      "A=M-1"
;;      "M=-1"
;;      "@15"
;;      "A=M"
;;      "0;JMP"
;;      "@15"
;;      "M=D"
;;      "@18"
;;      "AM=M-1"
;;      "D=M"
;;      "A=A-1"
;;      "D=M-D"
;;      "M=0"
;;      "@35"
;;      "D;JLE"
;;      "@18"
;;      "A=M-1"
;;      "M=-1"
;;      "@15"
;;      "A=M"
;;      "0;JMP"
;;      "@15"
;;      "M=D"
;;      "@18"
;;      "AM=M-1"
;;      "D=M"
;;      "A=A-1"
;;      "D=M-D"
;;      "M=0"
;;      "@51"
;;      "D;JGE"
;;      "@18"
;;      "A=M-1"
;;      ...)
;;     {"RET_ADDRESS_CALL198" {:og-line 15561, :projected-line 15088},
;;      "RET_ADDRESS_EQ33" {:og-line 26627, :projected-line 25829},
;;      "RET_ADDRESS_GT37" {:og-line 24062, :projected-line 23357},
;;      "RET_ADDRESS_GT11" {:og-line 7427, :projected-line 7108},
;;      "RET_ADDRESS_CALL124" {:og-line 7246, :projected-line 6934},
;;      "string.intvalue$if_true1" {:og-line 26807, :projected-line 25999},
;;      "math.divide$while_exp0" {:og-line 7841, :projected-line 7504},
;;      "RET_ADDRESS_CALL145" {:og-line 11113, :projected-line 10693},
;;      "RET_ADDRESS_EQ11" {:og-line 5047, :projected-line 4846},
;;      "ball.draw" {:og-line 529, :projected-line 514},
;;      "RET_ADDRESS_CALL59" {:og-line 4338, :projected-line 4177},
;;      "ponggame.moveball$if_false1" {:og-line 6018, :projected-line 5771},
;;      "RET_ADDRESS_CALL40" {:og-line 3329, :projected-line 3200},
;;      "math.divide$if_false0" {:og-line 7671, :projected-line 7341},
;;      "screen.drawline$if_false2" {:og-line 22497, :projected-line 21844},
;;      "RET_ADDRESS_CALL280" {:og-line 23877, :projected-line 23179},
;;      "ball.setdestination$if_true0" {:og-line 850, :projected-line 826},
;;      "RET_ADDRESS_CALL111" {:og-line 6391, :projected-line 6118},
;;      "RET_ADDRESS_CALL239" {:og-line 20034, :projected-line 19484},
;;      "RET_ADDRESS_CALL77" {:og-line 4918, :projected-line 4729},
;;      "RET_ADDRESS_LT49" {:og-line 24105, :projected-line 23398},
;;      "output.createshiftedmap$while_exp1" {:og-line 19162, :projected-line 18646},
;;      "ponggame.getinstance" {:og-line 4709, :projected-line 4531},
;;      "RET_ADDRESS_CALL180" {:og-line 14017, :projected-line 13562},
;;      "RET_ADDRESS_EQ25" {:og-line 20373, :projected-line 19804},
;;      "LOOP_screen.drawrectangle" {:og-line 23028, :projected-line 22356},
;;      "RET_ADDRESS_GT30" {:og-line 22482, :projected-line 21830},
;;      "RET_ADDRESS_CALL266" {:og-line 22604, :projected-line 21948},
;;      "ponggame.run$if_true2" {:og-line 5012, :projected-line 4816},
;;      "ponggame.new" {:og-line 4084, :projected-line 3930},
;;      "ball.move$if_false8" {:og-line 1738, :projected-line 1675},
;;      "RET_ADDRESS_CALL333" {:og-line 28341, :projected-line 27461},
;;      "math.divide$if_true4" {:og-line 8405, :projected-line 8054},
;;      "RET_ADDRESS_GT34" {:og-line 23121, :projected-line 22445},
;;      "RET_ADDRESS_CALL276" {:og-line 23521, :projected-line 22835},
;;      "sys.wait" {:og-line 28057, :projected-line 27194},
;;      "ball.move$if_false1" {:og-line 1378, :projected-line 1335},
;;      "output.getmap$if_false0" {:og-line 19454, :projected-line 18925},
;;      "RET_ADDRESS_LT19" {:og-line 7687, :projected-line 7356},
;;      "RET_ADDRESS_CALL326" {:og-line 28031, :projected-line 27172},
;;      "RET_ADDRESS_CALL210" {:og-line 16533, :projected-line 16048},
;;      "RET_ADDRESS_LT58" {:og-line 26312, :projected-line 25526},
;;      "math.min$if_false0" {:og-line 8818, :projected-line 8444},
;;      "RET_ADDRESS_CALL45" {:og-line 3771, :projected-line 3627},
;;      "ball.move$if_end6" {:og-line 1607, :projected-line 1551},
;;      "sys.wait$while_end1" {:og-line 28195, :projected-line 27323},
;;      "RET_ADDRESS_LT54" {:og-line 25425, :projected-line 24680},
;;      "RET_ADDRESS_CALL191" {:og-line 14965, :projected-line 14499},
;;      "RET_ADDRESS_CALL179" {:og-line 13931, :projected-line 13477},
;;      "RET_ADDRESS_CALL288" {:og-line 24380, :projected-line 23665},
;;      ...}
;;     {0 32,
;;      "RET_ADDRESS_CALL198" 636,
;;      "RET_ADDRESS_EQ33" 986,
;;      "R4" 4,
;;      "RET_ADDRESS_GT37" 898,
;;      "RET_ADDRESS_GT11" 418,
;;      "RET_ADDRESS_CALL124" 411,
;;      "string.intvalue$if_true1" 995,
;;      "math.divide$while_exp0" 443,
;;      "RET_ADDRESS_CALL145" 538,
;;      "RET_ADDRESS_EQ11" 280,
;;      "ball.draw" 55,
;;      "RET_ADDRESS_CALL59" 223,
;;      "ponggame.moveball$if_false1" 324,
;;      "RET_ADDRESS_CALL40" 178,
;;      "math.divide$if_false0" 427,
;;      "screen.drawline$if_false2" 844,
;;      "RET_ADDRESS_CALL280" 891,
;;      "ball.setdestination$if_true0" 68,
;;      8192 802,
;;      "RET_ADDRESS_CALL111" 366,
;;      "RET_ADDRESS_CALL239" 738,
;;      "RET_ADDRESS_CALL77" 268,
;;      "RET_ADDRESS_LT49" 901,
;;      "output.createshiftedmap$while_exp1" 708,
;;      "ponggame.getinstance" 200,
;;      "RET_ADDRESS_CALL180" 603,
;;      "output.2" 505,
;;      "RET_ADDRESS_EQ25" 762,
;;      "LOOP_screen.drawrectangle" 864,
;;      121 686,
;;      "RET_ADDRESS_GT30" 845,
;;      "RET_ADDRESS_CALL266" 848,
;;      "ponggame.run$if_true2" 278,
;;      65 585,
;;      "ponggame.new" 250,
;;      "ball.move$if_false8" 112,
;;      70 595,
;;      "RET_ADDRESS_CALL333" 1069,
;;      "math.divide$if_true4" 450,
;;      "RET_ADDRESS_GT34" 868,
;;      "RET_ADDRESS_CALL276" 878,
;;      "ball.move$if_false1" 89,
;;      "output.getmap$if_false0" 717,
;;      "RET_ADDRESS_LT19" 429,
;;      62 573,
;;      "RET_ADDRESS_CALL326" 1051,
;;      74 602,
;;      "RET_ADDRESS_CALL210" 656,
;;      "RET_ADDRESS_LT58" 976,
;;      ...}]

;; => [("@0" "D=M" "@1" "D=D-M" "@10" "D;JGT" "@1" "D=M" "@12" "0;JMP" "@0" "D=M" "@2" "M=D" "@14" "0;JMP")
;;     {"OUTPUT_FIRST" {:og-line 10, :projected-line 10},
;;      "OUTPUT_D" {:og-line 13, :projected-line 12},
;;      "INFINITE_LOOP" {:og-line 16, :projected-line 14}}
;;     {"R4" 4,
;;      "R3" 3,
;;      "R13" 13,
;;      "INFINITE_LOOP" 22,
;;      "R8" 8,
;;      "R12" 12,
;;      "R6" 6,
;;      "R10" 10,
;;      "R5" 5,
;;      "R16" 16,
;;      "R7" 7,
;;      "R2" 2,
;;      "R9" 9,
;;      "R14" 14,
;;      "OUTPUT_FIRST" 19,
;;      "R15" 15,
;;      "OUTPUT_D" 20,
;;      "R11" 11,
;;      "R0" 0,
;;      "R1" 1}]

;; => [("@0"
;;      "D=M"
;;      "@1"
;;      "D=D-M"
;;      "@10"
;;      "D;JGT"
;;      "@1"
;;      "D=M"
;;      "@12"
;;      "0;JMP"
;;      "(OUTPUT_FIRST)"
;;      "@0"
;;      "D=M"
;;      "(OUTPUT_D)"
;;      "@2"
;;      "M=D"
;;      "(INFINITE_LOOP)"
;;      "@14"
;;      "0;JMP")
;;     {"OUTPUT_FIRST" {:og-line 10, :projected-line 10},
;;      "OUTPUT_D" {:og-line 13, :projected-line 12},
;;      "INFINITE_LOOP" {:og-line 16, :projected-line 14}}
;;     {"R4" 4,
;;      "R3" 3,
;;      "R13" 13,
;;      "INFINITE_LOOP" 22,
;;      "R8" 8,
;;      "R12" 12,
;;      "R6" 6,
;;      "R10" 10,
;;      "R5" 5,
;;      "R16" 16,
;;      "R7" 7,
;;      "R2" 2,
;;      "R9" 9,
;;      "R14" 14,
;;      "OUTPUT_FIRST" 19,
;;      "R15" 15,
;;      "OUTPUT_D" 20,
;;      "R11" 11,
;;      "R0" 0,
;;      "R1" 1}]

;; => [("@0"
;;      "D=M"
;;      "@1"
;;      "D=D-M"
;;      "@{:og-line 10, :projected-line 10}"
;;      "D;JGT"
;;      "@1"
;;      "D=M"
;;      "@{:og-line 13, :projected-line 12}"
;;      "0;JMP"
;;      "(OUTPUT_FIRST)"
;;      "@0"
;;      "D=M"
;;      "(OUTPUT_D)"
;;      "@2"
;;      "M=D"
;;      "(INFINITE_LOOP)"
;;      "@{:og-line 16, :projected-line 14}"
;;      "0;JMP")
;;     {"OUTPUT_FIRST" {:og-line 10, :projected-line 10},
;;      "OUTPUT_D" {:og-line 13, :projected-line 12},
;;      "INFINITE_LOOP" {:og-line 16, :projected-line 14}}
;;     {"R4" 4,
;;      "R3" 3,
;;      "R13" 13,
;;      "INFINITE_LOOP" 22,
;;      "R8" 8,
;;      "R12" 12,
;;      "R6" 6,
;;      "R10" 10,
;;      "R5" 5,
;;      "R16" 16,
;;      "R7" 7,
;;      "R2" 2,
;;      "R9" 9,
;;      "R14" 14,
;;      "OUTPUT_FIRST" 19,
;;      "R15" 15,
;;      "OUTPUT_D" 20,
;;      "R11" 11,
;;      "R0" 0,
;;      "R1" 1}]

;; => [("@0"
;;      "D=M"
;;      "@1"
;;      "D=D-M"
;;      "@19"
;;      "D;JGT"
;;      "@1"
;;      "D=M"
;;      "@20"
;;      "0;JMP"
;;      "(OUTPUT_FIRST)"
;;      "@0"
;;      "D=M"
;;      "(OUTPUT_D)"
;;      "@2"
;;      "M=D"
;;      "(INFINITE_LOOP)"
;;      "@22"
;;      "0;JMP")
;;     {"OUTPUT_FIRST" {:og-line 10, :projected-line 10},
;;      "OUTPUT_D" {:og-line 13, :projected-line 12},
;;      "INFINITE_LOOP" {:og-line 16, :projected-line 14}}
;;     {"R4" 4,
;;      "R3" 3,
;;      "R13" 13,
;;      "INFINITE_LOOP" 22,
;;      "R8" 8,
;;      "R12" 12,
;;      "R6" 6,
;;      "R10" 10,
;;      "R5" 5,
;;      "R16" 16,
;;      "R7" 7,
;;      "R2" 2,
;;      "R9" 9,
;;      "R14" 14,
;;      "OUTPUT_FIRST" 19,
;;      "R15" 15,
;;      "OUTPUT_D" 20,
;;      "R11" 11,
;;      "R0" 0,
;;      "R1" 1}]

;; => Execution error (ClassCastException) at bangfe.nand-assembler/create-ref-map (REPL:104).
;;    class clojure.lang.PersistentArrayMap cannot be cast to class java.util.Map$Entry (clojure.lang.PersistentArrayMap is in unnamed module of loader 'app'; java.util.Map$Entry is in module java.base of loader 'bootstrap')

;; => [("@17"
;;      "D=M"
;;      "@18"
;;      "D=D-M"
;;      "@19"
;;      "D;JGT"
;;      "@18"
;;      "D=M"
;;      "@20"
;;      "0;JMP"
;;      "(OUTPUT_FIRST)"
;;      "@17"
;;      "D=M"
;;      "(OUTPUT_D)"
;;      "@21"
;;      "M=D"
;;      "(INFINITE_LOOP)"
;;      "@22"
;;      "0;JMP")
;;     {"OUTPUT_FIRST" {:og-line 10, :projected-line 10},
;;      "OUTPUT_D" {:og-line 13, :projected-line 12},
;;      "INFINITE_LOOP" {:og-line 16, :projected-line 14}}
;;     {"INFINITE_LOOP" 22, "R2" 21, "R0" 17, "OUTPUT_D" 20, "R1" 18, "OUTPUT_FIRST" 19}]

;; => [("@18"
;;      "D=M"
;;      "@19"
;;      "D=D-M"
;;      "@20"
;;      "D;JGT"
;;      "@19"
;;      "D=M"
;;      "@21"
;;      "0;JMP"
;;      "(OUTPUT_FIRST)"
;;      "@18"
;;      "D=M"
;;      "(OUTPUT_D)"
;;      "@22"
;;      "M=D"
;;      "(INFINITE_LOOP)"
;;      "@23"
;;      "0;JMP")
;;     {"OUTPUT_FIRST" {:og-line 10, :projected-line 10},
;;      "OUTPUT_D" {:og-line 13, :projected-line 12},
;;      "INFINITE_LOOP" {:og-line 16, :projected-line 14}}
;;     {"INFINITE_LOOP" 23, "R2" 22, "R0" 18, "OUTPUT_D" 21, "R1" 19, "OUTPUT_FIRST" 20}]

;; => [("@1"
;;      "D=M"
;;      "@2"
;;      "D=D-M"
;;      "@3"
;;      "D;JGT"
;;      "@2"
;;      "D=M"
;;      "@4"
;;      "0;JMP"
;;      "(OUTPUT_FIRST)"
;;      "@1"
;;      "D=M"
;;      "(OUTPUT_D)"
;;      "@5"
;;      "M=D"
;;      "(INFINITE_LOOP)"
;;      "@6"
;;      "0;JMP")
;;     {"OUTPUT_FIRST" {:og-line 10, :projected-line 10},
;;      "OUTPUT_D" {:og-line 13, :projected-line 12},
;;      "INFINITE_LOOP" {:og-line 16, :projected-line 14}}
;;     {"INFINITE_LOOP" 6, "R2" 5, "R0" 1, "OUTPUT_D" 4, "R1" 2, "OUTPUT_FIRST" 3}]

;; => [("@28"
;;      "D=M"
;;      "@23"
;;      "D=D-M"
;;      "@21"
;;      "D;JGT"
;;      "@23"
;;      "D=M"
;;      "@25"
;;      "0;JMP"
;;      "(OUTPUT_FIRST)"
;;      "@28"
;;      "D=M"
;;      "(OUTPUT_D)"
;;      "@31"
;;      "M=D"
;;      "(INFINITE_LOOP)"
;;      "@34"
;;      "0;JMP")
;;     {"OUTPUT_FIRST" {:og-line 10, :projected-line 10},
;;      "OUTPUT_D" {:og-line 13, :projected-line 12},
;;      "INFINITE_LOOP" {:og-line 16, :projected-line 14}}
;;     {"R0" 28, "R1" 23, "OUTPUT_FIRST" 21, "OUTPUT_D" 25, "R2" 31, "INFINITE_LOOP" 34}]

;; => [("@28"
;;      "D=M"
;;      "@23"
;;      "D=D-M"
;;      "@21"
;;      "D;JGT"
;;      "@23"
;;      "D=M"
;;      "@25"
;;      "0;JMP"
;;      "(OUTPUT_FIRST)"
;;      "@28"
;;      "D=M"
;;      "(OUTPUT_D)"
;;      "@31"
;;      "M=D"
;;      "(INFINITE_LOOP)"
;;      "@34"
;;      "0;JMP")
;;     {"OUTPUT_FIRST" {:og-line 10, :projected-line 10},
;;      "OUTPUT_D" {:og-line 13, :projected-line 12},
;;      "INFINITE_LOOP" {:og-line 16, :projected-line 14}}]

;; => [("@R0 - is op"
;;      "D=M"
;;      "@R1 - is op"
;;      "D=D-M"
;;      "@OUTPUT_FIRST - is op"
;;      "D;JGT"
;;      "@R1 - is op"
;;      "D=M"
;;      "@OUTPUT_D - is op"
;;      "0;JMP"
;;      "(OUTPUT_FIRST)"
;;      "@R0 - is op" - ===10
;;      "D=M"
;;      "(OUTPUT_D)"
;;      "@R2 - is op" - ===12
;;      "M=D"
;;      "(INFINITE_LOOP)"
;;      "@INFINITE_LOOP - is op" - === 15
;;      "0;JMP")
;;     {"OUTPUT_FIRST" {:og-line 10, :projected-line 10},
;;      "OUTPUT_D" {:og-line 13, :projected-line 12},
;;      "INFINITE_LOOP" {:og-line 16, :projected-line 14}}]

;; => ("@R0 - is op"
;;     "D=M"
;;     "@R1 - is op"
;;     "D=D-M"
;;     "@OUTPUT_FIRST - is op"
;;     "D;JGT"
;;     "@R1 - is op"
;;     "D=M"
;;     "@OUTPUT_D - is op"
;;     "0;JMP"
;;     "(OUTPUT_FIRST)"
;;     "@R0 - is op"
;;     "D=M"
;;     "(OUTPUT_D)"
;;     "@R2 - is op"
;;     "M=D"
;;     "(INFINITE_LOOP)"
;;     "@INFINITE_LOOP - is op"
;;     "0;JMP")

;; => ("@R0 - is op"
;;     nil
;;     "@R1 - is op"
;;     nil
;;     "@OUTPUT_FIRST - is op"
;;     nil
;;     "@R1 - is op"
;;     nil
;;     "@OUTPUT_D - is op"
;;     nil
;;     nil
;;     "@R0 - is op"
;;     nil
;;     nil
;;     "@R2 - is op"
;;     nil
;;     nil
;;     "@INFINITE_LOOP - is op"
;;     nil)

;; => ("@R0"
;;     "D=M"
;;     "@R1"
;;     "D=D-M"
;;     "@OUTPUT_FIRST"
;;     "D;JGT"
;;     "@R1"
;;     "D=M"
;;     "@OUTPUT_D"
;;     "0;JMP"
;;     "(OUTPUT_FIRST)"
;;     "@R0"
;;     "D=M"
;;     "(OUTPUT_D)"
;;     "@R2"
;;     "M=D"
;;     "(INFINITE_LOOP)"
;;     "@INFINITE_LOOP"
;;     "0;JMP")



;; => {"OUTPUT_FIRST" 10, "OUTPUT_D" 13, "INFINITE_LOOP" 16}

;; => {"OUTPUT_FIRST" 10, "OUTPUT_D" 13, "INFINITE_LOOP" 16}

;; => {"@R0" 11, "@R1" 6, "@OUTPUT_FIRST" 4, "@OUTPUT_D" 8, "@R2" 14, "@INFINITE_LOOP" 17}

;; => {"(OUTPUT_FIRST)" 10,
;;     "D=M" 12,
;;     "@R1" 6,
;;     "@INFINITE_LOOP" 17,
;;     "@OUTPUT_FIRST" 4,
;;     "M=D" 15,
;;     "@R2" 14,
;;     "@R0" 11,
;;     "D=D-M" 3,
;;     "(OUTPUT_D)" 13,
;;     "(INFINITE_LOOP)" 16,
;;     "0;JMP" 18,
;;     "@OUTPUT_D" 8,
;;     "D;JGT" 5}

;; => {"(OUTPUT_FIRST)" "(OUTPUT_FIRST)",
;;     "D=M" "D=M",
;;     "@R1" "@R1",
;;     "@INFINITE_LOOP" "@INFINITE_LOOP",
;;     "@OUTPUT_FIRST" "@OUTPUT_FIRST",
;;     "M=D" "M=D",
;;     "@R2" "@R2",
;;     "@R0" "@R0",
;;     "D=D-M" "D=D-M",
;;     "(OUTPUT_D)" "(OUTPUT_D)",
;;     "(INFINITE_LOOP)" "(INFINITE_LOOP)",
;;     "0;JMP" "0;JMP",
;;     "@OUTPUT_D" "@OUTPUT_D",
;;     "D;JGT" "D;JGT"}

;; => nil

;; => nil

;; => nil


;; => ("@R0"
;;     "D=M"
;;     "@R1"
;;     "D=D-M"
;;     "@OUTPUT_FIRST"
;;     "D;JGT"
;;     "@R1"
;;     "D=M"
;;     "@OUTPUT_D"
;;     "0;JMP"
;;     "(OUTPUT_FIRST)"
;;     "@R0"
;;     "D=M"
;;     "(OUTPUT_D)"
;;     "@R2"
;;     "M=D"
;;     "(INFINITE_LOOP)"
;;     "@INFINITE_LOOP"
;;     "0;JMP")

;; => Error printing return value (ArityException) at clojure.lang.AFn/throwArity (AFn.java:429).
;;    Wrong number of args (3) passed to: clojure.core/replace





(def phone-number "672-345-456-3212")
(def matcher (re-matcher #"( D)(//)" sample))
(re-find matcher)
;; => ("@R0"
;;     "@R1"
;;     "@OUTPUT_FIRST"
;;     "@R1"
;;     "@OUTPUT_D"
;;     "(OUTPUT_FIRST)"
;;     "@R0"
;;     "(OUTPUT_D)"
;;     "@R2"
;;     "(INFINITE_LOOP)"
;;     "@INFINITE_LOOP")

;; => ("   @R0"
;;     "   @R1"
;;     "   @OUTPUT_FIRST"
;;     "   @R1"
;;     "   @OUTPUT_D"
;;     "(OUTPUT_FIRST)"
;;     "   @R0             "
;;     "(OUTPUT_D)"
;;     "   @R2"
;;     "(INFINITE_LOOP)"
;;     "   @INFINITE_LOOP")

;; => Syntax error compiling at (src/bangfe/nand_assembler.clj:25:1).
;;    Unable to resolve symbol: process in this context




;; => ("@2" "D=A" "@3" "D=D+A" "@0" "M=D")

;; => ("" "" "@2" "D=A" "@3" "D=D+A" "@0" "M=D")

;; => Error printing return value (ArityException) at clojure.lang.AFn/throwArity (AFn.java:429).
;;    Wrong number of args (1) passed to: bangfe.nand-assembler/strip-whitespace/fn--7264

;; => ("// This file is part of www.nand2tetris.org"
;;     "// and the book \"The Elements of Computing Systems\""
;;     "// by Nisan and Schocken, MIT Press."
;;     "// File name: projects/06/add/Add.asm"
;;     ""
;;     "// Computes R0 = 2 + 3  (R0 refers to RAM[0])"
;;     ""
;;     "@2"
;;     "D=A"
;;     "@3"
;;     "D=D+A"
;;     "@0"
;;     "M=D")

;; => Syntax error compiling at (src/bangfe/nand_assembler.clj:26:1).
;;    Unable to resolve symbol: strip-whitespace in this context

;; => ("// This file is part of www.nand2tetris.org"
;;     "// and the book \"The Elements of Computing Systems\""
;;     "// by Nisan and Schocken, MIT Press."
;;     "// File name: projects/06/add/Add.asm"
;;     ""
;;     "// Computes R0 = 2 + 3  (R0 refers to RAM[0])"
;;     ""
;;     "@2"
;;     "D=A"
;;     "@3"
;;     "D=D+A"
;;     "@0"
;;     "M=D")


