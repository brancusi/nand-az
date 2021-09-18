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


(deref-asm "resources/Max.asm")
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


