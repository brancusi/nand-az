(ns bangfe.vm-translator
  (:require
   [clojure.string]
   [nano-id.core :refer [nano-id]]
   [bangfe.utils.file :as f]
   [bangfe.utils.string :as s]
   [clojure.tools.cli :refer [parse-opts]]
   [bangfe.constants.memory-segments :as memory-segments])
  (:gen-class))

(defn stash-constant-in-d
  "Stash a constant int in the d register. This uses A to set a constant"
  [i]
  [(format "@%s" i)
   ["D=A"]])

(defn stash-stack-head-in-d
  "Push the current stored d to the head of the stack
   This does not increment the pointer"
  []
  [["@SP"]
   ["A=M"]
   ["D=M"]])

(defn push-d-to-stack-head
  "Push the d to the head of the stack
   This does not increment the pointer"
  []
  [["@SP"]
   ["A=M"]
   ["M=D"]])

(defn move-to-current-stack-pointer
  []
  [["@SP"]
   ["A=M"]])

(defn move-to-previous-stack-pointer
  []
  [["@SP"]
   ["A=M"]])

(defn inc-stack-pointer
  "Increment the stack pointer"
  []
  [(move-to-current-stack-pointer)
   ["@SP"]
   ["M=M+1"]])

(defn dec-stack-pointer
  "Decrement the stack pointer"
  []
  ["@SP"
   "M=M-1"])

(defn push-constant
  [i]
  [(stash-constant-in-d i)
   (push-d-to-stack-head)
   (inc-stack-pointer)])

(defn add-cmd
  []
  [(dec-stack-pointer)
   (stash-stack-head-in-d)
   (dec-stack-pointer)
   (move-to-current-stack-pointer)
   ["D=M+D"]
   ["M=D"]
   (inc-stack-pointer)])

(defn sub-cmd
  []
  [(dec-stack-pointer)
   (stash-stack-head-in-d)
   (dec-stack-pointer)
   (move-to-current-stack-pointer)
   ["D=M-D"]
   ["M=D"]
   (inc-stack-pointer)])

(defn and-cmd
  []
  [(dec-stack-pointer)
   (stash-stack-head-in-d)
   (dec-stack-pointer)
   (move-to-current-stack-pointer)
   ["D=M&D"]
   ["M=D"]
   (inc-stack-pointer)])

(defn or-cmd
  []
  [(dec-stack-pointer)
   (stash-stack-head-in-d)
   (dec-stack-pointer)
   (move-to-current-stack-pointer)
   ["D=M|D"]
   ["M=D"]
   (inc-stack-pointer)])

(defn not-cmd
  []
  [(dec-stack-pointer)
   (stash-stack-head-in-d)
   ["D=!D"]
   ["M=D"]
   (inc-stack-pointer)])

(defn neg-cmd
  []
  [(dec-stack-pointer)
   (stash-stack-head-in-d)
   ["D=-D"]
   ["M=D"]
   (inc-stack-pointer)])

(defn jump-label
  [label]
  [(format "(%s)" label)])

(defn label
  [label]
  [(format "@%s" label)])

;; (defn eq
;;   []
;;   (let [eq-label (str "EQ_" (nano-id))
;;         neq-label (str "NEQ_" (nano-id))
;;         next-label (str "NEXT_" (nano-id))]
;;     [(dec-stack-pointer)
;;      (stash-stack-head-in-d)
;;      (dec-stack-pointer)
;;      (move-to-current-stack-pointer)

;;     ;;  The core cmd to check eq
;;      ["D=M-D"]

;;      (label eq-label)
;;      ["D;JEQ"]

;;      (label neq-label)
;;      ["0;JMP"]

;;      (jump-label eq-label)
;;      ["D=-1"]
;;      (push-d-to-stack-head)
;;      (label next-label)
;;      ["0;JMP"]

;;      (jump-label neq-label)
;;      ["D=0"]
;;      (push-d-to-stack-head)

;;      (jump-label next-label)

;;      (inc-stack-pointer)]))

(def comp-types
  "Each comparison type should stash to D"
  {:eq {:jmp-cmd ["D;JEQ"]
        :comp-cmd [["D=M-D"]]}
   :lt {:jmp-cmd ["D;JLT"]
        :comp-cmd [["D=M-D"]]}
   :gt {:jmp-cmd ["D;JGT"]
        :comp-cmd [["D=M-D"]]}})

(defn end-loop
  []
  (let [end-label (str "END_LOOP_" (nano-id))]
    [(jump-label end-label)
     (label end-label)
     ["0;JMP"]]))

(defn build-comp-cmd
  [type]
  (let [{:keys [jmp-cmd comp-cmd]} (get comp-types type)
        true-label (str "TRUE_" (nano-id))
        false-label (str "FALSE_" (nano-id))
        next-label (str "NEXT_" (nano-id))]
    [(dec-stack-pointer)
     (stash-stack-head-in-d)
     (dec-stack-pointer)
     (move-to-current-stack-pointer)

     comp-cmd

     (label true-label)
     jmp-cmd

     (label false-label)
     ["0;JMP"]

     (jump-label true-label)
     ["D=-1"]

    ;;  True branch
     (push-d-to-stack-head)
     (label next-label)
     ["0;JMP"]

    ;;  False branch
     (jump-label false-label)
     ["D=0"]
     (push-d-to-stack-head)

    ;;  End of cmd
     (jump-label next-label)
     (inc-stack-pointer)]))

;; Hard coded to constant. @TODO Make generic for memory segs
(defn push
  [line]
  (push-constant (last line)))

(defn match
  [line]
  (case (first line)
    "push" (push line)
    "add" (add-cmd)
    "sub" (sub-cmd)
    "neg" (neg-cmd)
    "and" (and-cmd)
    "or" (or-cmd)
    "not" (not-cmd)
    "eq" (build-comp-cmd :eq)
    "lt" (build-comp-cmd :lt)
    "gt" (build-comp-cmd :gt)))

(defn process
  [path]
  (-> (->> (f/load-lines path)
           s/strip-whitespace
           (map #(clojure.string/split % #" "))
           (map match))
      vec
      (conj (end-loop))
      flatten))

(defn -main [& args]
  (let [file (-> (parse-opts args [])
                 :arguments
                 first)]
    (println "Starting")
    (println file)))


(f/to-file
 (process "resources/samples/vm/SimpleAdd.vm")
 "/Users/nevadasmith/Documents/projects/nand2tetris/projects/07/StackArithmetic/SimpleAdd/SimpleAdd.asm")

(f/to-file
 (process "resources/samples/vm/StackTest.vm")
 "/Users/nevadasmith/Documents/projects/nand2tetris/projects/07/StackArithmetic/StackTest/StackTest.asm")
;; => nil


(process "resources/samples/vm/StackTest.vm")
