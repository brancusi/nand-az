(ns bangfe.vm-translator
  (:require
   [clojure.string]
   [bangfe.utils.file :as f]
   [bangfe.utils.string :as s]
   [bangfe.constants.memory-segments :as memory-segments]
   [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(defn stash-in-d
  [i]
  [(format "@%s" i)
   "D=A"])

(defn stash-stack-head-in-d
  "Push the current stored d to the head of the stack
   This does not increment the pointer"
  []
  ["@SP"
   "A=M"
   "D=M"])

(defn move-to-current-stack-pointer
  []
  ["@SP"
   "A=M"])

(defn move-to-previous-stack-pointer
  []
  ["@SP"
   "A=M"])

(defn inc-stack-pointer
  "Increment the stack pointer"
  []
  ["@SP"
   "M=M+1"])

(defn dec-stack-pointer
  "Decrement the stack pointer"
  []
  ["@SP"
   "M=M-1"])

(defn push-d-to-stack-head
  []
  ["M=D"])

(defn push-constant
  [i]
  [(stash-in-d i)
   (move-to-current-stack-pointer)
   (push-d-to-stack-head)
   (inc-stack-pointer)])

(defn add
  []
  [(dec-stack-pointer)
   (stash-stack-head-in-d)
   (dec-stack-pointer)
   (move-to-current-stack-pointer)
   ["D=M+D"]
   ["M=D"]
   (inc-stack-pointer)])

(defn push
  [line]
  (push-constant (last line)))

(defn match
  [line]
  (case (first line)
    "push" (push line)
    "add" (add)))

(defn process
  [path]
  (->> (f/load-lines path)
       s/strip-whitespace
       (map #(clojure.string/split % #" "))
       (map match)
       flatten))

(defn -main [& args]
  (let [file (-> (parse-opts args)
                 :arguments
                 first)]
    (println "Starting")
    (println file)))


(f/to-file
 (process "resources/samples/vm/SimpleAdd.vm")
 "/Users/nevadasmith/Documents/projects/nand2tetris/projects/07/StackArithmetic/SimpleAdd/SimpleAdd.asm")