(ns bangfe.vm-translator
  (:require
   [clojure.string]
   [nano-id.core :refer [nano-id]]
   [bangfe.utils.file :as f]
   [bangfe.utils.string :as s]
   [clojure.tools.cli :refer [parse-opts]]
   [bangfe.constants.memory-segments :as memory-segments :refer [offset-for]])
  (:gen-class))

(defn stash-constant-in-d
  "Stash a constant int in the d register. This uses A to set a constant"
  [i]
  [(format "@%s" i)
   ["D=A"]])

(defn stash-stack-head-in-d
  "Set D to the current value of stack head
   This does not increment the pointer"
  []
  [["@SP"]
   ["A=M"]
   ["D=M"]])

(defn stash-d-to-stack-head
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
  [["@SP"]
   ["M=M+1"]])

(defn dec-stack-pointer
  "Decrement the stack pointer"
  []
  ["@SP"
   "M=M-1"])

(defn push-constant-to->stack
  [i]
  [(stash-constant-in-d i)
   (stash-d-to-stack-head)
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
     (stash-d-to-stack-head)
     (label next-label)
     ["0;JMP"]

    ;;  False branch
     (jump-label false-label)
     ["D=0"]
     (stash-d-to-stack-head)

    ;;  End of cmd
     (jump-label next-label)
     (inc-stack-pointer)]))

(defn sanatize-number
  [n]
  (if (number? n)
    n
    (Integer/parseInt n)))

(defn push-pointer-to->stack
  [address]
  (let [pointer-offset (offset-for :pointer)
        address (sanatize-number address)
        target-address (+ pointer-offset address)]
    [[(format "@%d" target-address)]
     ["D=M"]
     (stash-d-to-stack-head)
     (inc-stack-pointer)]))

(defn push-dynamic-to->stack
  [seg address]
  (let [pointer-offset (offset-for seg)
        sub-address (sanatize-number address)]
    [[(format "@%d" pointer-offset)]
     ["D=M"]
     [(format "@%d" sub-address)]
     ["D=D+A"]
     ["A=D"]
     ["D=M"]

     (stash-d-to-stack-head)
     (inc-stack-pointer)]))

(defn pop-to->pointer
  [address]
  (let [pointer-offset (offset-for :pointer)
        address (sanatize-number address)
        target-address (+ pointer-offset address)]
    [(dec-stack-pointer)
     (stash-stack-head-in-d)
     [(format "@%d" target-address)]
     ["M=D"]]))

(defn pop-to->dynamic
  [seg address]
  (let [pointer-offset (offset-for seg)
        sub-address (sanatize-number address)]
    [[(format "@%d" pointer-offset)]
     ["D=M"]
     [(format "@%d" sub-address)]
     ["D=D+A"]
     ["@STASH_ADDRESS"]
     ["M=D"]

     (dec-stack-pointer)
     (stash-stack-head-in-d)

     ["@STASH_ADDRESS"]
     ["A=M"]

     ["M=D"]]))

(defn push-cmd
  [[_ seg address]]
  (case seg
    "constant" (push-constant-to->stack address)
    "pointer" (push-pointer-to->stack address)
    "local" (push-dynamic-to->stack :local address)
    "argument" (push-dynamic-to->stack :argument address)
    "temp" (push-dynamic-to->stack :temp address)
    "this" (push-dynamic-to->stack :this address)
    "that" (push-dynamic-to->stack :that address)
    "static" (push-dynamic-to->stack :static address)))

(defn pop-cmd
  [[_ seg address]]
  (case seg
    "pointer" (pop-to->pointer address)
    "local" (pop-to->dynamic :local address)
    "argument" (pop-to->dynamic :argument address)
    "temp" (pop-to->dynamic :temp address)
    "this" (pop-to->dynamic :this address)
    "that" (pop-to->dynamic :that address)
    "static" (pop-to->dynamic :static address)))

(defn match
  [line]
  (case (first line)
    "push" (push-cmd line)
    "pop" (pop-cmd line)
    "add" (add-cmd)
    "sub" (sub-cmd)
    "neg" (neg-cmd)
    "and" (and-cmd)
    "or" (or-cmd)
    "not" (not-cmd)
    "eq" (build-comp-cmd :eq)
    "lt" (build-comp-cmd :lt)
    "gt" (build-comp-cmd :gt)))

(defn bootstrap
  []
  [["@5"]
   ["D=A"]
   ["M=D"]])

(defn process
  [path]
  (-> (->> (f/load-lines path)
           s/strip-whitespace
           (map #(clojure.string/split % #" "))
           (map match))
      (conj (bootstrap))
      vec
      (conj (end-loop))
      flatten))

(defn -main [& args]
  (let [file (-> (parse-opts args [])
                 :arguments
                 first)
        name (first (clojure.string/split file #"\."))
        output (str name ".asm")]

    (f/to-file
     (process file)
     output)))

(comment

  (f/to-file
   (process "resources/samples/vm/SimpleAdd.vm")
   "/Users/nevadasmith/Documents/projects/nand2tetris/projects/07/StackArithmetic/SimpleAdd/SimpleAdd.asm")

  (f/to-file
   (process "resources/samples/vm/StackTest.vm")
   "/Users/nevadasmith/Documents/projects/nand2tetris/projects/07/StackArithmetic/StackTest/StackTest.asm")


  (f/to-file
   (process "/Users/nevadasmith/Documents/projects/nand2tetris/projects/07/MemoryAccess/PointerTest/PointerTest.vm")
   "/Users/nevadasmith/Documents/projects/nand2tetris/projects/07/MemoryAccess/PointerTest/PointerTest.asm")

  (f/to-file
   (process "/Users/nevadasmith/Documents/projects/nand2tetris/projects/07/MemoryAccess/StaticTest/StaticTest.vm")
   "/Users/nevadasmith/Documents/projects/nand2tetris/projects/07/MemoryAccess/StaticTest/StaticTest.asm")

  (f/to-file
   (process "/Users/nevadasmith/Documents/projects/nand2tetris/projects/07/MemoryAccess/BasicTest/BasicTest.vm")
   "/Users/nevadasmith/Documents/projects/nand2tetris/projects/07/MemoryAccess/BasicTest/BasicTest.asm")

  (f/load-lines "resources/samples/vm/StackTest.vm")
  (process "resources/samples/vm/StackTest.vm")

  (-> (->> (f/load-lines "/Users/nevadasmith/Documents/projects/nand2tetris/projects/07/MemoryAccess/PointerTest/PointerTest.vm")
           s/strip-whitespace
           (map #(clojure.string/split % #" "))
           (map match))
      vec
      (conj (end-loop))
      flatten))