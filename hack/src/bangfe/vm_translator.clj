(ns bangfe.vm-translator
  (:require
   [clojure.string]
   [nano-id.core :refer [nano-id]]
   [bangfe.utils.number]
   [bangfe.utils.file :as f]
   [bangfe.utils.string :as s]
   [clojure.tools.cli :refer [parse-opts]]
   [bangfe.constants.memory-segments :as memory-segments :refer [offset-for]])
  (:gen-class))

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

(defn move-to-stack-head
  "Move to head of stack pointer"
  []
  [["@SP"]
   ["A=M"]])

(defn constant-to->d
  "Stash a constant int in the d register. This uses A to set a constant"
  [i]
  [(format "@%s" i)
   ["D=A"]])

(defn peek-stack-to->d
  "Peek at the stack and push to d"
  []
  [(move-to-stack-head)
   ["A=A-1"]
   ["D=M"]])

(defn stack-head-to->d
  "Set D to the current value of stack head
   This does not increment the pointer"
  []
  [(move-to-stack-head)
   ["D=M"]])

(defn d-to->stack-head
  "Push the d to the head of the stack
   This does not increment the pointer"
  []
  [(move-to-stack-head)
   ["M=D"]])

(defn push-constant-to->stack
  "Push a constant to the head of the stack"
  [i]
  [(constant-to->d i)
   (d-to->stack-head)
   (inc-stack-pointer)])

(defn pop-stack-head-to->d
  "This pops the last element from the stack, storing the first is d"
  []
  [(dec-stack-pointer)
   (stack-head-to->d)])

(defn generic-computation1
  "Used when there is a single element on the stack to calculate on"
  [cmd]
  [(pop-stack-head-to->d)
   cmd
   ["M=D"]
   (inc-stack-pointer)])

(defn generic-computation2
  "Used when there are two elements on the stack to calculate on"
  [cmd]
  [(pop-stack-head-to->d)
   (dec-stack-pointer)
   (move-to-stack-head)
   cmd
   ["M=D"]
   (inc-stack-pointer)])

(defn jump-label
  "Create a jump label"
  [label]
  [(format "(%s)" label)])

(defn label
  "Create a symbol"
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

(defn build-comparison-cmd
  [type]
  (let [{:keys [jmp-cmd comp-cmd]} (get comp-types type)
        true-label (str "TRUE_" (nano-id))
        false-label (str "FALSE_" (nano-id))
        next-label (str "NEXT_" (nano-id))]
    [(pop-stack-head-to->d)
     (move-to-stack-head)

     comp-cmd

     (label true-label)
     jmp-cmd

     (label false-label)
     ["0;JMP"]

     (jump-label true-label)
     ["D=-1"]

    ;;  True branch
     (d-to->stack-head)
     (label next-label)
     ["0;JMP"]

    ;;  False branch
     (jump-label false-label)
     ["D=0"]
     (d-to->stack-head)

    ;;  End of cmd
     (jump-label next-label)
     (inc-stack-pointer)]))

(defn push-pointer-to->stack
  [address]
  (let [pointer-offset (offset-for :pointer)
        address (bangfe.utils.number/sanatize-number address)
        target-address (+ pointer-offset address)]
    [[(format "@%d" target-address)]
     ["D=M"]
     (d-to->stack-head)
     (inc-stack-pointer)]))

(defn push-dynamic-to->stack
  [seg address]
  (let [pointer-offset (offset-for seg)
        sub-address (bangfe.utils.number/sanatize-number address)]
    [[(format "@%d" pointer-offset)]
     ["D=M"]
     [(format "@%d" sub-address)]
     ["D=D+A"]
     ["A=D"]
     ["D=M"]

     (d-to->stack-head)
     (inc-stack-pointer)]))

(defn pop-to->pointer
  [address]
  (let [pointer-offset (offset-for :pointer)
        address (bangfe.utils.number/sanatize-number address)
        target-address (+ pointer-offset address)]
    [(pop-stack-head-to->d)
     [(format "@%d" target-address)]
     ["M=D"]]))

(defn pop-to->dynamic
  "A generic pop function that looks up a virtual pointer and selects"
  [seg address]
  (let [pointer-offset (offset-for seg)
        sub-address (bangfe.utils.number/sanatize-number address)]
    [[(format "@%d" pointer-offset)]
     ["D=M"]

     [(format "@%d" sub-address)]
     ["D=D+A"]

     ["@STASH"]
     ["M=D"]

     (pop-stack-head-to->d)

     ["@STASH"]
     ["A=M"]

     ["M=D"]]))

(defn push-static-to->stack
  [address]
  (let [static-address (str "@STATIC." address)]
    [[static-address]
     ["D=M"]
     (d-to->stack-head)
     (inc-stack-pointer)]))

(defn pop-to->static
  [address]
  (let [static-address (str "@STATIC." address)]
    [(pop-stack-head-to->d)
     [static-address]
     ["M=D"]]))

(defn push-temp-to->stack
  [address]
  (let [address (bangfe.utils.number/sanatize-number address)
        offset-address (+ address (memory-segments/offset-for :temp))
        formatted-address (str "@" offset-address)]
    [[formatted-address]
     ["D=M"]
     (d-to->stack-head)
     (inc-stack-pointer)]))

(defn pop-to->temp
  [address]
  (let [address (bangfe.utils.number/sanatize-number address)
        offset-address (+ address (memory-segments/offset-for :temp))
        formatted-address (str "@" offset-address)]
    [(pop-stack-head-to->d)
     [formatted-address]
     ["M=D"]]))

(defn push-cmd
  [[_ seg address]]
  (case seg
    "constant"  (push-constant-to->stack address)

    "local"     (push-dynamic-to->stack :local address)
    "argument"  (push-dynamic-to->stack :argument address)

    "this"      (push-dynamic-to->stack :this address)
    "that"      (push-dynamic-to->stack :that address)

    "temp"      (push-temp-to->stack address)

    "static"    (push-static-to->stack address)

    "pointer"   (push-pointer-to->stack address)))

(defn pop-cmd
  [[_ seg address]]
  (case seg
    "local"    (pop-to->dynamic :local address)
    "argument" (pop-to->dynamic :argument address)

    "this"     (pop-to->dynamic :this address)
    "that"     (pop-to->dynamic :that address)

    "temp"     (pop-to->temp address)

    "static"   (pop-to->static address)

    "pointer"  (pop-to->pointer address)))

(defn goto-if-cmd
  [[_ address]]
  [(pop-stack-head-to->d)
   [(label address)]
   ["D;JNE"]])

(defn goto-cmd
  [[_ address]]
  [[(label address)]
   ["0;JMP"]])

(defn match
  [line]
  (case (first line)
    "push"  (push-cmd line)
    "pop"   (pop-cmd line)

    "not"   (generic-computation1 ["D=!D"])
    "neg"   (generic-computation1 ["D=-D"])

    "add"   (generic-computation2 ["D=M+D"])
    "sub"   (generic-computation2 ["D=M-D"])

    "and"   (generic-computation2 ["D=M&D"])
    "or"    (generic-computation2 ["D=M|D"])

    "eq"    (build-comparison-cmd :eq)
    "lt"    (build-comparison-cmd :lt)
    "gt"    (build-comparison-cmd :gt)

    "label" (jump-label (last line))

    "if-goto" (goto-if-cmd line)
    "goto" (goto-cmd line)))

(defn bootstrap
  "Bootstrap code to set up"
  []
  [])

(defn process
  [path]
  (-> (->> (f/load-lines path)
           s/strip-whitespace
           (map #(clojure.string/split % #" "))
           (map (fn [line] [(str "// " line) (match line)])))
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

  (f/to-file
   (process "/Users/nevadasmith/Documents/projects/nand2tetris/projects/08/ProgramFlow/BasicLoop/BasicLoop.vm")
   "/Users/nevadasmith/Documents/projects/nand2tetris/projects/08/ProgramFlow/BasicLoop/BasicLoop.asm")

  (f/to-file
   (process "/Users/nevadasmith/Documents/projects/nand2tetris/projects/08/ProgramFlow/FibonacciSeries/FibonacciSeries.vm")
   "/Users/nevadasmith/Documents/projects/nand2tetris/projects/08/ProgramFlow/FibonacciSeries/FibonacciSeries.asm")

  (f/load-lines "resources/samples/vm/StackTest.vm")
  (process "resources/samples/vm/StackTest.vm")

  (-> (->> (f/load-lines "/Users/nevadasmith/Documents/projects/nand2tetris/projects/07/MemoryAccess/PointerTest/PointerTest.vm")
           s/strip-whitespace
           (map #(clojure.string/split % #" "))
           (map match))
      vec
      (conj (end-loop))
      flatten))