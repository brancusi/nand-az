(ns bangfe.vm-translator
  (:require
   [clojure.string]
   [nano-id.core :refer [nano-id]]
   [bangfe.utils.number]
   [bangfe.utils.utils :refer [for-indexed]]
   [bangfe.utils.file :as f]
   [bangfe.utils.string :as s]
   [clojure.tools.cli :refer [parse-opts]]
   [bangfe.constants.memory-segments :as memory-segments :refer [offset-for]])
  (:gen-class))

(declare process process-lines process-line load-lines)

(def sf-lines (load-lines "/Users/nevadasmith/Documents/projects/nand2tetris/projects/08/FunctionCalls/SimpleFunction/SimpleFunction.vm"))

(defn load-dir
  [])

(defn load-lines
  [path]
  (let [lines (->> (f/load-lines  path)
                   s/strip-whitespace
                   (map #(clojure.string/split % #" ")))]
    (vec (for-indexed  [line idx lines]
                       (with-meta line {:line-number idx
                                        :uuid (nano-id)})))))

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

(defn pointer->var
  "Set the value of a custom var to the specified pointer value
   
   Example (pointer->var :local 'endFrame')"
  [mem-segment var-name]
  (let [pointer-address (memory-segments/offset-for mem-segment)]
    [(format "@%s" pointer-address)
     ["D=M"]
     [(format "@%s" var-name)]
     ["M=D"]]))

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

(defn d-to->pointer
  [seg-pointer]
  (let [pointer-address (offset-for seg-pointer)]
    [[(format "@%d" pointer-address)]
     ["M=D"]]))

(defn pop-to->dynamic
  "A generic pop function that looks up a virtual pointer and selects"
  [seg & [address]]
  (let [pointer-offset (offset-for seg)
        sub-address (bangfe.utils.number/sanatize-number address)
        needs-offset-adjustment? (and (not (nil? sub-address))
                                      (not (zero? sub-address)))]
    [[(format "@%d" pointer-offset)]
     ["D=M"]

     (if needs-offset-adjustment?
       [[(format "@%d" sub-address)]
        ["D=D+A"]]
       [])

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

(defn push-mem-segment-address-to-stack
  "Push the memory segment pointer address to the stack head
   Will increment the stack pointer"
  [segment]
  (let [pointer-address (memory-segments/offset-for segment)]
    [[(str "@" pointer-address)
      ["D=M"]
      (d-to->stack-head)
      (inc-stack-pointer)]]))

(defn save-caller-state
  "Push lcl, args, this, that, to stack head"
  []
  [(push-mem-segment-address-to-stack :local)
   (push-mem-segment-address-to-stack :argument)
   (push-mem-segment-address-to-stack :this)
   (push-mem-segment-address-to-stack :that)])

(defn- build-call-return-pairs
  "Creates call return pairs"
  [lines]
  (let [results (reduce
                 (fn
                   [acc cur]
                   (case (first cur)
                     "call" (update-in acc [:call-stack] conj cur)
                     "return" (let [stack (:call-stack acc)]
                                (if (empty? stack)
                                  (-> acc
                                      (update-in [:call-return-pair] conj {:call (peek (:call-stack acc))
                                                                           :return cur}))
                                  (-> acc
                                      (update-in [:call-return-pair] conj {:call (peek (:call-stack acc))
                                                                           :return cur})
                                      (assoc-in [:call-stack] (pop (:call-stack acc))))))
                     acc))

                 {:call-return-pair []
                  :call-stack []} lines)]

    (:call-return-pair results)))

(def fn-lines (load-lines "/Users/nevadasmith/Documents/projects/nand2tetris/projects/08/FunctionCalls/FibonacciElement"))
fn-lines
;; => [["function" "Main.fibonacci" "0"]
;;     ["push" "argument" "0"]
;;     ["push" "constant" "2"]
;;     ["lt"]
;;     ["if-goto" "IF_TRUE"]
;;     ["goto" "IF_FALSE"]
;;     ["label" "IF_TRUE"]
;;     ["push" "argument" "0"]
;;     ["return"] -                         return to Sys.init - call 1
;;     ["label" "IF_FALSE"]
;;     ["push" "argument" "0"]
;;     ["push" "constant" "2"]
;;     ["sub"]
;;     ["call" "Main.fibonacci" "1"]        Call 2
;;     ["push" "argument" "0"]
;;     ["push" "constant" "1"]
;;     ["sub"]
;;     ["call" "Main.fibonacci" "1"]         Call 3
;;     ["add"]
;;     ["return"]                            return to call 3
;;     ["function" "Sys.init" "0"]
;;     ["push" "constant" "4"]
;;     ["call" "Main.fibonacci" "1"]          Call 1
;;     ["label" "WHILE"]
;;     ["goto" "WHILE"]]



(build-call-return-pairs fn-lines)
;; => [{:call nil, :return ["return"]} {:call ["call" "Main.fibonacci" "1"], :return ["return"]}]

;; => [{:call ["call" "Main.fibonacci" "1"], :return ["return"]}]


(defn matching-call-or-return?
  [pair line]
  (let [{:keys [call return]} pair]
    (or (= (meta return) (meta line))
        (= (meta call) (meta line)))))

(defn symbols-for-call-return
  "Create symbols for a given fn call
   
   Example: (matching-call-or-return? lines line)

   Returns a map with keys :variable and :label

   {:variable '@Main.fibonacci-c.0', :label '(Main.fibonacci-c.0)'}

   "
  [lines line]
  (let [pairs (build-call-return-pairs lines)
        match (first (filter #(matching-call-or-return? % line) pairs))
        match-index (.indexOf pairs match)
        call-name (-> match :call (second))]
    {:variable [(format "@%s$ret.%d" call-name match-index)]
     :label [(format "(%s$ret.%d)" call-name match-index)]}))

(defn push-return-address
  "Push the return address to the stack head
   Will increment stack pointer"
  [return-variable]
  [return-variable
   ["D=A"]
   ["@SP"]
   ["A=M"]
   ["M=D"]
   (inc-stack-pointer)])

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

(defn set-memory-segment
  "Update the location of the memory segment pointer based on an offset from the stack head
   Example: (set-memory-segment :local 0) 

   This will set LCL to the current SP"
  [segment offset-from-sp]
  (let [address (memory-segments/offset-for segment)
        is-offset-neg? (neg-int? offset-from-sp)
        normalized-offset (if is-offset-neg?
                            (- offset-from-sp)
                            offset-from-sp)]
    [["@SP"]
     ["D=A"]
     [(str "@" normalized-offset)]
     (if is-offset-neg? ["D=D-A"] ["D=D+A"])
     [(str "@" address)]
     ["M=D"]]))

(defn sandbox-mem-segments-to-new-caller
  "Reset the memory segment pointer to the new caller stack"
  [arg-count]
  (let [sanatized-arg-count (bangfe.utils.number/sanatize-number arg-count)]
    [(set-memory-segment :local 0)
     (set-memory-segment :this 2)
     (set-memory-segment :that 3)

    ;;  ARG = (SP - 5 - arg count)
     (set-memory-segment :argument (- (+ 5 sanatized-arg-count)))]))

(defn write-zeros-to-mem-segment
  [segment offset total]
  (let [address (memory-segments/offset-for segment)
        offset (bangfe.utils.number/sanatize-number offset)
        total (bangfe.utils.number/sanatize-number total)]
    [[(str "@" address)]

     ["D=M"]
     [(str "@" offset)]
     ["D=D+A"]
     ["A=D"]
     (mapv (fn [_]
             [["M=0"]
              ["A=A+1"]])
           (range total))]))

(write-zeros-to-mem-segment :local 0 "2")

(defn func-symbols
  "Create a function label (Funcname.i) for func declaration"
  [[_ name]]
  {:label [(format "(%s)" name)]
   :variable [(format "@%s" name)]})

(defn call-cmd
  [lines line]
  (let [[_ _ arg-count] line
        uuid (nano-id)
        return-label (format "(returnAdd.%s)" uuid)
        return-variable (format "@returnAdd.%s" uuid)
        #_#_return-symbols (symbols-for-call-return lines line)]
    [(push-return-address return-variable)
     (save-caller-state)
     (sandbox-mem-segments-to-new-caller arg-count)

     ;;  Now go to fn
     ["//Go to func"]
     (:variable (func-symbols line))
     ["0;JMP"]
     [return-label]]))

(defn function-cmd
  [_ line]
  (let [[_ _ local-var-count] line]
    [[(:label (func-symbols line))]
     (write-zeros-to-mem-segment :local 0 local-var-count)]))

(defn from-var-pointer-to->mem-segment-pointer
  "Move the value of a var pointr with negative offset to a mem segment pointer
   
   This moves to the pointer value of var and offsets and then moves that value to the mem segment
   Used in endFrame rehydration.

   Example: (from-var-pointer-to->mem-segment-pointer 'endFrame' :this 1)"
  [from to neg-offset]
  [[(str "@" from)]
   ["D=M"]
   [(str "@" neg-offset)]
   ["A=D-A"]
   ["D=M"]

   (d-to->pointer to)])

(defn rehydrate-caller-state
  "Hydrate the previous caller's state, sp, args, lcl, this, that"
  []
  [;; Set SP to ARG + 1
   ["//Update to Args"]
   ["@ARG"]
   ["D=M"]
   ["@SP"]
   ["M=D+1"]

   ["//rehydrate that"]
   (from-var-pointer-to->mem-segment-pointer "endFrame" :that 1)

   ["//rehydrate this"]
   (from-var-pointer-to->mem-segment-pointer "endFrame" :this 2)

   ["//rehydrate argument"]
   (from-var-pointer-to->mem-segment-pointer "endFrame" :argument 3)

   ["//rehydrate local"]
   (from-var-pointer-to->mem-segment-pointer "endFrame" :local 4)])

(defn return-cmd
  [lines line]
  [;;  Create end frame and stash in variable. Equals LCL
   ["//Stash endframe"]
   ["@LCL"]
   ["D=M"]
   ["@endFrame"]
   ["M=D"]

  ;;  Load endframe with offset
   ["@endFrame"]
   ["D=M"]
   ["@5"]
   ["D=D-A"]
   ["A=D"]
   ["D=M"]

  ;;  Stash it in new var
   ["//Stash retAddr"]
   ["@retAddr"]
   ["M=D"]

  ;;  Pop the stack as the return value back to arguments which will be the new stack head - 1
   ["//Pop the stack to args which will be new stack head"]
   (dec-stack-pointer)
   ["@SP"]
   ["A=M"]
   ["D=M"]
   ["@ARG"]
   ["A=M"]
   ["M=D"]

  ;;  Load in old state
   (rehydrate-caller-state)

  ;;  Return!
   ["//Return"]
   ["@retAddr"]
   ["A=M"]
   ["0;JMP"]])

(defn match
  [lines line]
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
    "goto" (goto-cmd line)

    "call" (call-cmd lines line)

    "function" (function-cmd lines line)

    "return" (return-cmd lines line)))

(defn set-sp-pointer-address
  [address]
  [[(format "@%d" address)]
   ["D=A"]
   ["@SP"]
   ["M=D"]])

(defn bootstrap
  "Bootstrap code to set up"
  []
  [(set-sp-pointer-address 256)
   (call-cmd [] ["call" "Sys.init" "0"])])

(defn add-comment
  [line]
  (str "// " line))

(defn process-line
  "Process a single line of VM code"
  [lines line]
  [(add-comment line)
   (match lines line)])

(defn process-lines
  "Process all lines"
  [lines]
  (map #(process-line lines %) lines))

(defn process
  [path]
  (let [lines (load-lines  path)
        bootstrap-code (if (f/is-directory? path)
                         (bootstrap)
                         [])
        processed-lines (process-lines lines)
        end-loop-code (end-loop)]
    (flatten
     [bootstrap-code
      processed-lines
      end-loop-code])))

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

  (f/to-file
   (process "/Users/nevadasmith/Documents/projects/nand2tetris/projects/08/FunctionCalls/SimpleFunction/SimpleFunction.vm")
   "/Users/nevadasmith/Documents/projects/nand2tetris/projects/08/FunctionCalls/SimpleFunction/SimpleFunction.asm")

  (f/to-file
   (process "/Users/nevadasmith/Documents/projects/nand2tetris/projects/08/FunctionCalls/FibonacciElement")
   "/Users/nevadasmith/Documents/projects/nand2tetris/projects/08/FunctionCalls/FibonacciElement.asm")

  (f/to-file
   (process "/Users/nevadasmith/Documents/projects/nand2tetris/hack/resources/samples/vm/Main.vm")
   "/Users/nevadasmith/Documents/projects/nand2tetris/hack/resources/samples/vm/Main.asm")


  (f/load-lines  "resources/samples/vm/StackTest.vm")
  (process "resources/samples/vm/StackTest.vm"))
