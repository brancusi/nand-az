(ns bangfe.constants.memory-segments)

(def config
  {:pointer-addresses {"sp" 0
                       "local" 1
                       "argument" 2
                       "this" 3
                       "that" 4
                       "static" 5
                       "pointer" 6
                       "temp" 7}
   :segment-offsets {:sp 256
                     :lcl 300
                     :arg 400
                     :this 3000
                     :that 3010}})

;; set RAM[0] 256,   // stack pointer
;; set RAM[1] 300,   // base address of the local segment
;; set RAM[2] 400,   // base address of the argument segment
;; set RAM[3] 3000,  // base address of the this segment
;; set RAM[4] 3010,  // base address of the that segment