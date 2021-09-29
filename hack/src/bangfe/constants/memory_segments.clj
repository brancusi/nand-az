(ns bangfe.constants.memory-segments)

(def config
  ;; Static offsets
  {:pointer-addresses {:sp 0
                       :local 1
                       :argument 2
                       :pointer 3
                       :this 3
                       :that 4
                       :temp 5}
   :segments {:general-purpose 13
              :static 16
              :stack 256}})


(defn offset-for
  "Offset for the pointer. Returns an int"
  [key]
  (let [key (if (keyword? key)
              key
              (keyword key))
        pa (-> config
               :pointer-addresses
               key)]
    (if pa
      pa
      (+ (offset-for :pointer) (-> config
                                   :segment-offsets
                                   key)))))


;; set RAM[0] 256,   // stack pointer
;; set RAM[1] 300,   // base address of the local segment
;; set RAM[2] 400,   // base address of the argument segment
;; set RAM[3] 3000,  // base address of the this segment
;; set RAM[4] 3010,  // base address of the that segment