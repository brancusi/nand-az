(ns bangfe.constants.memory-segments)

(def config
  ;; Static offsets
  {:pointer-addresses {:sp 0
                       :local 1
                       :argument 2
                       :pointer 3}

  ;;  Dynamic offsets based on the pointer offset
  ;;  i.e. this = RAM[3] - pointer + 0
   :segment-offsets {:this 0
                     :that 1
                     :temp 2
                     :static 3}})


(defn offset-for
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