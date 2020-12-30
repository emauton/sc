(ns sc.core
  (:require [sc.machine :as machine]
            [sc.util :as util])
  (:gen-class))

(defn -main
  "Instantiate our VM from the text in 'resources/filename'
   and exec or disassemble"
  [cmd filename]
  (let [input (util/slurp-resource filename)
        state (machine/state input)]
    (case cmd
      "exec" (machine/execute state)
      "dis"  (machine/disasm (machine/to-words input) 0)
      (println "Usage: lein run <exec|dis> <filename>"))))
