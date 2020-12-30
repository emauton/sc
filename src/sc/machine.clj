(ns sc.machine
  (:require [sc.util :as util]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as s]))

(defn register
  [word]
  (when (>= 32775 word 32768) (rem word 32768)))

(defn register?
  [word]
  (util/not-nil? (register word)))

(defn valid?
  [word]
  (<= 0 word 32775))

(defn value
  "Return the value at a particular address, dereferencing register values"
  [{:keys [reg mem]} addr]
  (let [v (get mem addr)]
    (if (register? v)
      (get reg (register v))
      v)))

(def op-data {0  ["halt" 0]
              1  ["set" 2]
              2  ["push" 1]
              3  ["pop" 1]
              4  ["eq" 3]
              5  ["gt" 3]
              6  ["jmp" 1]
              7  ["jt" 2]
              8  ["jf" 2]
              9  ["add" 3]
              10 ["mult" 3]
              11 ["mod" 3]
              12 ["and" 3]
              13 ["or" 3]
              14 ["not" 2]
              15 ["rmem" 2]
              16 ["wmem" 2]
              17 ["call" 1]
              18 ["ret" 0]
              19 ["out" 1]
              20 ["in" 1]
              21 ["nop" 0]})

(defn- str-arg
  [arg]
  (if (register? arg)
    (str "reg[" (register arg) "]")
    (str arg)))

(defn- out-arg
  [arg]
  (if (register? arg)
    (str "[" (register arg) "]")
    (if (= \newline (char arg))
      "\\n"
      (char arg))))

(defn disasm
  [words pc]
  (if (>= pc (count words))
    nil
    (let [op (get words pc)
          [op-name args] (get op-data op)]
      (print pc)
      (cond
        (nil? op-name) (do (println ":" op "unrecognized")
                           (recur words (inc pc)))
        (= op-name "out") (do (println ": out" (out-arg (get words (inc pc))))
                              (recur words (+ 2 pc)))
        (= args 0) (do (println ":" op-name)
                       (recur words (inc pc)))
        :else (do (let [next-pc (+ args (inc pc))]
                    (println ":" op-name
                             (s/join " " (map str-arg (subvec words (inc pc) next-pc))))
                  (recur words next-pc)))))))

(defn halt-op
  [state]
  (assoc state :pc -1))

(defn set-op
  [{:keys [pc reg mem] :as state}]
  (let [v (get mem (inc pc))
        a (register v)
        b (value state (+ pc 2))]
    (merge state {:pc (+ pc 3)
                  :reg (assoc reg a b)})))

(defn push-op
  [{:keys [pc stk] :as state}]
  (let [a (value state (inc pc))]
    (merge state {:pc (+ pc 2)
                  :stk (conj stk a)})))

(defn pop-op
  [{:keys [pc mem reg stk] :as state}]
  (let [a (get mem (inc pc))
        v (first stk)]
    (cond
      (nil? v) (throw (Exception. (str "stack underflow at " pc)))
      (register? a) (merge state {:pc (+ pc 2)
                                  :reg (assoc reg (register a) v)
                                  :stk (rest stk)})
      :else (merge state {:pc (+ pc 2)
                          :mem (assoc mem a v)
                          :stk (rest stk)}))))

(defn comparison-op
  "Comparison ops - eq, gt."
  [op {:keys [pc reg mem] :as state}]
  (let [a (get mem (inc pc))
        b (value state (+ pc 2))
        c (value state (+ pc 3))
        result (if (op b c) 1 0)]
    (if (register? a)
      (merge state {:pc (+ pc 4)
                    :reg (assoc reg (register a) result)})
      (merge state {:pc (+ pc 4)
                    :mem (assoc mem a result)}))))

(def eq-op (partial comparison-op =))
(def gt-op (partial comparison-op >))

(defn jmp-op
  [{:keys [pc] :as state}]
  (let [a (value state (inc pc))]
    (assoc state :pc a)))

(defn jt-op
  [{:keys [pc] :as state}]
  (let [a (value state (inc pc))
        b (value state (+ pc 2))]
    (if (not= 0 a)
      (assoc state :pc b)
      (assoc state :pc (+ pc 3)))))

(defn jf-op
  [{:keys [pc] :as state}]
  (let [a (value state (inc pc))
        b (value state (+ pc 2))]
    (if (= 0 a)
      (assoc state :pc b)
      (assoc state :pc (+ pc 3)))))

(defn binary-op
  "Handle all the binary ops - add, mult, mod, and, or.
   Apply modulus to the result has no effect on mod, and, or."
  [op {:keys [pc reg mem] :as state}]
  (let [a (get mem (inc pc))
        b (value state (+ pc 2))
        c (value state (+ pc 3))
        result (mod (op b c) 32768)]
    (if (register? a)
      (merge state {:pc (+ pc 4)
                    :reg (assoc reg (register a) result)})
      (merge state {:pc (+ pc 4)
                    :mem (assoc mem a result)}))))

(def add-op (partial binary-op +))
(def mult-op (partial binary-op *))
(def mod-op (partial binary-op rem))
(def and-op (partial binary-op bit-and))
(def or-op (partial binary-op bit-or))

(defn not-op
  [{:keys [pc reg mem] :as state}]
  (let [a (get mem (inc pc))
        b (value state (+ pc 2))
        result (bit-and (bit-not b) 32767)]  ; 15-bit 1s mask
    (if (register? a)
      (merge state {:pc (+ pc 3)
                    :reg (assoc reg (register a) result)})
      (merge state {:pc (+ pc 3)
                    :mem (assoc mem a result)}))))

(defn rmem-op
  [{:keys [pc reg mem] :as state}]
  (let [a (get mem (inc pc))
        b (value state (+ pc 2))
        b-val (get mem b)]
    (if (register? a)
      (merge state {:pc (+ pc 3)
                    :reg (assoc reg (register a) b-val)})
      (merge state {:pc (+ pc 3)
                    :mem (assoc mem a b-val)}))))

(defn wmem-op
  [{:keys [pc reg mem] :as state}]
  (let [a (value state (inc pc))
        b (value state (+ pc 2))]
    (if (register? a)
      (merge state {:pc (+ pc 3)
                    :reg (assoc reg (register a) b)})
      (merge state {:pc (+ pc 3)
                    :mem (assoc mem a b)}))))

(defn call-op
  [{:keys [pc stk] :as state}]
  (let [a (value state (inc pc))]
    (merge state {:pc a
                  :stk (conj stk (+ pc 2))})))

(defn ret-op
  [{:keys [pc mem reg stk] :as state}]
  (let [v (first stk)]
    (if (nil? v)
      (merge state {:pc -1 :stk (rest stk)})
      (merge state {:pc  v :stk (rest stk)}))))

(defn out-op
  [{:keys [pc reg mem] :as state}]
  (let [c (value state (inc pc))]
    (print (char c))
    (assoc state :pc (+ 2 pc))))

(defn no-op
  [{:keys [pc] :as state}]
  (assoc state :pc (inc pc)))

(def opcodes {0  halt-op
              1  set-op
              2  push-op
              3  pop-op
              4  eq-op
              5  gt-op
              6  jmp-op
              7  jt-op
              8  jf-op
              9  add-op
              10 mult-op
              11 mod-op
              12 and-op
              13 or-op
              14 not-op
              15 rmem-op
              16 wmem-op
              17 call-op
              18 ret-op
              19 out-op
              21 no-op})

(defn step
  [{:keys [pc mem] :as state}]
  (let [o (nth mem pc)
        f (get opcodes o)]
    (if (nil? f)
      (throw (Exception. (str "opcode not implemented: " o " at " pc)))
      (f state))))

(defn execute
  [state]
  (let [next-state (step state)]
    (cond (neg? (:pc next-state)) next-state
          :else (recur next-state))))

(defn to-word
  "Convert two-byte seq into a 16b word"
  [[low high]]
  (let [low (bit-and low 0xff)    ; throw away Java signed byte per
        high (bit-and high 0xff)] ; https://stackoverflow.com/a/5144107
    (+ (int low) (bit-shift-left (int high) 8))))

(defn to-words
  "Convert byte seq into 16b word vector"
  [text]
  (mapv to-word (partition 2 text)))

(defn state
  "Return a new machine state with memory initialized to text"
  [text]
  (let [words (to-words text)
        mem (concat words
                    (repeat (- (math/expt 2 15) (count words))
                            0))]
    {:pc 0
     :reg (vec (repeat 8 0))
     :mem (vec mem)
     :stk '()}))
