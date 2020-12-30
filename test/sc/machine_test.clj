(ns sc.machine-test
  (:require [clojure.test :refer :all]
            [sc.machine :as machine]
            [clojure.math.numeric-tower :as math]))

(deftest state-test
  (testing "empty text produces an empty machine state"
    (let [m (machine/state [])]
      (is (= (math/expt 2 15)
             (count (:mem m))))
      (is (= [0 0 0]
             (take 3 (:mem m))))))
  (testing "text produces initialized machine state"
    (let [m (machine/state [1 0 2 0 1 3])]
      (is (= (math/expt 2 15)
             (count (:mem m))))
      (is (= [1 2 769 0]  ; Note 3 << 8 = 768
             (take 4 (:mem m))))))
  (testing "'signed' bytes are converted properly"
    (let [m (machine/state [255 1 1 128])]
      (is (= 511 (get (:mem m) 0)))
      (is (= 32769 (get (:mem m) 1))))))

(deftest word-test
  (testing "valid word 0 <= 32767 + 8 register values 32768-32775"
    (is (not (machine/valid? -1)))
    (is (not (machine/valid? 32776)))
    (is (not (machine/valid? 65000)))
    (is (machine/valid? 0))
    (is (machine/valid? 32767))
    (is (machine/valid? 32768))
    (is (machine/valid? 32775)))
  (testing "register values 32768-32775"
    (is (not (machine/register? 0)))
    (is (not (machine/register? 32767)))
    (is (machine/register? 32768))
    (is (= 0 (machine/register 32768)))
    (is (= 7 (machine/register 32775)))))

(deftest ops-test
  (testing "halt sets pc to -1"
    (let [m (machine/state [0 0])
          n (machine/halt-op m)]
      (is (= (:pc n) -1))))

  (testing "set puts value in register"
    (let [m (machine/state [1 0 0 128 20 0]) ; 128 << 8 = 32768, reg 0
          n (machine/set-op m)]
      (is (= (:pc n) 3))
      (is (= 20 (get (:reg n) 0)))))

  (testing "push pushes a register value onto the stack"
    (let [m (machine/state [3 0 0 128]) ; 128 << 8 = 32768, reg 0
          m (assoc-in m [:reg 0] 20)
          n (machine/push-op m)]
      (is (= [20] (:stk n)))
      (is (= 2 (:pc n)))))
  (testing "push pushes a memory value onto the stack"
    (let [m (machine/state [3 0 20 0])
          n (machine/push-op m)]
      (is (= [20] (:stk n)))
      (is (= 2 (:pc n)))))

  (testing "pop a value off the stack into a register"
    (let [m (machine/state [4 0 0 128]) ; 128 << 8 = 32768, reg 0
          m (assoc m :stk '(20))
          n (machine/pop-op m)]
      (is (= [] (:stk n)))
      (is (= 20 (get-in n [:reg 0])))
      (is (= 2 (:pc n)))))
  (testing "pop a value off the stack into memory"
    (let [m (machine/state [4 0 8 0])
          m (assoc m :stk '(20))
          n (machine/pop-op m)]
      (is (= [] (:stk n)))
      (is (= 20 (get-in n [:mem 8])))
      (is (= 2 (:pc n)))))
  (testing "popping an empty stack is an exception"
    (let [m (machine/state [4 0 8 0])]
      (is (thrown? Exception (machine/pop-op m)))))

  (testing "eq puts value in register (+ equality)"
    (let [m (machine/state [4 0 0 128 5 0 5 0]) ; 128 << 8 = 32768, reg 0
          n (machine/eq-op m)]
      (is (= (:pc n) 4))
      (is (= 1 (get (:reg n) 0)))))
  (testing "eq puts value in memory (+ inequality)"
    (let [m (machine/state [4 0 10 0 5 0 4 0])
          n (machine/eq-op m)]
      (is (= (:pc n) 4))
      (is (= 0 (get (:mem n) 10)))))

  (testing "jmp sets pc to new address"
    (let [m (machine/state [6 0 20 0])
          n (machine/jmp-op m)]
      (is (= (:pc n) 20))))
  (testing "jmp sets pc to new address from register"
    (let [m (machine/state [6 0 0 128]) ; 128 << 8 = 32768, reg 0
          m (assoc-in m [:reg 0] 30)
          n (machine/jmp-op m)]
      (is (= (:pc n) 30))))

  (testing "jt sets pc to new address if addr is nonzero"
    (let [m (machine/state [7 0 1 0 20 0])
          n (machine/jt-op m)]
      (is (= (:pc n) 20))))
  (testing "jt just skips over if addr is zero"
    (let [m (machine/state [7 0 0 0 20 0])
          n (machine/jt-op m)]
      (is (= (:pc n) 3))))

  (testing "jf sets pc to new address if addr is zero"
    (let [m (machine/state [7 0 0 0 20 0])
          n (machine/jf-op m)]
      (is (= (:pc n) 20))))
  (testing "jf just skips over if addr is nonzero"
    (let [m (machine/state [7 0 1 0 20 0])
          n (machine/jf-op m)]
      (is (= (:pc n) 3))))

  (testing "add puts value in register"
    (let [m (machine/state [9 0 0 128 20 0 30 0]) ; 128 << 8 = 32768, reg 0
          n (machine/add-op m)]
      (is (= (:pc n) 4))
      (is (= 50 (get (:reg n) 0)))))
  (testing "add puts value in memory"
    (let [m (machine/state [9 0 10 0 20 0 30 0])
          n (machine/add-op m)]
      (is (= (:pc n) 4))
      (is (= 50 (get (:mem n) 10)))))
  (testing "add wraps properly"
    (let [m (machine/state [9 0 10 0 15 0 246 127]) ; (127 << 8) + 246 = 32758
          n (machine/add-op m)]
      (is (= (:pc n) 4))
      (is (= 5 (get (:mem n) 10)))))

  ; add tests the edges of binary-op, so remaining bin-ops are simpler
  (testing "mult wraps properly"
    (let [m (machine/state [10 0 10 0 205 12 10 0]) ; (12 << 8) + 205 = 32770
          n (machine/mult-op m)]
      (is (= (:pc n) 4))
      (is (= 2 (get (:mem n) 10)))))

  (testing "mod works"
    (let [m (machine/state [11 0 10 0 200 0 9 0])
          n (machine/mod-op m)]
      (is (= (:pc n) 4))
      (is (= 2 (get (:mem n) 10)))))

  (testing "and works"
    (let [m (machine/state [11 0 10 0 255 127 6 0])
          n (machine/and-op m)]
      (is (= (:pc n) 4))
      (is (= 6 (get (:mem n) 10)))))

  (testing "or works"
    (let [m (machine/state [11 0 10 0 0 127 255 0])
          n (machine/or-op m)]
      (is (= (:pc n) 4))
      (is (= 32767 (get (:mem n) 10)))))

  (testing "not flips all 0s"
    (let [m (machine/state [11 0 10 0 0 0])
          n (machine/not-op m)]
      (is (= (:pc n) 3))
      (is (= 32767 (get (:mem n) 10)))))

  (testing "not flips all 1s"
    (let [m (machine/state [11 0 10 0 255 127])
          n (machine/not-op m)]
      (is (= (:pc n) 3))
      (is (= 0 (get (:mem n) 10)))))

  (testing "rmem reads from *address* b and writes to a"
    (let [m (machine/state [15 0 0 128 4 0 0 0 20 0]) ; 128 << 8 = 32768, reg 0
          n (machine/rmem-op m)]
      (is (= (:pc n) 3))
      (is (= 20 (get (:reg n) 0)))))

  (testing "wmem reads b and writes to *address* a"
    (let [m (machine/state [16 0 0 128 20 0]) ; 128 << 8 = 32768, reg 0
          m (assoc-in m [:reg 0] 4)
          n (machine/wmem-op m)]
      (is (= (:pc n) 3))
      (is (= 20 (get (:mem n) 4)))))

  (testing "call pushes pc onto stack and jumps to register value"
    (let [m (machine/state [17 0 0 128]) ; 128 << 8 = 32768, reg 0
          m (assoc-in m [:reg 0] 20)
          n (machine/call-op m)]
      (is (= [2] (:stk n)))
      (is (= 20 (:pc n)))))
  (testing "call pushes pc onto stack and jumps to memory value"
    (let [m (machine/state [17 0 20 0])
          n (machine/call-op m)]
      (is (= [2] (:stk n)))
      (is (= 20 (:pc n)))))

  (testing "ret pops a value off the stack and jumps to it"
    (let [m (machine/state [18 0])
          m (assoc m :stk '(20))
          n (machine/ret-op m)]
      (is (= [] (:stk n)))
      (is (= 20 (:pc n)))))
  (testing "ret from an empty stack halts"
    (let [m (machine/state [18 0])
          n (machine/ret-op m)]
      (is (= -1 (:pc n)))))

  (testing "out-op prints char and advances pc by 2"
    (let [m (machine/state [19 0 116 0])] ; "t"
      (is (= "t" (with-out-str (machine/out-op m))))
      (is (= (:pc (machine/out-op m)) 2))))
  (testing "out-op prints register contents"
    (let [m (machine/state [19 0 0 128]) ; 128 << 8 = 32768, reg 0
          m (assoc-in m [:reg 0] 114)] ; "r"
      (is (= "r" (with-out-str (machine/out-op m))))))

  (testing "noop advances pc by 1"
    (let [m (machine/state [21 0])
          n (machine/no-op m)]
      (is (= (:pc n) 1)))))

(deftest execute-test
  (testing "executing a nop-sled results in halt"
    (let [m (machine/state [21 0 21 0 21 0 0 0])
          n (machine/execute m)]
      (is (= (:pc n) -1))))
  (testing "executing a print routine prints results and halts"
    (let [m (machine/state [19 0 116 0    ; "t"
                            19 0 101 0    ; "e"
                            19 0 115 0    ; "s"
                            19 0 116 0])] ; "t"
      (is (= "test" (with-out-str (machine/execute m))))
      (is (= (:pc (machine/execute m)) -1)))))
