(ns y2017.day18.solution-2
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [y2017.day18.domain :refer [->Value above-zero add
                                        modulo mul]]
            [y2017.day18.parser :refer [parse]])
  (:import [y2017.day18.domain
            Add
            Jgz
            Mod
            Mul
            Rcv
            Register
            Set
            Snd
            Value])
  (:import [clojure.lang PersistentQueue]))

(defrecord ProgramState [^int instruction-pointer, ^clojure.lang.IPersistentMap registers, ^clojure.lang.PersistentQueue outgoing-queue, ^clojure.lang.PersistentQueue incoming-queue, ^int messages-sent])

(defn print-program-state [^ProgramState state]
  (prn "IP: " (:instruction-pointer state))
  (pr "RG: ")
  (pprint/pprint (:registers state))
  (pr "OQ: ")
  (pprint/pprint (:outgoing-queue state))
  (pr "IQ: ")
  (pprint/pprint (:incoming-queue state))
  (prn "MS: " (:messages-sent state))
  (prn))

(defrecord State [^ProgramState state-0, ^ProgramState state-1])

(defn new-ip [^ProgramState state, diff]
  (+ (:instruction-pointer state) diff))

(defn next-command [^ProgramState state, commands]
  (when (<= 0 (:instruction-pointer state) (dec (count commands)))
    (nth commands (:instruction-pointer state))))

(defn update-register [^ProgramState state, ^Register register, ^Value value]
  (let [updated-registers (assoc (:registers state) (:id register) value)]
    (->ProgramState (new-ip state 1) updated-registers (:outgoing-queue state) (:incoming-queue state) (:messages-sent state))))

(defn get-register [^ProgramState state, ^Register register]
  (get (:registers state) (:id register) (->Value 0)))

(defprotocol Command
  (^ProgramState apply-command [^Command this, ^ProgramState state]))

(defprotocol Operand
  (^Value extract-value [^Operand this, ^ProgramState state]))

(extend-type Value
  Operand
  (extract-value [this _state]
    this))

(extend-type Register
  Operand
  (extract-value [this state]
    (get-register state this)))

(extend-type Snd
  Command
  (apply-command [this state]
    (let [value (extract-value (:x this) state)
          new-outgoing-queue (conj (:outgoing-queue state) value)
          new-messages-sent (+ (:messages-sent state) 1)]
      (->ProgramState (new-ip state 1) (:registers state) new-outgoing-queue (:incoming-queue state) new-messages-sent))))

(extend-type Set
  Command
  (apply-command [this state]
    (let [new-value (extract-value (:y this) state)]
      (update-register state (:x this) new-value))))

(extend-type Add
  Command
  (apply-command [this state]
    (let [x-value (get-register state (:x this))
          y-value (extract-value (:y this) state)
          new-value (add x-value y-value)]
      (update-register state (:x this) new-value))))

(extend-type Mul
  Command
  (apply-command [this state]
    (let [x-value (get-register state (:x this))
          y-value (extract-value (:y this) state)
          new-value (mul x-value y-value)]
      (update-register state (:x this) new-value))))

(extend-type Mod
  Command
  (apply-command [this state]
    (let [x-value (get-register state (:x this))
          y-value (extract-value (:y this) state)
          new-value (modulo x-value y-value)]
      (update-register state (:x this) new-value))))

(extend-type Rcv
  Command
  (apply-command [this state]
    (if (empty? (:incoming-queue state))
      nil
      (let [new-value (peek (:incoming-queue state))
            new-queue (pop (:incoming-queue state))]
        (-> state
            (assoc :incoming-queue new-queue)
            (update-register (:x this) new-value))))))

(extend-type Jgz
  Command
  (apply-command [this state]
    (let [jmp (if (above-zero (extract-value (:x this) state))
                (:value (extract-value (:y this) state))
                1)]
      (->ProgramState (new-ip state jmp) (:registers state) (:outgoing-queue state) (:incoming-queue state) (:messages-sent state)))))

(defn new-program-state [id]
  (let [registers (assoc {} \p (->Value id))]
    (->ProgramState 0 registers PersistentQueue/EMPTY PersistentQueue/EMPTY 0)))

(defn update-queues [^State state]
  (let [outgoing-queue-0 (:outgoing-queue (:state-0 state))
        outgoing-queue-1 (:outgoing-queue (:state-1 state))
        new-incoming-queue-0 (into (:incoming-queue (:state-0 state)) outgoing-queue-1)
        new-incoming-queue-1 (into (:incoming-queue (:state-1 state)) outgoing-queue-0)]
    (-> state
        (assoc-in [:state-0 :incoming-queue] new-incoming-queue-0)
        (assoc-in [:state-0 :outgoing-queue] clojure.lang.PersistentQueue/EMPTY)
        (assoc-in [:state-1 :incoming-queue] new-incoming-queue-1)
        (assoc-in [:state-1 :outgoing-queue] clojure.lang.PersistentQueue/EMPTY))))

(defn calculate-next-state [^State state, commands]
  (let [command-0 (next-command (:state-0 state) commands)
        command-1 (next-command (:state-1 state) commands)
        possibly-0 (if command-0 (apply-command command-0 (:state-0 state)) nil)
        possibly-1 (if command-1 (apply-command command-1 (:state-1 state)) nil)]

    (cond
      possibly-0 (assoc state :state-0 possibly-0)
      possibly-1 (assoc state :state-1 possibly-1)
      :else nil)))

(defn solve-2 [commands]
  (loop [state (->State (new-program-state 0) (new-program-state 1))]
    ;; (prn "Current state:\n")
    ;; (print-program-state (:state-0 state))
    ;; (print-program-state (:state-1 state))
    ;; (prn)
    (let [with-updated-queues (update-queues state)
          next-state (calculate-next-state with-updated-queues commands)]
      (if next-state
        (recur next-state)
        (:messages-sent (:state-1 with-updated-queues))))))

(defn part-2 [^String file-name]
  (solve-2 (parse file-name)))
