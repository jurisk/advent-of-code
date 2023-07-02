(ns y2017.day18.solution-1
  (:gen-class)
  (:require [y2017.day18.domain :refer [->Value add mul modulo above-zero]]
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
            Value]))

(defrecord State [^int instruction-pointer, ^clojure.lang.IPersistentMap registers, ^Value last-note, ^Value result])

(defn new-ip [^State state, diff]
  (+ (:instruction-pointer state) diff))

(defn next-command [^State state, commands]
  (if (:result state)
    nil
    (nth commands (:instruction-pointer state))))

(defn update-register [^State state, ^Register register, ^Value value]
  (let [updated-registers (assoc (:registers state) (:id register) value)]
    (->State (new-ip state 1) updated-registers (:last-note state) (:result state))))

(defn get-register [^State state, ^Register register]
  (get (:registers state) (:id register) (->Value 0)))

(defprotocol Command
  (^State apply-command [^Command this, ^State state]))

(defprotocol Operand
  (^Value extract-value [^Operand this, ^State state]))

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
    (let [new-note (extract-value (:x this) state)]
      (->State (new-ip state 1) (:registers state) new-note (:result state)))))

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
    (if
     (above-zero (get-register state (:x this)))
      (->State (new-ip state 1) (:registers state) (:last-note state) (:last-note state))
      (->State (new-ip state 1) (:registers state) (:last-note state) (:result state)))))

(extend-type Jgz
  Command
  (apply-command [this state]
    (let [jmp (if (above-zero (get-register state (:x this)))
                (:value (extract-value (:y this) state))
                1)]
      (->State (new-ip state jmp) (:registers state) (:last-note state) (:result state)))))

(defn solve-1 [commands]
  (loop [state (->State 0 {} nil nil)]
    (println "Current state: " state)
    (let [command (next-command state commands)]
      (println "Chosen command: " command)
      (println)
      (if command
        (recur (apply-command command state))
        (:value (:result state))))))

(defn part-1 [^String file-name]
  (solve-1 (parse file-name)))