(ns y2017.day18.domain)

(defrecord Value [^Long value])

(defprotocol Numeric
  (add [this other])
  (mul [this other])
  (modulo [this other])
  (above-zero [this]))

(extend-type Value
  Numeric
  (add [this other]
    (->Value (+ (:value this) (:value other))))
  (mul [this other]
    (->Value (* (:value this) (:value other))))
  (modulo [this other]
    (->Value (mod (:value this) (:value other))))
  (above-zero [this]
    (> (:value this) 0)))

(defrecord Register [^char id])

(defrecord Snd [^Register x])
(defrecord Set [^Register x, y])
(defrecord Add [^Register x, y])
(defrecord Mul [^Register x, y])
(defrecord Mod [^Register x, y])
(defrecord Rcv [^Register x])
(defrecord Jgz [^Register x, y])
