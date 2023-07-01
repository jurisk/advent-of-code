(ns y2017.day18.parser
  (:require [clojure.string :as str]
            [y2017.day18.domain :refer :all]))

(defn parse-value [^String arg]
  (->Value (Long/parseLong arg)))

(defn parse-register [^String arg]
  (->Register (first arg)))

(defn parse-arg
  [^String arg]
  (if (Character/isLetter (.charAt arg 0))
    (parse-register arg)
    (parse-value arg)))

(defmulti parse-command (fn [command-string] (first (str/split command-string #" "))))

(defmethod parse-command "snd" [command-string]
  (let [[_ x] (str/split command-string #" ")]
    (->Snd (parse-register x))))

(defmethod parse-command "set" [command-string]
  (let [[_ x y] (str/split command-string #" ")]
    (->Set (parse-register x) (parse-arg y))))

(defmethod parse-command "add" [command-string]
  (let [[_ x y] (str/split command-string #" ")]
    (->Add (parse-register x) (parse-arg y))))

(defmethod parse-command "mul" [command-string]
  (let [[_ x y] (str/split command-string #" ")]
    (->Mul (parse-register x) (parse-arg y))))

(defmethod parse-command "mod" [command-string]
  (let [[_ x y] (str/split command-string #" ")]
    (->Mod (parse-register x) (parse-arg y))))

(defmethod parse-command "rcv" [command-string]
  (let [[_ x] (str/split command-string #" ")]
    (->Rcv (parse-register x))))

(defmethod parse-command "jgz" [command-string]
  (let [[_ x y] (str/split command-string #" ")]
    (->Jgz (parse-register x) (parse-arg y))))

(defmethod parse-command :default [_]
  (throw (Exception. "Unknown command")))

(defn parse [^String file-name]
  (let [contents (slurp file-name)
        lines (str/split-lines contents)
        commands (map parse-command lines)]
    (doseq [cmd commands] (println cmd))
    (doall commands)))