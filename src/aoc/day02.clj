(ns aoc.day02
  (:require [clojure.string :as str]))

(defn- apply-add [ops pc]
  (let [op1 (ops (inc pc))
        op2 (ops (+ 2 pc))
        dst (ops (+ 3 pc))
        sum (+ (ops op1) (ops op2))]
    (assoc ops dst sum)))

(defn- apply-multiply [ops pc]
  (let [op1     (ops (inc pc))
        op2     (ops (+ 2 pc))
        dst     (ops (+ 3 pc))
        product (* (ops op1) (ops op2))]
    (assoc ops dst product)))

(defn- proc-ops [noun verb opcodes]
  (let [opcodes (assoc opcodes 1 noun 2 verb)]
    (loop [opcodes opcodes, pc 0]
      (cond
        (= (opcodes pc) 99) (opcodes 0) 
        (= (opcodes pc) 1)  (recur (apply-add opcodes pc) (+ pc 4))
        (= (opcodes pc) 2)  (recur (apply-multiply opcodes pc) (+ pc 4))
        :else               nil))))

(defn p1 [file]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (vec)
       (proc-ops 12 2)))

(defn- find-noun-verb [looking-for opcodes]
  (loop [noun 0, verb 0]
    (let [result (proc-ops noun verb opcodes)]
      (cond
        (= result looking-for) (+ (* 100 noun) verb)
        (< verb 99)            (recur noun (inc verb))
        (< noun 99)            (recur (inc noun) 0)
        :else                  "No solution found."))))

(defn p2 [file val]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (vec)
       (find-noun-verb val)))
