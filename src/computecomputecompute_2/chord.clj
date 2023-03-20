;DEPRECATED

(ns computecomputecompute-2.chord
  (:require [clojure.math :as math]))

(defn- same-sign? [num1 num2]
  (if (< (* num1 num2) 0) true false))

(defn- update-border [border f x]; //TODO: better naming
  (if (same-sign? (f border) (f x)) border x))

(defn- x-i [f a b]
  (if (= a b)
    a
    (/ (- (* a (f b)) (* b (f a))) (- (f b) (f a)))))

(defn- calculation-finished? [f a b x prev-iteration-x eps]
  (or (< (abs (- x prev-iteration-x)) eps); use and here for perfect accuracy
      (< (abs (- a b)) eps)
      (< (abs (f x)) eps)))

(defn solve [f a0 b0 eps]
  (loop [iteration 0 prev-iteration-x ##Inf a a0 b b0]
    (let [x (x-i f a b)]
      (if (calculation-finished? f a b x prev-iteration-x eps)
        {:x x :iteration iteration}
        (recur (inc iteration) x (update-border a f x) (update-border b f x))))))



(defn func [x]
  (+ (- (math/pow x 3) x) 4))

(solve func -2 -1 1/100)