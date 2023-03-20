(ns computecomputecompute-2.helpers)

(defn same-sign? [num1 num2]
  (if (< (* num1 num2) 0) true false))