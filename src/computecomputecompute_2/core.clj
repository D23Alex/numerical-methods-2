(ns computecomputecompute-2.core
  (:require [clojure.math :as math]) 
  (:require [computecomputecompute-2.equasion-solving :as eq])
  (:require [clojure.string :as string])
  (:require [clojure.repl :as rep])
  (:gen-class))
(use 'sicmutils.env)

(def system [(fn [x1 x2] (- (+ (* 0.1 (expt x1 2)) x1 (* 0.2 (expt x2 2))) 0.3))
              (fn [x1 x2] (- (+ (* 0.2 (expt x1 2)) x2 (* 0.1 x1 x2)) 0.7))])

{:f (fn [x] (-> x (expt 3) (- x) (+ 4)))
 :left -2
 :right -1
 :eps 1/100
 :iteration-cap 10
 :solving-method :newton}

(defn- add-user-input-data-to-equasions [equasions-from-file]
  (loop [equasions-by-user [] current-equasion 0]
    (if (= current-equasion (count equasions-from-file))
      equasions-by-user
      (let [inputs (string/split (read-line) #" ")
            left (read-string (get inputs 0))
            right (read-string (get inputs 1))
            eps (read-string (get inputs 2))
            iteration-cap (read-string (get inputs 3))]
        (recur (conj equasions-by-user (merge (get equasions-from-file current-equasion)
                                              {:left left :right right :eps eps :iteration-cap iteration-cap}))
               (inc current-equasion))))))


(defn- read-equasions [args]
  (let [equasions (read-string (slurp (first args)))]
    (if (some #(= "--manual-input" %) args)
      (add-user-input-data-to-equasions equasions)
      equasions)))

(defn- read-systems [args])

(defn- solve-all-equasions [])

(defn- solve-all-systems [])

(defn- report-solved-equasions [equasions])

(defn- report-solved-systems [systems])

(defn -main
  [& args] 
  (let [equasions (read-equasions args) systems-of-equasions (read-systems args)]
    (println equasions)
    ;(println (report-solved-equasions (solve-all-equasions)))
    ;(println (report-solved-systems (solve-all-systems)))
    )
  )


; solv file1 file2 --manual-in