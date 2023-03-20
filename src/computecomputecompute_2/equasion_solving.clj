(ns computecomputecompute-2.equasion-solving
  (:require [clojure.math :as math])
  (:require [computecomputecompute-2.helpers :as h])
  (:require [clojure.string :as string])
  (:gen-class))
(use 'sicmutils.env)

(defn newton [f left right eps iteration-cap]
  (let [f' (D f) f'' (D f') guess (if (> (* (f left) (f'' left)) 0) 
                                    left 
                                    right)
        
        precision-achived?
        (fn [{:keys [guess prev-guess iteration] :or {prev-guess 0 iteration 0}}] 
          (or (<= (abs (- guess prev-guess)) eps) 
              (<= (abs (/ (f guess) (f' guess))) eps) 
              (<= (abs (f guess)) eps) 
              (> iteration iteration-cap)))
        
        increase-precision
        (fn [{:keys [guess iteration] :or {iteration 0}}] 
          {:guess (- guess (/ (f guess) (f' guess))):prev-guess guess :iteration (inc iteration)})]
    
    (first
     (drop-while #(not (precision-achived? %))
       (iterate #(increase-precision %)
                {:guess guess :f f :eps eps})))))

(defn chord [f left right eps iteration-cap]
  (let 
   [update-boundary-by-guess
    (fn [boundary guess] (if (h/same-sign? (f boundary) (f guess)) boundary guess))
    
    precision-achived?
    (fn [{:keys [left right current-guess prev-guess iteration] :or {iteration 0 current-guess left prev-guess ##Inf}}]
      (or (<= (abs (- left right)) eps) 
          (<= (abs (- current-guess prev-guess)) eps) 
          (<= (abs (f current-guess)) eps) 
          (> iteration iteration-cap)))
    
    increase-precision
    (fn [{:keys [left right iteration current-guess] :or {iteration 0 current-guess ##Inf}}]
      (let [guess (if (= left right) 
                    left 
                    (/ (- (* left (f right)) (* right (f left)))
                       (- (f right) (f left))))] 
        {:left (update-boundary-by-guess left guess)
         :right (update-boundary-by-guess right guess)
         :current-guess guess 
         :prev-guess current-guess 
         :iteration (inc iteration)}))]
    
    (first
     (drop-while #(not (precision-achived? %))
                 (iterate #(increase-precision %)
                          {:left left :right right :f f :eps eps})))))

(defn iteration [f left right eps iteration-cap]
  (let [f' (D f) 
        x-of-x (+ (fn [x] x)
                  (* (* -1 (/ 1 (max (abs (f' left)) (abs (f' right)))))
                     f))
        x-of-x' (D x-of-x)
        compression-coef (max (x-of-x' left) (x-of-x' right))
        
        precision-achived?
        (fn [{:keys [guess prev-guess iteration] :or {guess ##Inf iteration 0 prev-guess ##Inf}}]
          (or (<= (abs (- guess prev-guess)) eps)
              (> iteration iteration-cap)))

        increase-precision
        (fn [{:keys [guess iteration] :or {iteration 0}}]
          {:guess (x-of-x guess) :prev-guess guess :iteration (inc iteration)})]
    (if (>= compression-coef 1)
      nil
      (first
       (drop-while #(not (precision-achived? %)) 
         (iterate #(increase-precision %) 
                  {:guess left :f f :eps eps})))))); TODO: начальное приближение

(defn- to-phi-of-x [f f-arg f-args]
  (* -1 (- f (eval (read-string (str "(" "fn [" (string/join " " f-args) "] " f-arg ")"))))))



(defn- to-system-phi [system-f f-args]
  (vec (map #(to-phi-of-x %1 %2 f-args) system-f f-args)))


(defn- solve-phi-of-x [system initial-approximation eps iteration-cap]
  (let [increase-precision (fn increase-precision [{:keys [system guesses iteration]}] 
                             {:guesses (vec (map #(apply % guesses) system))
                              :prev-guesses guesses 
                              :iteration (inc iteration)
                              :system system}) 
        precision-achived?
        (fn [{:keys [guesses prev-guesses iteration]}]
          (or (<= iteration-cap iteration)
              (not (some #(= % false) (vec (map #(< (abs (- %1 %2)) eps) guesses prev-guesses))))
              ))] 
    (first (drop-while #(not (precision-achived? %))
                       (iterate #(increase-precision %)
                                {:system system
                                 :guesses initial-approximation
                                 :prev-guesses (vec (repeat (count initial-approximation) ##Inf))
                                 :iteration 0})))))


;TODO rename this
(defn phi-of-x-valid [phi-of-x number-of-args intervlas]
  (apply + (map (fn [arg-number]
                  (abs ((partial arg-number) phi-of-x))) 
                (take number-of-args (range)) intervlas)))


(defn iteration-system [system f-args initial-approximation eps iteration-cap]
  (let [phi-of-x (to-system-phi system f-args)]
    (if true
      (solve-phi-of-x phi-of-x initial-approximation eps iteration-cap)
      nil)))


