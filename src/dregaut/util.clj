(ns dregaut.util)

(defn uprod [xs ys]
  (set (for [x xs, y ys :when (not= x y)] #{x y})))

(defn upow2 [xs]
  (uprod xs xs))

(defn kleene* [& xs]
  (if (empty? xs)
    (list "")
    (let [xs (apply sorted-set (map str xs))]
      (letfn [(step [queue]
                (lazy-seq
                 (let [hd (peek queue), tl (pop queue)]
                   (cons hd (step (apply conj tl (for [x xs] (str hd x))))))))]
        (step (conj clojure.lang.PersistentQueue/EMPTY ""))))))
