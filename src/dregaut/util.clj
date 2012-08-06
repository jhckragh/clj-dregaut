(ns dregaut.util
  "Various set-theoretic operations.")

;; Given two sets, $X$ and $Y$, this function outputs
;; $$
;; \\{ \\{x,y\\} \mid x \in X \wedge y \in Y \wedge x \neq y \\}.
;; $$
(defn uprod [xs ys]
  (set (for [x xs, y ys :when (not= x y)] #{x y})))

;; A special case of `uprod`, this function takes a set $X$ and
;; outputs
;; $$
;; [X]^2 = \\{ \\{x,x'\\} \mid x,x' \in X \wedge x \neq x' \\}.
;; $$
(defn upow2 [xs]
  (uprod xs xs))

;; This function takes an alphabet $\Sigma$ and returns lazy sequence
;; listing the elements of $\Sigma^*$ in canonical order. (See
;; [Martin] page 18.)
(defn kleene* [& xs]
  (if (empty? xs)
    (list "")
    (let [xs (apply sorted-set (map str xs))]
      (letfn [(step [queue]
                (lazy-seq
                 (let [hd (peek queue), tl (pop queue)]
                   (cons hd (step (into tl (for [x xs] (str hd x))))))))]
        (step (conj clojure.lang.PersistentQueue/EMPTY ""))))))
