;; Deterministic finite automata and functions for operating on them.
;;
;; ### [Martin] Definition 2.11
;; Formally a deterministic finite automaton (DFA) is a 5-tuple
;; $(Q,\Sigma,q_0,A,\delta)$, where
;;
;;  * $Q$ is a finite set, the elements of which are called *states*;
;;  * $\Sigma$ is a finite input *alphabet*;
;;  * $q_0 \in Q$ is called the *initial* state;
;;  * $A \subseteq Q$ is a set of *accept* states;
;;  * $\delta \colon Q \times \Sigma \to Q$ is the transition function.
;;
(ns dregaut.dfa
  (:refer-clojure :exclude [complement])
  (:require [dregaut.util :as util]
            [clojure.set :as set]))

;; We represent a DFA as a record with five fields: one for each tuple
;; component.
(defrecord DFA [states alphabet initial accept delta])

;; Given a function $\delta$, a state set $Q$, and an alphabet
;; $\Sigma$ this function checks whether
;; $$
;; \forall q \in Q:
;;     \forall \sigma \in \Sigma:
;;         \delta(q, \sigma) \in Q.
;; $$
;; If that is the case, then we can use $\delta$ as a transition
;; function in a DFA with state set $Q$ and alphabet $\Sigma$.
(defn- well-defined-delta? [delta states alphabet]
  (every?
   (fn [q]
     (every?
      #(contains? states (delta q %))
      alphabet))
   states))

;; This is a factory function for creating DFAs; it ensures that its
;; arguments satisfy the constraints implicit in the above definition.
(defn create [states alphabet initial accept delta]
  {:pre [(set? states)
         (set? accept)
         (contains? states initial)
         (set/subset? accept states)
         (well-defined-delta? delta states alphabet)]}
  (DFA. states alphabet initial accept delta))

;; ### [Martin] Exercise 2.8 (b)

;; This function takes a transition function $\delta \colon Q \times
;; \Sigma \to Q$ and outputs an *extended transition function*
;; $\hat{\delta} \colon Q \times \Sigma^* \to Q$ defined by
;;
;;  1. $\forall q \in Q: \hat{\delta}(q, \Lambda) = q$.
;;  2. $\forall q \in Q, y \in \Sigma^*, \sigma \in \Sigma:
;;      \hat{\delta}(q, \sigma y) = \hat{\delta}(\delta(q,\sigma),y)$.
;;
;; (This defines the same function as [Martin] Definition 2.12.)
(defn extend-delta [delta]
  (fn [state s]
    (if (empty? s)
      state
      (recur (delta state (first s)) (rest s)))))

;; ### [Martin] Definition 2.14

;; This function takes a DFA $(Q,\Sigma,q_0,A,\delta)$ together with a
;; string $x$ and outputs whether $\hat{\delta}(q_0,x) \in A$, i.e.,
;; whether the DFA accepts $x$.
(defn accepts? [dfa x]
  (let [{:keys [initial accept delta]} dfa]
    (contains? accept ((extend-delta delta) initial x))))

;; ### [Martin] Theorem 2.15
;; If we have two DFAs with the same alphabet, we can construct a DFA
;; for the union of their languages. This is also possible for the
;; intersection of their languages and for the difference between
;; their languages.

;; Denote the two DFAs by $M\_1$ and $M\_2$, and write
;; $M_i=(Q\_i,\Sigma,q\_i,A\_i,\delta\_i)$ for $i=1,2$. Supplied with
;; a binary predicate $P$ this function constructs a DFA
;; $(Q,\Sigma,q_0,A,\delta)$, where
;;
;;  * $Q = Q\_1 \times Q\_2$
;;  * $q\_0 = (q\_1, q\_2)$
;;  * $A = \\{ (p,q) \in Q \mid P(p, q) \\}$
;;  * $\delta((p,q),\sigma) = (\delta\_1(p,\sigma),\delta\_2(q,\sigma))$
;;
;; This helper function is used by the following three functions:
(defn- prod [dfa1 dfa2 pred]
  {:pre [(= (.alphabet dfa1) (.alphabet dfa2))]}
  (let [states (set (for [p (.states dfa1), q (.states dfa2)] [p q]))
        alphabet (.alphabet dfa1)
        initial [(.initial dfa1) (.initial dfa2)]
        delta (fn [[p q] c] [((.delta dfa1) p c) ((.delta dfa2) q c)])
        accept (set/select (fn [[p q]] (pred p q)) states)]
    (create states alphabet initial accept delta)))

;; Supplying the predicate $P(p,q) \equiv (p \in A\_1) \vee (q \in
;; A\_2)$ gives us union,
(defn union [dfa1 dfa2]
  (let [pred #(or (contains? (.accept dfa1) %1) (contains? (.accept dfa2) %2))]
    (prod dfa1 dfa2 pred)))

;; while $P(p,q) \equiv (p \in A\_1) \wedge (q \in A\_2)$ gives us
;; intersection,
(defn intersection [dfa1 dfa2]
  (let [pred #(and (contains? (.accept dfa1) %1) (contains? (.accept dfa2) %2))]
    (prod dfa1 dfa2 pred)))

;; and $P(p,q) \equiv (p \in A\_1) \wedge (q \not\in A\_2)$ gives us
;; set-theoretic difference.
(defn difference [dfa1 dfa2]
  (let [pred #(and (contains? (.accept dfa1) %1)
                   (not (contains? (.accept dfa2) %2)))]
    (prod dfa1 dfa2 pred)))

;; For complement we don't need to use the above machinery: It's
;; sufficient to turn accept states into reject states and vice versa.
(defn complement [dfa]
  (let [{:keys [states alphabet initial accept delta]} dfa]
    (create states alphabet initial (set/difference states accept) delta)))

;; ### [Martin] Example 2.34
;; The following couple of functions implement decision algorithms for
;; various problems concerning DFAs.

;; We start out with an implementation of the "reachable cities"
;; algorithm from [Martin] Example 1.21; it will be used by some of
;; the following functions. The input is a DFA state and the output
;; is the set of states that can be reached from it. Of course, a
;; traditional breadth-first search solves this problem more
;; efficiently; see `shortest-example` for such an approach.
(defn- reachable-from [dfa q]
  {:pre [(contains? (.states dfa) q)]}
  (let [{:keys [alphabet delta]} dfa]
    (loop [r #{q}]
      (let [r' (set/union r (set (for [p r, c alphabet] (delta p c))))]
        (if (= r' r)
          r
          (recur r'))))))

;; The language of a DFA is empty iff none of its accept states
;; can be reached from its initial state.
(defn empty-language? [dfa]
  (let [{:keys [initial accept]} dfa]
    (empty? (set/intersection accept (reachable-from dfa initial)))))

;; If we're given two DFAs we can now easily determine whether the
;; language of one of them forms a subset of that of the other.
(defn subset-language? [dfa1 dfa2]
  (empty-language? (difference dfa1 dfa2)))

;; As explained in [Martin], the pumping lemma for regular languages
;; can be used to prove that the language of a DFA is infinite iff it
;; contains a string $x$ with $n \le |x| < 2n$, where $n$ is the
;; number of states belonging to the DFA. So checking all strings of
;; length between $n$ and $2n-1$ is a correct (though highly
;; inefficient) algorithm for determining whether the language of a
;; given DFA is finite.
(defn finite-language? [dfa]
  (let [n (count (.states dfa))
        alphabet (.alphabet dfa)]
    (loop [xs (drop-while #(< (count %) n) (apply util/kleene* alphabet))]
      (let [x (first xs)]
        (cond
         (>= (count x) (* 2 n)) true
         (accepts? dfa x) false
         :else (recur (rest xs)))))))

;; Unlike the three previous functions this one doesn't implement a
;; decision algorithm. It takes a DFA as input and outputs a shortest
;; string accepted by that DFA (or `nil` if the language is empty).
;;
;; The current implementation performs a breadth-first search of the
;; DFA's transition diagram, using the initial state as the source of
;; the search.
;;
;; Upon examining a state $p$ the search looks at each of the states
;; adjacent to it in the transition diagram. If $p$ is found to be
;; connected to a state $q$ via an alphabet symbol `c`, then the
;; mapping `q [c p]` is added to the `path-to` map (provided $q$
;; hasn't already been encountered).
;;
;; As soon as the search examines an accept state it terminates and
;; uses the `path-to` map to reconstruct a shortest path to it. If no
;; accept state is reachable from the initial state, then the search
;; terminates when all reachable states have been examined.
(defn shortest-example [dfa]
  (let [{:keys [alphabet initial accept delta]} dfa
        unravel (fn [path-to q acc]
                     (if-let [[c p] (path-to q)]
                       (recur path-to p (conj acc c))
                       (apply str acc)))]
    (loop [marked #{initial}
           path-to {initial nil}
           queue (conj clojure.lang.PersistentQueue/EMPTY initial)]
      (if (empty? queue)
        nil
        (let [p (peek queue)]
          (if (accept p)
            (unravel path-to p ())
            (let [unmarked (remove marked (for [c alphabet] (delta p c)))
                  marked (into marked unmarked)
                  path-to (merge
                           (into {} (for [c alphabet] {(delta p c) [c p]}))
                           path-to)
                  queue (into (pop queue) unmarked)]
              (recur marked path-to queue))))))))

;; ### [Martin] Section 2.6
;; We now turn to the problem of minimizing DFAs.

;; The most obvious way of reducing the number of states of a DFA is
;; to remove the states that can't be reached from the initial state;
;; this is what this function does.
(defn prune [dfa]
  (let [{:keys [alphabet initial delta]} dfa
        reachable (reachable-from dfa initial)
        accept (set/intersection reachable (.accept dfa))]
    (create reachable alphabet initial accept delta)))

;; The next step is to identify the states that must not be combined
;; when minimizing the DFA. Algorithm 2.40 from [Martin], which this
;; function implements, details how to do this. Formally, the algorithm
;; takes a DFA $(Q,\Sigma,q_0,A,\delta)$ as input, and it outputs
;; $$
;; \\{ \\{p,q\\} \mid p,q \in Q \wedge p \not\equiv q \\},
;; $$
;; using the notation from [Martin] page 74.
(defn- distinguishable-pairs [dfa]
  (let [{:keys [states alphabet initial accept delta]} dfa
        reject (set/difference states accept)]
    (loop [marked (util/uprod accept reject)
           unmarked (set/union (util/upow2 accept) (util/upow2 reject))]
      (let [extra (set/select
                   (fn [pair]
                     (let [r (first pair), s (second pair)]
                       (some #(marked (set [(delta r %) (delta s %)])) alphabet)))
                     unmarked)]
        (if (empty? extra)
          marked
          (recur (set/union marked extra) (set/difference unmarked extra)))))))

;; Finally we come to the actual minimization algorithm, which is
;; described on [Martin] page 75.

;; The algorithm starts by removing unreachable states followed by
;; identifying the states that mustn't be combined during
;; minimization.
;;
;; After that it examines each reachable state, starting with the
;; initial state, to determine which equivalence class it belongs to.
;;
;;  * `prev` holds the states that have been examined so far.
;;  * `equiv` holds the equivalence classes that have been identified so far.
;;  * `todo` holds the states that haven't been examined yet.
;;
;; When examining a state $p$ one of two things can happen:
;;
;;  * If $p \equiv q$ for some $q$ in `prev`, then $p$ belongs to the
;;    same equivalence class as $q$, which means that it can be
;;    combined with $q$.
;;  * Otherwise, it doesn't belong to any of the equivalence classes
;;    that have been identified so far, so it can't be combined
;;    with any of the states in `prev`.
;;
;; Having exhausted `todo` the function constructs a minimal DFA with
;; the same language as the input DFA: Formally, if the input DFA is
;; $(Q,\Sigma,q_0,A,\delta)$, the constructed DFA is
;; $(Q',\Sigma,[q_0],A',\delta')$, where
;;
;;  * $Q' = \\{ [q] \mid q \text{ is reachable from } q_0 \\}$,
;;  * $A' = \\{ [q] \mid q \in A \\}$,
;;  * $\delta'([q],\sigma) = [\delta(q,\sigma)]$.
(defn minimize [dfa]
  (let [{:keys [states alphabet initial accept delta] :as dfa} (prune dfa)
        dis (distinguishable-pairs dfa)]
    (letfn [(find-equivalent-state [q prev]
              (some #(when (not (dis #{% q})) %) prev))
            (find-equivalence-class [q equiv]
              (some #(when (% q) %) equiv))]
      (loop [prev #{initial}
             equiv #{#{initial}}
             todo (disj states initial)]
        (if-let [todo (seq todo)]
          (let [q (first todo)]
            (if-let [p (find-equivalent-state q prev)]
              (let [equiv (reduce
                           (fn [acc s]
                             (if (s p)
                               (conj acc (conj s q))
                               (conj acc s)))
                           #{}
                           equiv)]
                (recur (conj prev q) equiv (rest todo)))
              (recur (conj prev q) (conj equiv #{q}) (rest todo))))
          (create equiv
                  alphabet
                  (find-equivalence-class initial equiv)
                  (set/select #(not (empty? (set/intersection accept %))) equiv)
                  (fn [s c]
                    (let [q (first s)]
                      (find-equivalence-class (delta q c) equiv)))))))))
