(ns dregaut.dfa-test
  (:use clojure.test)
  (:require [dregaut.dfa :as dfa]))

(deftest bare-minimum-test
  (let [m (dfa/create #{'q0 'q1 'q2}
                      #{\a \b}
                      'q0
                      #{'q2}
                      #(get-in
                        {'q0 {\a 'q1, \b 'q0}
                         'q1 {\a 'q2, \b 'q0}
                         'q2 {\a 'q2, \b 'q0}}
                        [%1 %2]))]
    (is (dfa/accepts? m "aa"))
    (is (dfa/accepts? m "baa"))
    (is (dfa/accepts? m "aabaa"))
    (is (not (dfa/accepts? m "")))
    (is (not (dfa/accepts? m "a")))
    (is (not (dfa/accepts? m "b")))
    (is (not (dfa/accepts? m "ab")))
    (is (not (dfa/accepts? m "aab")))
    (is (not (dfa/accepts? m "aaba")))))

(deftest complement-test
  (let [m (dfa/complement (dfa/create #{'q0 'q1 'q2}
                                      #{\a \b}
                                      'q0
                                      #{'q2}
                                      #(get-in
                                        {'q0 {\a 'q1, \b 'q0}
                                         'q1 {\a 'q2, \b 'q0}
                                         'q2 {\a 'q2, \b 'q0}}
                                        [%1 %2])))]
    (is (not (dfa/accepts? m "aa")))
    (is (not (dfa/accepts? m "baa")))
    (is (not (dfa/accepts? m "aabaa")))
    (is (dfa/accepts? m ""))
    (is (dfa/accepts? m "a"))
    (is (dfa/accepts? m "b"))
    (is (dfa/accepts? m "ab"))
    (is (dfa/accepts? m "aab"))
    (is (dfa/accepts? m "aaba"))))

(deftest union-test
  (let [m1 (dfa/create #{:A :B :C}
                       #{\a \b}
                       :A
                       #{:A :B}
                       #(get-in
                         {:A {\a :B, \b :A}
                          :B {\a :C, \b :A}
                          :C {\a :C, \b :C}}
                         [%1 %2]))
        m2 (dfa/create #{:P :Q :R}
                       #{\a \b}
                       :P
                       #{:R}
                       #(get-in
                         {:P {\a :Q, \b :P}
                          :Q {\a :Q, \b :R}
                          :R {\a :Q, \b :P}}
                         [%1 %2]))
        m (dfa/union m1 m2)]
    (is (= (:states m) #{[:A :P] [:A :Q] [:A :R]
                         [:B :P] [:B :Q] [:B :R]
                         [:C :P] [:C :Q] [:C :R]}))
    (is (= (:alphabet m) #{\a \b}))
    (is (= (:initial m) [:A :P]))
    (is (= (:accept m) #{[:A :P] [:A :Q] [:A :R]
                         [:B :P] [:B :Q] [:B :R]
                         [:C :R]}))
    (let [delta (:delta m)]
      (is (= (delta [:A :P] \a) [:B :Q]))
      (is (= (delta [:A :P] \b) [:A :P]))
      (is (= (delta [:A :Q] \a) [:B :Q]))
      (is (= (delta [:A :Q] \b) [:A :R]))
      (is (= (delta [:A :R] \a) [:B :Q]))
      (is (= (delta [:A :R] \b) [:A :P]))
      
      (is (= (delta [:B :P] \a) [:C :Q]))
      (is (= (delta [:B :P] \b) [:A :P]))
      (is (= (delta [:B :Q] \a) [:C :Q]))
      (is (= (delta [:B :Q] \b) [:A :R]))
      (is (= (delta [:B :R] \a) [:C :Q]))
      (is (= (delta [:B :R] \b) [:A :P]))

      (is (= (delta [:C :P] \a) [:C :Q]))
      (is (= (delta [:C :P] \b) [:C :P]))
      (is (= (delta [:C :Q] \a) [:C :Q]))
      (is (= (delta [:C :Q] \b) [:C :R]))
      (is (= (delta [:C :R] \a) [:C :Q]))
      (is (= (delta [:C :R] \b) [:C :P])))))

(deftest empty-language-test
  (let [m (dfa/create #{1} #{\a} 1 #{} (fn [q c] 1))]
    (is (dfa/empty-language? m)))

  (let [m (dfa/create #{1 2 3 4 5 6 7}
                      #{\a \b}
                      1
                      #{2 4 6}
                      #(get-in
                        {1 {\a 1, \b 3}
                         2 {\a 6, \b 3}
                         3 {\a 5, \b 7}
                         4 {\a 6, \b 1}
                         5 {\a 1, \b 7}
                         6 {\a 2, \b 7}
                         7 {\a 5, \b 3}}
                        [%1 %2]))]
    (is (dfa/empty-language? m)))

  (let [m (dfa/create #{1} #{\a \b} 1 #{1} (fn [q c] 1))]
    (is (not (dfa/empty-language? m))))

  (let [m (dfa/create #{1 2 3}
                      #{\a \b}
                      1
                      #{3}
                      #(get-in
                        {1 {\a 2, \b 1}
                         2 {\a 2, \b 3}
                         3 {\a 3, \b 3}}
                        [%1 %2]))]
    (is (not (dfa/empty-language? m)))))

(deftest subset-test
  (let [m1 (dfa/create #{1} #{\a} 1 #{} (fn [q c] 1))
        m2 (dfa/create #{1} #{\a} 1 #{1} (fn [q c] 1))]
    (is (dfa/subset-language? m1 m2))
    (is (not (dfa/subset-language? m2 m1))))

  (let [m1 (dfa/create #{1 2 3}
                      #{\a \b}
                      1
                      #{3}
                      #(get-in
                        {1 {\a 2, \b 1}
                         2 {\a 2, \b 3}
                         3 {\a 3, \b 3}}
                        [%1 %2]))
        m2 (dfa/create #{1} #{\a \b} 1 #{1} (fn [q c] 1))]
    (is (dfa/subset-language? m1 m2))
    (is (not (dfa/subset-language? m2 m1)))))

(deftest prune-test
  (let [m (dfa/prune (dfa/create #{1 2 3 4 5 6 7}
                                 #{\a \b}
                                 1
                                 #{2 4 6}
                                 #(get-in
                                   {1 {\a 1, \b 3}
                                    2 {\a 6, \b 3}
                                    3 {\a 5, \b 7}
                                    4 {\a 6, \b 1}
                                    5 {\a 1, \b 7}
                                    6 {\a 2, \b 7}
                                    7 {\a 5, \b 3}}
                                   [%1 %2])))]
    (is (= (:states m) #{1 3 5 7}))
    (is (= (:alphabet m) #{\a \b}))
    (is (= (:initial m) 1))
    (is (= (:accept m) #{})))

  (let [m (dfa/prune (dfa/create #{1 2 3}
                                  #{\a}
                                  1
                                  #{2 3}
                                  #(get-in
                                    {1 {\a 1}
                                     2 {\a 1}
                                     3 {\a 1}}
                                    [%1 %2])))]
    (is (= (:states m) #{1}))
    (is (= (:alphabet m) #{\a}))
    (is (= (:initial m) 1))
    (is (= (:accept m) #{}))
    (is (= ((:delta m) 1 \a) 1)))

  (let [m (dfa/prune (dfa/create #{:P :Q :R}
                                  #{\a \b}
                                  :P
                                  #{:R}
                                  #(get-in
                                    {:P {\a :Q, \b :P}
                                     :Q {\a :Q, \b :R}
                                     :R {\a :Q, \b :P}}
                                    [%1 %2])))]
    (is (= (:states m) #{:P :Q :R}))
    (is (= (:alphabet m) #{\a \b}))
    (is (= (:initial m) :P))
    (is (= (:accept m) #{:R}))))

(deftest finite-language-test
  (let [m (dfa/create #{1} #{\a} 1 #{} (fn [q c] 1))]
    (is (dfa/finite-language? m)))

  (let [m (dfa/create #{1} #{\a} 1 #{1} (fn [q c] 1))]
    (is (not (dfa/finite-language? m))))

  (let [m (dfa/create #{1 2 3}
                      #{\a \b}
                      1
                      #{2}
                      #(get-in
                        {1 {\a 2, \b 3}
                         2 {\a 3, \b 3}
                         3 {\a 3, \b 3}}
                        [%1 %2]))]
    (is (dfa/finite-language? m)))

  (let [m (dfa/create #{"bb" "ba" "ab" "aa"}
                      #{\a \b}
                      "bb"
                      #{"aa" "ab"}
                      #(get-in
                        {"bb" {\a "ba", \b "bb"}
                         "ba" {\a "aa", \b "ab"}
                         "ab" {\a "ba", \b "bb"}
                         "aa" {\a "aa", \b "ab"}}
                        [%1 %2]))]
    (is (not (dfa/finite-language? m))))

  (let [m (dfa/create #{:P :Q :R :S}
                      #{\a \b}
                      :P
                      #{:Q :R}
                      #(get-in
                        {:P {\a :Q, \b :S}
                         :Q {\a :S, \b :R}
                         :R {\a :S, \b :S}
                         :S {\a :S, \b :S}}
                        [%1 %2]))]
    (is (dfa/finite-language? m))))

(deftest shortest-example
  (let [m (dfa/create #{:P :Q :R :S}
                      #{\a \b}
                      :P
                      #{:Q :R}
                      #(get-in
                        {:P {\a :Q, \b :S}
                         :Q {\a :S, \b :R}
                         :R {\a :S, \b :S}
                         :S {\a :S, \b :S}}
                        [%1 %2]))]
    (is (= (dfa/shortest-example m) "a")))

  (let [m (dfa/create #{#{}} #{\a} #{} #{#{}} (fn [q c] #{}))]
    (is (= (dfa/shortest-example m) "")))

  (let [m (dfa/create #{1 2 3}
                      #{\a \b}
                      1
                      #{2}
                      #(get-in
                        {1 {\a 2, \b 3}
                         2 {\a 3, \b 3}
                         3 {\a 3, \b 3}}
                        [%1 %2]))]
    (is (= (dfa/shortest-example m) "a")))

  (let [m (dfa/create #{0} #{\a \b} 0 #{} (fn [q c] 0))]
    (is (nil? (dfa/shortest-example m)))))

(deftest minimize-test
  (let [m (dfa/minimize (dfa/create #{0 1 2 3 4 5 6 7 8 9}
                                    #{\a \b}
                                    0
                                    #{3 4 8 9}
                                    #(get-in
                                      {0 {\a 1, \b 9}
                                       1 {\a 8, \b 2}
                                       2 {\a 3, \b 2}
                                       3 {\a 2, \b 4}
                                       4 {\a 5, \b 8}
                                       5 {\a 4, \b 5}
                                       6 {\a 7, \b 5}
                                       7 {\a 6, \b 5}
                                       8 {\a 1, \b 3}
                                       9 {\a 7, \b 8}}
                                      [%1 %2])))]
    (is (= (:states m) #{#{0} #{1 2 5} #{3 4 8} #{6 7} #{9}}))
    (is (= (:alphabet m) #{\a \b}))
    (is (= (:initial m) #{0}))
    (is (= (:accept m) #{#{3 4 8} #{9}}))

    (is (= ((:delta m) #{0} \a) #{1 2 5}))
    (is (= ((:delta m) #{0} \b) #{9}))

    (is (= ((:delta m) #{1 2 5} \a) #{3 4 8}))
    (is (= ((:delta m) #{1 2 5} \b) #{1 2 5}))

    (is (= ((:delta m) #{3 4 8} \a) #{1 2 5}))
    (is (= ((:delta m) #{3 4 8} \b) #{3 4 8}))

    (is (= ((:delta m) #{6 7} \a) #{6 7}))
    (is (= ((:delta m) #{6 7} \b) #{1 2 5}))

    (is (= ((:delta m) #{9} \a) #{6 7}))
    (is (= ((:delta m) #{9} \b) #{3 4 8}))))
