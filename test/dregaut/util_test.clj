(ns dregaut.util-test
  (:use clojure.test)
  (:require [dregaut.util :as util]))

(deftest uprod-test
  (is (= (util/uprod #{1 2 3} #{2 4})
         #{#{1 2} #{1 4} #{2 4} #{3 2} #{3 4}}))
  (is (= (util/uprod #{1 2 3} #{1 2 3})
         #{#{1 2} #{1 3} #{2 3}}))
  (is (= (util/uprod #{1} #{1 2})
         #{#{1 2}}))
  (is (= (util/uprod #{} #{1 2 3 4})
         #{}))
  (is (= (util/uprod #{1 3} #{1 2 3})
         #{#{1 2} #{1 3} #{3 2}})))

(deftest upow2-test
  (is (= (util/upow2 #{1 2 3})
         #{#{1 2} #{1 3} #{2 3}})))

(deftest kleene*-test
  (is (= (take 9 (util/kleene* \a \b))
         ["" "a" "b" "aa" "ab" "ba" "bb" "aaa" "aab"]))
  (is (= (util/kleene*) [""]))
  (is (= (take 5 (util/kleene* "foo" "bar"))
         ["" "bar" "foo" "barbar" "barfoo"])))
