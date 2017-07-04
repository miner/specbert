(ns miner.test-specbert
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [miner.specbert :as sp]))

(deftest a-test
  (testing "specbert"
    (is (s/valid? (s/* integer?) [0 2 3]))
    (is (not (s/valid? (s/* integer?) [0 2 :kw 3])))))


