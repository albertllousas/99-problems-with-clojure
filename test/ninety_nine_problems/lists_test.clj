(ns ninety-nine-problems.lists-test
  (:require [clojure.test :refer :all])
  (:require [ninety-nine-problems.lists :refer :all]))

(def empty-list ())

(deftest find-the-last-element-test
  (testing "should find nothing when the list is empty"
    (is (= nil (find-the-last-element empty-list))))
  (testing "should find the only element for a singleton list"
    (is (= :a (find-the-last-element '(:a)))))
  (testing "should find the last element for a list of more than one element"
    (is (= :c (find-the-last-element '(:a :b :c))))))

(deftest find-the-last-but-one-element-test
  (testing "should find nothing when the list is empty"
    (is (= nil (find-the-last-but-one-element empty-list))))
  (testing "should find nothing for a singleton list"
    (is (= nil (find-the-last-but-one-element '(:a)))))
  (testing "should find the last element but one for a list of more than one element"
    (is (= :b (find-the-last-but-one-element '(:a :b :c))))))

(deftest find-the-kth-element-test
  (testing "should find nothing when the list is empty"
    (is (= nil (find-the-kth-element empty-list 1))))
  (testing "should find nothing for the list is shorter than k"
    (is (= nil (find-the-kth-element '(:a) 10))))
  (testing "should find the k element for a list"
    (is (= :b (find-the-kth-element '(:a :b :c) 2)))))

(deftest count-test
  (testing "should count zero when a list is empty"
    (is (= 0 (count' empty-list))))
  (testing "should count the number of elements of a non-empty list"
    (is (= 3 (count' '(:a :b :c))))))

(deftest reverse-test
  (testing "should do nothing for an empty list"
    (is (and
           (= (reverse' empty-list) empty-list)
           (= (reverse'' empty-list) empty-list))))
  (testing "should do nothing for a non-empty list"
    (is (and
          (= (reverse' '(:a :b :c)) '(:c :b :a))
          (= (reverse'' '(:a :b :c)) '(:c :b :a))))))

(deftest is-palindrome-test
  (are [expected i] (= expected (is-palindrome i))
                    true (seq "racecar")
                    true '(:a :b :b :a)
                    true '()
                    false (seq "albert")))

(deftest flatten-test
  (testing "should flatten a inner nested list"
    (is (= (flatten' (list :a (list :b :c) :d :e)) '(:a :b :c :d :e))))
  (testing "should flatten a multiple nested lists"
    (is (= (flatten' (list :a (list :b :c) (list (list :d)  (list :e)))) '(:a :b :c :d :e))))
  (testing "should do nothing for an already flattened list"
    (is (= (flatten' '(:a :b :c :d :e)) '(:a :b :c :d :e)))))

(deftest eliminate-consecutive-duplicates-test
  (testing "should do nothing for an empty list"
    (is (= (eliminate-consecutive-duplicates '())) '()))
  (testing "should do nothing for a list with no consecutive duplicates"
    (is (= (eliminate-consecutive-duplicates '(:a :b :a :b)) '(:a :b :a :b))))
  (testing "should remove consecutive duplicates"
    (is (= (eliminate-consecutive-duplicates '(:a :b :b :c :d :d :b)) '(:a :b :c :d :b)))))