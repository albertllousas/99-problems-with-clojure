(ns ninety-nine-problems.lists-test
  (:require [clojure.test :refer :all])
  (:require [ninety-nine-problems.lists :refer :all]))

(def empty-list ())

(deftest last-test
  (testing "should find nothing when the list is empty"
    (do
      (is (= nil (last' empty-list)))
      (is (= nil (last'' empty-list)))
      (is (= nil (last''' empty-list)))))
  (testing "should find the only element for a singleton list"
    (do
      (is (= :a (last' '(:a))))
      (is (= :a (last'' '(:a))))
      (is (= :a (last''' '(:a))))))
  (testing "should find the last element for a list of more than one element"
    (do
      (is (= :c (last' '(:a :b :c))))
      (is (= :c (last'' '(:a :b :c))))
      (is (= :c (last''' '(:a :b :c)))))))

(deftest last-but-one-test
  (testing "should find nothing when the list is empty"
    (do
      (is (= nil (last-but-one empty-list)))
      (is (= nil (last-but-one' empty-list)))))
  (testing "should find nothing for a singleton list"
    (do
      (is (= nil (last-but-one '(:a))))
      (is (= nil (last-but-one' '(:a))))))
  (testing "should find the last element but one for a list of more than one element"
    (do
      (is (= :b (last-but-one '(:a :b :c))))
      (is (= :b (last-but-one' '(:a :b :c)))))))

(deftest find-the-kth-element-test
  (testing "should find nothing when the list is empty"
    (do
      (is (= nil (find-the-kth-element empty-list 1)))
      (is (= nil (find-the-kth-element' empty-list 1)))))
  (testing "should find nothing for the list is shorter than k"
    (do
      (is (= nil (find-the-kth-element '(:a) 10)))
      (is (= nil (find-the-kth-element' '(:a) 10)))))
  (testing "should find the k element for a list"
    (do
      (is (= :b (find-the-kth-element '(:a :b :c) 2)))
      (is (= :b (find-the-kth-element' '(:a :b :c) 2))))))

(deftest count-test
  (testing "should count zero when a list is empty"
    (do
      (is (= 0 (count' empty-list)))
      (is (= 0 (count'' empty-list)))
      (is (= 0 (count''' empty-list)))))
  (testing "should count the number of elements of a non-empty list"
    (do
      (is (= 3 (count' '(:a :b :c))))
      (is (= 3 (count'' '(:a :b :c))))
      (is (= 3 (count''' '(:a :b :c)))))))

(deftest reverse-test
  (testing "should do nothing for an empty list"
    (do
      (is (= (reverse' empty-list) empty-list))
      (is (= (reverse'' empty-list) empty-list))
      (is (= (reverse''' empty-list) empty-list))))
  (testing "should do nothing for a non-empty list"
    (do
      (is (= (reverse' '(:a :b :c)) '(:c :b :a)))
      (is (= (reverse'' '(:a :b :c)) '(:c :b :a)))
      (is (= (reverse''' '(:a :b :c)) '(:c :b :a))))))

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
    (do
      (is (= (eliminate-consecutive-duplicates empty-list)) empty-list)
      (is (= (eliminate-consecutive-duplicates' empty-list)) empty-list)
      (is (= (eliminate-consecutive-duplicates'' empty-list)) empty-list)))
  (testing "should do nothing for a list with no consecutive duplicates"
    (do
      (is (= (eliminate-consecutive-duplicates '(:a :b :a :b)) '(:a :b :a :b)))
      (is (= (eliminate-consecutive-duplicates' '(:a :b :a :b)) '(:a :b :a :b)))
      (is (= (eliminate-consecutive-duplicates'' '(:a :b :a :b)) '(:a :b :a :b)))))
  (testing "should remove consecutive duplicates"
    (do
      (is (= (eliminate-consecutive-duplicates '(:a :b :b :c :d :d :b)) '(:a :b :c :d :b)))
      (is (= (eliminate-consecutive-duplicates' '(:a :b :b :c :d :d :b)) '(:a :b :c :d :b)))
      (is (= (eliminate-consecutive-duplicates'' '(:a :b :b :c :d :d :b)) '(:a :b :c :d :b))))))

(deftest pack-consecutive-duplicates-test
  (testing "should do nothing for an empty list"
    (is (= (pack-consecutive-duplicates empty-list) empty-list)))
  (testing "should pack consecutive duplicate into a sublists"
    (is (= (pack-consecutive-duplicates '(:a :b :b :c :d :d :d)) (list (list :a) (list :b :b) (list :c) (list :d :d :d))))))

(deftest encode-test
  (testing "should do nothing for an empty list"
    (do
      (is (= (encode '()) '()))
      (is (= (encode' '()) '()))))
  (testing "should encode a list with consecutive duplicates, encoding elements [#element,element]"
    (do
      (is (= (encode '(:a :b :b :c :d :d :b)) (list (list 1 :a) (list 2 :b) (list 1 :c) (list 2 :d) (list 1 :b))))
      (is (= (encode' '(:a :b :b :c :d :d :b)) (list (list 1 :a) (list 2 :b) (list 1 :c) (list 2 :d) (list 1 :b)))))))

(deftest modified-encode-test
  (testing "should do nothing for an empty list"
    (is (= (modified-encode '()) '())))
  (testing "should encode a list with consecutive duplicates, encoding only duplicates as [#element,element]"
    (is (= (modified-encode '(:a :b :b :c :d :d :b)) (list :a (list 2 :b) :c (list 2 :d) :b)))))

(deftest decode-test
  (testing "should do nothing for an empty list"
    (do
      (is (= (decode '()) '()))
      (is (= (decode' '()) '()))))
  (testing "should decode elements in a list like [#element,element] to the expanded version"
    (do
      (is (= (decode (list :a (list 2 :b) :c (list 2 :d) :b)) '(:a :b :b :c :d :d :b)))
      (is (= (decode' (list :a (list 2 :b) :c (list 2 :d) :b)) '(:a :b :b :c :d :d :b))))))


(deftest duplicate-each-test
  (testing "should duplicate each element of a given list"
    (do
      (is (= (duplicate-each '(:a :b :b :c :d :d :b)) '(:a :a :b :b :b :b :c :c :d :d :d :d :b :b)))
      (is (= (duplicate-each' '(:a :b :b :c :d :d :b)) '(:a :a :b :b :b :b :c :c :d :d :d :d :b :b)))
      (is (= (duplicate-each'' '(:a :b :b :c :d :d :b)) '(:a :a :b :b :b :b :c :c :d :d :d :d :b :b)))
      (is (= (duplicate-each''' '(:a :b :b :c :d :d :b)) '(:a :a :b :b :b :b :c :c :d :d :d :d :b :b))))))

(deftest duplicate-each-n-times-test
  (testing "should duplicate each element of a given list"
    (do
      (is (= (duplicate-each-n-times '(:a :b) 3) '(:a :a :a :b :b :b)))
      (is (= (duplicate-each-n-times' '(:a :b) 3) '(:a :a :a :b :b :b))))))

(deftest drop-every-nth-test
  (testing "should drop every N'th element from a given list"
    (do
      (is (= (drop-every-nth '(:a :b :c :d :e :f :g :h :i :k) 3) '(:a :b :d :e :g :h :k)))
      (is (= (drop-every-nth' '(:a :b :c :d :e :f :g :h :i :k) 3) '(:a :b :d :e :g :h :k))))))

(deftest split-at-test
  (testing "should split a list into two parts; the length of the first part is given"
    (do
      (is (= (split-at' 3 '(:a :b :c :d :e :f :g :h :i :k)) (list (list :a :b :c) (list :d :e :f :g :h :i :k))))
      (is (= (split-at'' 3 '(:a :b :c :d :e :f :g :h :i :k)) (list (list :a :b :c) (list :d :e :f :g :h :i :k))))))
  (testing "should not split a list if position is out of range"
    (do
      (is (= (split-at' 11 '(:a :b :c :d :e :f :g :h :i :k)) '(:a :b :c :d :e :f :g :h :i :k)))
      (is (= (split-at'' 11 '(:a :b :c :d :e :f :g :h :i :k)) '(:a :b :c :d :e :f :g :h :i :k))))))

(deftest slice-test
  (testing "should extract a slice from a list"
    (do
      (is (= (slice '(:a :b :c :d :e :f :g :h :i :k) 3 7) '(:c :d :e :f :g)))
      (is (= (slice' '(:a :b :c :d :e :f :g :h :i :k) 3 7) '(:c :d :e :f :g))))))

(deftest rotate-to-left-test
  (testing "should rotate a list N places to the left"
    (do
      (is (= (rotate-to-left '(:a :b :c :d :e :f :g :h) 3) '(:d :e :f :g :h :a :b :c)))
      (is (= (rotate-to-left '(:a :b :c :d :e :f :g :h) -2) '(:g :h :a :b :c :d :e :f ))))))


