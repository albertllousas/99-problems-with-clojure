(ns ninety-nine-problems.lists)

;; all solutions without the use of clojure built-in fns to operate with lists

;; P01
(defn find-the-last-element
  [[first & remaining]]
  (cond
    (empty? remaining) first
    :else (recur remaining)))

;; P02
(defn find-the-last-but-one-element
  [[first second & remaining]]
  (cond
    (and (nil? second) (empty? remaining)) nil
    (empty? remaining) first
    :else (recur (conj remaining second))))

;; P03
(defn find-the-kth-element
  [[head & remaining] k]
  (cond
    (and (nil? head) (empty? remaining)) nil
    (= k 1) head
    :else (recur remaining (dec k))))

;; P04
(defn count'
  [coll]
  (letfn
    [(inner
       [[head & tail] counter]
       (if (nil? head)
         counter
         (recur tail (inc counter))))]
    (inner coll 0)))

;; P05
(defn reverse'
  [[head & tail]]
  (if (nil? head) () (concat (reverse' tail) (list head))))

;; P05 tail-recursive
(defn reverse''
  [coll]
  (letfn
    [(inner
       [[head & tail] acc]
       (if (nil? head)
         acc
         (recur tail (concat (list head) acc))))]
    (inner coll ())))

;; P06
(defn is-palindrome [coll] (= coll (reverse' coll)))

;; P07
(defn flatten'
  [coll]
  (letfn
    [(inner
       [[head & tail] acc]
       (cond
         (nil? head) acc
         (seq? head) (recur tail (flatten' (concat acc head)))
         :else (recur tail (concat acc (list head)))))]
    (inner coll ())))

;; P08
(defn eliminate-consecutive-duplicates
  [coll]
  (letfn
    [(inner
       [[first second & tail :as all] acc]
       (cond
         (empty? all) acc
         (nil? second) (concat acc (list first))
         (= first second) (recur tail (concat acc (list first)))
         :else (recur (concat (list second) tail) (concat acc (list first)))))]
    (inner coll ())))

;; P09
(defn pack-consecutive-duplicates-into-sublists
  [coll]
  )
