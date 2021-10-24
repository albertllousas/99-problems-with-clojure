(ns ninety-nine-problems.lists)

;; P01
(defn last' [[first & remaining]]
  (cond
    (empty? remaining) first
    :else (recur remaining)))

;; P01 - with built-in functions
(defn last'' [coll]
  (first (reverse coll)))

(defn last''' [coll]
  (last coll))

;; P02
(defn last-but-one [[first second & remaining]]
  (cond
    (and (nil? second) (empty? remaining)) nil
    (empty? remaining) first
    :else (recur (conj remaining second))))

;; P02 - with built-in functions
(defn last-but-one' [coll]
  (if (> (count coll) 1) (nth (reverse coll) 1) nil))

;; P03
(defn find-the-kth-element [[head & remaining] k]
  (cond
    (and (nil? head) (empty? remaining)) nil
    (= k 1) head
    :else (recur remaining (dec k))))

;; P03 - with built-in functions
(defn find-the-kth-element' [coll k]
  (nth coll (dec k) nil))

;; P04
(defn count' [coll]
  (letfn
    [(inner [[head & tail] counter]
       (if (nil? head)
         counter
         (recur tail (inc counter))))]
    (inner coll 0)))

;; P04 - with built-in functions
(defn count'' [coll]
  (reduce (fn [acc _] (inc acc)) 0 coll))

(defn count''' [coll]
  (count coll))

;; P05
(defn reverse' [[head & tail]]
  (if (nil? head) () (concat (reverse' tail) (list head))))

;; P05 tail-recursive
(defn reverse'' [coll]
  (letfn [(inner [[head & tail] acc]
            (if (nil? head)
              acc
              (recur tail (concat (list head) acc))))]
    (inner coll ())))

;; P05 - with built-in functions
(defn reverse''' [coll]
  (reduce (fn [acc element] (cons element acc)) '() coll))

;; P06
(defn is-palindrome [coll]
  (= coll (reverse' coll)))

;; P07
(defn flatten' [coll]
  (letfn
    [(inner [[head & tail] acc]
       (cond
         (nil? head) acc
         (seq? head) (recur tail (flatten' (concat acc head)))
         :else (recur tail (concat acc (list head)))))]
    (inner coll ())))

;; P08
(defn eliminate-consecutive-duplicates [coll]
  (letfn
    [(inner [[first second & tail :as all] acc]
       (cond
         (empty? all) acc
         (nil? second) (concat acc (list first))
         (= first second) (recur tail (concat acc (list first)))
         :else (recur (concat (list second) tail) (concat acc (list first)))))]
    (inner coll ())))

;; P08 - with built-in functions
(defn eliminate-consecutive-duplicates' [coll]
  (reduce
    (fn [acc element]
      (if (= (last acc) element)
        acc
        (concat acc (list element))))
    '()
    coll))

(defn eliminate-consecutive-duplicates'' [coll]
  (dedupe coll))

;; P09
(defn pack-consecutive-duplicates
  [coll]
  (letfn
    [(inner [[first & tail :as all] acc]
       (cond
         (empty? all) acc
         (not= first (last' (last' acc))) (recur tail (add-new-sublist acc first))
         :else (recur tail (append-to-last-sublist acc first))))
     (add-new-sublist [acc new-element]
       (concat acc (list (list new-element))))
     (append-to-last-sublist [acc consecutive-element]
       (concat (drop-last acc) (list (concat (last' acc) (list consecutive-element)))))]
    (inner coll '()))
  )

;; P09 - with built-in functions
(defn pack-consecutive-duplicates' [coll]
  (partition-by (fn [x] x) coll))

;; P10
(defn encode [coll]
  (letfn
    [(inner [[first & tail :as all] acc]
       (cond
         (empty? all) acc
         (= (count' first) 1) (recur tail (concat acc (list (list 1 (last' first)))))
         :else (recur tail (concat acc (list (list (count' first) (last' first)))))))]
    (inner (pack-consecutive-duplicates coll) '())))

;; P10
(defn encode' [coll]
  (->> (partition-by identity coll)
       (map (fn [sublist] (list (count sublist) (first sublist))))))


;; P11
(defn modified-encode [coll]
  (->> (encode' coll)
       (map (fn [sublist] (if (= 1 (first sublist)) (last sublist) sublist)))))
;; P12
(defn decode [coll]
  (letfn
    [(inner [[first & tail :as all] acc]
       (cond
         (empty? all) acc
         (list? first) (recur tail (concat acc (expand first)))
         :else (recur tail (concat acc (list first)))))
     (expand [[repetitions element]]
       (repeat repetitions element))]
    (inner coll ())))

;; P12 - with built-in functions
(defn decode' [coll]
  (->> (map #(if (list? %) (repeat (first %) (last %)) %) coll)
    flatten))