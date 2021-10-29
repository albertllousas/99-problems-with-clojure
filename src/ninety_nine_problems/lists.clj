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
    [(inner [[head & tail :as all] acc]
       (cond
         (empty? all) acc
         (not= head (last' (last' acc))) (recur tail (add-new-sublist acc head))
         :else (recur tail (append-to-last-sublist acc head))))
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
    [(inner [[head & tail :as all] acc]
       (cond
         (empty? all) acc
         (= (count' head) 1) (recur tail (concat acc (list (list 1 (last' head)))))
         :else (recur tail (concat acc (list (list (count' head) (last' head)))))))]
    (inner (pack-consecutive-duplicates coll) '())))

;; P10
(defn encode' [coll]
  (->> (partition-by identity coll)
       (map (fn [sublist] (list (count sublist) (first sublist))))))


;; P11 & P13
(defn modified-encode [coll]
  (->> (encode' coll)
       (map (fn [sublist] (if (= 1 (first sublist)) (last sublist) sublist)))))
;; P12
(defn decode [coll]
  (letfn
    [(inner [[head & tail :as all] acc]
       (cond
         (empty? all) acc
         (list? head) (recur tail (concat acc (expand head)))
         :else (recur tail (concat acc (list head)))))
     (expand [[repetitions element]]
       (repeat repetitions element))]
    (inner coll ())))

;; P12 - with built-in functions
(defn decode' [coll]
  (->> (map #(if (list? %) (repeat (first %) (last %)) %) coll)
       flatten))

;; P14
(defn duplicate-each [coll]
  (letfn
    [(inner [[head & tail :as all] acc]
       (if (empty? all)
         acc
         (recur tail (concat acc (list head head)))))]
    (inner coll '())))

;; P14 - with built-in functions
(defn duplicate-each' [coll]
  (->> (map #(list % %) coll)
       flatten))

(defn duplicate-each'' [coll]
  (reduce #(concat %1 (list %2 %2)) '() coll))

(defn duplicate-each''' [coll]
  (mapcat #(list % %) coll))

;; P15
(defn duplicate-each-n-times [coll n]
  (letfn
    [(inner [[head & tail :as all] n acc]
       (if (empty? all)
         acc
         (recur tail n (concat acc (repeat n head)))))]
    (inner coll n '())))

;; P15 - with built-in functions
(defn duplicate-each-n-times' [coll n]
  (mapcat #(repeat n %) coll))

;; P16
(defn drop-every-nth [coll n]
  (letfn
    [(inner [[head & tail :as all] n index acc]
       (cond
         (empty? all) acc
         (= 0 (rem index n)) (recur tail n (inc index) acc)
         :else (recur tail n (inc index) (concat acc (list head)))
         ))]
    (inner coll n 1 '())))

;; P16 - with built-in functions
(defn drop-every-nth' [coll n]
  (->> (map-indexed #(hash-map :index %1 :value %2) coll)
       (filter #(not= 0 (rem (inc (get % :index)) n)))
       (map #(get % :value))))

;; P17
(defn split-at' [position coll]
  (letfn
    [(inner [position [head & tail :as all] index left-acc]
       (cond
         (empty? all) left-acc
         (= (inc index) position) (list (concat left-acc (list head)) tail)
         :else (recur position tail (inc index) (concat left-acc (list head)))))]
    (inner position coll 0 '())))

;; P17 - with built-in functions
(defn split-at'' [position coll]
  (if (<= 1 position (count coll))
    (list (take position coll) (drop position coll))
    coll))

;; P18
(defn slice [coll start end]
  (letfn
    [(inner [[head & tail :as all] start end index acc]
       (cond
         (empty? all) acc
         (<= start (inc index) end) (recur tail start end (inc index) (concat acc (list head)))
         :else (recur tail start end (inc index) acc)))]
    (inner coll start end 0 '())))

;; P18 - with built-in functions
(defn slice' [coll start end]
  (->> (drop (dec start) coll)
       (take (- (inc end) start))))

;; P19
(defn rotate-to-left [coll places]
  (letfn
    [(inner [[head & tail :as all] places position acc]
       (cond
         (empty? all) acc
         (> position places) (concat all acc)
         :else (recur tail places (inc position) (concat acc (list head)))))]
    (if (< 0 places)
      (inner coll places 1 '())
      (inner coll (+ (count' coll) places) 1 '()))))

;; P20
(defn remove-at [position coll]
  (letfn
    [(inner [position [head & tail :as all] index acc]
       (cond
         (empty? all) acc
         (= (inc index) position) (concat acc tail)
         :else (recur position tail (inc index) (concat acc (list head)))))]
    (inner position coll 0 '())))

;; P20 - with built-in functions
(defn remove-at' [position coll]
  (concat (take (dec position) coll) (drop position coll)))

;; P21
(defn insert-at [position element coll]
  (let [split-coll (split-at' (dec position) coll)]
    (concat (first split-coll) (list element) (last' split-coll))))

(defn insert-at' [position element coll]
  (letfn
    [(inner [position element [head & tail :as all] index acc]
       (cond
         (empty? all) acc
         (= (inc index) position) (concat acc (list element) all)
         :else (recur position element tail (inc index) (concat acc (list head)))))]
    (inner position element coll 0 '())))

;; P22
(defn range' [start end]
  (letfn
    [(inner [current end acc]
       (if (= current end)
         (concat acc (list current))
         (recur (inc current) end (concat acc (list current)))))]
    (if (< start end)
      (inner start end '())
      '())))

;; P22 - with built-in functions
(defn range'' [start end]
   (take (- end (dec start)) (iterate inc start)))
