(ns clojure-training.core
  (:require
   [clojure.set :as s]))

;; Problem #19 Last Element
(defn last-element [pcoll]
  (first (reverse pcoll)))

;; Problem #20 Penultimate Element
(defn p-element [pcoll]
  (second (reverse pcoll)))

;; Problem #21 Nth Element
(defn nth-element [pcoll idx]
  (get (vec pcoll) idx))

;;Problem #22 Count a Sequence.
;;A function which returns the total number of elements in a sequence.
(defn count-seq [pcoll]
  (reduce (fn [x _] (+ x 1)) 0 pcoll))

;; Problem #23 Reverse a Sequence
(defn reverse-seq [pcoll]
  (let [rcoll (into () pcoll)]
    rcoll))

;; Problem #24 Sum It All Up.
(defn sum-all [pcoll]
  (reduce + pcoll))

;; Problem #25 Find the odd numbers  
(defn odd-numbers [pcoll]
  (for [x pcoll :when (odd? x)]
    x))

;; Problem #26 Fibonacci Sequence
(defn fibonacci
  [n]
  (loop [result [1 1]]
    (if (= (count result) n)
      result
      (recur (conj result (+ (last result) (last (butlast result))))))))

;; other Fib
(defn fibonacci-2
  [n]
  (let [aux (fn aux [x y]
              (lazy-seq (cons y (aux y (+ x y)))))]
    (take n (aux 0 1))))

 ;; Other Fib
(defn fibonacci-3 [nu]
  (take nu (map first (iterate (fn [[x y]] [y (+ x y)]) [1 1]))))

;; Problem #27 Palindrome Detector
(defn palindrome? [se]
  (= (seq se) (reverse se)))

;; Problem #28 Flat a collection    
(defn flat-coll [coll]
  (loop [result []
         c (vec coll)]
    (if-let [frst (first c)]
      (if (coll? frst)
        (recur (vec (concat result (flat-coll frst))) (rest c))
        (recur (conj result frst) (rest c)))
      result)))

;; Problem #29 Get the CAPs
(defn just-capital [string]
  (apply str (re-seq #"[A-Z]+" string)))

;; Problem #30 Compress a Sequence 
;; fn removes consecutive duplicates from a sequence.    
(defn remove-consecutive [sq]
  (reduce (fn [coll-result elem]
            (let [ult (last coll-result)]
              (if (not= ult elem)
                (conj coll-result elem)
                coll-result)))
          [] sq))

;; Problem #31 Pack a Sequence
(defn pack-sequence [coll]
  (partition-by (fn [elem] elem) coll))

;; Problem #32 Duplicate a Sequence. 
;; Duplicate each element of a sequence  
(defn duplicate [pcoll]
  (reduce concat (map #(repeat 2 %) pcoll)))

;; Problem #33 Replicate a Sequence.
;; based on Problem #32 ,repeat each element n times instead of just 2 times  
(defn replicate-ntimes [pcoll n]
  (reduce concat (map #(repeat n %) pcoll)))

 ;; Problem #41
(defn drop-nth-elment [coll nth]
  (loop [res []
         c coll]
    (if (empty? c)
      res
      (recur
       (concat res (take (dec nth) c))
       (drop nth c)))))

;; Problem #41
(defn drop-nth-other [coll nth]
  (let [result (transient (empty coll))]
    (dotimes [n (count coll)]
      (when (not= n (- nth 1))
        (conj! result (coll n))))
    (persistent! result)))

;; Problem #43 reverse interleave
(defn rev-interleave [coll nc]
  (let [part (quot (count coll) nc)]
    (->> coll
         (partition nc)
         (apply interleave)
         (partition part))))

;; Problem #44 rotate sequence
(defn rotate [nn coll]
  (loop [n nn
         c coll]
    (if (= n 0)
      c
      (if (< n 0)
        (recur (inc n) (cons (last c) (butlast c)))
        (recur (dec n) (conj (vec (rest c)) (first c)))))))

;;Problem #46 Flipping-out
(defn flipping-out [function]
  (fn [x y]
    (function y x)))

;; Problem #49
(defn split-seq [at coll]
  [(take at coll) (drop at coll)])

;; Problem #50 Split by type
(defn split-by-type [coll]
  (reduce (fn [setr [_ value]]
            (conj setr value))
          #{}
          (group-by type coll)))

;;Problem #53 Longest Incresing subsequence
(defn longest-inc-subseq [coll]
  (let [inc-subseq (fn []                                    ;;I get courage and I tried to make a transducer.Maybe It'snt
                     (fn [rf]
                       (let [a (java.util.ArrayList.)
                             pv (atom ::none)]
                         (fn
                           ([] (rf))
                           ([result]
                            (let [result (if (or (.isEmpty a) (= (.size a) 1))
                                           result
                                           (let [v (vec (.toArray a))]
                                             (.clear a)
                                             (rf result v)))]
                              (rf result)))
                           ([result input]
                            (let [before @pv]
                              (reset! pv input)
                              (if (= before (dec input))
                                (do
                                  (.add a input)
                                  result)
                                (do
                                  (let [v (vec (.toArray a))]
                                    (.clear a)
                                    (.add a input)
                                    (if (> (count v) 1)
                                      (rf result v)
                                      result))))))))))
        fun ((inc-subseq) conj)
        candidates (fun (reduce fun [] coll))]
    (if (empty? candidates)
      []
      (apply max-key count candidates))))

;;Problem #54 Partition a sequence
(defn part-seq [n coll]
  (loop [result []
         c coll]
    (let [part (take n c)]
      (if (= n (count part))
        (recur (conj result part) (drop n c))
        result))))

;; Problem #55 Count occurrences
(defn occurrence-count [coll]
  (->> (group-by identity coll)
       (reduce (fn [result [k v]]
                 (conj result [k (count v)]))
               {})))

;;Problem #56 Distinct Items
(defn distinct-items [coll]
  (reduce (fn [res items]
            (if (some #{items} res)
              res
              (conj res items)))
          [] coll))

;; Problem #58 Function composition
(defn fn-comp [& fns]
  (fn [& xs]
    (let [frst (first (reverse fns))
          remaining (rest (reverse fns))]

      (reduce (fn [r f] (f r))
              (apply frst xs) remaining))))

;; Problem #59 juxtaposition
(defn jxtpos [& fns]
  (fn [& args]
    (map #(apply % args) fns)))

;; Problem #61
(defn map-construction [kys vls]
  (first (reduce (fn [[m v] k]
                   (if (not (empty? v))                                ;; si no esta vacio para evitar asociar un valor nil a la llave k
                     (vector (assoc m k (first v)) (rest v))
                     (vector m v)))                                     ;; si no, lo retornamos igual
                 [{} vls] kys)))

;; Problem #62
(defn reimplement-iterate [fun value]
  (lazy-seq
   (cons value (reimplement-iterate fun (fun value)))))

;; Problems #63
(defn group-seq [pred sequen]
  (loop [resp {}
         sequen sequen]
    (if-let [value (first sequen)]
      (let [k (pred value)]
        (recur
         (assoc resp k (conj (get resp k []) value))
         (rest sequen)))
      resp)))

;;Problem #67 Prime number
(defn firstn-prime [n]
  (let [prime? (fn [primes candidate]
                 (if (some #(when (= 0 (rem candidate %)) true) primes)
                   false
                   true))]
    (loop [primes [2]
           candidates (iterate #(+ % 2) 3)]
      (if (= n (count primes))
        primes
        (recur (if (prime? primes (first candidates))
                 (conj primes (first candidates))
                 primes)
               (rest candidates))))))

;; Problem #70 Word Sorting
(defn sort-words [text]
  (->> (re-seq #"[a-zA-Z]+" text)
       (map (fn [e] [(.toLowerCase e) e]))
       (into (sorted-map))
       (map (fn [[_ v]] v))))

;; Problem #74 Filter Perfect Square
(defn filter-square
  "Generate the squares by the Pascal's Triangle property."
  [string]
  (let [prow (fn prow [[_ _ f :as row]]
               (let [[_ _ s :as nxt] (map +' (concat [0] row) (concat row [0]))]
                 (lazy-seq
                  (cons (+' f s) (prow nxt)))))
        squares (prow [1 2 1])
        nums (->> (-> string
                      (clojure.string/split #","))
                  (map #(Integer/parseInt %)))]

    (->> nums
         (filter (fn [n]
                   (let [candidate (take-while #(<= % n)
                                               squares)]
                     (if (= n (last candidate))
                       true
                       false))))
         (clojure.string/join \,))))

;;Problem #77 Anagram finder
(defn ana-for [coll]
  (->> (map (fn [word]
              (let [letters (sort word)]
                (filter #(and (= letters (sort %))) coll)))
            coll)

       (reduce (fn [r e]
                 (if (> (count e) 1)
                   (conj r (set e))
                   r))
               #{})))

;; Problem #78 Implement Trampoline
(defn tramp [f & args]
  (loop [v (apply f args)]
    (if (and (ifn? v) (not (and
                            (vector? v) (map? v) (set? v))))
      (recur (v))
      v)))

;; Problem #79 Triangle minimal path
(defn trian-min-path [triangle]
  (let [stop (dec (count triangle))
        aux (fn aux [tr m n]
              (if (= m stop)
                ((tr m) n)
                (+ ((tr m) n)
                   (min (aux tr (inc m) n)
                        (aux tr (inc m) (inc n))))))]
    (aux (vec triangle) 0 0)))

;; Problem #80 Perfect numbers
(defn perfect-n [number]
  (->> (range 1 (+ 1 (/ number 2))) ;; Generate candidates
       (filter #(= 0 (rem number %)))
       (reduce +)
       (= number)))

;; Problem #81  Set Intersection
(defn set-intersection [s1 s2]
  (first (reduce (fn [[sr sp] elme]
                   (if (sp elme)
                     [(conj sr (sp elme)) sp]
                     [sr sp]))
                 [#{} s2] s1)))

;; Problem #88 Symetric difference
(defn symetric-dif [s1 s2]
  (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1)))

;;Problem #90 Cartesian Product
(defn cartesian-prod [s1 s2]
  (into #{}  (for [x s1 y s2] [x y])))

;;Problem #95 To Tree, or not to Tree
(defn tree? [tree]
  (let [[root left right] tree]
    (if (= (count tree) 3)
      (and root (not (false? left)) (not (false? right))
           (or (nil? left) (tree? left))
           (or (nil? right) (tree? right)))
      false)))

;;Problem #96 Beauty is Symetry
(defn symetry? [t]
  (let [mirror (fn mirror [[root left right :as t]]
                 (if t
                   [root (mirror right) (mirror left)]))]
    (= t (mirror t))))

;; Problem #97 Pascal triangle
(defn pascal [n]
  (let [aux (fn [row]
              (map #(apply + %) (partition 2 1 row)))]
    (loop [r [1]]
      (if (= (count r) n)
        r
        (recur (concat [1] (aux r) [1]))))))

;; Problem #99 Product digits
(defn product-digits [x y]
  (->> (* x y)
       (str)
       (seq)
       (map #(Character/getNumericValue %))))

;;Problem #100 least common multiple
(defn mcm [a b & xs]
  (let [mcd (fn mcd
              ([a b]
               (if (zero? (rem a b))
                 b
                 (mcd b (rem a b))))

              ([a b & xs]
               (reduce mcd (mcd a b) xs)))]
    (/ (apply * a b xs) (apply mcd a b xs))))

;;Problem #102 IntoCamelCase
(defn to-camel-case [string]
  (let [[frst & rest] (re-seq #"[a-zA-Z]+" string)
        up (map (fn [x]
                  (.replaceFirst x (str (first x)) (str (Character/toUpperCase (first x))))) rest)]
    (reduce #(.concat %1 %2) "" (cons frst up))))

;;Problem #105 Identify keys and values
(defn identify-kv [coll]
  (->> (partition-by keyword? coll)
       (partition 2)
       (reduce (fn [r [kseq vseq]]
                 (if (= (count kseq) 1)
                   (assoc! r (first kseq) (vec vseq))
                   (let [lastkey (last kseq)]
                     (reduce (fn [rr e]
                               (assoc! rr e [])) r (butlast kseq))
                     (assoc! r lastkey vseq))))
               (transient {}))
       (persistent!)))

;; Problem #115 The Balance of N
(defn balanced? [n]
  (let [transform (comp #(reduce + %) #(map (fn [n] (Character/getNumericValue n)) %))
        halft (fn [coll]
                (let [at (quot (count coll) 2)]
                  (if (even? (count coll))
                    (split-at at coll)
                    [(take at coll) (drop (+ 1 at) coll)])))]

    (if (= (count (seq (str n))) 1)
      true
      (apply = (map transform (halft (seq (str n))))))))

;; Problem #118 Re-implement Map
(defn reimplement-map [f coll]
  (lazy-seq
   (let [r (empty coll)
         fa (fn [c i]
              (conj c (f i)))]
     (reduce fa r coll))))

(defn other-map [f coll]
  (lazy-seq
   (if-let [one (first coll)]
     (cons (f one) (other-map f (rest coll))))))

;; Problem 120  Sum of square of digits
(defn sum-square [integers]
  (reduce (fn [acc number]
            (let [digits (seq (str number))
                  faux #(Math/pow (Character/getNumericValue %) 2)
                  sq (apply + (map faux digits))]
              (if (> sq number)
                (inc acc)
                acc)))
          0 integers))

;; Problem #122 Read a binary number
(defn read-bin-num [string]
  (Integer/parseInt string 2))

;;Problem #128 Playing cards
(defn pcard [card]
  (let [suits {\S :spade \H :heart \D :diamond \C :club}
        ranks {\2 0 \3 1 \4 2 \5 3 \6 4 \7 5 \8 6 \9 7 \T 8 \J 9 \Q 10 \K 11 \A 12}
        [s r] (seq card)]
    {:suit (suits s) :rank (ranks r)}))

;;Problem #135 Infix Calculator
(defn infix-calc [& args]
  (loop [r args]
    (let [[x op y] r]
      (if (= 1 (count r))
        (first r)
        (recur (cons (op x y) (drop 3 r)))))))

;; Problems #143 Dot product
(defn dot-product [v1 v2]
  (apply + (map * v1 v2)))

;;Problems #146 Tree into tables
(defn tree-to-tables
  [param]
  (into {} (for [[k vmap] param [km v] vmap]
             [[k km] v])))

;;Problem #147 pascal trapezoide
(defn trapezoid [row]
  (let [aux (fn [row]
              (concat [(first row)]
                      (map #(apply + %) (partition 2 1 row))
                      [(last row)]))]
    (iterate aux row)))

;; Problem # 153 Pairwise Disjoint set
(defn p-disjoin? [sss]
  (every? empty? (for [sa sss sb sss :when (not= sa sb)]
                   (clojure.set/intersection sa sb))))

;; Problem #156
(defn map-defaults [defaults kys]
  (reduce #(assoc %1 %2 defaults) {} kys))

;;
(defn gcd [x y]
  (let [mx (max x y)
        mn (min x y)]
    (if (= (rem mx mn) 0)
      mn
      (gcd (rem mx mn) mx))))

;; Problem #157
(defn indexs-seq [coll]
  (partition 2 (interleave coll (range (count coll)))))

;; Problem, #166 Comparisions
(defn comparisions [less x y]
  (cond
    (less x y) :lt
    (less y x) :gt
    :else :eq))

;; Problems #171 Intervals
(defn intervals [coll]
  (let [interval (fn []
                   (fn [rf]
                     (let [a (java.util.ArrayList.)
                           pv (atom ::none)]
                       (fn
                         ([] (rf))
                         ([result]
                          (let [result (if (.isEmpty a)
                                         result
                                         (let [v (vec (.toArray a))]
                                           (.clear a)
                                           (rf result v)))]
                            (rf result)))
                         ([result input]
                          (let [before @pv]
                            (reset! pv input)
                            (if (= before (dec input))
                              (do
                                (.add a input)
                                result)
                              (do
                                (let [v (vec (.toArray a))]
                                  (.clear a)
                                  (.add a input)
                                  (if (not-empty v)                              ;;
                                    (rf result v)
                                    result))))))))))
        xform (comp (dedupe) (interval))
        fun (xform conj)]
    (map (fn [x] [(first x) (last x)])
         (fun (reduce fun [] (sort coll))))))

;; Problem #83
(defn half-truth [& arg]
  (and (not (every? false? arg)) (not (every? true? arg))))

(defn oscilrate [ivalue & functions]
  (let [vf (vec functions)
        aux (fn aux [ivalue [f & rest]]
              (lazy-seq
               (cons ivalue (aux (f ivalue) (conj (vec rest) f)))))]
    (aux ivalue vf)))
(defn nasss []
  (println "hoals"))

