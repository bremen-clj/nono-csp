(ns clojure-teaser.core
  (:require [clocop.core :refer :all]
            [clocop.constraints :refer :all]))
;;nonogram solver via constraint satisfication

(defn start-indices [constraint numel] ;; start-indices [3 6] means, the first block starts at pos 3 and the second at 6
  (if (empty? constraint) [[]]
    (let [num-csts (count constraint)
          positions-for-first (range (- numel (reduce + constraint) num-csts -1 -1))] ;;range is exclusive, so add an extra 1
      (for [fi positions-for-first ;;for all possibilities of choosing the first one
            other-indices (start-indices (rest constraint) (- numel (first constraint) fi 1))] ;;get all possibilities choosing the remaining
        (cons fi (map (partial + fi (first constraint) 1) other-indices)))))) 

(defn start-positions-to-constraint [start-positions constraint vars] ;; from start-indices to vector of 0 and 1 and constrain the vars.
  (let [state-vec (loop [v [] cur-idx 0 [p & ps :as pos] (concat start-positions [(count vars)])
                         [c & cs :as cons] constraint]
                    (cond
                      (= cur-idx (count vars)) v ;;exit loop
                      (< cur-idx p)
                      (recur (concat v (repeat (- p cur-idx) 0)) p pos cons)
                      (= cur-idx p)
                      (recur (concat v (repeat c 1)) (+ cur-idx c) ps cs)))]
    (apply $and (map $= vars state-vec))))

(defn apply-constraint [constraint vars] ;;one of the possible start-indices has to be realized.
  (-> (apply $or (map #(start-positions-to-constraint % constraint vars) (start-indices constraint (count vars))))
      constrain!))

(defn to-matrix [res num-rows num-cols] ;;converts clocop output to matrix format for easy pretty printing
  (loop [mat (vec (map vec (partition num-cols (repeat (* num-rows num-cols) nil)))) [[k v] & rs] (seq res)]
    (if k
      (let [[f s] (map #(Integer/parseInt %) (.split k ","))] (recur (assoc-in mat [f s] v) rs))
      mat)))

(defn solve-nono [{:keys [num-rows num-cols row-constraints column-constraints]}]
  (with-store (store) ;;initialize constraint store
    (let [lvars (partition num-cols (for [i (range num-rows) j (range num-cols)] (int-var (str i "," j) 0 1)))] ;;create vars
      (doall (map apply-constraint row-constraints lvars))
      (doall (map apply-constraint column-constraints (apply map vector lvars)))
      (solve!)
      (to-matrix (solve! :log? true) num-rows num-cols))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; example nonograms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;wikipedia example
(def example-nonogramm
  {:num-rows 11  :num-cols 8
   :row-constraints [[] [4] [6] [2 2] [2 2] [6] [4] [2] [2] [2] []]
   :column-constraints [[] [9] [9] [2 2] [2 2] [4] [4] []]})

(def another-example
  {:num-rows 5 :num-cols 5
   :row-constraints [[1] [2] [1 2] [4] [3]]
   :column-constraints [[3] [2] [2] [3] [3]]})
;;other 15x15 nnogram
(def bigger-example-nonogramm
  {:num-rows 15 :num-cols 15
   :row-constraints [[8 2 1] [2 8 2] [4 3 2] [1 2 1] [3 1 4] [1 3 8] [2 5 3] [2 2 1 1 2]
                     [1 5] [1 4 6] [3 2 1 3 2] [4 1] [5 1 1 1] [3 3 1 2] [2 4 3]]
   :column-constraints [[1 2 2 1 1] [2 5 1 1 1] [3 1 2 2] [1 1 1 2 3] [3 1 1 2 3] [3 2 1 3 1]
                        [2 1 1 2] [2 3 5] [3 2 1 1 1] [3 3 2] [32 3] [2 2 4 2] [1 4 2 1 1] [2 5 1 2] [3 5 1 2]]})

(def biggggger-example-nonogramm
  {:num-rows 25,
 :num-cols 25,
 :row-constraints
 [[2 7 4 1 2]
  [1 10 4 1 2]
  [8 1 5 2]
  [5 5 2 2]
  [2 1 4 7]
  [5 3 5]
  [6 10]
  [8 4 3]
  [7 6 3]
  [2 5 4 1 3]
  [2 7 2 1 1]
  [1 3 4 1 1 1]
  [7 1 1 1 1]
  [6 4]
  [5 1 5 4]
  [4 3 7]
  [3 4 4]
  [1 3 3]
  [1 1 1 2]
  [1 1 1 1 2 1]
  [5 2 1]
  [1 7 3 2]
  [5 3 2 2]
  [8 1 3]],
 :col-constraints
 [[2 3 1 3]
  [1 4 1 3]
  [3 1 6 7]
  [5 5 5]
  [2 4 15]
  [4 9 2 2]
  [4 3 4 5]
  [4 3 2 3]
  [4 4 1 4 2]
  [3 3 3]
  [3 3 7]
  [5 2 1 5]
  [1 1 4 2]
  [4 4]
  [1 8 1]
  [7 2]
  [2 5 3 1]
  [3 5 3]
  [3 1 1 1 1 6]
  [3 1 1 1 1 4]
  [5 4 2 1]
  [12 3 1]
  [6 1 3]
  [12 1 4]
  [6 3 2]]})
