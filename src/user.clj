(ns user
  (:require [clojure.math.combinatorics :as cmb]
            [clojure.set :refer [difference]]
            [clojure.data.priority-map :refer [priority-map]]))

(def graph-with-weights '{:0 ((:3 4) (:1 1)) :1 ((:3 9) (:2 8)) :2 ((:3 6)) :3 ((:1 9) (:0 4))})

(defn kw-vertices
  "
  Generate up to n keywordized vertices

  (kw-vertices 10) => (:0 :1 :2 :3 :4 :5 :6 :7 :8 :9)
  "
  [n]
  (map (comp keyword str) (range n)))

(defn random-vector
  "Returns vector of n nodes randomly shuffled."
  [n]
  (shuffle (into [] (kw-vertices n))))

(defn add-pairs
  "
  Add random pairs to existing pairs constituting a connected graph to
  satisfy the number of edges.
  "
  [vs pairs to-add]
  (let [combs (cmb/combinations vs 2)
        full-combs (concat combs (map reverse combs))
        diff (difference (set full-combs) (set pairs))]
    (concat pairs (take to-add diff))))

(comment
  (add-pairs [:2 :1 :3 :0 :4]  '((:2 :1) (:1 :0) (:0 :2)) 10)
  ;; => ((:2 :1) (:1 :0) (:0 :2) (:2 :3)
  ;; (:0 :4) (:0 :3) (:3 :1) (:1 :3) (:3 :2) (:0 :1) (:2 :0) (:1 :4) (:4 :0)))
  )

(defn create-vertex-pairs
  "
  Given number of vertices create n//2 + 1 pairs,
  using first and last element to pair if n is odd
  "
  [n num-of-pairs]
  (if (< n 2)
    '((:0 :1))
    (let [vertices (random-vector n)
          first-el (first vertices)
          last-el (last vertices)
          simplest-connected (take num-of-pairs (partition 2 1 [first-el last-el] vertices))]
      (if (> num-of-pairs n)
        (add-pairs vertices simplest-connected (- num-of-pairs n))
        simplest-connected))))
;; filter from combinations those in pairs
(comment
  (create-vertex-pairs 5 15))
;; => ((:0 :4) (:4 :2) (:2 :1) (:1 :3) (:3 :0) (:2 :3) (:0 :3) (:3 :1) (:3 :2) (:0 :2) (:0 :1) (:2 :0) (:1 :4) (:1 :0) (:4 :0))


(defn make-graph
  "Randomly generate a simple connected graph with N vertices and S edges.
  For example:
  {:1 ((:0 6)), :0 ((:1 0) (:2 8)), :2 ((:1 0))}
  "
  [n s]
  (if (or (< n 2) (> n s)) ;; check if there are enough nodes for all edges
    nil
    (into {} (reduce
              (fn [acc pair] (update acc (first pair) conj (list (second pair) (rand-int 10))))
              {}
              (create-vertex-pairs n s))))) 
(comment
  (make-graph 3 4))
;; => {:1 ((:0 6)), :0 ((:1 0) (:2 8)), :2 ((:1 0))}

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(remove-keys {:1 2 :3 4} {:1 6})

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.
  Returns a map from nodes to their distance from start."
  [start f]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) (map-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(comment
  (def random-graph' (make-graph 4 7)))

(defn get-distance
  "Return a function which takes a node and returns map
  of node to distance from start."
  [g]
  (fn [n] (apply hash-map (flatten (n g)))))

(defn shortest-path
  "Computes shortest part in graph from start src to finish dest."
  [g src dest]
  (dest (dijkstra src (get-distance g))))

(comment
  (shortest-path random-graph' :1 :2)
  ;; => 8
  )

(defn eccentricity
  "Computes eccentricity (distance between v and farthest vertice) of a graph g for vertice v"
  [g v]
  (let [vs (keys g)]
    (second (last (sort-by val
                           (loop [vs (remove #{v} vs)
                                  distances {}]
                             (if (empty? vs)
                               distances
                               (let [el (first vs)]
                                 (recur (remove #{el} vs)
                                        (conj distances
                                              [el (shortest-path g v el)]))))))))))

(comment
  (eccentricity random-graph' :1)
  ;; => 13
  )

(defn radius
  "Computes radius (min eccentricity of all vertices) of a graph g"
  [g]
  (let [vs (keys g)]
    (apply min (map #(eccentricity g %) vs))))

(comment
  (radius random-graph')
  ;; => 9
  )

(defn diameter
  "Computes diameter (max eccentricity of all vertices) of a graph g"
  [g]
  (let [vs (keys g)]
    (apply max (map #(eccentricity g %) vs))))

(comment
  (diameter random-graph')
  ;; => 13
  )
