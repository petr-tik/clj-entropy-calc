(ns clj-entropy-calc.core)


(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))


(defn calc-shannon [in-str]
  (let [char-freqs (vals (frequencies in-str))
        strlen     (count in-str)
        char-probs (map (fn [letter-count]
                          (/ letter-count strlen))
                        char-freqs)
        char-logs (map log2 char-probs)
        char-products (map * char-logs char-probs)]
  (* -1 (reduce + char-products))))

;(calc-shannon "1223334444")

;(calc-shannon "int main(int argc, char *argv[])")

;; (to-hoffman "abc")

(defn- 
  build-tree 
  [[top & rest]]
  (if rest {:left (first top) :right (build-tree rest)} {:left (first top) :right nil}))

(defn- 
  annotate-with-paths
  [path tree]
  (if (not (char? tree)) (when tree (conj (annotate-with-paths (conj path \1) (:right tree)) (annotate-with-paths (conj path \0) (:left tree)))) [tree (apply str path)]))
  
(defn to-hoffman [text]
  (let [freq-of-chars (vec (frequencies text))
        sorted-freq-of-chars (sort (fn [a b] (> (second a) (second b))) freq-of-chars)
        tree (build-tree sorted-freq-of-chars)
        char-to-path (into {} (annotate-with-paths [] tree))]
        {:encoding (apply str (map #(get char-to-path %1) text))
         :tree tree
         :paths char-to-path}))
