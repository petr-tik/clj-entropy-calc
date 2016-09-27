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

(defn to-hoffman [text]
  (let [x (vec (frequencies text))
        y (sort (fn [a b] (> (second a) (second b))) x)
        step (fn [[top & rest]] (if rest {:left (first top) :right (step rest)} {:left (first top) :right nil}))
        z (step y)
        walk (fn [path tree] (if (not (char? tree)) (when tree (conj (walk (conj path \1) (:right tree)) (walk (conj path \0) (:left tree)))) [tree (apply str path)]))
        code (into {} (walk [] z))]
        {:encoding (apply str (map #(get code %1) text))
         :tree z
         :paths code}))
