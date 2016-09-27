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
