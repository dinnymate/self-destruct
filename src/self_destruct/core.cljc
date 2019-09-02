(ns self-destruct.core)

(defn destruct [disentanglers bindings]
  (into []
    (apply concat
      (loop [bindings (partition 2 bindings)]
        (let [destructured
              (apply concat
                (for [[to from] bindings]
                  (if (simple-symbol? to)
                    (list [to from])
                    (let [from-sym (gensym "from__")]
                      (cons
                        [from-sym from]
                        (loop [[disentangler & disentanglers] disentanglers]
                          (let [res (disentangler to from-sym)]
                            (cond
                              (not (nil? res))
                              res
                              (empty? disentanglers)
                              (throw (ex-info "Could not find suitable disentangler" {}))
                              :else
                              (recur disentanglers)))))))))]
          (if (every? #(simple-symbol? (first %)) destructured)
            destructured
            (recur destructured)))))))


(defn associative-destructure [to from]
  (if (map? to)
    (let [default (get to :or {})
          mappings
          (reduce-kv
            (fn [a k v]
              (cond
                (#{:as :or :&} k) a
                (and (keyword? k) (= (keyword (name k)) :keys))
                (concat a (mapv #(vector % (keyword (namespace k) (name %))) v))

                (and (keyword? k) (= (keyword (name k)) :syms))
                (concat a (mapv #(vector % (symbol (namespace k) (name %))) v))

                (= k :strs) (concat a (mapv #(vector % (str (name %))) v))
                :else (conj a [k v]))) [] (dissoc to :or))

          bindings (mapv (fn [[k v]] [k `(~from ~v ~@`(~(default k)))]) mappings)]
      (->> bindings
           (#(if (contains? to :as) (conj % [(:as to) from]) %))))
    nil))

(defn associative-destructure-rest [to from]
  (if (map? to)
    (let [default (get to :or {})
          mappings
          (reduce-kv
            (fn [a k v]
              (cond
                (#{:as :or :&} k) a
                (and (keyword? k) (= (keyword (name k)) :keys))
                (concat a (mapv #(vector % (keyword (namespace k) (name %))) v))

                (and (keyword? k) (= (keyword (name k)) :syms))
                (concat a (mapv #(vector % (symbol (namespace k) (name %))) v))

                (= k :strs) (concat a (mapv #(vector % (str (name %))) v))
                :else (conj a [k v]))) [] (dissoc to :or))

          rest `(dissoc ~from ~@(map second mappings))
          bindings (mapv (fn [[k v]] [k `(~from ~v ~@`(~(default k)))]) mappings)]
      (->> bindings
           (#(if (contains? to :as) (conj % [(:as to) from]) %))
           (#(if (contains? to :&) (conj % [(:& to) rest]) %))))
    nil))


(defn sequential-destructure [to from]
  (if (vector? to)
    (let [has-as (some #(= :as %) to)
          has-rest (some #(= '& %) to)
          length (+ (count to) (if has-rest -2 0) (if has-as -2 0))]
      (concat
        (mapv
          (fn [k v]
            [k `(~from ~v)])
          (take length to) (iterate inc 0))
        (if has-rest
          [(to (+ length 1)) `(into [] (drop ~length ~from))] [])
        (if has-as
          [(to (+ length (if has-rest 2 0) 1)) from] [])))
    nil))
