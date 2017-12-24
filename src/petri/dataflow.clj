(ns petri.dataflow
  "Dataflow petri nets with markings as value bags.")

(defn enabled? [transition markings]
  (every? #(contains? markings %)
          (:inputs transition)))

(defn add-mark [markings place value]
  (update markings place (fnil conj []) value))

(defn use-mark [markings place]
  (let [values (markings place)
        _ (assert (seq values))
        value (peek values)
        values (pop values)]
    {:markings (if (seq values)
                 (assoc markings place values)
                 (dissoc markings place))
     :value value}))

(defn use-marks [markings places]
  (reduce (fn [{:keys [markings values]} place]
            (let [{:keys [markings value]} (use-mark markings place)]
              {:markings markings
               :values (assoc values place value)}))
          {:markings markings
           :values {}}
          places))

(defn exec [markings [op & args]]
  (case op
    :output (let [[place value] args]
              (add-mark markings place value))))

(defn fire [transition markings]
  (let [{:keys [markings values]} (use-marks markings (:inputs transition))
        _ (println "firing" (:label transition) values)
        effects ((:exec transition) values)]
    (reduce exec markings effects)))

(defn step [transitions markings]
  (reduce (fn [markings transition]
            (if (enabled? transition markings)
              (fire transition markings)
              markings))
          markings
          (shuffle transitions)))

(defn run [transitions markings]
  (loop [markings markings]
    (let [markings* (step transitions markings)]
      (if (= markings markings*)
        markings
        (recur markings*)))))

(defn evaluate [transitions markings place]
  (-> (run transitions markings) (get place) first))

(comment

  (require 'fipp.repl)
  (require '[fipp.edn :refer [pprint]])

  (-> {}
      (add-mark :x 1))

  (= (let [a 5
           b 10]
       (/ (+ a b)
          (- a b)))
     (evaluate
       [{:label :a+b
         :inputs #{:a :b}
         :exec (fn [{:keys [a b]}]
                 [[:output :a+b (+ a b)]])}
        {:label :a-b
         :inputs #{:a :b}
         :exec (fn [{:keys [a b]}]
                 [[:output :a-b (- a b)]])}
        {:label :a/b
         :inputs #{:a+b :a-b}
         :exec (fn [{:keys [a+b a-b]}]
                 [[:output :a/b (/ a+b a-b)]])}]
       ;; Each is used twice, so must supply twice.
       {:a [5 5]
        :b [10 10]}
       :a/b))

)
