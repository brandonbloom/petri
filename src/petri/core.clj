(ns petri.core)

(defn enabled? [transition markings]
  (every? (fn [[place marks]]
            (>= (markings place 0) marks))
          (:in transition)))

(defn adjust-markings [markings xs f]
  (reduce-kv (fn [markings place x]
               (let [y (f (markings place 0) x)]
                 (if (zero? y)
                   (dissoc markings place)
                   (assoc markings place y))))
             markings, xs))

(defn fire [transition markings]
  (println "firing" (:label transition transition))
  (when-let [f (:effect transition)]
    (f))
  (-> markings
      (adjust-markings (:in transition) -)
      (adjust-markings (:out transition) +)))

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

(comment

  (require 'fipp.repl)
  (require '[fipp.edn :refer [pprint]])

  ;; Chemical reaction.
  (= (run #{{:label "reaction"
             :in {:H2 2
                  :O2 1}
             :out {:H2O 2}}}
          {:H2 2
           :O2 2})
     {:O2 1
      :H2O 2})

  ;; Candy state machine.
  (run #{{:in {[:state 0] 1} :out {[:state 5] 1}}
         {:in {[:state 0] 1} :out {[:state 10] 1}}
         {:in {[:state 5] 1} :out {[:state 10] 1}}
         {:in {[:state 5] 1} :out {[:state 15] 1}}
         {:in {[:state 10] 1} :out {[:state 15] 1}}
         {:in {[:state 10] 1} :out {[:state 20] 1}}
         {:in {[:state 15] 1} :out {[:state 20] 1}}
         ;; Intentionally omit returning to [:state 0] to avoid inf loop.
         {:in {[:state 20] 1} :effect #(println "get 15 cent candy!")}
         {:in {[:state 15] 1} :effect #(println "get 20 cent candy!")}}
       {[:state 0] 1})

)
