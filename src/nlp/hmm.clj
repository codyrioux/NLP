(ns nlp.hmm)

;; Some data structures to assist in testing and development.
(def example-state-graph {:start {:hot 0.8, :cold 0.2}
                          :hot {:hot 0.7, :cold 0.3}
                          :cold {:hot 0.4, :cold 0.6}})
(def example-observation-likelihoods {:hot {1 0.2, 2 0.4, 3 0.4}
                                      :cold {1 0.5, 2 0.4, 3 0.1}})

(defn b [o, j, observation-likelihoods]
  "The state observation likelihood of the observation symbol o given
  the current state j."
 ((j observation-likelihoods) o))

(defn a [i, j, transition-table]
  "The transition probability from previous state i to current state j."
  ((i transition-table) j))

(defn get-previous-states [state state-graph]
 "Returns a list of the previous states except for :start."
  (remove #{:start} (keys (select-keys state-graph (for [[k v] state-graph :when (v state)] k)))))

;; This alpha function is a disaster, needs to be rewritten in a mapreduce fashion
;; see the forward function further down for an example.
;;
;; Some notes about this function:
;; - The t value could be derived from (count observations)
;; - It should use recur to avoid a stack overflow on long sequences.
(defn alpha [t, state, observations, state-graph, observation-likelihoods]
  "Computes the probablity of being in state after the first t observations.
  Observations must be a vector of observations in order of observation sequence."
  (cond (= 1 t) (* (a :start state state-graph) (b (last observations) state observation-likelihoods))
        :else (reduce #(+ % (* (alpha (dec t) %2 (pop observations) state-graph, observation-likelihoods)
                            (a %2 state state-graph)
                            (b (last observations) state observation-likelihoods)))
                      0
                      (get-previous-states state state-graph))))

(def alpha (memoize alpha))

;; Forward Algorithm
;;
;; Determines the likelihood of an observation sequence.
;;
;; This algorithm makes the assumption that it is possible to end in all states.
(defn forward [observations state-graph likelihoods]
  "An implementation of the forward algorithm for computing HMM probability.
   Returns the observation probability of the given observation sequence."
  (reduce + 0 (map #(alpha (count observations) % observations state-graph likelihoods)
                   (remove #{:start} (keys state-graph)))))

;; Helper Functions for Viterbi
(defn most-likely-tuple [x y] (if (> (second x) (second y)) x y))
(defn ab [prev state obs state-graph likelihoods] (* (a prev state state-graph) (b obs state likelihoods)))

;; Viterbi Algorithm
;;
;; Determines the best hidden sequence.
;;
;; Some Notes on this function:
;; - The only difference between the two conds are that v = 1 in the first one
;; - This function should use recur to avoid a stack overflow on long sequences
(defn viterbi [obs state-graph likelihoods]
  "Some docs..."
  (cond (= (count obs) 1) (reduce most-likely-tuple
                        (map #(vector [%] (ab :start % (last obs) state-graph likelihoods))
                             (keys (:start state-graph))))
        :else (let [vval (viterbi (pop obs) state-graph likelihoods)]
                (reduce most-likely-tuple 
                        (map #(vector 
                                (conj (first vval) %) 
                                (* (second vval) 
                                   (ab :hot % (last obs) state-graph likelihoods)))
                             (remove #{:start} (keys state-graph)))))))
