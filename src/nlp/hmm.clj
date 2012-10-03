(ns nlp.hmm)

;; Some data structures to assist in testing and development.
(def ex-sgraph {:start {:hot 0.8, :cold 0.2}
                :hot {:hot 0.7, :cold 0.3}
                :cold {:hot 0.4, :cold 0.6}})
(def ex-likely {:hot {1 0.2, 2 0.4, 3 0.4}
                :cold {1 0.5, 2 0.4, 3 0.1}})

(defn b [o, j, observation-likelihoods]
  "The state observation likelihood of the observation symbol o given
  the current state j."
 ((j observation-likelihoods) o))

(defn a [i, j, transition-table]
  "The transition probability from previous state i to current state j."
  ((i transition-table) j))

(defn get-parent-states [state state-graph]
 "Returns a list of the parent states except for :start."
  (remove #{:start} (keys (select-keys state-graph (for [[k v] state-graph :when (v state)] k)))))

(defn alpha [t, state, observations, state-graph, observation-likelihoods]
  "Computes the probability of being in state after the first t observations.
  Observations must be a vector of observations in order of observation sequence."
  (cond (= 1 t) (* (a :start state state-graph) (b (last observations) state observation-likelihoods))
        :else (reduce #(+ % (* (alpha (dec t) %2 (pop observations) state-graph, observation-likelihoods)
                            (a %2 state state-graph)
                            (b (last observations) state observation-likelihoods)))
                      0
                      (get-parent-states state state-graph))))

(def alpha (memoize alpha))

(defn forward [observations state-graph likelihoods]
  "An implementation of the forward algorithm for computing HMM probability.
   Returns the observation probability of the given observation sequence."
  (reduce + 0 (map #(alpha (count observations) % observations state-graph likelihoods)
                   (remove #{:start} (keys state-graph)))))

;; Helper Functions for Viterbi
(defn most-likely-tuple [x y] (if (> (second x) (second y)) x y))
(defn ab [prev state obs state-graph likelihoods] (* (a prev state state-graph) (b obs state likelihoods)))
(defn get-children [state state-graph] (keys (state state-graph)))

;; This function should use recur to avoid a stack overflow on long sequences
(defn viterbi [obs state-graph likelihoods]
  "An implementation of the Viterbi algorithm for hmm decoding.
  Returns the best hidden sequence for an observation sequence, and the likelihood
  of that sequence."
  (cond (= (count obs) 1) (reduce most-likely-tuple
                        (map #(vector [%] (ab :start % (last obs) state-graph likelihoods))
                             (keys (:start state-graph))))
        :else (let [vval (viterbi (pop obs) state-graph likelihoods)]
                (reduce most-likely-tuple 
                        (map #(vector 
                                (conj (first vval) %) 
                                (* (second vval) 
                                   (ab (last (first vval)) % (last obs) state-graph likelihoods)))
                             (get-children (last (first vval)) state-graph))))))

(def viterbi (memoize viterbi))
