(ns nlp.grammar
  "The grammar namespace currently contains a public function for converting
  Context-Free Grammars to Chomsky Normal Form. This namespace has some outstanding
  candidates for refactoring.
  
  Consider this a note for myself to refactor some of the code in this namespace
  when I get a chance to spend some time improving existing code (Christmas Break)
  There are a few ideal candidates for macros here, as well as perhaps reducing
  the number of functions.")

;;
;; Test Data
;;

(def l1-grammar { :S #{[:NP :VP] [:Aux :NP :VP] [:VP]}
                  :NP #{[:Pronoun] [:Proper-Noun] [:Det :Nominal]}
                  :Nominal #{[:Noun] [:Nominal :Noun] [:Nominal :PP]}
                  :VP #{[:Verb] [:Verb :NP] [:Verb :NP :PP] [:Verb :PP] [:VP :PP]}
                  :PP #{[:Preposition :NP]}
                  :Det #{["that"] ["this"] ["a"]}
                  :Noun #{["book"] ["flight"] ["meal"] ["money"]}
                  :Verb #{["book"] ["include"] ["prefer"]}
                  :Pronoun #{["I"] ["she"] ["me"]}
                  :Proper-Noun #{["Houston"] ["NWA"]}
                  :Aux #{["does"]}
                  :Preposition #{["from"] ["to"] ["on"] ["near"] ["through"]}
                 })

;;
;; Helper Functions
;;

(defn- map-fn-to-map-val [m f]
  "Returns a new map containing {k f(v)} for each key value pair.
   There seems to be some issue with this function that I cannot pinpoint."
  (into {} (for [[k v] m] [k (f v)])))

(defn filter-map [f m]
  (select-keys m (for [[k v] m :when (f v)] k)))

(defn replace-start-state [grammar start new-start]
  "Implements the first step of the conversion to CNF:
  Add S0 and S0 -> S where s is the old start state."
  (assoc grammar new-start #{[start]}))

;;
;; epsilon Rule Functions
;;

(defn- epsilon-rule? [rule]
  "Determines if the rule can generate an empty string."
  (contains? rule []))

(defn- remove-epsilon-permutations-from-rule [epsilon rule]
    (filter (fn [item] (not= [nil] item)) (reduce (fn [rules arg]
              (let [new-rules (map (fn [x] (conj x arg)) rules)] 
                (if (some #{arg} epsilon)
                  (concat rules new-rules)
                  new-rules)))
            (if (some #{(first rule)} epsilon) [[(first rule)] []] [[(first rule)]])
            (rest rule))))

(defn- eliminate-epsilon-rules [grammar]
 "Eliminate all rules that generate an empty string."
 (into {} (let [epsilon-rules (keep #(if (epsilon-rule? (get % 1)) % nil) grammar)
       perm-generator (partial remove-epsilon-permutations-from-rule (keys epsilon-rules))]
   (->>
    (into {} (for [[k ruleset] grammar] [k ((fn [rule] (set (apply concat (map perm-generator rule))))  ruleset)]))
    (keep #(if (not (empty? (get % 1))) % nil))))))

;;
;; Unit Rule Functions
;;

(defn- unit-rule? [rule]
  (and (= (count rule) 1) (keyword? (first rule))))

(defn- contains-unit-rules? [ruleset]
  (some unit-rule? ruleset))

(defn- remove-unit-rules [unit-rules grammar]
  (into {} (for [[k v] grammar] [k (apply disj v (k unit-rules ))])))

(defn- merge-unit-rules [unit-rules grammar]
 (into {} (for [[k v] grammar] [k (apply concat v (map grammar (flatten (seq (k unit-rules)))))])))

(defn- eliminate-unit-rules [start grammar]
  "Eliminate all rules with one non-terminal on the right hand side.
  Assumes the input grammar is the output of eliminate-epsilon-rules."
  (let [rules-containing-unit-rules (filter-map contains-unit-rules? (dissoc grammar start))
        get-unit-rules (partial filter unit-rule?)
        unit-rules (into {} (for [[k v] rules-containing-unit-rules] [k (set (get-unit-rules v))]))]
    (->>
      (remove-unit-rules unit-rules grammar)
      (merge-unit-rules unit-rules))))

;;
;; Non Chomsky Rule Functions
;;

(defn- non-chomsky? [rule]
  (>= 3 (count rule)))

(defn- random-keyword [] (keyword (str (java.util.UUID/randomUUID))))

(defn- needs-terminals-substituted? [rule] 
  "Determines if the rule contains terminals that need to be converted to non-terminals." 
  (let [not-keyword? (comp not keyword?)]
  (and (some keyword? rule) (some not-keyword? rule)))) 

(defn- get-terminals [rule] 
  (let [not-keyword? (comp not keyword?)]
    (filter not-keyword? rule)))

(defn- get-terminals-for-ruleset [ruleset]
  (flatten (map get-terminals ruleset)))

(defn- make-terminal-map [terminals]
 (into {} (for [terminal terminals] [terminal (random-keyword)])))

(defn- invert-terminal-map [m] 
  (into {} (for [[k v] m] [v [[k]]])))

(defn- replace-terminals [terminal-map ruleset]
  (let [terminal-replacer (fn [rule] (map (fn [x] (if (keyword? x) x (terminal-map x))) rule))]
  (concat 
    (filter #(= (count %) 1) ruleset)
    (map terminal-replacer (filter #(> (count %) 1) ruleset) )
    )))

(defn- too-many-nonterminals? [rule]
 (>= (count rule) 3))

(defn generate-normalized-rules [initial-symbol rule]
  (cond
    (> (count rule) 2) (let [new-id (random-keyword)]
                         (cons {initial-symbol #{[(first rule) new-id]}}
                               (seq (generate-normalized-rules new-id (rest rule)))))
    :else (cons {initial-symbol #{rule}} nil)))

(defn cleanup-non-chomsky-rules [grammar]
  "Eliminate all rules that have over two variables on the right hand size.
  If RHS has any terminals just create a new nonterminal that generates it.
  If the RHS has > 2 nonterminals just create a new nonterminal that creates two of them, repeat..."
  (let [terminals-to-replace (set (get-terminals-for-ruleset
                                       (filter needs-terminals-substituted?
                                              (apply concat (vals grammar)))))
        terminal-map (make-terminal-map terminals-to-replace)
        terminal-replacer (partial replace-terminals terminal-map)]
    (remove-gt3-rules
      (into {} (concat (into {} (for [[k v] grammar] [k (terminal-replacer v)]))
                       (invert-terminal-map terminal-map))))))

(defn remove-gt3-rules [grammar]
  (->> 
    (for [[k v] grammar] (let [gnr (partial generate-normalized-rules k)] (map gnr v)))
    (map (partial apply merge-with concat) (map (partial apply concat)))
    (reduce (partial merge-with concat))))

(defn cfg-to-cnf [grammar start]
  "Converts an arbitrary context free grammar to Chomsky Normal Form.
  This implementation is intended to satisfy exercise 13.1 in Jurafsky text.
  
  Inputs:
    grammar - A CFG specified in a map 
  
              {:A #{[:B :C] ['a']} :B #{[:C 'b']} :C #{['c'] []}}

              in which each key is a nonterminal keyword and each value is a
              set of vectors of productions. Note that double quotes are needed
              for a real clojure data structure.
              
              The above map represents:
              A -> AB|a
              B -> Cb
              C -> c

    start - The keyword that represents S, the start state."
  (->>
    (replace-start-state grammar start :S0)
    (eliminate-epsilon-rules)
    (eliminate-unit-rules :S0)
    (cleanup-non-chomsky-rules))) 
