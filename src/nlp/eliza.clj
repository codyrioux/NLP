(ns nlp.eliza
  (:require [clojure.string :as str])
  (:gen-class))

(def rules {#"(?i:^hi)" "Hello!"
            #"i feel" "Do you often feel that way?"
            #"i was" "Were you really?"
            #"no" "Are you just saying no to be negative?"
            #"i felt" "Tell me about your feelings."
            #"all" "In what way?"
            #"always" "Can you think of a specific example?"
            })

(defn find-matching-rule [rule, input]
  "If the input matches the rule regex, return the rule response."
  (if (re-find (first rule) input) (second rule)))

(defn use-eliza-rules [input]
  (some #(find-matching-rule % input) rules))

(defn eliza []
  "Starts an ELIZA prompt. One can respond simply by typing in
  queries directly. Do not type quotes or anything around your
  input."
  (while true
    (print "Eliza> ")
    (flush)
    (println (use-eliza-rules (read-line)))))
