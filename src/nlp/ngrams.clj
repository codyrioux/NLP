(ns nlp.ngrams
  (:require [clojure.string :as str]))

(defn tokenize [input] (str/split input #" |\n"))

(defn ngrams [n coll]
  "Computes a map with the ngram as the key, and the frequency as the value."
  (frequencies (partition n 1 coll)))

(defn ngrams-to-prob-dist [coll]
  "Computes a probability distribution for the passed in ngram collection."
  (let [x (reduce + (vals coll))]
    (map #(list (first %) (/ (second %) x)) coll)))

(defn get-ngrams-matching [target ngrams]
  "Retuns a list of ngrams that begin with the list denoted by target.
  If target is empty then it will return all ngrams, as they all begin
  with empty."
  (if (= 0 (count target)) ngrams 
  (filter #(= (take (count target) (first %)) target) ngrams)))

(defn get-random-next-word [ngrams words]
  "Gets a randomized next word based on the passed in word and ngram set.
  If no ngram matches the words leading into it, it will select a completely random word."
  (let [candidate-words (seq (get-ngrams-matching words ngrams))]
    (if (= 0 (count candidate-words)) (first (first (rand-nth (seq ngrams))))
    (nth (first (rand-nth candidate-words)) (count words)))))

(defn generate-random-sentence
  "Generates a random sentence of length sentence-length using the supplied ngrams."
  ([sentence-length ngrams] (generate-random-sentence sentence-length ngrams []))
  ([sentence-length ngrams sentence]
  (if (= 0 sentence-length) (str/join " " sentence)
  (let [n  (count (first (first ngrams)))]
    (generate-random-sentence (dec sentence-length) ngrams (conj sentence
    (get-random-next-word ngrams (take-last (dec n) sentence))))))))
