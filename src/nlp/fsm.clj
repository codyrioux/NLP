(ns 'nlp.fsm)

; A deterministic finite automaton recognizer.
; 
; nil is used as the error state and should not be a state
; in the provided machine.
;
; machine is a map of maps where the keys to the first map
; are states, the second map maps input values on the tape
; to new states.
;
; initial-state is a symbol representing the start state
;
; final-states is a collection of 'accepting' final states
;
; tape is a collection of input symbols
;
(defn recognize-dfa
  [machine initial-state final-states tape]
  (contains? (set final-states)
             (reduce
               (fn [state input]
                 (if (input (state machine))
                   (input (state machine))
                   nil))
               initial-state
               tape)))
