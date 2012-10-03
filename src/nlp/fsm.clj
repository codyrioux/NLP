(ns 'nlp.fsm)

(defn recognize-dfa
  "A deterministic finite state automaton recognizer.
  
  NOTE: nil is used as the error state and should not be used
  as a state in your machines.
  
  machine is a map of maps which maps states to their transitions
  based on input.
  ex. {:state1 {:input1 :state2, :input2 :state3}}
  
  initial-state is a single starting state.
  
  final-states is a collection of accepting states.
  
  tape is an ordered collection of input symbols."
  [machine initial-state final-states tape]
  (contains? (set final-states)
             (reduce
               (fn [state input]
                 (if (input (state machine))
                   (input (state machine))
                   nil))
               initial-state
               tape)))
