(ns hexlet.fsm)

;; Hexlet FSM course studies
(def in "\n\n  hi   \n
  hey how are you doing?\n
  hello who are you")

(def out ["hi" "hey" "hello"])

(declare solution)
(defn test [] (= out (solution in)))

(defn solution [text]
  (second
   (reduce (fn [[state out word :as acc] ch]
             (case state
               :before (case ch
                         \space acc
                         \newline acc
                         [:at out (str word ch)])
               :at (case ch
                     \space [:after (conj out word) nil]
                     \newline [:before (conj out word) nil]
                     [:at out (str word ch)])
               :after (case ch
                        \space acc
                        \newline [:before out nil]
                        acc)))
           [:before [] nil]
           text)))

(defn capitalize-words [chars]
  (:out
   (reduce (fn [{:keys [state out] :as acc} el]
             (case state
               :outside
               (if (= \space el)
                 {:state :outside
                  :out (str out el)}
                 {:state :inside
                  :out (str out (java.lang.Character/toUpperCase el))})
               :inside
               (if (= \space el)
                 {:state :outside
                  :out (str out el)}
                 {:state :inside
                  :out (str out el)})))
           {:state :outside
            :out ""}
           chars)))

(defn capitalize-words2
  ([chars] (capitalize-words2 {:state :outside :out ""} chars))
  ([{:keys [state out]} [fchar & rchars]]
   (if fchar
     (recur
      (case state
        :outside
        (if (= \space fchar)
          {:state :outside
           :out (str out fchar)}
          {:state :inside
           :out (str out (java.lang.Character/toUpperCase fchar))})
        :inside
        (if (= \space fchar)
          {:state :outside
           :out (str out fchar)}
          {:state :inside
           :out (str out fchar)}))
      rchars)
     out)))


