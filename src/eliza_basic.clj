(ns eliza-basic)

;;; A Clojure port of Peter Norvig's simplified ELIZA implementation
;;; in Common Lisp from Paradigms of Artificial Intelligence Programming
;;; Chapter 5.
;;; (See https://github.com/norvig/paip-lisp/blob/master/docs/chapter5.md.)

;;; As a first attempt, this one pretty closely follows the original,
;;; with just minor modifications to make it idiomatic enough, plus some
;;; refactorings to make the algorithm easier to understand, like
;;; extracting or renaming variables here and there.
;;; Lists of symbols have been kept as the data structure for input and
;;; patterns, only the a-lists representing bindings have been replaced
;;; by maps.


;;; 5.2

(defn variable? [x]
  "Return true if `x` is a symbol starting with '?'."
  (and (symbol? x) (= (first (name x)) \?)))

;; This is just a predicate yet, so is's logical to name it accordingly.
(defn pat-match?
  "Return true if input matches pattern."
  [pattern input]
  (if (variable? pattern)
    true
    ;; In Common Lisp the atom function covered the case of the empty
    ;; input (rest-ing a one-element list), but here we need to check
    ;; that separately with the empty? predicate.
    (if (some (some-fn symbol? empty?) [pattern input])
      (= pattern input)
      (and (pat-match? (first pattern) (first input))
           (pat-match? (rest pattern) (rest input))))))

(defn match-variable
  "Match a variable against input."
  [variable input bindings]
  (cond
    (not (contains? bindings variable)) (assoc bindings variable input)
    (= input (get bindings variable)) bindings
    ; Since here pat-match returns {} in case of a successful match
    ; with no bindings, there's no problem with nil indicating failure.
    :else nil))

(defn pat-match
  "Return possibly updated bindings if input matches pattern."
  ([pattern input]
   (pat-match pattern input {}))
  ([pattern input bindings]
   (cond
     (nil? bindings) nil
     (variable? pattern) (match-variable pattern input bindings)
     (= pattern input) bindings
     ; Note the use of sequential? instead of seq? for flexibility
     ; (possible to switch lists to vectors).
     (and (sequential? pattern) (sequential? input))
     (->> (pat-match (first pattern) (first input) bindings)
          (pat-match (rest pattern) (rest input)))
     :else nil)))


;;; 5.3

(defn segment-pattern
  "Return true if pattern is of the form ((?* var) rest-of-pattern)."
  [pattern]
  (and (sequential? pattern)
       (sequential? (first pattern))
       (= (ffirst pattern) '?*)))

(defn segment-match
  "Match a segment pattern against input."
  ([pattern input bindings]
   (segment-match pattern input bindings 0))
  ([pattern input bindings start]
   (let [segment-var (second (first pattern))
         rest-of-pattern (rest pattern)]
     (if (empty? rest-of-pattern)
       (match-variable segment-var input bindings)
       (let [next-pattern-elem (first rest-of-pattern)]
         ; TODO: simpler/idiomatic solution for getting pos?
         (when-let [pos (->> (drop start input)
                             (take-while #(not= next-pattern-elem %))
                             count
                             (+ start)
                             (#(when (< % (count input)) %)))]
           (let [segment-candidate (take pos input)
                 rest-of-input (drop pos input)
                 bindings-updated-with-segment
                 (match-variable segment-var segment-candidate bindings)]
             (if-let [new-bindings (pat-match rest-of-pattern rest-of-input
                                              bindings-updated-with-segment)]
               new-bindings
               (segment-match pattern input bindings (inc pos))))))))))

;; Updated with segment matching.
(defn pat-match
  "Return possibly updated bindings if input matches pattern."
  ([pattern input]
   (pat-match pattern input {}))
  ([pattern input bindings]
   (cond
     (nil? bindings) nil
     (variable? pattern) (match-variable pattern input bindings)
     (= pattern input) bindings
     (segment-pattern pattern) (segment-match pattern input bindings)  ; ***
     (and (sequential? pattern) (sequential? input))
     (->> (pat-match (first pattern) (first input) bindings)
          (pat-match (rest pattern) (rest input)))
     :else nil)))


;;; 5.4

(def eliza-rules
  [['((?* ?X) hello (?* ?y))
    '(How do you do. Please state your problem.)]
   ['((?* ?X) I want (?* ?y))
    '(What would it mean if you got ?y)
    '(Why do  you want ?y)
    '(Suppose you got ?y soon)]
   ['((?* ?X) if (?* ?y))
    '(Do you really think its likely that ?y)
    '(Do you wish that ?y)
    '(What do you think about ?y)
    '(Really-- if ?y)]
   ['((?* ?X) no (?* ?y))
    '(Why not?)
    '(You are being a bit negative)
    '(Are you saying "NO" just to be negative?)]
   ['((?* ?X) I was (?* ?y))
    '(Were you really?)
    '(Perhaps I already knew you were ?y)
    '(Why do you tell me you were ?y now?)]
   ['((?* ?X) I feel (?* ?y))
    '(Do you often feel ?y ?)]
   ['((?* ?X) I felt (?* ?y))
    '(What other feelings do you have?)]])


(defn pattern [rule] (first rule))
(defn response-templates [rule] (rest rule))

;; TODO: Handle case sensitivity
(defn switched-viewpoints
  "Change I to you and vice versa, and so on."
  [bindings]
  (let [subst-map {'I 'you, 'you 'I, 'me 'you, 'am 'are}
        substitute #(get subst-map % %)]
    (zipmap (keys bindings)
            (map (fn [binding-val]
                     (if (sequential? binding-val)  ; i.e., a segment
                       (map substitute binding-val)
                       (substitute binding-val)))
                 (vals bindings)))))

(defn use-rules
  "Return input transformed according to some appropriate rule."
  [input rules]
  (some (fn [rule]
          (when-let [bindings (pat-match (pattern rule) input)]
            (flatten
              (replace (switched-viewpoints bindings)
                       (rand-nth (response-templates rule))))))
        rules))

(defn eliza
  "Respond to user input using pattern matching rules."
  []
  (print "ELIZA> ")
  (flush)
  (let [input (read)]
    (when (not= input 'quit)
      (println (use-rules input eliza-rules))
      (recur))))
