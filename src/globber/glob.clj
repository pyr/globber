(ns globber.glob
  "Globbing searches in clojure."
  (:require [clojure.string :as str]))

(defn char->token
  "Our simple lexer, understands escaped characters."
  [[prev c]]
  (if (= prev \\)
    c
    (case c
      \\ nil
      \* :wc ;; wildcard
      \{ :oc ;; open curly
      \} :cc ;; close curly
      \[ :ob ;; open bracket
      \] :cb ;; close bracket
      \? :ao ;; any one char
      c)))

(defn expr->tokens
  "Looks at characters two at a time to yield a list of tokens."
  [expr]
  (->> (conj (seq expr) nil)
       (partition 2 1)
       (map char->token)
       (remove nil?)))

(defn make-class
  "Build a list of all characters between `beg` and `end`"
  [beg end]
  (let [bi (int beg)
        ei (int end)]
    (when (> bi ei)
      (throw (ex-info "invalid character class" {:begin beg :end end})))
    (for [x (range bi (inc ei))] (str (char x)))))

(defn negated-cc?
  "Is this representing a negated character class?"
  [ast]
  (and (set? ast) (:negated? (meta ast))))

(defn close-charclass
  "We're finished running through a class.
   Yield a set with some additional metadata to
   indicate whether the class is negated.

   This also handles edge-cases (zero or single element
   classes)."
  [cc nchar]
  (let [ccount (count cc)]
    (cond (and nchar (zero? ccount)) (format "[%s]" nchar)
          nchar                      (with-meta (set cc) {:negated? true})
          (zero? ccount)             "[]"
          (= 1 (count cc))           (first cc)
          :else                      (with-meta (set cc) {:negated? false}))))

(defn scan-charclass
  "Eat tokens until the character class is closed."
  [tokens]
  (let [[tokens nchar] (if (#{\! \^} (first tokens))
                         [(rest tokens) (first tokens)]
                         [tokens false])]
    (loop [tokens tokens
           charclass    []]
      (if (and (>= (count tokens) 3)
               (= (second tokens) \-))
        (recur
         (drop 3 tokens)
         (vec (concat charclass (make-class (first tokens) (nth tokens 2)))))
        (let [[c & tokens] tokens]
          (case c
            nil (throw (ex-info "invalid parser state: unclosed charclass" {}))
            :wc (recur tokens (conj charclass "*"))
            :ao (recur tokens (conj charclass "?"))
            :oc (recur tokens (conj charclass "{"))
            :cc (recur tokens (conj charclass "}"))
            :ab (recur tokens (conj charclass "{"))
            :cb [(close-charclass charclass nchar) tokens]
            (recur tokens (conj charclass (str c)))))))))

(defn ast-type
  "Determine if the ast can be unrolled to a string, can
   be exploded to a set of strings or needs to run through
   our algorithm."
  [ast]
  (cond
    (vector? ast)     (reduce max 0 (map ast-type ast))
    (negated-cc? ast) 2
    (set? ast)        (reduce max 1 (map ast-type (seq ast)))
    (= :wc ast)       2
    (= :ao ast)       2
    :else             0))

(def ast-typenames
  "Yield a keyword name for the AST type"
  {0 :string
   1 :explodable
   2 :compound})

;; These are cross-referenced.
(declare tokens->ast)
(declare scan-branches)

(defn close-branch
  "We succesfuly parsed a branch, now yield
   the best possible AST for it."
  [branch]
  (let [raw?    (fn [e] (or (char? e) (keyword? e)))
        parsed? (complement raw?)]
    (loop [tokens branch
           ast    []]
      (if-let [token (first tokens)]
        (if (raw? token)
          (let [ast-tokens (take-while raw? tokens)]
            (recur (drop-while raw? tokens)
                   (conj ast (tokens->ast ast-tokens))))
          (let [sub-asts (take-while parsed? tokens)]
            (recur (drop-while parsed? tokens) (vec (concat ast sub-asts)))))
        (if (= 1 (count ast)) (first ast) ast)))))

(defn scan-branch
  "Eat tokens until the end of a branch."
  [tokens]
  (loop [[c & tokens] tokens
         branch       []]
    (case c
      nil (throw (ex-info "invalid parser state: unclosed branch" {}))
      \,  [(close-branch branch) tokens true]
      :cc [(close-branch branch) tokens false]
      :oc (let [[sub-ast trail] (scan-branches tokens)]
            (recur trail (conj branch sub-ast)))
      (recur tokens (conj branch c)))))

(defn scan-branches
  "A tree has started, scan all its branches and yield
   the best possible AST for it."
  [tokens]
  (loop [tokens       tokens
         branches     []]
    (let [[branch tokens more?] (scan-branch tokens)
          bcount                (count branches)]
      (if more?
        (recur tokens (conj branches branch))
        (cond
          (and (= 0 bcount) (empty? branch) (not more?))
          ["{}" tokens]

          (and (= 0 bcount) (not more?))
          [branch tokens]

          (every? empty? (conj branches branch))
          [(format "{%s}" (reduce str (repeat bcount ","))) tokens]
          :else
          [(set (conj branches branch)) tokens])))))

(defn tokens->ast
  "Transform a list of tokens to an AST, if possible."
  [tokens]
  (loop [tokens tokens
         stack  []]
    (if (seq tokens)
      (if-let [s (seq (take-while char? tokens))]
        (recur (seq (drop-while char? tokens))
               (conj stack (apply str s)))
        (case (first tokens)
          :ob (let [[charclass trail] (scan-charclass (rest tokens))]
                (recur trail (conj stack charclass)))
          :ao (recur (rest tokens) (conj stack :ao))
          :wc (recur (rest tokens) (conj stack :wc))
          :oc (let [[branches trail] (scan-branches (rest tokens))]
                (recur trail (conj stack branches)))
          :cb (throw (ex-info "invalid parser state: dangling bracket" {:tokens tokens}))
          :cc (throw (ex-info "invalid parser state: dangling curly" {:tokens tokens}))
          (throw (ex-info "invalid parser state: unhandled char" {:tokens tokens}))))
      (if (= 1 (count stack)) (first stack) stack))))

(defn merge-ast
  "Merge two non-compound ASTs by yielding all possible combinations."
  ([]
   [])
  ([left right]
   (let [factor (* (count left) (count right))]
     (cond (empty? left)  right
           (empty? right) left
           :else          (for [l left r right] (str l r))))))

(defn explode-ast
  "Explode a non-compound AST to all its possible combinations."
  [ast]
  (cond (string? ast)     (list ast)
        (negated-cc? ast) (throw (ex-info "cannot explode negated class" {}))
        (set? ast)        (mapcat explode-ast ast)
        (sequential? ast) (reduce merge-ast (map explode-ast ast))))

(defn stringify-ast
  "Reduce a string-only AST to its equivalent string"
  [ast]
  (if (sequential? ast)
    (reduce str (map stringify-ast ast))
    ast))

(defn partition-compound-ast
  "Partition a compound AST into a list of (eaters, ast) tuples where
   eaters is a list of eager character eaters and a potentially explodable
   AST"
  [tokens]
  (loop [eaters []
         ast []
         partitions []
         [token & tokens] tokens]
    (cond
      (nil? token)        (if (or (seq eaters) (seq ast))
                            (conj partitions [(seq eaters) (seq ast)])
                            partitions)
      (negated-cc? token) (if (seq ast)
                            (recur []
                                   []
                                   (-> partitions
                                       (conj [(seq eaters) ast])
                                       (conj [nil token]))
                                   tokens)
                            (recur []
                                   []
                                   (conj partitions [(seq eaters) token])
                                   tokens))
      (#{:ao :wc} token)  (if (seq ast)
                            (recur [token]
                                   []
                                   (conj partitions [(seq eaters) (seq ast)])
                                   tokens)
                            (recur (conj eaters token)
                                   ast
                                   partitions
                                   tokens))
      :else               (recur eaters (conj ast token) partitions tokens))))

(defn explode-compound-ast
  "Transform a partitioned compound AST by exploding eligible sub-ASTs"
  [partitions]
  (vec
   (for [[eaters ast] partitions]
     [eaters (if (negated-cc? ast) ast (explode-ast ast))])))

(defn all-indices
  "Find all indices of a substring in a given string"
  [s pat]
  (loop [res nil
         i   (.lastIndexOf (str s) (str pat))]
    (cond (neg? i)  res
          (zero? i) (conj res i)
          :else     (recur (conj res i)
                           (.lastIndexOf (str s) (str pat) (long (dec i)))))))

(defn find-pattern
  "Match a string against a list of eaters and a pattern"
  [pos candidate s eaters pattern]
  (let [wc?     (some #{:wc} eaters)
        minpos  (count (filter #{:ao} eaters))]
    (for [match (all-indices s pattern)
          :when ((if wc? >= =) match minpos)]
      [(+ match pos (count pattern))
       candidate
       (= (count candidate)
          (+ match pos (count pattern)))])))

(defn filter-compound-partition
  "Match a list of candidates against a single partition of
   a compound AST. When a match occurs, yield a list of
   the new candidates this may have generated and if they
   are eligible as a terminal match."
  [[eaters patterns] [pos candidate]]
  (let [s      (.substring (str candidate) (str pos))
        minpos (if eaters (count (filter #{:ao} eaters)) 0)
        wc?    (boolean (seq (filter #{:wc} eaters)))
        exact? (fn [i] (= i (count candidate)))]
    (cond
      (and (nil? eaters) (negated-cc? patterns))
      (when-not ((set patterns) (str (first s)))
        [[(inc pos) candidate (exact? (inc pos))]])

      (negated-cc? patterns)
      (let [comp (if wc? >= =)]
        (for [[i c] (map-indexed vector (map str (seq s)))
              :when (and (not ((set patterns) c))
                         (comp i minpos))]
          [(+ pos 1 i) candidate (exact? (+ pos 1 i))]))

      (nil? eaters) ;; first partition
      (for [prefix patterns
            :let [pcount (count prefix)]
            :when (.startsWith s prefix)]
        [(+ pos pcount) candidate (exact? (+ pos pcount))])

      (nil? patterns) ;; last partition
      (when ((if wc? >= =) (count s) minpos)
        [[(+ pos (if wc? (count s) minpos))
          candidate
          (if wc?
            true
            (exact? (+ pos minpos)))]])

      :else
      (->> patterns
           (mapcat (partial find-pattern pos candidate s eaters))
           (remove nil?)))))

(defn filter-compound-ast
  "Given a compound AST, yield the set of matching candidates."
  [partitions candidates]
  (let [candidates (map #(vector 0 % false) candidates)]
    (loop [[p & partitions] partitions
           candidates       candidates]
      (if p
        (recur partitions
               (mapcat (partial filter-compound-partition p) candidates))
        (->> candidates
             (filter (fn [[_ _ exact?]] exact?))
             (map second)
             (set))))))
(defn glob
  "Perform globbing, matching the supplied `expression` against
   the list of supplied `candidates`.

   - `candidates` is a collection of strings and must be seqable.
   - `expression` adheres mostly to the bash notion of globbing

   Globbing syntax:

   - Any stray character is matched exactly
   - Wildcards ('*') mean any number (including zero) of arbitrary chars
   - Anyones ('?') mean a single arbitrary char
   - Character classes are enclosed in square brackets and may contain
     arbitrary list of characters to match. If a character class begins
     with a ('!') or ('^') character, the class will be negated, i.e:
     will only match characters absent from the class. Empty charclasses,
     i.e: ('[]'), ('[!]'), and ('[^]') match their representation, not
     their content.
   - Trees match any of their branches. Trees are delimited by curly
     brackets and content are separated by commas. Empty trees, i.e:
     ('{}'), ('{,}'), ('{,,}') match their representation, not their
     content.

   Examples:

       (glob \"foobar\" [\"foobar\"])  ;; => (\"foobar\")
       (glob \"fo[a-z]\" [\"foobar\"]) ;; => (\"foobar\")

"
  [expression candidates]
  (let [ast  (-> expression expr->tokens tokens->ast)
        type (get ast-typenames (ast-type ast))]
    (case type
      :string     (filter (partial = (stringify-ast ast)) candidates)
      :explodable (filter (set (explode-ast ast)) candidates)
      (let [partitions (cond
                         (sequential? ast)
                         (-> ast partition-compound-ast explode-compound-ast)

                         (#{:ao :wc} ast)
                         [[(list ast) nil]]

                         :else
                         [[nil ast]])]
        (filter-compound-ast partitions candidates)))))
