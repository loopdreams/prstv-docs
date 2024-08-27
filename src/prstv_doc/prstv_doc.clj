;; # PRSTV-Sim: Notebook

(ns prstv-doc.prstv-doc)

;; This is a notebook detailing some of the code behind my [link]
;; project.
;;
;; Since many of the functions in that project depend on
;; assumptions that I've made about the vote-counting system, I'm
;; detailing some of the code here in the hopes that someone will
;; correct any mistakes.
;;
;; There are two main parts of the logic for that application:
;;
;;  1. Generating 'ballots'
;;
;;  2. Counting the votes according to the single transferrable vote counting system

;; ## Generating Ballots
;; Before even starting to develop an approach to 'counting' vote,
;; I needed some way to generate them.
;;
;; This ended up being the slowest part of the system, performance
;; wise, for reasons that I'll go through below.
;;
;; ### Key Inputs
;; The main components for generating the ballots can be seen in an 'example'
;; configuration below:

(def example-config
  {:n-votes              500                     ;; Number of votes
   :candidates           #{:A :B :C :D :E :F :G} ;; List of candidate names
   :candidate-popularity {:A 25 ;; Candidate popularity (percentage)
                          :B 24
                          :C 11
                          :D nil
                          :E 10
                          :F 10
                          :G 10}
   :candidate-party      {:A 1 ;; Candidate party (number/index)
                          :B 2
                          :C 1
                          :D 3
                          :E 3
                          :F 2
                          :G 5}
   :party-popularity     {1 10 ;; Party popularity (percentage)
                          2 10
                          3 10
                          4 10
                          5 10}
   :preference-depth     :mid ;; Preference depth (:shallow :mid :deep)
   :volatility           1
   :volatility-pp        1})


;; In the application itself, all of these areas are configurable by the user.
;;
;; There were also a couple of other variables defined seperately:

(def same-party-buff 40)
(def party-popularity-weighting 15)

;; These determine how big a role the candidate's party plays in determining a vote selection.

;; ### Weighted Randomizer
;; For a lot of the 'randomization', I used a basic approach where you would have something like the following:
;; - Values: [:A :B :C] (Candidate names)
;; - Weights: [20 30 50]
;;
;; In the above case, candidate ':C' has the highest weighting.
;;
;; Below is the function that takes the values and weights and picks a random value based on these:

(defn weighted-randomizer
  "Returns a weighted random number"
  [values weights]
  (if (apply = weights) (rand-nth values) ;; If all the wights are equal, skip the next steps and just return a random value.
      (loop [[i & rest] (range (count values)) ;; Indexes of each of the values
             rnd (rand-int (apply + weights))] ;; A random number between 1 and the sum of weights (e.g., 64)
        (when i
          (if (< rnd (nth weights i)) ;; If the random number (64) is less than the weight (e.g., 20), then
            (nth values i) ;; Return the value, Else, subtract the weight from the random number (64 - 20 = 44), and check the next weight
            (recur rest (- rnd (nth weights i))))))))

;; A way to visualise this could be if the following boxes were drawn on the ground:
;;
;;```
;; |---------|--------------------------|-------|
;; |    A    |           B              |   C   |
;; |---------|--------------------------|-------|
;; ```
;;
;; If you were to toss a coin, there is a greater likelihood of it ending up on B
;;
;; ### Ballot Depth
;; The first function more specific to this application is one to determine 'Ballot Depth'.
;; This function returns a number which indicates how many preferences you should 'mark' on the ballot.
;;
;; First, the configuration of ballot-depth (:shallow :mid :deep) is used along with the weighted randomizer
;; to determine a range, then a random number is picked from that range.
;;
;; For example, the the depth was :mid and the ballot was 10 candidates long, then we might expect that around 5
;; candidates should be voted for.
;;
;; One challenge here is in dividing up the list of candidates into 3 parts (which would correspond to the weights, :shallow :mid :deep).
;;
;; Clojure has some useful partitioning functions, but they didn't really work for my case.
;;
;; For example, first get the ideal partition size with some math. In the case of the example
;; configuration above, there are *7 candidates*.

(let [n-candidates 7]
  (Math/round (double (/ n-candidates 3))))

;; Next, let's try partition-all to divide up the list according to this size:

(partition-all 2 (range 1 8))

;; As you can see, there is a 'remainder' here (7) that doesn't get included in the groups.
;;
;; A useful thing I learned here is that you can make 'overlapping' partitions with clojure:

(partition 3 2 (range 1 8))

;; Something like this is closer to what I was looking for, but still not quite right since some
;; numbers would be more common than others.
;;
;; So, in the end I just wrote my own simple function:

(defn my-partition-all [coll n]
  (loop [coll coll
         partitioned []]
   (cond
     (= 3 (count partitioned)) partitioned
     (= 2 (count partitioned)) (conj partitioned coll)
     :else (recur
            (into [] (drop n coll))
            (conj partitioned (into [] (take n coll)))))))

;; I'm sure this could be improved, but it does the job:

(my-partition-all (range 1 8) 2)

;; My thinking here in terms of having the remainder at the 'end' of the list, was that it would be less likely
;; for people to vote for _all_ of the candidates as opposed to just _one_. Having it at the end of the list (in a group of 3
;; in this case) makes it less likely to get chosen.
;;
;; With that in place, the function for generating the random ballot depth was as follows:

(defn ballot-depth
  "Returns the number of candidates to assign preferences to."
  [{:keys [candidates preference-depth]}]
  (let [n-candidates   (count candidates)
        partition-size (Math/round (double (/ n-candidates 3)))
        c-batches      (my-partition-all (range 1 (inc n-candidates)) partition-size)
        weights        (case preference-depth
                         :deep    [5 10 50]
                         :mid     [5 50 5]
                         :shallow [50 10 5]
                         [(rand-int 10) (rand-int 10) (rand-int 10)])] ;; If no preference set, just assign random weightings
    (-> c-batches
        (weighted-randomizer weights)
        (rand-nth))))

;; Testing it out:
(take 10 (repeatedly #(ballot-depth example-config)))

;; With the :deep preference:
(let [config (assoc example-config :preference-depth :deep)]
  (take 10 (repeatedly #(ballot-depth config))))

;; With no preference (random)
(let [config (assoc example-config :preference-depth nil)]
  (take 10 (repeatedly #(ballot-depth config))))

;; Looking at the averages:

(defn average [coll]
  (Math/round (double (/ (reduce + coll) (count coll)))))

(average (take 100 (repeatedly #(ballot-depth example-config))))

(average (take 100 (repeatedly #(ballot-depth (assoc example-config :preference-depth :deep)))))

(average (take 100 (repeatedly #(ballot-depth (assoc example-config :preference-depth :shallow)))))

;; Or, with a longer list of candidates:

(def test-candidate-list {:candidates (take 21 (repeat :candidate))})


(average (take 100 (repeatedly #(ballot-depth (assoc test-candidate-list :preference-depth :mid)))))

(average (take 100 (repeatedly #(ballot-depth (assoc test-candidate-list :preference-depth :deep)))))

(average (take 100 (repeatedly #(ballot-depth (assoc test-candidate-list :preference-depth :shallow)))))

;; I'm not a data scientist, so I'm not sure what the optimum results are here. The point
;; was to preserve some randomness, but have the selections just trend in a general direction.

;; There could be a case made to remove the 'preference' for how many candidates will be voted for all together and just
;; return a random number.
;;
;; However my thinking here was that a populace may have a certain tendancy on average toward
;; filling out more of the ballot. For example, in an important general election, people might want to vote for more
;; candidates, but perhaps in a local election, candidates may be less well-known and therefore people wouldn't vote
;; for as many.
