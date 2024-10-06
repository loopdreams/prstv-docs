;; # PRSTV-Sim: Notebook

(ns prstv-doc.prstv-doc)

;; This is a notebook detailing some of the code behind my
;; [single transferrable vote](https://eoin.site/prstv) webapp.
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
;; Before even starting to develop an approach to 'counting' the votes,
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
                          :D 14
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
   :volatility           10
   :volatility-pp        10})


;; In the application itself, all of these areas are configurable by the user.
;;
;; There were also a couple of other variables defined seperately:

(def same-party-buff 40)
(def party-popularity-weighting 15)

;; These determine how big a role the candidate's party plays in determining a vote selection.
;; The same-party-buff adds 40(!) percentage points to a candidates popularity if another member
;; from their party has already been selected on the ballot. This seems pretty high, but the point
;; here is that this kind of voting system is very party-driven.

;; ### Weighted Randomizer
;; For a lot of the 'randomization', I used a basic approach where you would have something like the following:
;;
;; - Candidate Names: [:A :B :C]
;;
;; - Candidate Weights: [20 30 50]
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
;; This function returns a number which indicates how many preferences will be 'marked' on
;; a single ballot.
;;
;; First, the configuration of ballot-depth (:shallow :mid :deep) is used along with the weighted randomizer
;; to determine a range, then a random number is picked from that range.
;;
;; For example, if the depth was set to :mid and the ballot was 10 candidates long, then we might expect that around 5
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

;; Looking at the averages for a 21 candidate ballot:

(defn average [coll]
  (Math/round (double (/ (reduce + coll) (count coll)))))

(def test-candidate-list {:candidates (take 21 (repeat :candidate))})

;; 'Mid' ballot depth:
(average (take 100 (repeatedly #(ballot-depth (assoc test-candidate-list :preference-depth :mid)))))

;; 'Deep' ballot depth:
(average (take 100 (repeatedly #(ballot-depth (assoc test-candidate-list :preference-depth :deep)))))

;; 'Shallow' ballot depth:
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

;; ### Volatility
;; I was writing this webapp in a time where Irish politics was quite unpredictable, so I
;; wanted to add some kind of 'volatility' variable to help simulate some of that.
;;
;; In the end, this didn't work too well. I can't really see the impacts of higher
;; volatility when applied to the votes as a whole. I would like if this could
;; be a bit more 'sensitive'.


(defn- volatility-adjust
  "Adds or subtracts % of value, based on volatility level (0-100)"
  [val volatility]
  (let [adjust (rand-nth [+ -])]
    (if (and (> volatility 100) (= adjust -))
      0
      (adjust val (* (/ volatility 100) val)))))

(defn- adjust-popularity [val vol]
  (volatility-adjust val vol))

(defn- adjust-popularity-party [val vol party-popularity-val]
  (+ val (* (volatility-adjust party-popularity-val vol)
            (/ party-popularity-weighting 100))))

;; Some sample 'adjustments' for a candidate who has a 40% popularity rating:
(int (adjust-popularity 40 10))

(int (adjust-popularity 40 30))

(int (adjust-popularity 40 50))

(int (adjust-popularity 40 70))

(int (adjust-popularity 40 90))

;; ### Party Preference
;; A key assumption here is that after a voter has picked a candidate,
;; they are more likely to pick another candidate from the same party.

;; This function just returns 'true' if another candidate from the same party has already
;; been choosen on the ballot.

(defn party-preference?
  "If another candidate from the same party has already been selected, return true."
  [party candidate-party voted-candidates]
  (when (seq voted-candidates)
    (some #{party} (map #(% candidate-party) voted-candidates))))

;; Then, this function calculates a 'weighting' for a candidate, by:
;;
;; - adjusting their popularity based on the volatility functions above
;;
;; - I also added a 'volatility-pp' value (volatility percentage points), which just adds or subtracts
;; a fixed percentage point from the popularity (again, I was trying to make the volatility adjustments more sensitive)
;;
;; - Finally, if a candidate from the same party has already been choosen, add some percentage points to the value

(defn calc-candidate-weighting [{:keys [candidate-popularity party-popularity volatility candidate-party volatility-pp]}
                                candidate
                                voted-candidates]

  (let [popularity          (or (candidate candidate-popularity) (rand-int 100))
        party               (candidate candidate-party)
        adjusted-popularity (-> popularity
                                (adjust-popularity volatility)
                                (adjust-popularity-party volatility (get party-popularity party)))
        adjusted-popularity (if volatility-pp
                              ((rand-nth [- +]) adjusted-popularity (rand-nth (range volatility-pp)))
                              adjusted-popularity)
        adjusted-popularity (if (< adjusted-popularity 0) 0 adjusted-popularity)]
    (int
     (if (party-preference? party candidate-party voted-candidates)
       (+ adjusted-popularity same-party-buff)
       adjusted-popularity))))


;; For example, in the example config, candidate :A has a popularity of 25%
;;
;; Here :A and :C are in the same party and :C has already been selected:

(->> #(calc-candidate-weighting example-config :A [:C])
     repeatedly
     (take 10))

;; And here :B is not in :A's party
(->> #(calc-candidate-weighting example-config :A [:B])
     repeatedly
     (take 10))

;; Here are the same functions with higher volatility

(def example-config-2 (-> example-config
                          (assoc :volatility 70)
                          (assoc :volatility-pp 30)))

(->> #(calc-candidate-weighting example-config-2 :A [:C])
     repeatedly
     (take 10))

(->> #(calc-candidate-weighting example-config-2 :A [:B])
     repeatedly
     (take 10))

;; As we can see, at smaller scales, higher volatility does seem to have an impact.
;; However, at an aggregate level there is a tendancy toward an average closer to the
;; starting point:


(->> #(calc-candidate-weighting example-config-2 :A [:C])
     repeatedly
     (take 1000)
     (average))

;; Remember, the 'same party buff' is set quite high (40 percentage points), so we would expect
;; something like 64 here (24% starting point + 40% same party buff)

(->> #(calc-candidate-weighting example-config-2 :A [:B])
     repeatedly
     (take 1000)
     (average))

;; The key point here is that being in the same party as a candiate who has already been
;; elected will give an significant boost to the probability of being selected (and will likely
;; override any 'volatility' settings). This was a conscious choice on my part and assumes that
;; this is how things work in reality.

;; ### Marking a Full Ballot
;;
;; The main performance hit in terms of this vote generator comes from my desire to have
;; the 'same party' effect that I mentioned above. Because of this, the ballot has to
;; be 'marked' **sequentially**. Existing choices have an impact on later choices.
;;
;; If this wasn't necessary then you could generate a complete 'ballot' in one go with
;; some kind of algorithm.
;;
;; This kind of algorithm may still be possible while also mainting the 'same party preference'
;; factor, but I didn't come across anything along those lines myself.
;;

;; So, for each 'mark' on the ballot we have a distinct process, this is defined by the
;; function below:

(defn- determine-vote [{:keys [candidates] :as vote} voted-candidates]
  (let [candidates (sort (into [] candidates))
        weightings (for [c candidates]
                     (calc-candidate-weighting vote c voted-candidates))]
    (weighted-randomizer candidates weightings)))

;; As you can see we are using the 'wighted-randomizer' function again. For **each** time
;; we mark the ballot we have to recalculate all the candidate weightings (since these may have
;; changed if a same party candidate has already been selected)

;; Here is an example of running this function in relation to the first preference.
;; The results should be similar to the starting popularity 'percentages' in the example config:



(->> #(determine-vote example-config [])
     repeatedly
     (take 10))

;; Let's look at how often each candidate is selected:

(->> #(determine-vote example-config [])
     repeatedly
     (take 1000)
     frequencies
     (sort-by val)
     reverse)

;; And with a higher volatility:

(->> #(determine-vote example-config-2 [])
     repeatedly
     (take 1000)
     frequencies
     (sort-by val)
     reverse)

;; And with a candidate from party-1 already selected (:C)

(def example-config-3
  (let [candidates (:candidates example-config)
        remove-elected (disj candidates :C)]
    (-> example-config
        (assoc :candidates remove-elected))))

(->> #(determine-vote example-config-3 [:C])
     repeatedly
     (take 1000)
     frequencies
     (sort-by val)
     reverse)


;; Finally, the last two functions put all these together:

(defn- mark-ballot [vote]
  (let [n-marks (ballot-depth vote)]
    (loop [[i & rest] (range 1 (inc n-marks))
           ballot {}
           voted-candidates []
           v vote]
      (if-not i
        ballot
        (let [vote-for (determine-vote v voted-candidates)
              remove-candidate (disj (:candidates v) vote-for)]
          (recur rest
                 (assoc ballot vote-for i)
                 (conj voted-candidates vote-for)
                 (assoc v :candidates remove-candidate)))))))


(defn prstv-vote-generator [{:keys [n-votes] :as vote-config}]
  (reduce #(assoc %1 %2 (mark-ballot vote-config)) {} (range 1 (inc n-votes))))


(mark-ballot example-config)

;; A higher volatility ballot
(mark-ballot example-config-2)

;; Ten ballots:

(prstv-vote-generator (-> example-config (assoc :n-votes 10)))

;; ## Counting the Ballots

;; ### Calculating the Quota
;; The first step in counting the ballots is to calculate a
;; **quota**. In Ireland, this is done by dividing the number of votes
;; by the number of seats + 1. Finally, 1 is added to this total.
;;
;;For example, if there were 50,000 votes cast and three seats to
;;be filled:
;;
;;```
;; 50,000
;; ------ + 1
;; 3 + 1
;;
;; = 12,501
;;```

(defn quota [n-votes n-seats]
  (-> n-votes
      (/ (inc n-seats))
      inc))

(quota 50000 3)

;; When a candidate recieves more votes than the quota, they are elected.

;; ### First Count
;;
;; The first count is relatively straight-forward, we just have to count all the **first preference** votes
;; for a candidate.
;;
;; I tried to model the physical situation of the vote count as closely as possible. In Ireland,
;; votes are counted mannually. This involves _sorting_ them into **piles**.
;;
;; So, before counting, we have to sort the piles:

(defn sort-votes-by-next-preference
  "t-votes are the all the ballots, as generated by prstv-vote-generator"
  [active-candidates t-votes & voteids]
  (reduce (fn [piles [id ballot]]
            (let [target (->> (select-keys ballot active-candidates)
                              (sort-by val)
                              ffirst)]
              (update piles (or target :no-preference) (fnil conj []) id)))
          {}
          (if voteids (select-keys t-votes (first voteids)) t-votes)))

;; Let's also define some sample ballots:

(def example-ballots (prstv-vote-generator (assoc example-config :n-votes 50)))

(sort-votes-by-next-preference (:candidates example-config) example-ballots)

;; After sorting, each candidate has a 'pile' which is a vector of vote-ids
;;
;; To count these, we just have to count the sizes of the vectors:

(defn vote-counts [piles]
  (reduce (fn [res [c vs]]
            (assoc res c (count vs)))
          {}
          piles))

(-> (sort-votes-by-next-preference (:candidates example-config) example-ballots)
    vote-counts)


;; ### Subsequent counts
;;
;; On subsequent counts, things get more complicated.
;;
;; After each count, you will be in one of two situations:
;;
;; 1. A candidate has exceeded the quota and is therefore elected
;;
;; 2. No candidate has exceeded the quota
;;
;; In the case of **1**, there are two further possibilities:
;;
;; a. The candidate's surplus is **distributable**
;;
;; b. The candidate's surplus is not eleigible for distribution
;;
;; What we mean by 'surplus' here is simply the number of votes above the quota.
;; For example, if at the end of a count the candidate now has **250** votes
;; in their pile, and the quota was **200**, then their surplus is **50**.
;;
;; In order to determine if the surplus should be re-distributed to other
;; candidates, we have to check for three things (the last of these is
;; ignored in my case)
;;
;; - The surplus can elect the hightest continuing candidate

;; - The surplus can bring the lowest continuing candidate level with or above the second lowest continuing candidate
;;
;; - The surplus can qualify the lowest continuing candidate for recoupment of their election expenses or deposit (if applicable)
;;
;; So, for example, if the surplus is **50** and the next continuing candidate currently has **170** votes in their pile,
;; then the surplus *can* be distributed (since 170 + 50 is higher than the quota of 200). If the next
;; candidate only had 149 votes in their pile, then the surplus **could not** be distributed on this basis.
;;
;; If there is the same surplus of 50, and the bottom 2 candidata have 80 and 110 votes respectively, then the
;; surplus can be redistributed on this basis, since theoretically the bottom candidate (80) could
;; recieve these 50 votes and bring them past the next candidate (110).
;;
;; The function for determining this was as follows:


(defn redistribute-surplus? [quota candidate-counts surplus]
  (let [vs (vals candidate-counts)
        plus-surplus (map #(+ % surplus) vs)
        diff-lowest (->> (sort vs)
                         (take 2)
                         reverse
                         (apply -))
        above-quota (filter #(>= % quota) plus-surplus)]
    (or (seq above-quota)
        (<= diff-lowest surplus))))


;; For example, a quota of 10 and a surplus of '1':

(redistribute-surplus? 10 {:A 3 :B 6 :C 8} 1)

;; Or, a quota of 201 and a surplus of '13', where the difference between the bottom two candidates can be bridged by the
;; surplus:

(redistribute-surplus? 201 {:peter-pan 187, :snow-white 159, :cinderella 158, :micky-mouse 130, :minnie-mouse 143} 13)

;; ### Distributing surpluses
;;
;; Once a surplus **can** be distributed, the process for actually distributing is also quite complicated.
;;
;; Let's go back to a previous example where a candidate got 250 votes and the quota was 200, meaning there
;; is a surplus of **50** votes to be re-distributed. The way to determine *how* these votes will be re-distributed,
;; at least according to the Irish system, is as follows:
;;
;; a. If the ballots for the candidate consist of **only first preference votes** (i.e., this will be the case after the first count),
;; then **all of the elected candidate's** ballots are examined to determine how the surplus will be split
;; proportionally
;;
;; b. If the ballots are not only first preference for that candidate, then **the last parcel of votes** that brought the
;; elected candidate over the quota are examined to determine the proportionality.
;;
;; So, in the above case, if the candidate reached 250 votes on the first count (which counts first preferences), then
;; all 250 votes will be examined. Let's say that there are two other candidates in the race, :B and :C. When examining
;; the 250 votes of candidate :A, 200 contain a second preference for candidate :B and 50 contain a second preference
;; for candidate :C. In other words, 4/5 of the surplus should go to :B and 1/5 to :C - :B gets 40 of the surplus votes
;; and :C gets 10.
;;
;; In the case of 'b', where the votes that got :A over the line were transferred from another candidate, then the "last parcel"
;; is examined to determine the proportionality. A 'parcel' is a physical bundle of votes, and let's say the defaul parcel size
;; in this case was 100 votes. So, here the last 100 votes in :A's pile will be examined to determine the proportionality along
;; the same lines as the above. I took one important liberty with regard to this step (for performance reasons), instead of taking
;; the last "parcel" of votes to deterime the proprtionality, I took the last x votes that were given to the candidate in the previous
;; count (for example, if, in the previous count the candidate recieve 74 votes, I took all of these to examine for next preferences.
;; From what I understand, this would be very similar - the same? - to taking an actual 'parcel' size.
;;
;; In order to calculate these steps, first there is a small helper function to get the proportions by counts:


(defn counts->proportions
  "Takes vote counts, e.g., {:A 20 :B 13} and translates them to percetage value"
  [counts]
  (let [total (reduce + (vals counts))]
    (reduce (fn [result candidate]
              (assoc result candidate (double (/ (candidate counts) total))))
            {}
            (keys counts))))

(counts->proportions {:A 130 :B 34 :C 56})


;; Next, there is another helper function that is used to 'fairly' distribute shares according to proportions.
;; I say 'fairly' in inverted commas, as there are cases where this approach doesn't really work, however
;; I'm not sure what the best option is (or how the vote counters *actually* solve these kinds of cases in practice)
;;
;; The problem here is as follows:
;;
;;  - We have 21 shares to distribute (surplus)
;;
;;  - We examine the ballots and see that the following proportions of next preferences are present:
;;
;;      - Candidate B: 25%
;;      - Candidate C: 25%
;;      - Candidate D: 25%
;;      - Candidate E: 25%
;;
;; In this case each candidate would get at least 5 of the 21 shares. But who gets the remaining 1?
;;
;; In the real world, this kind of case would be extremely unlikely, but it is something to point
;; out in relation to the function below.
;;
;; One last thing to note. As you will see, I am discounting the ballots that have no **next preference**
;; from the share distribution. As far as I am aware, this is how it works in Ireland. In other countries,
;; votes with no 'next preference' are treated differently. The deeper, philosophical question here is around
;; whether the 'no next preference' votes should play a role in how the surplus is distributed. For example, in an
;; extreme case where, out of everyone who voted for candidate :A, only one one person expressed a preference for
;; another candidate (let's say they voted for candidate :B as their second preference), then under the
;; Irish system (as far as I am aware), 100% of the surplus (50 votes) would be transferred to candidate :B



(defn share-distributor
  "Takes a map of counts and an integer (shares) to be distributed. Returns a map"
  [counts shares]
  (let [proportions (counts->proportions (dissoc counts :no-preference))]
    (loop [shares        shares
           need          (reduce #(assoc %1 %2 (- (%2 proportions))) {} (keys proportions))
           shares-p      (reduce #(assoc %1 %2 0) {} (keys proportions))
           distributions shares-p]
      (if (zero? shares) distributions
          (let [target          (ffirst (sort-by val need))
                dist-update     (update distributions target inc)
                shares-p-update (counts->proportions dist-update)
                shares-p-diff   (zipmap (keys (sort shares-p))
                                        (map (fn [a b] (- a b))
                                             (vals (sort shares-p))
                                             (vals (sort shares-p-update))))
                need-update     (reduce #(assoc %1 %2 (- (%2 need) (or (%2 shares-p-diff) 0))) {} (keys need))]
            (recur
             (dec shares)
             need-update
             shares-p-update
             dist-update))))))

(share-distributor {:A 30 :B 30} 10)

(share-distributor {:A 4 :B 3 :D 3 :E 1} 10)

;; The 'unfair' case outlined above:
(share-distributor {:A 1 :B 1 :C 1 :D 1} 21)


(defn vote-preferences
  "Returns a list of preferences in pile. Used to determine if count is first count."
  [piles t-votes candidate]
  (let [voteids (candidate piles)]
    (map #(candidate (t-votes %)) voteids)))

(defn surplus-distribute [piles t-votes elected-candidate active-candidates quota count-changes]
  (let [voteids    (elected-candidate piles)
        cand-count (count voteids)
        surplus    (- cand-count quota)]
    (if-not (pos? surplus)
      piles
      (let [first-pref-votes  (vote-preferences piles t-votes elected-candidate)
            first-prefs-only? (apply = first-pref-votes)
            votes-d           (if first-prefs-only?
                                (sort-votes-by-next-preference active-candidates t-votes voteids)
                                (sort-votes-by-next-preference
                                 active-candidates
                                 t-votes
                                 (take (or (elected-candidate count-changes) 0) ;; If not first preferences only, take only recent votes to determine proportionality
                                       (reverse voteids))))
            vs                (dissoc votes-d :no-preference) ;; Ignore 'no further preference' votes
            counts            (vote-counts vs)
            shares            (share-distributor (counts->proportions counts) surplus)
            piles             (assoc piles elected-candidate
                                     (if first-prefs-only?  []
                                         (drop (elected-candidate count-changes)
                                               (reverse voteids))))]
        (->
         (reduce (fn [p [c vs]]
                   (let [share (c shares)
                         dist  (take share vs)
                         keep  (drop share vs)]
                     (-> p
                         (update c concat dist)
                         (update elected-candidate concat keep))))
                 piles
                 vs)
         (update elected-candidate concat (:no-preference votes-d)))))))


(surplus-distribute
 (sort-votes-by-next-preference (:candidates example-config) example-ballots)
 example-ballots
 :A
 #{:B :C :D :E :F}
 50
 nil)


;; If the surplus cannot be distributed or if there is no candidate that has exceeded the quota, then
;; the **lowest** candidate is eliminated, and their votes are redistributed.
;;
;; Distributing the votes of the eliminated candiate is much simpler, all the votes are always examined
;; to determine the proportions of the surplus distribution:

(defn elimination-distribute [piles t-votes eliminated-candidate active-candidates]
  (let [voteids (eliminated-candidate piles)
        votes-d (sort-votes-by-next-preference active-candidates t-votes voteids)
        vs (dissoc votes-d :no-preference)]
    (->
     (reduce (fn [p [c vs]]
               (update p c concat vs))
             piles
             vs)
     (assoc (keyword (str (name eliminated-candidate) "-non-transferrable")) (:no-preference votes-d))
     (assoc eliminated-candidate []))))

(elimination-distribute
 (sort-votes-by-next-preference (:candidates example-config) example-ballots)
 example-ballots
 :F
 #{:B :C :D :E})

(elimination-distribute
 (sort-votes-by-next-preference (:candidates example-config) example-ballots)
 example-ballots
 :E
 #{:B :C :D})

;; ### Counting the Votes
;;
;; The main counting function is a bit large/unweildy to discuss here. You can go
;; look at it in the [source code](https://github.com/loopdreams/prstv-sim.git) of the app if you are interested.
;;
;; In general/pseudo-code terms it does the following:
;;
;; i. Calculate the quota
;;
;; ii. Sort the ballots into candidate piles based on first perference votes
;;
;; Then, the main loop starts,
;; which continues as long as the target number of candidates haven't been elected yet:
;;
;; 1. Get the counts for each candidate (how many ballots are in their pile)
;;
;; 2. Check to see if anyone has reached the surplus
;;
;;      + If they have, mark candidate 'elected' and check to see if the surplus can be distributed. If it can, go to step 3
;;
;;      + If no one has reached the quota, or if surplus cannot be distributed, go to step 4
;;
;; 3. Distribute surplus of elected candidates.
;;
;; 4. Distribute the elimination votes for the lowest candidate and mark that candidate 'eliminated'

