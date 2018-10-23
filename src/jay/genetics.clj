(ns jay.genetics
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(defn agouti [s d] (or (:A s) (:A d) :a))

(defn brown [s d] (or (:B s) (:B d) (:b s) (:b d) :b'))

(defn albino [s d] (or (:C s) (:C d)
                       (and (:cs s) (:cs d))
                       (and (:cb s) (:cb d))
                       #{:cb :cs}))

(defn density [s d] (or (:D s) (:D d) :d))

(defn length [s d] (or (:L s) (:L d) :l))

(defn inhibitor [s d] (or (:I s) (:I d) :i))

(defn spotting [s d] (or (:S s) (:S d) :s))

(def colors #{:X :Xo :Y})

(defn orange [s d] (into (set/intersection colors s)
                         (set/intersection colors d)))

(def color-name
  {:A "tabby" :a "solid"
   :B "black" :b "chocolate" :b' "cinnamon"
   :Bd "blue" :bd "lilac" :b'd "fawn"
   :C nil :cs "point" :cb "sepia"
   :D nil :d nil
   :L "shorthair" :l "longhair"
   :I "silver" :i nil
   :S "with white" :s nil
   :X nil :Xo "red" :Y nil})

(def brown-dilute #{:B :b :b'})

(comment
  agouti :A tabby :a solid
  brown :B black :b chocolate :b' cinnamon [irrelevant if :XoY or :XoXo]
  albino :C :cs :cb [:cs :cb :cbcs]
  density :D :d dilute [affects :B - :Bd blue :bd lilac :b'd fawn]
  length :L shorthair :l
  inhibitor :I silver :i
  spotting :S white-spotted :s
  orange :X :Xo :Y [:XoXo orange-female :XoY orange-male :XoX tortie-female :XY black male :XX black female])

(def order [:B :b :b' :Bd :bd :b'd
            :I :i :A :a :C :cs :cb :S :s :L :l])

(defn setify [s] (if (set? s) s (hash-set s)))

(defn display [cat]
  (let [cat' (if (:d cat)
               (let [b (first (set/intersection brown-dilute cat))
                     b' (keyword (str (name b) "d"))]
                 (-> cat (disj b) (conj b')))
               cat)]
    (str/join " " (remove nil? (map #(color-name (cat' %)) order)))))

(defn produce [s d]
  (reduce (fn [child f] (into child (setify (f s d))))
          #{}
          [agouti brown albino density length inhibitor spotting orange]))

(s/def ::agouti #{:A :a})
(s/def ::brown #{:B :b :b'})
(s/def ::albino #{:C :cs :cb})
(s/def ::density #{:D :d})
(s/def ::length #{:L :l})
(s/def ::inhibitor #{:I :i})
(s/def ::orange-x #{:X :Xo})
(s/def ::orange-y #{:Y})
(s/def ::male (s/and (s/cat :x ::orange-x :y ::orange-y)
                     (s/conformer #(hash-set (:x %) (:y %)))))
(s/def ::female (s/and (s/cat :x1 ::orange-x :x2 ::orange-x)
                       (s/conformer #(hash-set (:x1 %) (:x2 %)))))
(s/def ::color (s/and (s/cat :a ::agouti :b ::brown :c ::albino
                             :d ::density :l ::length :i ::inhibitor)
                      (s/conformer #(hash-set (:a %) (:b %) (:c %)
                                              (:d %) (:l %) (:i %)))))
(s/def ::sire (s/and (s/cat :m ::male :c ::color)
                     (s/conformer #(into (:m %) (:c %)))))
(s/def ::dam (s/and (s/cat :f ::female :c ::color)
                    (s/conformer #(into (:f %) (:c %)))))

(comment
  (s/exercise ::male)
  (s/exercise ::female)
  (s/exercise ::color)
  (s/exercise ::sire)
  (s/exercise ::dam)
  (println order)
  (map (comp display second) (s/exercise ::sire))
  (for [sire (map second (s/exercise ::sire))
        dam  (map second (s/exercise ::dam))]
    (println (display sire) "x" (display dam) "=" (display (produce sire dam))))
  (map produce
       (map second (s/exercise ::sire))
       (map second (s/exercise ::dam))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
