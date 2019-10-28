(ns travelling-salesman-problem.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [criterium.core :as criterium]
            [uncomplicate.neanderthal
             [math :refer [abs ; absolute
                           sqrt ; square root
                           pow ; power x by y
                           ]]]))



;; (criterium/quick-bench (-main))

;; -----
;; ## Records
;; Create a record for Cities and Individuals.
;; This is the same as creating a class for each one.
;; In clojure it is a simple map.

(defrecord City [name x y])
(defrecord Individual [parents route total-distance fitness])


;; -----
;; ## Variables

(def generations 10000)
(def population-size 500)
(def population-selection-size 0.2)
(def mutation-rate 0.01)
(def death-rate 0.5)
(def cities
  [(City. "A" 160 189)
   (City. "B" 170 188)
   (City. "C" 104 118)
   (City. "D" 63 149)
   (City. "E" 63 177)
   (City. "F" 33 185)
   (City. "G" 27 178)
   (City. "H" 4 180)
   (City. "I" 35 118)
   (City. "J" 36 37)
   (City. "K" 20 9)
   (City. "L" 68 26)
   (City. "M" 78 22)
   (City. "N" 109 49)
   (City. "O" 131 9)
   (City. "P" 176 15)
   (City. "Q" 139 48)
   (City. "R" 139 65)
   (City. "S" 160 70)
   (City. "T" 191 57)
   (City. "U" 181 90)
   (City. "V" 195 139)
   (City. "W" 197 185)
   (City. "X" 184 178)
   (City. "Y" 176 187)])


;; -----
;; ## Genetic Algorithm

(defn distance-between-cities
  "Calculate the distance between two cities."
  [city-pair]
  (let [city-a (first city-pair)
        city-b (second city-pair)
        dist-x (abs (- (:x city-a) (:x city-b)))
        dist-y (abs (- (:y city-a) (:y city-b)))]
    (sqrt (+ (pow dist-x 2) (pow dist-y 2)))))

(distance-between-cities [(first cities) (second cities)])
;; => 10.04987562112089


(defn total-distance
  "Calculate the total distance of the route."
  [route]
  (let [city-pairs (mapv #(vector %1 %2) (butlast route) (rest route))
        first-city (first route)
        last-city (last route)
        distances (map distance-between-cities city-pairs)
        last-city-distance (distance-between-cities [last-city first-city])]
    (+ (reduce + distances) last-city-distance)))

(total-distance cities)
;; => 921.6346703083029


(defn fitness
  "Calculate the fitness based on the route total distance."
  [total-dist]
  (/ 1.0 (+ 1 (pow total-dist 8))))

(fitness (total-distance cities))
;; => 0.0010850286260015397


(defn normalize-fitness
  "Normalize fitness based on the sum of fitness of the population."
  [individual population]
  (let [fitn (:fitness individual)
        distance-sum (reduce + (map :total-distance population))]
    (assoc-in individual [:fitness] fitn)))


(defn generate-route
  "Generate a random route."
  [city-list]
  (shuffle city-list))


(defn selection
  "Select the best number of individuals and randomly select among the rest."
  [population]
  (let [best (reverse (sort-by :fitness population))
        n-size (int (* population-selection-size (count population)))
        best-selected (take n-size best)
        not-selected (drop n-size best)
        random-pick (shuffle (random-sample (- 1 death-rate) not-selected))]
    (concat best-selected random-pick)))


(defn crossover
  "Generate a new individual based on 2 parents.
  Select a random number of genes of parent 1 and parent 2 and then
  concat them."
  [parent-pair]
  (let [gene-a (rand-int (count cities))
        gene-b (rand-int (count cities))
        parent-1 (first parent-pair)
        ;; If there isn't a second parent, generate one randomly.
        parent-2 (if (second parent-pair)
                   (second parent-pair)
                   (let [route (generate-route cities)
                         distance (total-distance route)]
                     (Individual. nil route distance (fitness distance))))
        child-p1 (vec (take gene-a (:route parent-1)))
        ;; Use a hash-set to allow only unique cities.
        cities-p1 (into #{} (map :name child-p1))
        child-p2 (vec
                  (filter #(not (nil? %))
                          (for [gene (:route parent-2)]
                            (if-not (contains? cities-p1 (:name gene))
                              gene))))
        route (vec (concat child-p1 child-p2))
        distance (total-distance route)]
    (Individual.
     [parent-1 parent-2]
     route
     distance
     (fitness distance))))


(defn select-parents
  "Select a pair of parents from the population"
  [population]
  (loop [pop population
         parent-pairs []]
    (if (empty? pop)
      parent-pairs
      (recur (drop 2 pop) (conj parent-pairs (take 2 pop))))))


(defn crossover-population
  "Breed the population."
  [population]
  (let [selected-population (selection population)
        selected-pairs (select-parents selected-population)]
    (map crossover selected-pairs)))


(defn swap-cities
  "Swap the order of some elements randomly for an individual."
  [individual]
  (let [i (:route individual)
        r1 (rand-int (count i))
        r2 (rand-int (count i))
        prob (rand)]
    (if (< prob mutation-rate)
      (let [s1 (assoc i r1 (nth i r2))
            s2 (assoc s1 r2 (nth i r1))
            distance (total-distance s2)]
        (Individual. (:parents individual) s2 distance (fitness distance)))
      individual)))


(defn mutate
  [individual]
  (loop [i individual
         counter (count cities)]
    (if (= 0 counter)
      i
      (recur (swap-cities individual) (dec counter)))))


(defn mutate-population
  "Mutate each individual of a population."
  [population]
  (map mutate population))


(defn new-generation
  "Generate a new generation of a given population."
  [population]
  (let [selected-population (selection population)
        breeding (crossover-population population)
        mutation (mutate-population (into selected-population breeding))]
    (if (< (count mutation) population-size)
      (let [new-population (for [i (range (- population-size (count population)))]
                             (let [route (generate-route cities)
                                   distance (total-distance route)]
                               (Individual. nil route distance (fitness distance))))]
        (into mutation new-population))
      mutation)))


(defn save-historical-data
  "Save the evolution data in a file."
  [file-name data]
  (let [d (str/join "\n" data)]
    (with-open [w (io/writer (str "resources/" file-name) :append true)]
      (.write w d))))


;; ## Start Evolution

(defn genetic-algorithm
  []
  (let [initial-population (for [i (range population-size)]
                             (let [route (generate-route cities)
                                   distance (total-distance route)]
                               (Individual. nil route distance (fitness distance))))]
    (loop [generation 1
           population initial-population
           historical-distance []
           historical-fitness []
           best nil]
      (let [pop (map #(normalize-fitness %1 population) population)
            best (first (reverse (sort-by :fitness pop)))]
        (println (str "Generation: " generation
                      ", Best distance: " (:total-distance best)
                      ", Best fitness: " (:fitness best)))
        (if (= generation generations)
          {:population pop
           :historical-distance historical-distance
           :historical-fitness historical-fitness
           :best best}
          (recur
           (inc generation)
           (new-generation pop)
           (conj historical-distance (:total-distance best))
           (conj historical-fitness (:fitness best))
           best))))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "\nStart Genetic Algorithm")
  (let [gen (genetic-algorithm)
        best (:best gen)]
    (save-historical-data "historical-distance.csv" (:historical-distance gen))
    (save-historical-data "historical-fitness.csv" (:historical-fitness gen))
    (println "Lowest Distance: " (:total-distance best))
    (println "Best Route: " (map :name (:route best)))
    (println "Best Fitness: " (:fitness best))))

(-main)
