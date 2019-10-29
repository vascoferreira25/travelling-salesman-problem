;; # Travelling Salesman Problem
;; This is an alternative implementation in Clojure of the
;; Python tutorial in [Evolution of a salesman: A complete genetic algorithm tutorial for Python](https://towardsdatascience.com/evolution-of-a-salesman-a-complete-genetic-algorithm-tutorial-for-python-6fe5d2b3ca35)
;; And also changed a few details as in [Coding Challenge #35.4: Traveling Salesperson with Genetic Algorithm](https://www.youtube.com/watch?v=M3KTWnTrU_c)

;; ## The Problem The travelling Salesman Problem asks que following question:
;;
;; "Given a list of cities and the distances between each pair of cities, what
;; is the shortest possible route that visits each city and returns to the
;; origin city?"
;; - [Wikipedia](https://en.wikipedia.org/wiki/Travelling_salesman_problem)
;;
;; This implementations uses a genetic algorithm to find the shortest route
;; between cities.


;; -----
;; ## Requirements
;; - Neanderthal: math functions
;; - Clojure.java.io: write data to file
;; - Clojure.string: join all the data in a single string
(ns travelling-salesman-problem.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uncomplicate.neanderthal
             [math :refer [abs sqrt pow]]]))


;; -----
;; ## Records
;; Create a record for Cities and Individuals.
;; This is the same as creating a class for each one.
;; In Clojure it is the same as a simple map.

;; The City is a point and has 3 keys, a `:name` and a `:x` and `:y` coordinates.
(defrecord City [name x y])

;; The Individual represents the genomes in the Genetic Algorithm.
;; Each individual has a parent (except on the initial population)
;; and the `:parents` key may be used to track its evolution in each
;; generation.
(defrecord Individual [parents route total-distance fitness])


;; -----
;; ## Utility Functions

;; As the cities are just points, just calculate the distance
;; between two points with `sqrt((xa - xb)^2 + (ya - yb)^2)`.
(defn distance-between-cities
  "Calculate the distance between two cities."
  [city-pair]
  (let [city-a (first city-pair)
        city-b (second city-pair)
        dist-x (abs (- (:x city-a) (:x city-b)))
        dist-y (abs (- (:y city-a) (:y city-b)))]
    (sqrt (+ (pow dist-x 2) (pow dist-y 2)))))

;; Distance between the first and second city:

;; (distance-between-cities [(first cities) (second cities)])
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

;; Total distance of the cities-list:

;; (total-distance cities)

;; => 921.6346703083029


(defn fitness
  "Calculate the fitness based on the route total distance.
  As distance decreases, fitness increases.
  0 is the absolute best value."
  [total-dist]
  (/ 1.0 (+ 1 (pow total-dist 8))))

;; Fitness of the cities-list:

;; (fitness (total-distance cities))

;; => 0.0010850286260015397


(defn normalize-fitness
  "Normalize fitness based on the sum of fitness of the population.
  This makes each fitness value correspond to a probability."
  [individual population]
  (let [fit (:fitness individual)
        fitness-sum (reduce + (map :fitness population))]
    (assoc-in individual [:fitness] (/ fit fitness-sum))))


(defn generate-route
  "Generate a random route."
  [city-list]
  (shuffle city-list))


(defn pick-one
  "With a random probability, pick the better individuals more often and
  the worse individuals less often."
  [population]
  (let [r (rand)
        pop (filter #(> (:fitness %) r) population)]
    (if (empty? pop)
      (rand-nth population)
      (rand-nth pop))))


;; By selecting the best individuals we are performing elitism.
(defn selection
  "Select the best individuals from the population and
  randomly select among the rest."
  [population population-size elitism-size]
  (let [best (take elitism-size (reverse (sort-by :fitness population)))
        selected-rest (repeatedly (- population-size elitism-size) #(pick-one population))]
    (into best selected-rest)))


(defn crossover
  "Generate a new individual based on 2 parents.
  Select a random number of genes of parent 1 and fill with genes from parent 2 ."
  [parent-pair cities-list]
  (let [genes-a (rand-int (count cities-list))
        parent-1 (first parent-pair)
        ;; If there isn't a second parent, generate one randomly.
        parent-2 (if (second parent-pair)
                   (second parent-pair)
                   (let [route (generate-route cities-list)
                         distance (total-distance route)]
                     (Individual. nil route distance (fitness distance))))
        child-p1 (vec (take genes-a (:route parent-1)))
        ;; Use a hash-set to only allow unique cities.
        cities-p1 (into #{} (map :name child-p1))
        child-p2 (vec
                  (filter #(not (nil? %))
                          (for [genes-b (:route parent-2)]
                            (if-not (contains? cities-p1 (:name genes-b))
                              genes-b))))
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
  [population population-size elitism-size cities-list]
  (let [selected-population (selection population population-size elitism-size)
        selected-pairs (select-parents selected-population)]
    (map #(crossover % cities-list) selected-pairs)))


;; -----
;; ## Mutation

(defn swap-cities
  "Swap the order of some elements randomly for an individual given a probability."
  [individual mutation-rate]
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
  "Mutate a single individual."
  [individual cities-list mutation-rate]
  (loop [i individual
         counter (count cities-list)]
    (if (= 0 counter)
      i
      (recur (swap-cities individual mutation-rate) (dec counter)))))


(defn mutate-population
  "Mutate each individual of a population."
  [population cities-list mutation-rate]
  (map #(mutate % cities-list mutation-rate) population))


;; A new generation is created based on selected individuals, which are crossed
;; in pairs and then mutated.
(defn new-generation
  "Generate a new generation of a given population."
  [population cities-list population-size elitism-size mutation-rate]
  (let [selected-population (selection population population-size elitism-size)
        breeding (crossover-population population population-size elitism-size cities-list)]
    (mutate-population breeding cities-list mutation-rate)))


;; Save all the evolution data to a csv file to make some graphs.
(defn save-historical-data
  "Save the evolution data in a file."
  [file-name data]
  (let [d (str/join "\n" data)]
    (with-open [w (io/writer (str "resources/" file-name) :append true)]
      (.write w d))))


;; -----
;; ## Genetic Algorithm

;; The algorithms works as follows:
;; - Start with a random initial-population
;; - Normalize the fitness values
;; - Start a new generation
;; - Loop
(defn genetic-algorithm
  [initial-population
   cities-list
   population-size
   elitism-size
   generations
   mutation-rate
   print-progress]
  (loop [generation 1
         population initial-population
         historical-best-distance []
         historical-distance []
         historical-fitness []
         global-best {:fitness 0}]
    (let [current-generation (map #(normalize-fitness %1 population) population)
          ;; Dont use normalized fitness values as it is a percentage of the
          ;; current population. This means it doesn't correspond to the global
          ;; best value.
          sorted-gen (sort-by :fitness population)
          gen-best (last sorted-gen)]
      (if print-progress
        (println (str "Generation: " generation
                      ", Best distance: " (:total-distance global-best))))
      (if (= generation generations)
        {:population current-generation
         :historical-best-distance historical-best-distance
         :historical-distance historical-distance
         :historical-fitness historical-fitness
         :global-best global-best}
        (recur
         (inc generation)
         (new-generation current-generation
                         cities-list
                         population-size
                         elitism-size
                         mutation-rate)
         (conj historical-best-distance (:total-distance global-best))
         (conj historical-distance (:total-distance gen-best))
         (conj historical-fitness (:fitness gen-best))
         (if (> (:fitness gen-best) (:fitness global-best))
           gen-best
           global-best))))))


;; -----
;; ## Variables

;; Number of iterations
(def generations 5000)

;; Number of individuals in each generation
(def population-size 100)

;; Number of best performant individuals to be selected
(def elitism-size 20)

;; Rate at which the cities in a route will be swapped
(def mutation-rate 0.1)

;; All the cities to find the route for.
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
;; ## Main Function

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "\nStart Genetic Algorithm")
  (let [initial-population (for [i (range population-size)]
                             (let [route (generate-route cities)
                                   distance (total-distance route)]
                               (Individual. nil route distance (fitness distance))))
        ga (genetic-algorithm initial-population
                               cities
                               population-size
                               elitism-size
                               generations
                               mutation-rate
                               true)
        best (:global-best ga)]
    (save-historical-data "historical-best-distance.csv" (:historical-best-distance ga))
    (save-historical-data "historical-distance.csv" (:historical-distance ga))
    (save-historical-data "historical-fitness.csv" (:historical-fitness ga))
    (println "Lowest Distance: " (:total-distance best))
    (println "Best Route: " (map :name (:route best)))
    (println "Best Fitness: " (:fitness best))))

;; Execute code in REPL:

;; `(-main)`

