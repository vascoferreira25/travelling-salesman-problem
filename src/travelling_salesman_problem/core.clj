;; # Travelling Salesman Problem
;;
;; This is an alternative implementation in Clojure of the
;; Python tutorial in [Evolution of a salesman: A complete genetic algorithm tutorial for Python](https://towardsdatascience.com/evolution-of-a-salesman-a-complete-genetic-algorithm-tutorial-for-python-6fe5d2b3ca35).
;;
;; I also changed the selection progress to Roulette Selection as shown in
;; [Coding Challenge #35.4: Traveling Salesperson with Genetic Algorithm](https://www.youtube.com/watch?v=M3KTWnTrU_c)
;;
;; ## The travelling Salesman Problem asks the following question:
;;
;; "Given a list of cities and the distances between each pair of cities, what
;; is the shortest possible route that visits each city and returns to the
;; origin city?"
;; - [Wikipedia](https://en.wikipedia.org/wiki/Travelling_salesman_problem)
;;
;; This implementation uses a genetic algorithm to find the shortest route
;; between a list of cities.
;;
;; -----
;; ## Requirements
;;
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
;;
;; Create a record for Cities and Individuals. This is the same as creating a
;; class for each one. In Clojure it works as a simple map.

;; The City is a point and has 3 keys, a `:name` and a `:x` and `:y`
;; coordinates. The name is used a unique identifier of the city.
;; The coordinates are used to calculate the distance between
;; cities. The total-distance of a route and the fitness functions depend on the
;; coordinates of each city.
(defrecord City [name x y])

;; The Individual represents the genomes in the Genetic Algorithm.
;; Each individual has a parent (except on the initial population)
;; and the `:parents` key may be used to track its evolution in each
;; generation.
;;
;; The `route` is a vector of cities and is used to calculate the
;; the `total-distance` and the fitness of the individual.
(defrecord Individual [route total-distance fitness])


;; -----
;; ## Utility Functions

(defn dir?
  "Check if the given directory path exists."
  [path]
  (.isDirectory (java.io.File. path)))


(defn mkdir
  "Create a directory."
  [path]
  (.mkdirs (io/file path)))


(defn ensure-directory!
  "If the path doesn't exist, create it."
  [path]
  (when-not (dir? path)
    (mkdir path)))


(defn save-historical-data
  "Save the evolution data into a CSV file for statistics purposes.
  Example path: \"./ga_output\"."
  [path file-name data]
  (ensure-directory! path)
  (let [d (str/join "\n" data)]
    (with-open [w (io/writer (str path "/" file-name) :append true)]
      (.write w d))))


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
;;
;; (distance-between-cities [(first cities) (second cities)])
;; => 10.04987562112089


(defn total-distance
  "Calculate the total distance of the route."
  [route]
  (let [city-pairs (mapv #(vector %1 %2) (butlast route) (rest route))
        first-city (first route)
        last-city (last route)
        distances (map distance-between-cities city-pairs)
        ;; As city-pairs is a sequence of pairs from the first city to the last,
        ;; it misses the last pair which is last-city -> first-city. To make
        ;; this last calculation, calculate the distance between the last city
        ;; and the first to conclude the full circuit and sum it to the total.
        last-city-distance (distance-between-cities [last-city first-city])]
    (+ (reduce + distances) last-city-distance)))

;; Total distance of the cities-list:
;;
;; (total-distance cities)
;;
;; => 921.6346703083029


(defn fitness
  "Calculate the fitness based on the route total distance.
  As distance decreases, fitness increases.
  0 is the absolute best value."
  [total-dist]
  (/ 1.0 (+ 1 (pow total-dist 8))))

;; Fitness of the cities-list:
;;
;; (fitness (total-distance cities))
;;
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


(defn generate-initial-population
  "Generate the initial population for the genetic algorithm with random routes."
  [population-size cities-list]
  (repeatedly population-size
              (fn []
                (let [route (generate-route cities-list)
                      distance (total-distance route)
                      fit (fitness distance)]
                  (Individual. route distance fit)))))


(defn select-parents
  "Select a pair of parents from the population"
  [population]
  (loop [pop population
         parent-pairs []]
    (if (empty? pop)
      parent-pairs
      (recur (drop 2 pop) (conj parent-pairs (take 2 pop))))))


(defn combine-routes
  "Combine the cities not present in the first route
  with the cities of the second route."
  [first-route second-route]
  (let [fr-cities (set (map :name first-route))]
    (loop [sl second-route
           new-route first-route]
      (if (empty? sl)
        (do
          new-route)
        (let [city (first sl)
              is-present (contains? fr-cities (:name city))]
          (if is-present
            (recur (rest sl) new-route)
            (recur (rest sl) (conj new-route city))))))))


;; -----
;; ## Elitism
;; Select the best performing individuals.

(defn roulette-selection
  "With a random probability, pick the best individuals more often and
  the worse individuals less often."
  [population]
  (let [r (rand)
        pop (filter #(> (:fitness %) r) population)]
    (if (empty? pop)
      (rand-nth population)
      (rand-nth pop))))


(defn selection
  "Select the population based on roulette selection."
  [population population-size]
  (loop [cnt 1
         pop []]
    (if (> cnt population-size)
      pop
      (let [selected-individual (roulette-selection population)]
        (recur (inc cnt) (conj pop selected-individual))))))


;; -----
;; ## Crossover
;; Combine a pair of individuals into a new one


(defn crossover
  "Generate a new individual based on 2 parents.
  Select a random number of genes of parent 1 and fill with genes from parent 2."
  [parent-pair cities-list]
  (let [[parent-1 parent-2] parent-pair]
    (if parent-2
      (let [route-1 (vec (take 10 (:route parent-1)))
            route-2 (:route parent-2)
            new-route (combine-routes route-1 route-2)
            distance (total-distance new-route)
            fit (fitness distance)]
        (Individual. new-route distance fit))
      (let [route (generate-route cities-list)
            distance (total-distance route)
            fit (fitness distance)
            new-parent (Individual. route distance fit)]
        (crossover [parent-1 new-parent] cities-list)))))


(defn crossover-population
  "Crossover the population to generate new individuals."
  [population cities-list]
  (map #(crossover % cities-list) (select-parents population)))


;; -----
;; ## Mutation
;; Change the genes of an individual.

(defn swap-cities
  "Swap the order of two elements randomly for an individual given a probability."
  [individual mutation-rate]
  (if (< (rand) mutation-rate)
    (let [i (:route individual)
          r1 (rand-int (count i))
          r2 (rand-int (count i))
          first-city (nth i r1)
          second-city (nth i r2)
          s1 (assoc i r1 second-city)
          s2 (assoc s1 r2 first-city)
          distance (total-distance s2)
          fit (fitness distance)]
      (Individual. s2 distance fit))
    individual))


(defn mutate-population
  "Mutate each individual of a population."
  [population cities-list mutation-rate]
  (map #(swap-cities % mutation-rate) population))


;; -----
;; ## Genetic Algorithm
;;
;; The algorithms works as follows:
;; - Start with a random initial-population
;; - Normalize the fitness values
;; - Start a new generation
;; - Select the best individuals
;; - Crossover the individuals
;; - Mutate them
;; - Loop


(defn new-generation
  "Generate a new generation of a given population.
  A new generation is created based on selected individuals, which are crossed
  in pairs and then mutated."
  [population cities-list global-best population-size mutation-rate]
  (let [pop (conj population global-best) ; with this, new generations will
                                        ; not forget the best individual
        normalized-population (map #(normalize-fitness %1 pop) pop)
        selected-population (selection normalized-population (* 2 population-size))
        crossed-population (crossover-population selected-population cities-list)]
    (mutate-population crossed-population cities-list mutation-rate)))


(defn genetic-algorithm
  "Run the genetic algorithm."
  [initial-population
   cities-list
   generations
   population-size
   mutation-rate
   print-progress]
  (loop [generation 1
         population initial-population
         historical-best-distance []
         historical-distance []
         historical-fitness []
         global-best (first population)]
    (let [current-generation (map #(normalize-fitness %1 population) population)
          ;; Dont use normalized fitness values to select the best performing
          ;; individual as it is a percentage of the current population
          ;; and does not reflect the global best value.
          sorted-gen (sort-by :fitness population)
          gen-best (last sorted-gen)]
      (if print-progress
        (println (str "Generation: " generation
                      ", Best distance: " (:total-distance gen-best)
                      ", Best fitness: " (:fitness gen-best))))
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
                         global-best
                         population-size
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
(def generations 10000)

;; Number of individuals in each generation
(def population-size 500)

;; Rate at which the cities in a route will be swapped
(def mutation-rate 0.1)

;; All the cities to find the route for.
(def cities
  [(City. "1" -100	0)
   (City. "2" 100	0)
   (City. "3" 0	100)
   (City. "4" 0	-100)
   (City. "5" -90	10)
   (City. "6" -80	20)
   (City. "7" -70	30)
   (City. "8" -60	40)
   (City. "9" -50	50)
   (City. "10" -40	60)
   (City. "11" -30	70)
   (City. "12" -20	80)
   (City. "13" -10	90)
   (City. "14" 10	90)
   (City. "15" 20	80)
   (City. "16" 30	70)
   (City. "17" 40	60)
   (City. "18" 50	50)
   (City. "19" 60	40)
   (City. "20" 70	30)
   (City. "21" 80	20)
   (City. "22" 90	10)
   (City. "23" 90	-10)
   (City. "24" 80	-20)
   (City. "25" 70	-30)
   (City. "26" 60	-40)
   (City. "27" 50	-50)
   (City. "28" 40	-60)
   (City. "29" 30	-70)
   (City. "30" 20	-80)
   (City. "31" 10	-90)
   (City. "32" -10	-90)
   (City. "33" -20	-80)
   (City. "34" -30	-70)
   (City. "35" -40	-60)
   (City. "36" -50	-50)
   (City. "37" -60	-40)
   (City. "38" -70	-30)
   (City. "39" -80	-20)
   (City. "40" -90	-10)])


;; -----
;; ## Main Function

(defn -main
  "Run the genetic algorithm, print the best results and save the evolution-data."
  [& args]
  (println "\nStart Genetic Algorithm")
  (let [initial-population (generate-initial-population population-size cities)
        ga (genetic-algorithm initial-population
                               cities
                               generations
                               population-size
                               mutation-rate
                               true)
        best (:global-best ga)]
    (save-historical-data "./ga_output" "historical-best-distance.csv" (:historical-best-distance ga))
    (save-historical-data "./ga_output" "historical-distance.csv" (:historical-distance ga))
    (save-historical-data "./ga_output" "historical-fitness.csv" (:historical-fitness ga))
    (println "Lowest Distance: " (:total-distance best))
    (println "Best Route: " (map :name (:route best)))
    (println "Route size" (count (map :name (:route best))))
    (println "Best Fitness: " (:fitness best))))

;; Execute code in REPL:
;;
;; (-main)
