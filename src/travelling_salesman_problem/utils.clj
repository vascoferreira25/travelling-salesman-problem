(ns travelling-salesman-problem.utils
  (:gen-class)
  (:require [criterium.core :as criterium]
            [uncomplicate.neanderthal
    [native :refer [dv ; double precision vector
                    dge ; double precision dense matrix
                    fv ; float vector
                    fge ; float dense matrix
                    native-double ; default double precision factory
                    native-float]] ; default single precision factory
    [core :refer [submatrix ; returns submatrix of matrix a, from
                            ; row i, column, j, that has k rows and l columns
                  native ; ensures that x is the native main memory
                  iamax ; index of the absolute max value
                  imax ; the index of the max value
                  iamin ; index of the absolute min value
                  imin ; the index of the min value
                  asum ; absolute value of all the elements
                  sum ; sum of the elements
                  dot ; dot product of matrices x and y
                  dim ; returns the dimension of vector x
                  mm ; matrix multiplication - returns new instance
                  ge ; dense matrix in the context of factory
                  scal ; multiplies all entries of x by scaler alpha
                  trans ; transposes matrix
                  trans! ; transposes matrix in-place
                  col ; returns the j column of matrix a
                  cols ; returns lazy sez of column vectors of a
                  row ; returns the i row of matrix a
                  rows ; returns lazy seq of row vectors of a
                  ]]
    [math :refer [abs ; absolute
                  sqrt ; square root
                  log ; logarithm
                  sin ; sin function
                  cos ; cos function
                  pi ; pi number
                  pow ; power x by y
                  sqr]]
    [random :refer [rand-normal! ; populate vector with normal distributed values
                                 ; rng-state, mu (average), sigma (std-dev), x
                    rand-uniform! ; populate vector with uniform distributed values
                                  ; rng-state, [lower], [upper], x
                    rng-state]]]))

(defn shuffle-vec
  "Shuffle a vector randomly."
  [v]
  (loop [vec-left v
         shuffled-vec []]
    (if (empty? vec-left)
      shuffled-vec
      (let [vec-count (count vec-left)
            r (rand-int vec-count)
            sub-1 (subvec vec-left 0 r)
            sub-2 (subvec vec-left (inc r))
            vl (vec (concat sub-1 sub-2))]
        (recur vl (conj shuffled-vec (nth vec-left r)))))))


(defn )
