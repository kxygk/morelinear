(ns morelinear.vandermonde
  (:require [clojure.core.matrix :refer :all]
	    [clojure.core.matrix.linear :as linear]))
(set-current-implementation :vectorz)

(defn polynomial-matrix
  "Take a vector of input X's and build a vandermonde matrix.
  The DEGREE shouldn't be larger than the number of values given.
  If non provided it will be chosen to be the maximal value
  and your resulting matrix will be square"
  ([polynomial-input-values
    degree]
   (mapv (fn [x]
	   (mapv (fn [power]
		   (pow x power))
		 (range degree)))
	 polynomial-input-values))

  ;; Default square matrix case
  ([polynomial-input-values]
   (polynomial-matrix polynomial-input-values
		      (ecount polynomial-input-values))))
