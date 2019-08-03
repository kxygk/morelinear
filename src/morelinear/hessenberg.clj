(ns morelinear.hessenberg
  (:require [clojure.core.matrix :refer :all
	     morelinear.householder :refer :all]))

(defn first-subcolumn-reflector
  "Build a matrix that will reflect the first column of INPUT-MATRIX 
  on to the first elementary vector [ 1 0 0 .. 0 ]"
  [input-matrix]
  (let [dimension (dec (dimension-count input-matrix
					0))]
    (block-diagonal-matrix [[[1.0]]
			    (elementary-coordinate-reflector (subvector (get-column input-matrix 0)
									1
									dimension)
							     (get-row (identity-matrix dimension) 0))])))

(defn hessenberg-reduction
  "Increase the dimension of a reflector by padding it with an identity matrix"
  [reduction-matrix input-matrix]
  (if (= 2 (row-count input-matrix))
    reduction-matrix
    (let [reflector (first-subcolumn-reflector input-matrix)]
      (do (assign! input-matrix
		   (mmul reflector
			 input-matrix
			 reflector))
	  (recur (mmul reduction-matrix
		       (block-diagonal-matrix [(identity-matrix (- (row-count reduction-matrix)
								   (row-count input-matrix)))
					       reflector]))
		 (submatrix input-matrix
			    1
			    (dec (row-count input-matrix))
			    1
			    (dec (column-count input-matrix))))))))

(defn hessenberg
  "A wrapper for the real function"
  [input-matrix]
  (hessenberg-reduction (identity-matrix (row-count input-matrix))
		       input-matrix))
