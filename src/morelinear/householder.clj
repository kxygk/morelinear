(ns morelinear.householder
  (:require [clojure.core.matrix :refer :all]))

(defn elementary-reflector
  "Build a matrix that will reflect vector across the hyperplane orthogonal to REFLECTION-AXIS"
  [reflection-axis]
  (let [dimension (dimension-count reflection-axis 0)]
    (sub (identity-matrix dimension)
	 (mul (outer-product reflection-axis reflection-axis)
	      (/ 2 (length-squared reflection-axis))))))

(defn elementary-coordinate-reflector
 "Build a matrix that will reflect the INPUT-VECTOR on to the COORDINATE-AXIS"
 [input-vector coordinate-axis] 
 (let [vector-orthogonal-to-reflection-plane
       (sub input-vector
            (mul coordinate-axis
                 (length input-vector)))]
   (if (zero-matrix? vector-orthogonal-to-reflection-plane)
     ;; degenerate case where the input is on the coordinate axis
     (identity-matrix (dimension-count input-vector 0))
     ;; normal case
     (elementary-reflector vector-orthogonal-to-reflection-plane))))

(defn first-column-reflector
  "Build a matrix that will reflect the first column of INPUT-MATRIX 
  on to the first elementary vector [ 1 0 0 .. 0 ]"
  [input-matrix]
  (elementary-coordinate-reflector (get-column input-matrix
					       0)
				   (get-row (identity-matrix (dimension-count input-matrix 0)) 0)))

(defn reduce-to-r
  "Reduce a matrix to a lower triangular orthonormal matrix"
  [input-matrix]
  (do (assign! input-matrix
	       (mmul (first-column-reflector input-matrix)
		     input-matrix))
      (if (or (= 1 (row-count input-matrix))
	      (= 1 (column-count input-matrix)))
	input-matrix ;; base case
	(recur (submatrix input-matrix
			  1
			  (dec (row-count input-matrix))
			  1
			  (dec (column-count input-matrix)))))))

(defn- make-padded-reflector
  "Make a reflector that leaves the first columns untouched"
  [padding-size reflector]
  (let [padding (identity-matrix padding-size)]
    (join-along 0
		(join-along 1
			    padding
			    (zero-matrix (row-count padding)
					 (column-count reflector)))
		(join-along 1
			    (zero-matrix (row-count reflector)
					 (column-count padding))
			    reflector))))
;; crazy code b/c `block-diagonal-matrix` code is broken
;; see: https://github.com/mikera/vectorz-clj/issues/70


(defn- reduce-to-qr
  "Increase the dimension of a reflector by padding it with an identity matrix"
  [reduction-matrix input-matrix]
  (let [reflector (first-column-reflector input-matrix)
	padding-size (- (row-count reduction-matrix)
			(row-count input-matrix))
	padding (identity-matrix padding-size)
	padded-reflector (if (zero? padding-size)
			   reflector
			   (make-padded-reflector padding-size
						  reflector))]
    (do (assign! input-matrix
		 (mmul reflector
		       input-matrix))
	(if (or (= 1 (row-count input-matrix))
		(= 1 (column-count input-matrix)))
	  reduction-matrix ;; base case
	  (recur (mmul reduction-matrix
		       padded-reflector)
		 (submatrix input-matrix
			    1
			    (dec (row-count input-matrix))
			    1
			    (dec (column-count input-matrix))))))))
  (defn qr!
    "A wrapper for the real function"
    [input-matrix]
    (reduce-to-qr (identity-matrix (row-count input-matrix))
			      input-matrix))
