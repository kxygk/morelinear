(ns morelinear.core
  (:require [clojure.core.matrix :refer :all])  ;[denisovan.core :as den]
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

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
  (if (or (= 1 (row-count input-matrix))
	  (= 1 (column-count input-matrix))) 
    input-matrix ;; base case
    (do (assign! input-matrix
		 (mmul (first-column-reflector input-matrix)
		       input-matrix))
	(recur (submatrix input-matrix
			  1
			  (dec (row-count input-matrix))
			  1
			  (dec (column-count input-matrix)))))))

(defn householder-reduce-to-QR
  "Increase the dimension of a reflector by padding it with an identity matrix"
  [reduction-matrix input-matrix]
  (if (or (= 0 (row-count input-matrix))
	  (= 0 (column-count input-matrix)))
    reduction-matrix ;; base case
    (let [reflector (first-column-reflector input-matrix)]
      (do (assign! input-matrix
		   (mmul reflector
			 input-matrix))
	  (recur (mmul reduction-matrix
		       (block-diagonal-matrix [(identity-matrix (- (row-count reduction-matrix)
								   (row-count input-matrix)))
					       reflector]))
		 (submatrix input-matrix
			    1
			    (dec (row-count input-matrix))
			    1
			    (dec (column-count input-matrix))))))))

(defn householder-QR
  "A wrapper for the real function"
  [input-matrix]
  (householder-reduce-to-QR (identity-matrix (row-count input-matrix))
			    input-matrix))

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

(defn matrix-template
"template"
[matrix]
)
