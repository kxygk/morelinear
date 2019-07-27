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
  "Build a matrix that will reflect the INPUT-MATRIX on to the first elementary vector [ 1 0 0 .. 0 ]"
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
  (if (or (= 1 (row-count input-matrix))
	  (= 1 (column-count input-matrix)))
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

(defn hessenberg-form-first-partial-reflector
  "Builds a matrix that will reduce the first column of INPUT-MATRIX to  Hessenberg Form"
  [input-matrix]
  (if
      ;; Degenerate Case: 1 x 1 matrix
      (or (= (column-count input-matrix) 1) (= (row-count input-matrix) 1))
    [[ 1 ]]
  (let [first-column (get-column input-matrix 0)
        subdiagonal-column (subvector first-column 1 (dec (row-count first-column)))
        orthogonal-reducer (first-column-reflector subdiagonal-column)]
    (raise-rank orthogonal-reducer))))

(defn hessenberg-form-reduction
  "Reduce the INPUT-MATRIX to  Hessenberg Form  - H , using reflectors - P. Result will be in the form [P^T H]"
[input-matrix]
(let [reflector-to-zero-out-first-column
      (hessenberg-form-first-partial-reflector input-matrix)
      input-matrix-with-first-column-zeroed-out
      (mmul reflector-to-zero-out-first-column input-matrix (transpose reflector-to-zero-out-first-column))]
  (if
      ;; Base Case: We're out of columns/rows to reduce
      ;;            Return the reflector and the reduced column
      (or (= (column-count input-matrix) 1) (= (row-count input-matrix) 1))
      [reflector-to-zero-out-first-column input-matrix-with-first-column-zeroed-out]
      ;; Recursive step: Get the Q^{-1}R of the submatrix
      ;;                 Then and combine it with your reflector and reduced matrix
      (let [submatrix (submatrix
                       input-matrix-with-first-column-zeroed-out
                       1 (dec (row-count input-matrix))
                       1 (dec (column-count input-matrix)))
            [submatrix-P submatrix-H] ( hessenberg-form-reduction submatrix)]
        [(mmul (raise-rank submatrix-P)
               reflector-to-zero-out-first-column)
         (raise-rank-and-insert-row-column submatrix-H
                                   (subvector (get-column input-matrix-with-first-column-zeroed-out 0) 1 (dec (row-count input-matrix-with-first-column-zeroed-out)))
                                   (get-row input-matrix-with-first-column-zeroed-out 0))]))))

(defn matrix-template
"template"
[matrix]
)
