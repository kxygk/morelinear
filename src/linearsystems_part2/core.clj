
(ns linearsystems-part2.core
  (:require [clojure.core.matrix :refer :all])  ;[denisovan.core :as den]
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn matrix-elementary-reflector
  "Build a matrix that will reflect vector across the hyperplane orthogonal to REFLECTION-AXIS"
  [reflection-axis]
  (let [dimension (dimension-count reflection-axis 0)]
    (sub (identity-matrix dimension)
         (mul (outer-product reflection-axis reflection-axis)
              (/ 2 (length-squared reflection-axis))))))

(defn matrix-elementary-coordinate-reflector
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
     (matrix-elementary-reflector vector-orthogonal-to-reflection-plane))))

(defn raise-rank
  "Add a row and column of zeroes to the top left of a matrix. With a 1 in the top left position (0,0)"
  [input-matrix]
  (join-along 1 (column-matrix (get-column (identity-matrix (inc (row-count input-matrix))) 0))
      (join-along 0 (row-matrix (zero-vector (column-count input-matrix)))
            input-matrix)))

(defn raise-rank-and-insert-row
  "Takes a submatrix and put it's in the lower right corner of a larger matrix.
   The submatrix is 1 row and column smaller"
  [input-matrix insert-row]
  (join-along 0 (row-matrix insert-row)
              (join-along 1 (column-matrix (zero-vector (column-count input-matrix)))
                    input-matrix)))

(defn matrix-householder-QR
  "Use reflection matrices to build the QR matrix. Returns a [Q^T R] pair"
  [input-matrix]
  (let [reflector-to-zero-out-first-column
        (matrix-elementary-coordinate-reflector
         (get-column input-matrix 0)
         (get-row (identity-matrix
                   (row-count input-matrix)) 0))
        input-matrix-with-first-column-zeroed-out
        (mmul reflector-to-zero-out-first-column input-matrix)]
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
              [submatrix-Q submatrix-R] (matrix-householder-QR submatrix)]
          [(mmul (raise-rank submatrix-Q)
                 reflector-to-zero-out-first-column)
           (raise-rank-and-insert-row submatrix-R
                                      (get-row input-matrix-with-first-column-zeroed-out 0))]))))

(defn matrix-template
"template"
[matrix]
)
