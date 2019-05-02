(ns linearsystems-part2.core
  (:use [uncomplicate.neanderthal core native])
  (:gen-class))

  (defn -main
    "I don't do a whole lot ... yet."
    [& args]
    (println "Hello, World!"))

(defn identity-matrix
  "Create an identity matrix of DIMENSION"
  [dimension]
  (entry! (dgd dimension) 1))

(defn outer-product
  "Returns the outer product of a vector"
  [input-vector]
  (let [dimension (dim input-vector)]
    (mm (view-ge input-vector dimension 1) (view-ge input-vector 1 dimension))))

(defn elementary-reflector
  "Build a matrix that will reflect vectors across the hyperplane orthogonal to REFLECTION-AXIS"
  [reflection-axis]
  (let [outer-product-matrix (outer-product reflection-axis)]
    (axpy!
     (dia (identity-matrix (dim reflection-axis)))
     (dia (scal! (/ -2 (dot reflection-axis reflection-axis))
		 outer-product-matrix)))
    outer-product-matrix))

(defn elementary-coordinate-reflector
  "Build a matrix that will reflect the INPUT-VECTOR on to the COORDINATE-AXIS"
  [input-vector coordinate-axis]
  (scal! (nrm2 input-vector) coordinate-axis)   ; scale coordinate axis
  (if (= input-vector coordinate-axis) ; degenerate case
    (identity-matrix (dim input-vector))                           ; return identity matrix
    (do (axpy! -1                      ; make input-vector orthogonal to the bisecting plane
	       coordinate-axis
	       input-vector)
	(elementary-reflector input-vector)))) ; return the reflector

;; (defn first-elementary-coordinate-reflector
;;   "Build a matrix that will reflect the INPUT-VECTOR on to the first elementary vector [ 1 0 0 .. 0 ]"
;;   [input-vector]
;;   (elementary-coordinate-reflector input-vector
;;                                          (get-row (identity-matrix (dimension-count input-vector 0)) 0)))

;; (defn raise-rank
;;   "Add a row and column of zeroes to the top left of a matrix. With a 1 in the top left position (0,0)
;;   Optionally pass in a RANK variable to pad with that many rows (default: 1)"
;;   ([input-matrix]
;;    (raise-rank input-matrix 1))
;;   ([input-matrix rank]
;;    (if (zero? rank)
;;      input-matrix
;;      (raise-rank
;;       (join-along 1 (column-matrix (get-column (identity-matrix (inc (row-count input-matrix))) 0))
;;                   (join-along 0 (row-matrix (zero-vector (column-count input-matrix)))
;;                               input-matrix))
;;       (dec rank)))))

;; (defn raise-rank-and-insert-row-column
;;   "Takes a submatrix and put it's in the lower right corner of a larger matrix.
;;   The submatrix is 1 row and column smaller.
;;   First insert a column (size of input-matrix  and then a row (size + 1)"
;;   [input-matrix insert-column insert-row]
;;   (join-along 0 (row-matrix insert-row)
;;               (join-along 1 (column-matrix insert-column)
;;                           input-matrix)))

;; (defn raise-rank-and-insert-row
;;   "Takes a submatrix and put it's in the lower right corner of a larger matrix.
;;   The submatrix is 1 row and column smaller
;;   First insert a column of zeroes and then the passed in row (size + 1)"
;;       [input-matrix insert-row]
;;       (raise-rank-and-insert-row-column
;;        input-matrix
;;        (zero-vector (column-count input-matrix))
;;        insert-row))

;; (defn householder-QR
;;   "Use reflection matrices to build the QR matrix. Returns a [Q^T R] pair"
;;   [input-matrix]
;;   (let [reflector-to-zero-out-first-column
;;         (first-elementary-coordinate-reflector (get-column input-matrix 0))
;;         input-matrix-with-first-column-zeroed-out
;;         (mmul reflector-to-zero-out-first-column input-matrix)]
;;     (if
;;         ;; Base Case: We're out of columns/rows to reduce
;;         ;;            Return the reflector and the reduced column
;;         (or (= (column-count input-matrix) 1) (= (row-count input-matrix) 1))
;;         [reflector-to-zero-out-first-column input-matrix-with-first-column-zeroed-out]
;;         ;; Recursive step: Get the Q^{-1}R of the submatrix
;;         ;;                 Then and combine it with your reflector and reduced matrix
;;         (let [submatrix (submatrix
;;                          input-matrix-with-first-column-zeroed-out
;;                          1 (dec (row-count input-matrix))
;;                          1 (dec (column-count input-matrix)))
;;               [submatrix-Q submatrix-R] (householder-QR submatrix)]
;;           [(mmul (raise-rank submatrix-Q)
;;                  reflector-to-zero-out-first-column)
;;            (raise-rank-and-insert-row submatrix-R
;;                                       (get-row input-matrix-with-first-column-zeroed-out 0))]))))

;; (defn hessenberg-form-first-partial-reflector
;;   "Builds a matrix that will reduce the first column of INPUT-MATRIX to  Hessenberg Form"
;;   [input-matrix]
;;   (if
;;       ;; Degenerate Case: 1 x 1 matrix
;;       (or (= (column-count input-matrix) 1) (= (row-count input-matrix) 1))
;;     [[ 1 ]]
;;   (let [first-column (get-column input-matrix 0)
;;         subdiagonal-column (subvector first-column 1 (dec (row-count first-column)))
;;         orthogonal-reducer (first-elementary-coordinate-reflector subdiagonal-column)]
;;     (raise-rank orthogonal-reducer))))

;; (defn hessenberg-form-reduction
;;   "Reduce the INPUT-MATRIX to  Hessenberg Form  - H , using reflectors - P. Result will be in the form [P^T H]"
;; [input-matrix]
;; (let [reflector-to-zero-out-first-column
;;       (hessenberg-form-first-partial-reflector input-matrix)
;;       input-matrix-with-first-column-zeroed-out
;;       (mmul reflector-to-zero-out-first-column input-matrix (transpose reflector-to-zero-out-first-column))]
;;   (if
;;       ;; Base Case: We're out of columns/rows to reduce
;;       ;;            Return the reflector and the reduced column
;;       (or (= (column-count input-matrix) 1) (= (row-count input-matrix) 1))
;;       [reflector-to-zero-out-first-column input-matrix-with-first-column-zeroed-out]
;;       ;; Recursive step: Get the Q^{-1}R of the submatrix
;;       ;;                 Then and combine it with your reflector and reduced matrix
;;       (let [submatrix (submatrix
;;                        input-matrix-with-first-column-zeroed-out
;;                        1 (dec (row-count input-matrix))
;;                        1 (dec (column-count input-matrix)))
;;             [submatrix-P submatrix-H] ( hessenberg-form-reduction submatrix)]
;;         [(mmul (raise-rank submatrix-P)
;;                reflector-to-zero-out-first-column)
;;          (raise-rank-and-insert-row-column submatrix-H
;;                                    (subvector (get-column input-matrix-with-first-column-zeroed-out 0) 1 (dec (row-count input-matrix-with-first-column-zeroed-out)))
;;                                    (get-row input-matrix-with-first-column-zeroed-out 0))]))))

(defn matrix-template
"template"
[matrix]
)
