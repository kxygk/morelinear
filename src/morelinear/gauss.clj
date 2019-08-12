(ns morelinear.gauss
  (:require [clojure.core.matrix :refer :all]
	    [clojure.core.matrix.linear :as linear]))
(set-current-implementation :vectorz)

(defn- forward-substitution-rec
  ""
  [lower-triangular output-vector partial-input-vector step-number]

  (let [substitution-sum (scalar (inner-product (subvector (get-row lower-triangular
								    0)
							   0
							   step-number)
						(subvector partial-input-vector
							   0
							   step-number)))

	new-input-value (/ (- (mget output-vector
				    step-number)
			      substitution-sum)
			   (mget lower-triangular
				 0
				 step-number))]
    (if (= step-number
           (dec (ecount partial-input-vector)))
      (mset partial-input-vector 
            step-number
            new-input-value)
      (recur (matrix (submatrix lower-triangular
			        1
			        (dec (row-count lower-triangular))
			        0
			        (column-count lower-triangular)))
	     output-vector
	     (mset partial-input-vector 
	           step-number
	           new-input-value)
	     (inc step-number)))))

(defn forward-substitution
  "Lx=b by forward subsitution, where L is lower triangular"
  [lower-triangular output-vector]
  (forward-substitution-rec lower-triangular
			    output-vector
			    (zero-array (shape output-vector))
			    0))

(defn backward-substitution
  "Ux=b by backward subsitution, where U is upper triangular"
  [upper-triangular output-vector]
  (reshape (reverse (to-vector (forward-substitution
                                (reshape (reverse (to-vector upper-triangular))
	                                 (shape upper-triangular))
                                (reshape (reverse (to-vector output-vector))
	                                 (shape output-vector)))))
           (shape output-vector)))
;; flips the matrix around to make it lower triangular..
;; then reuses the forward-substitution code
;; finally flips the result. A bit ugly, but short and easier to debug

(defn solve-with-lu
  "Solve Ax=b directly using Gaussian elimination with backward/forward substitution"
  [A b]
  (let [{L :L
	 U :U
	 P :P} (linear/lu A)
	Pb (mmul P b)
	y (forward-substitution L Pb)]
    (backward-substitution U y)))
