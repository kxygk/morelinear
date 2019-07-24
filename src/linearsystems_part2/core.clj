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

(defn self-outer-product
  "Returns the outer product of a vector with itself"
  [input-vector]
  (view-sy (rk input-vector
	       input-vector)))

(defn elementary-reflector
  "Build a matrix that will reflect vectors across the hyperplane orthogonal to REFLECTION-AXIS"
  [reflection-axis]
  (let [outer-product-matrix (self-outer-product reflection-axis)]
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
    (elementary-reflector (axpy -1                      ; make input-vector orthogonal to the bisecting plane
				coordinate-axis
				input-vector))))

(defn elementary-vector
  "Make an elemntary vector of INDEX and DIMENSION"
  [index dimension]
  (dv (assoc (into [] (repeat dimension 0)) index 1)))

(defn first-column-reflector
  "Build a matrix that will reflect the INPUT-MATRIX such that the first column end up on [ 1 0 0 .. 0 ]"
  [input-matrix]
  (elementary-coordinate-reflector (col input-matrix 0)
				   (elementary-vector 0 (mrows input-matrix))))

;;(dv (assoc (into [] (repeat (mrows input-matrix) 0)) 0 1))

(defn reduce-to-r
  "Reduce a matrix to a lower triangular orthonormal matrix"
  [input-matrix]
  (if (= 1 (dim input-matrix)) ;; base case - 1x1 matrix .. nothing to reduce
    (input-matrix)
    (do (mm! (first-column-reflector input-matrix)
	     input-matrix)
	(recur (submatrix input-matrix
			   1
			   1
			   (dec (mrows input-matrix))
			   (dec(ncols input-matrix)))))))

(defn matrix-template
"template"
[matrix]
)
