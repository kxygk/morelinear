(ns morelinear.leastsquares
  (:require [clojure.core.matrix :refer :all]
	    [clojure.core.matrix.linear :as linear]
	    [morelinear.gauss :as gauss]
	    [morelinear.householder :as householder]))
(set-current-implementation :vectorz)

(defn lu-direct
  "Solve ATA=ATb directly"
  [linear-system output-vector]
  (let [AT (transpose linear-system)
	ATA (mmul AT linear-system)
	ATb (mmul AT output-vector)]
    (gauss/solve-with-lu ATA ATb)))

(defn- jumbo-matrix
  "Use the big block matrix method to compute the least squares solution"
  [input-matrix]
  (let [num-rows (row-count input-matrix)
	num-columns (column-count input-matrix)
	identity (identity-matrix num-rows)
	AT (transpose input-matrix)
	zeroes (zero-matrix num-columns
			    num-columns)]
    (join-along 0
		(join-along 1
			    identity
			    input-matrix)
		(join-along 1
			    AT
			    zeroes))))

(defn- pad-with-zeroes
  "Takes the INPUT-VECTOR and make it longer with some zeroes padded on the end"
  [input-vector number-of-zeroes]
  (join-along 0
	      input-vector
	      (zero-vector number-of-zeroes)))

(defn lu-jumbo
  ""
  [input-matrix output-vector]

  (subvector (gauss/solve-with-lu (jumbo-matrix input-matrix)
				  (pad-with-zeroes output-vector
						   (column-count input-matrix)))
	     (row-count input-matrix)
	     (column-count input-matrix)))

(defn householder-qr
  ""
  [A b]
  (let [R (mutable A)
	Q (householder/qr! R)
	QT (transpose Q)
	QTb (mmul QT b)]
    (gauss/backward-substitution R QTb)))
