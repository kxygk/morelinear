(ns morelinear.vandermonde
  (:require [morelinear.leastsquares :as leastsquares]
	    [clojure.core.matrix :refer :all]
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

(defn split-into-x-y
  [points]
  (let [vector-of-xs-and-ys (apply mapv vector points)]
    {:x (mutable (first vector-of-xs-and-ys))
     :y (mutable (second vector-of-xs-and-ys))}))


(defmulti polynomial-factors
  "Solve for a polynomial equation of DEGREE that fits the given POINTS
  and return the list of polynomial factors.
  the underlying least squares METHOD used can be:
  :lu
  :lu-jumbo
  :householder"
  (fn [degree
       points
       method]
    method))

(defmethod polynomial-factors :lu
  [degree
   points
   method]
  (let [xy (split-into-x-y points)]
    (leastsquares/lu-direct (polynomial-matrix (:x xy)
					       degree)
			    (:y xy))))

(defmethod polynomial-factors :lu-jumbo
  [degree
   points
   method]
  (let [xy (split-into-x-y points)]
    (leastsquares/lu-jumbo (polynomial-matrix (:x xy)
					      degree)
			   (:y xy))))

(defmethod polynomial-factors :householder
  [degree
   points
   method]
  (let [xy (split-into-x-y points)
	vandermonde-matrix (mutable (polynomial-matrix (:x xy)
						       degree))]
    (leastsquares/householder-qr vandermonde-matrix
				 (:y xy))))
