(ns sicp)

;; util functions and values
(def tolerance 1e-8)

(defn close-enough? [a b]
    (< (Math/abs (- a b)) tolerance))

(defn average [x y] (/ (+ x y) 2))

(defn square [x] (* x x))

(defn cube [x] (* x x x))

(def pow #(Math/pow %1 %2))

;; procedure functions
(defn fixed-point [f init-value]
    (defn- fixed-point-iter [guess]
        ;(println guess)
        (let [new-guess (f guess)]
            (if (close-enough? guess new-guess)
                new-guess 
                (recur new-guess))))
    (fixed-point-iter init-value))

(defn fixed-point-of-transform [g transform guess]
    (fixed-point (transform g) guess))

(defn average-damp [f]
    (fn [x] (average x (f x))))

; transform function g(x) to f(x) that fixed point of f is 
; zero point of g
(def dx 0.000001)

(defn deriv [g]
    (fn [x] (/ (- (g (+ x dx)) (g x)) dx)))

(defn newton-transform [g]
    (fn [x] (- x (/ (g x) ((deriv g) x)))))


;; using fixed point method to calculate `sqrt` and `cube-root`.
;; v1 means using `fixed-point` and v2 means `fixed-point-transform`.
;; The results of v1 and v2 with the same method should be identical.
(defn sqrt-damp-v1 [x] 
    (fixed-point (average-damp (partial / x)) 1.0))
(defn sqrt-damp-v2 [x] 
    (fixed-point-of-transform (partial / x) average-damp 1.0))

(defn sqrt-newton-v1 [x]
    (fixed-point (newton-transform #(- (square %1) x)) 1.0))
(defn sqrt-newton-v2 [x]
    (fixed-point-of-transform #(- (square %1) x) newton-transform 1.0))

(defn cube-root-damp-v1 [x] 
    (fixed-point (average-damp #(/ x (square %1))) 1.0))
(defn cube-root-damp-v2 [x] 
    (fixed-point-of-transform #(/ x (square %1)) average-damp 1.0))

(defn cube-root-newton-v1 [x] 
    (fixed-point (newton-transform #(- (cube %1) x)) 1.0))
(defn cube-root-newton-v2 [x] 
    (fixed-point-of-transform #(- (cube %1) x) newton-transform 1.0))

;; attach symbols `sqrt` and `cube-root` to a certain method and version
;; e.g., if we attach `sqrt` to `sqrt-damp-v1`, calling to `sqrt` will
;; call `sqrt-damp-v1`. 
(defmacro attach [method version]
    (conj 
        (for [func ["sqrt" "cube-root"]]
            (list 'def (symbol func) (symbol (str func '- method '- version)))
        )
        'do
    )
)

;(defn attach [] (def sqrt sqrt-damp))

(def res (fixed-point #(Math/cos %1) 1.0))
(println res)

;(println (macroexpand '(attach "damp" "v1")))

(attach "damp" "v1")
;(attach "newton" "v1")
;(System/exit 1)

;(def res (sqrt-damp-v1 2))
(def res (sqrt 2))
(println res (square res))

(def res (cube-root 2))
(println res (cube res))
