;; Notice: `repeated` function means repeat N time of a function `foo`,
;; which has parameter `x`. `x` is a value regularly, e.g. squre(x).
;; But in the case of "smooth", we will repeat N time of `smooth`, but 
;; `smooth` is a function of function, that's a little difference which makes
;; me confused at first.
;; 
(defn compose [f g] (fn [x] (f (g x))))

(defn repeated-v1 [f times]
    (defn- repeated-iter [n x]
        ;(println x)
        (if (<= n 0)
            x
            (recur (dec n) (f x))))
    (fn [args] (repeated-iter times args))
)

(defn repeated-v2 [f times]
    (cond (<= times 0) #(%)
        (= times 1) f
        true (compose (repeated-v2 f (dec times)) f)
    )
)

(def repeated repeated-v1)

(defn square [x]
    ;(println "cal: " x)   ; for debug
    (* x x)
)

(println ((repeated square 2) 5))
(println ((repeated #(Math/cos %) 10) 1.0))

(defn mean [& args] (/ (apply + args) (count args)))

(def dx 0.1)
(defn smooth [f] 
    (fn [x] 
        (mean (f (- x dx)) (f x) (f (+ x dx)))
    )
)

(println ((smooth square) 1))
(println ((smooth (smooth square)) 1))
; use repeated, should be identical to the above
(println (((repeated smooth 2) square) 1))