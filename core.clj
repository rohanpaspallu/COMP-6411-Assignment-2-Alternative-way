(ns treasure.core
  (:gen-class))

(require ['clojure.string :as 'str])

(defn callMap [filename]
  (def readline (slurp filename))
  (clojure.string/split readline #"\r?\n"))


(defn read-in-board [map_file]
  (str/split-lines (slurp map_file)))


(defn adjacentMatrix [x y]
  (def top-down (make-array Integer/TYPE (* x y) 2))
  (def right-left (make-array Integer/TYPE (* x y) 2))
  (def i (atom 0))
  (while (< @i (* x y))
    (do
      (def w (mod @i y))
      (def h (/ (- @i w) y))
      (def top (+ (* y (- h 1)) w))
      (def down (+ (* y (+ h 1)) w))
      (def right (+ (* y h) (+ w 1)))
      (def left (+ (* y h) (- w 1)))

      (cond
        (and (>= (- @i 1) 0) (>= (- w 1) 0)) (aset-int right-left @i 1 left)
        :else (aset-int right-left @i 1 -1))
      (cond
        (and (< (+ @i 1) (* x y)) (< (+ w 1) y)) (aset-int right-left @i 0 right)
        :else (aset-int right-left @i 0 -1))
      (cond
        (and (>= (- h 1) 0) (>= (- @i y) 0)) (aset-int top-down @i 0 top)
        :else (aset-int top-down @i 0 -1))
      (cond
        (and (< (+ h 1) x) (< (+ @i y) (* x y))) (aset-int top-down @i 1 down)
        :else (aset-int top-down @i 1 -1))

      ;;(println @i "-->" top right down (aget right-left @i 1))
      ;;(println @i "-->" (aget top-down @i 0) (aget right-left @i 0) (aget top-down @i 1) (aget right-left @i 1))
      (swap! i inc))))


(defn papu [iinit-val, end-val, visited, ppl, xx, yy]
  (aset-int visited iinit-val 0 1)
  (if (= iinit-val end-val)
    (do
      (println "Woo hoo, I found the treasure :-)")
      (println " I can't display the output path because of time constraints ")
      ;(aset-int visited end-val 0 0
      (System/exit 0)))
  ;;(println (aget top-down iinit-val 0) (aget top-down iinit-val 1))
  (if (>= (aget top-down iinit-val 0) 0)
    (do
      (def t (aget top-down iinit-val 0))
      (def w1 (mod t yy))
      (def h1 (/ (- t w1) yy))
      (def tempLine (get (clojure.string/split readline #"\r?\n") h1))
      (def tempLine2 (get (clojure.string/split tempLine #"") w1))
      (if (and (or (= tempLine2 "@") (= tempLine2 "-")) (= (aget visited t 0) 0))
        (do
          (aset-int ppl t 0 1)
          (papu t end-val visited ppl xx yy)
          (aset-int ppl t 0 1)))))
  (if (>= (aget top-down iinit-val 1) 0)
    (do
      (def t (aget top-down iinit-val 1))
      (def w1 (mod t yy))
      (def h1 (/ (- t w1) yy))
      (def tempLine (get (clojure.string/split readline #"\r?\n") h1))
      (def tempLine2 (get (clojure.string/split tempLine #"") w1))
      (if (and (or (= tempLine2 "@") (= tempLine2 "-")) (= (aget visited t 0) 0))
        (do
          (aset-int ppl t 0 1)
          (papu t end-val visited ppl xx yy)
          (aset-int ppl t 0 1)))))
  (if (>= (aget right-left iinit-val 0) 0)
    (do
      (def t (aget right-left iinit-val 0))
      (def w1 (mod t yy))
      (def h1 (/ (- t w1) yy))
      (def tempLine (get (clojure.string/split readline #"\r?\n") h1))
      (def tempLine2 (get (clojure.string/split tempLine #"") w1))
      (if (and (or (= tempLine2 "@") (= tempLine2 "-")) (= (aget visited t 0) 0))
        (do
          (aset-int ppl t 0 1)
          (papu t end-val visited ppl xx yy)
          (aset-int ppl t 0 1)))))

  (if (>= (aget right-left iinit-val 1) 0)
    (do
      (def t (aget right-left iinit-val 1))
      (def w1 (mod t yy))
      (def h1 (/ (- t w1) yy))
      (def tempLine (get (clojure.string/split readline #"\r?\n") h1))
      (def tempLine2 (get (clojure.string/split tempLine #"") w1))
      (if (and (or (= tempLine2 "@") (= tempLine2 "-")) (= (aget visited t 0) 0))
        (do
          (aset-int ppl t 0 1)
          (papu t end-val visited ppl xx yy))))))


(defn pap [init-val, end-val, xx, yy]

  (def visited (make-array Integer/TYPE (* xx yy) 1))
  (def pl (make-array Integer/TYPE (* xx yy) 1))
  (aset-int pl init-val 0 1)
  ;;(println xx yy)
  (papu init-val end-val visited pl xx yy)
  (println "Uh oh, I could not find the treasure :-(")
  (println " I can't display the output path because of time constraints "))

(defn main []
  (println "THIS IS TREASURE GAME")
  (callMap "map.txt")
  (println readline)
  (def x (count (clojure.string/split readline #"\r?\n"))) ;;rows
  (def y (count (get (clojure.string/split readline #"\r?\n") 0)))  ;;columns
  ;;(println x y)
  (def i (atom 0))
  (while (< @i x)
    (do
      (def latest (get (clojure.string/split readline #"\r?\n") @i))
      (def currentCount (count latest))
      ;;(println @i currentCount)
      (cond
        (not= currentCount y)(System/exit 0))
      (swap! i inc)))
  (def str1(mapv vec (read-in-board "map.txt")))
  ;;(println str1)
  (def strArr (to-array-2d str1))

  (doseq
    [[value row] (map vector strArr (range))]
    (doseq [[value1 column] (map vector value (range))]
      (if (= (str (aget strArr row column)) "@")
        (def end-row row))
      (if (= (str (aget strArr row column)) "@")
        (def end-col column))))

  ;;(println end-row)
  ;;(println end-col)
  (def end-val (+ (* end-row y) end-col))
  ;;(println end-val)
  (def init-val 0)
  (adjacentMatrix x y)
  (pap init-val end-val x y))

(main)
