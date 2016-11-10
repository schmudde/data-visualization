(ns data-viz-lesson.lesson-3-basic-data
  (:require [quil.core :as q :include-macros true]
            [clojure.string :as str]))


(def header "Year,Commodore 64,Apple II,Macintosh,Atari 400/800,TRS-80,IBM PC + clones,,Amiga,Atari ST,NeXT,PET,Other,Altair,TOTAL")

(def sales-data
  "1975,0,0,0,0,0,0,0,0,0,0,0,0,5,5
  1976,0,0,0,0,0,0,0,0,0,0,0,40,6,46
  1977,0,0.6,0,0,100,0,0,0,0,0,4,50,10,150
  1978,0,7.6,0,0,100,0,0,0,0,0,30,100,4,258
  1979,0,35,0,100,200,0,0,0,0,0,45,200,0,580
  1980,0,78,0,200,290,0,0,0,0,0,90,424,0,724
  1981,0,210,0,300,250,35,0,0,0,0,40,605,0,1400
  1982,200,279,0,600,300,240,0,0,0,0,10,1181,0,2800
  1983,2000,420,0,500,200,1300,0,0,0,0,0,500,0,2920
  1984,2500,1000,372,200,50,2000,0,0,0,0,0,200,0,3822
  1985,2500,900,200,100,10,3700,0,100,100,0,0,0,0,5110
  1986,2500,700,380,0,0,5020,0,200,200,0,0,0,0,9000
  1987,1500,500,550,0,0,5950,0,300,400,0,0,0,0,9200
  1988,1250,200,900,0,0,11900,0,400,350,12,0,0,0,15000
  1989,1250,200,1100,0,0,17550,0,600,300,12,0,0,0,21000")

(defn csv->vector
  "Takes the raw csv data and returns a vector of string splits based on new line characters: ['a,b,c' 'd,e,f'].
   This vector of strings is mapped to an anonymous function that creates a collection of vector splits based on comma characters: (['a' 'b' 'c'] ['d' 'e' 'f']). Based on test double's 'clojurescript.csv' library. "
  [data]
  (map #(str/split % #",")
       (str/split data #"\n")))

(def data-vector
  (csv->vector sales-data))

(defn strings->num-vector
  "This takes a collection of vectors containing strings, (['1' '2' '3'] ['4' '5' '6']), and returns them as a seq of numbers: ((1 2 3) [4 5 6])."
  [data]
  (map
   #(map (fn [x] (js/parseInt x)) %) data))

(defn largest-number [data largest-num]
  (let [data-row (rest (first data)) ; rest removes the row's heading
        rows-largest-num (apply max data-row)
        remaining-rows (rest data)
        largest (if (> rows-largest-num largest-num) rows-largest-num largest-num)]

    (if (seq remaining-rows)
      (largest-number remaining-rows largest)
      largest)))

(defn graph-points []
  (let [chart-data (-> sales-data (csv->vector) (strings->num-vector))]
    chart-data))

(defn draw-axis-headers [data x y x-offset y-offset]
  (if (seq data)
    (do
      (q/text (first data) x y)
      (draw-axis-headers (rest data) (+ x x-offset) (+ y y-offset) x-offset y-offset))))

(defn draw-x-axis []
  (let [x-axis-units (map first (csv->vector sales-data))]
      (q/fill 0)
      (q/text-size 15)
      (q/text-align :left :baseline)
      (q/text-font (q/create-font "Sans-Serif" 10)) ; should I use load-font?
      (draw-axis-headers x-axis-units 0 450 50 0)))

(defn draw-y-axis []
  (let [units-sold (graph-points)
        largest-num (largest-number units-sold 0)
        y-axis-units (range 0 largest-num (/ largest-num 10))]
    (q/fill 0)
    (q/text-size 15)
    (q/text-align :left :baseline)
    (q/text-font (q/create-font "Sans-Serif" 10)) ; should I use load-font?
    (draw-axis-headers y-axis-units 0 500 0 -50)))

(defn draw-data-points [data x x-offset color]
  (if (seq data)
    (let [y-coordinate (first data)
          size 10]
      (q/fill (color :r) (color :g) (color :b))
      (q/ellipse x y-coordinate size size)
      (draw-data-points (rest data) (+ x x-offset) x-offset color))))

(defn draw []
  (let [frame (q/frame-count)
        commodore (map #(nth % 1) data-vector)
        apple (map #(nth % 2) data-vector)
        commodore-color {:r 255 :g 255 :b 0}
        apple-color {:r 255 :g 0 :b 255}]

    (q/background 255)

    (draw-x-axis)
    (draw-y-axis)
    (draw-data-points commodore 0 50 commodore-color)
    (draw-data-points apple 0 50 apple-color)))

(q/defsketch visual-data
  :host "shape-space"
  ;:features [:no-start]
  :draw draw
  :size [1000 500])
