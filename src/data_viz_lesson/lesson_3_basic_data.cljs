(ns data-viz-lesson.lesson-3-basic-data
  (:require [quil.core :as q :include-macros true]
            [clojure.string :as str]))


;;;;;;;;;;;;;;;;;;;;;
;; Utilities       ;;
;;;;;;;;;;;;;;;;;;;;;

(defn strings->num-vector
  "This takes a collection of vectors containing strings, (['1' '2' '3'] ['4' '5' '6']), and returns them as a seq of numbers: ((1 2 3) [4 5 6])."
  [data]
  (map
   #(map (fn [x] (js/parseInt x)) %) data))

(defn csv->vector
  "Takes the raw csv data and returns a vector of string splits based on new line characters: ['a,b,c' 'd,e,f'].
   This vector of strings is mapped to an anonymous function that creates a collection of vector splits based on comma characters: (['a' 'b' 'c'] ['d' 'e' 'f']). Based on test double's 'clojurescript.csv' library. "
  [data]
  (map #(str/split % #",")
       (str/split data #"\n")))

(defn ratio-scale
  "canvas-max / x (canvas-scaled-value) = data-max / data-value"
  [canvas-max data-max data-value]
  (/ canvas-max (/ data-max data-value)))


;;;;;;;;;;;;;;;;;;;;;
;; Raw Data        ;;
;;;;;;;;;;;;;;;;;;;;;

(def header "Year,Commodore 64,Apple II,Macintosh,Atari 400/800,TRS-80,Amiga,Atari ST,NeXT,PET,Other,Altair")

(def sales-data
  "1975,0,0,0,0,0,0,0,0,0,0,0,5
  1976,0,0,0,0,0,0,0,0,0,0,40,6
  1977,0,0.6,0,0,100,0,0,0,0,4,50,10
  1978,0,7.6,0,0,100,0,0,0,0,30,100,4
  1979,0,35,0,100,200,0,0,0,0,45,200,0
  1980,0,78,0,200,290,0,0,0,0,90,424,0
  1981,0,210,0,300,250,0,0,0,0,40,605,0
  1982,200,279,0,600,300,0,0,0,0,10,1181,0
  1983,2000,420,0,500,200,0,0,0,0,0,500,0
  1984,2500,1000,372,200,50,0,0,0,0,0,200,0
  1985,2500,900,200,100,10,0,100,100,0,0,0,0
  1986,2500,700,380,0,0,0,200,200,0,0,0,0
  1987,1500,500,550,0,0,0,300,400,0,0,0,0
  1988,1250,200,900,0,0,0,400,350,12,0,0,0
  1989,1250,200,1100,0,0,0,600,300,12,0,0,0")

(def data-vector
  (csv->vector sales-data))

(def canvas-size
  (let [y 500
        x 1000
        x-axis-footer 30
        x-axis-header 10
        x-space-between 50
        y-axis-margin 10]
    {:y y
     :x x
     :margin-x-footer x-axis-footer
     :margin-x-header x-axis-header
     :x-spacer x-space-between
     :margin-y-left-col y-axis-margin
     :y-available (- y x-axis-footer x-axis-header)}))

;;;;;;;;;;;;;;;;;;;;;
;; Parse Data      ;;
;;;;;;;;;;;;;;;;;;;;;

(defn largest-number
  "I find the largest number in the dataset provided"
  [data largest-num]
  (let [data-row (rest (first data)) ; rest removes the row's heading
        rows-largest-num (apply max data-row)
        remaining-rows (rest data)
        largest (if (> rows-largest-num largest-num) rows-largest-num largest-num)]

    (if (seq remaining-rows)
      (largest-number remaining-rows largest)
      largest)))

(defn units-sold []
  (let [chart-data (-> sales-data (csv->vector) (strings->num-vector))]
    chart-data))

(defn x-header-data []
  {:units (map first (csv->vector sales-data))
   :x (+ (canvas-size :x-spacer) (canvas-size :margin-y-left-col))
   :y (canvas-size :y)
   :x-offset (canvas-size :x-spacer)
   :y-offset 0})

(defn y-header-data [largest-num]
  (let [y-axis-units (range 0 (inc largest-num) (/ largest-num 10))
        y-offset (- (/ (canvas-size :y-available) 10))]
    {:units y-axis-units
     :x 0
     :y (- (canvas-size :y) (canvas-size :margin-x-footer))
     :x-offset 0
     :y-offset y-offset}))

(defn point-data [model-number]
  (let [model (map #(nth % model-number) data-vector)]
    {:units (map cljs.reader/read-string model)
     :x (+ (canvas-size :x-spacer) (canvas-size :margin-y-left-col))
     :y nil
     :x-offset (canvas-size :x-spacer)
     :y-offset (- (canvas-size :y) (canvas-size :margin-x-footer))}))

;;;;;;;;;;;;;;;;;;;;;
;; Draw Data       ;;
;;;;;;;;;;;;;;;;;;;;;

(defn grapher!
  ([x y label]
   (q/fill 0)
   (q/text label x y))

  ([x y size color]
   (q/fill (color :r) (color :g) (color :b))
   (q/ellipse x y size size)))

(defn draw-axis-header-values [data]
  (let [{units :units
         x :x
         y :y
         x-offset :x-offset
         y-offset :y-offset} data
        next-data {:units (rest units)
                   :x (+ x x-offset)
                   :y (+ y y-offset)
                   :x-offset x-offset
                   :y-offset y-offset}]
    (when (seq units)
      (grapher! x y (first units))
      (draw-axis-header-values next-data))))

(defn draw-axis-headers
  "I take the data and destructure it in preparation for a recursive graphing function.
   Before I do that, I determine whether I am drawing an x header or a y header and position the text accordingly."
  [data]
  (if (zero? (data :x-offset))
    (q/text-align :left :center)
    (q/text-align :center :baseline))
  (draw-axis-header-values data))

(defn draw-data-points [data x x-offset y-offset color largest-num]
  (if (seq data)
    (let [y-scaled (ratio-scale (canvas-size :y-available) largest-num (first data))
          y (- y-offset y-scaled)
          size 10]
      (grapher! x y size color)
      (draw-data-points (rest data) (+ x x-offset) x-offset y-offset color largest-num))))


(defn draw-data [largest-num]
  (let [commodore (point-data 1)
        apple (point-data 2)
        commodore-color {:r 255 :g 255 :b 0}
        apple-color {:r 255 :g 0 :b 255}]

   (draw-data-points (commodore :units) (commodore :x) (commodore :x-offset) (commodore :y-offset) commodore-color largest-num)
   (draw-data-points (apple :units) (apple :x) (apple :x-offset) (apple :y-offset) apple-color largest-num)))


(defn setup []
  (q/text-font (q/create-font "sans-serif" 10)))

(defn draw []
  (let [largest-num (largest-number (units-sold) 0)]

    (q/background 255)

    (draw-axis-headers (x-header-data))
    (draw-axis-headers (y-header-data largest-num))

    (draw-data largest-num)

    ))

(q/defsketch visual-data
  :host "shape-space"
  :setup setup
  :draw draw
  :size [(canvas-size :x) (canvas-size :y)])
