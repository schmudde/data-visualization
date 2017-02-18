(ns data-viz-lesson.lesson-1-basic-shape
  (:require [quil.core :as q :include-macros true]))

(defn draw []
  (q/background 255)
  (q/fill 10)
  (q/ellipse 56 46 55 55))

(q/defsketch circle-maker
  :host "shape-space"
  :draw draw
  :size [300 300])
