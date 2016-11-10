(ns data-viz-lesson.basic-example
    (:require [quil.core :as q :include-macros true]))

(defn draw []
  (q/background 255)
  (q/fill 255)
  (q/ellipse 56 46 55 55))

(q/defsketch circle-maker
  :draw draw
  :host "shape-space"
  :size [300 300])
