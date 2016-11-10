(ns data-viz-lesson.lesson-2-basic-animation
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 20)
  ; initial state
  {:x 100
   :y 47})


(defn update-state [state]
    {:y (+ (:y state) 0.5)
     :x (:x state)})

(defn draw-state [state]
    (q/background 0)
    (q/fill 255)

    ;; non-fun-mode update on every frame - introduce this first
    ;; (q/ellipse 56 (+ 42 q/frame-count) 55 55)

    (q/ellipse (:x state) (:y state) 55 55))

(defn move-me [state]
  {:y (:y state)
   :x (+ (:x state) 2.5)})

(q/defsketch circle-maker
  :host "shape-space"
  ; :features [:no-start]
  :setup setup
  :draw draw-state
  :update update-state
  :size [300 300]
  :key-released move-me
  :middleware [m/fun-mode]
  )
