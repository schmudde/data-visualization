(ns data-viz-lesson.core
    (:require ;;[data-viz-lesson.lesson-1-basic-shape :as drawing-1]
              ;;[data-viz-lesson.lesson-2-basic-animation :as drawing-2]
              [data-viz-lesson.lesson-3-basic-data :as drawing-3]
))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
