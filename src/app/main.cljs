(ns app.main
  (:require [reagent.core :as r]
            [stylefy.core :as style]))


(defn reload! []
  (println "Code updated."))

;; (defn my-comp []
;;   [:div.App (style/use-style {:width 200})
;;    [:div.Board
;;     [:div.Row "Eine Zeile"
;;      [:div.Row "noch eine"
;;       [:div.Row "noch eine ds"
;;        (my-comp2)]]]]])

(def state (atom {:board []
                  :lightTeam #{}
                  :darkTeam #{}
                  :lightKing nil
                  :darkKing nil}))



(defn init-app []
  ())

(defn ^:export main! []
  (println "App loaded!")
  (style/init)
  (init-app))
