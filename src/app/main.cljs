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

(def css-state {:cell 100
                :board (* 100 8)
                :figure-gap 10})
(def css-game {:display "flex"
               :justify-content "center"})
(def css-board {:width (:board css-state)
                :height (:board css-state)
                :display "flex"
                :flex (str 0 " " 0 " " (:cell css-state))
                :background-color "gray"})
(def css-square {:flex (str 0 " " 0 " " (:cell css-state))
                 :display "flex"
                 :justify-content "center"
                 :align-items "center"})
(def css-figure {:display "block"
                 :height (- (:cell css-state) (:figure-gap css-state))
                 :width (- (:cell css-state) (:figure-gap css-state))})

(defn init-board []
  (vec (repeat 8 (vec (range 1 9)))))

(def state (r/atom {:board (init-board)
                    :lightTeam #{}
                    :darkTeam #{}
                    :lightKing nil
                    :darkKing nil}))

(defn board []
  [:div.Game (style/use-style css-game)
   [:div.Board (style/use-style css-board)]])

(board)

(defn map-state [state]
  (map (fn [row] (map (fn [cell] [:div]) row)) (:board @state)))

(defn init-app []
  (style/class "*" {:box-sizing "border-box"})
  (r/render board (.getElementById js/document "root")))

(defn ^:export main! []
  (println "App loaded!")
  (style/init)
  (init-app))
