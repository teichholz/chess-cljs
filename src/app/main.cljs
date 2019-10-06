(ns app.main
  (:require [reagent.core :as r]
            [stylefy.core :as style]))

(defn reload! []
  (println "Code updated."))

(def css-state {:cell 100
                :board (* 100 8)
                :figure-gap 10})
(def css-game {:display "flex"
               :justify-content "center"})
(def css-board {:width (str (:board css-state) "px")
                :height (str (:board css-state) "px")
                :background-color "gray"})
(def css-row {:display "flex"
              :flex (str 0 " " 0 " " (:cell css-state) "px")
              :height (str (:cell css-state) "px")})
(def css-square {:flex (str 0 " " 0 " " (:cell css-state) "px")
                 :display "flex"
                 :justify-content "center"
                 :align-items "center"})
(def css-figure {:display "block"
                 :height (- (:cell css-state) (:figure-gap css-state))
                 :width (- (:cell css-state) (:figure-gap css-state))})


(defn dispatch-img [fig team]
  (let [team-prefix (if (= team :dark) "d" "l")
        type-prefix (subs (str fig) 1 2)]
    (str "/figures/Chess_" type-prefix team-prefix "t60.png")))

(defprotocol Figure
  "Protokoll für die moveSet funktion einer figur"
  (moveSet [this]))

(defrecord Pawn [team moved]
  Figure
  (moveSet [this] "not implemented yet"))

(defrecord King [team]
  Figure
  (moveSet [this] "not implemented yet"))

(defrecord Queen [team]
  Figure
  (moveSet [this] "not implemented yet"))

(defrecord Castle [team]
  Figure
  (moveSet [this] "not implemented yet"))

(defrecord Bishop [team]
  Figure
  (moveSet [this] "not implemented yet"))

(defrecord Noble [team]
  Figure
  (moveSet [this] "not implemented yet"))

(defn create-figure [fig team]
  (assoc
    (cond
      (= :pawn fig) (Pawn. team)
      (= :noble fig) (Noble. team)
      (= :bishop fig) (Bishop. team)
      (= :queen fig) (Queen. team)
      (= :king fig) (King. team)
      (= :castle fig) (Castle. team))
    :img (dispatch-img fig team)))

(create-figure :pawn :light)


(defn init-board []
  "Gibt ein 8x8 Spielbrett in Form von Vektoren (Arraylists) zurück"
  (vec (repeat 8 (vec (range 1 9)))))

(defn init-b []
  (for [lightPawns (repeatedly (fn [] (create-figure :pawn :light)))
        darkPawns (repeatedly (fn [] (create-figure :pawn :dark)))]
    (take 8 darkPawns)))


(def state (r/atom {:board [(init-board)] ; Letzte Brett in Board ist das aktuelle
                    :lightTeam #{}
                    :darkTeam #{}
                    :lightKing nil
                    :darkKing nil
                    :selected nil})) ; ausgewaehlte Figur
;; Selektoren
(defn current-board [state]
  "liefert das aktuelle Board"
  (peek (:board @state)))

;;

(defn figure [type team])

(defn map-board [board]
  "Gibt eine Sequenz valider Hiccup Syntax in From <row> 8 x <square /></row>"
  (let [tf (iterate (fn [x] (not x)) true)]
   (map-indexed
    (fn [rIndex row]
      (into [:div.Row (style/use-style css-row)]
            (map-indexed
             (fn [cIndex cell]
               [:div.Square (style/use-style (into css-square
                                                 (if (nth tf (+ (* rIndex 8) cIndex rIndex))
                                                   {:background-color "gray"}
                                                   {:background-color "#222222"})))])
             row)))
    board)))



(defn board [board]
  "Valide Hiccup Syntax zum darstellen des 8x8 Brett via divs"
  (into
    [:div.Board (style/use-style css-board)]
    (map-board board)))

(defn game [state]
  [:div.Game (style/use-style css-game)
   (board (current-board state))])

(defn init-app []
  (style/class "*" {:box-sizing "border-box"})
  (r/render (game state) (.getElementById js/document "root")))

(defn ^:export main! []
  (println "App loaded!")
  (style/init)
  (init-app))
