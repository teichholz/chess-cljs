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
                 :width (str (- (:cell css-state) (:figure-gap css-state)) "px")
                 :height (str (- (:cell css-state) (:figure-gap css-state)) "px")})


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
      (= :pawn fig) (Pawn. team nil)
      (= :noble fig) (Noble. team)
      (= :bishop fig) (Bishop. team)
      (= :queen fig) (Queen. team)
      (= :king fig) (King. team)
      (= :rastle fig) (Castle. team))
    :img (dispatch-img fig team)))


(defn init-board []
  "Gibt ein 8x8 Spielbrett in Form von Vektoren (Arraylists) zurück"
  (let [pawn-light (repeat 8 (create-figure :pawn :light))
        pawn-dark (repeat 8 (create-figure :pawn :dark))
        backline-light [(create-figure :rastle :light) (create-figure :noble :light) (create-figure :bishop :light) (create-figure :queen :light)
                        (create-figure :king :light) (create-figure :noble :light) (create-figure :bishop :light) (create-figure :rastle :light)]
        backline-dark [(create-figure :rastle :dark) (create-figure :noble :dark) (create-figure :bishop :dark) (create-figure :queen :dark)
                        (create-figure :king :dark) (create-figure :noble :dark) (create-figure :bishop :dark) (create-figure :rastle :dark)]]
    [backline-light (vec pawn-light) (vec (repeat 8 nil)) (vec (repeat 8 nil)) (vec (repeat 8 nil)) (vec (repeat 8 nil)) (vec pawn-dark) backline-dark]))




(def state (r/atom {:board [(init-board)] ; Letzte Brett in Board ist das aktuelle
                    :lightTeam #{}
                    :darkTeam #{}
                    :lightKing nil
                    :darkKing nil
                    :selected nil})) ; ausgewaehlte Figur
;; zeiger auf das board
(def board-sate (r/cursor state [:board]))
;; muss derefed werden, quasi cursor mit einer Funktion
(def last-board (reagent.ratom/make-reaction #(peek (:board @state))))



;; Selektoren
;; (defn current-board [state]
;;   "liefert das aktuelle Board"
;;   (peek (:board @state)))

(defn nth-in [board y x]
  (nth (nth board y) x))

(nth-in @last-board 6 1)

;; Fig movement
(defn square-empty? [board y x]
  "prueft ob square leer ist, wenn nicht gibt es die Farbe der Figur zurueck"
  (let [square (nth-in board y x)]
    (if (= square nil)
      true
      (:team square))))

;; (square-empty? @last-board 6 1)


(defn square-moveable? [figure board y x]
  "prueft ob die Figur auf ein Square bewegt werden kann"
  (let [moveable (square-empty? board y x)]
    (if (= moveable true)
      true
      (not (= (:team figure) moveable)))))

;; (square-moveable? (nth-in @last-board 6 0) @last-board 7 1)

(defn probe-while
  [figure board current-y current-x y-direction x-direction]
  "prueft ob die Figur sich in eine Richtung bewegen werden kann"
  (loop [board (vec (repeat 8 (vec (repeat 8 false))))
         cur-y current-y
         cur-x current-x
         moveable (square-moveable? figure board (+ current-y y-direction) (+ current-x x-direction))]
    (if moveable
      (let [new-board (assoc board cur-y (assoc (nth board cur-y) cur-x true))] ; setzt board an der richtigen stelle auf true
        (recur new-board (+ cur-y y-direction) (+ cur-x x-direction) (square-moveable? figure board (+ cur-y y-direction) (+ cur-x x-direction))))
      board)))

;; (def b (vec (repeat 8 (vec (repeat 8 false)))))
;; (assoc b 0 (assoc (nth b 0) 1 true))
;; (probe-while (nth-in @last-board 6 0) @last-board 6 0 -1 0)

;; (defn combine-probe-while [figure board current-y current-x y-dir x-dir]
;;   "prueft via probe-while mit allen Kombinationen aus dem array y-dir und x-dir"
;;   (reduce
;;    (fn [col previous] (map))
;;    (for [y y-dir]
;;         x x-dir
;;     (probe-while figure board current-y current-x y x))))


;;

(defn figure [figure]
  [:img (style/use-style css-figure {:src (:img figure)})])


(defn square [y x fig]
  "Eine Komponente zum darstellen eines Quadrats"
  (let [tf (iterate (fn [x] (not x)) true)]
    [:div.Square
      (style/use-style
                  (into css-square
                      (if (nth tf (+ (* y 8) x y))
                        {:background-color "gray"}
                        {:background-color "#222222"}))
                  {:on-click #(.log js/console "hey")})
     (when fig
      [figure fig])]))

(defn map-board [board]
  "Gibt eine Sequenz valider Hiccup Syntax in From <row> 8 x <square /></row>"
  (map-indexed
   (fn [rIndex row]
     (into [:div.Row (style/use-style css-row)]
           (map-indexed
            (fn [cIndex cell]
              [square rIndex cIndex cell])
            row)))
   board))


(defn board [board]
  "Valide Hiccup Syntax zum darstellen des 8x8 Brett via divs"
  (into
   [:div.Board (style/use-style css-board)]
   (map-board board)))

(defn game [state]
  [:div.Game (style/use-style css-game)
   [board @last-board]])

(defn init-app []
  (style/class "*" {:box-sizing "border-box"})
  (r/render [game state] (.getElementById js/document "root")))

(defn ^:export main! []
  (println "App loaded!")
  (style/init)
  (init-app))

