(ns app.main
  (:require [reagent.core :as r]
            [stylefy.core :as style]))

(defn reload! []
  (println "Code updated."))

;; Helper
(defn in-range [board y x]
  (not (or (> y 7) (< y 0) (> x 7) (< x 0))))

(defn nth-in-vec
  ([board y x]
   (if (in-range board y x)
     (nth (nth board y) x)
     nil))

  ([board col]
   (let [[key-value] [col]
         [x y] key-value]
     (if (empty? col)
       board
       (recur (nth-in-vec board x y) (rest col))))))

(defn assoc-in-vec
  "Setzt in Vektor vec die Werte von Collection col in Form von (list (y x value) ...)"
  ([vec y x value]
   (assoc vec y (assoc (nth vec y) x value)))
  ([vec col]
   (let [[index-value] col
         [y x value] index-value]
     (if (empty? col)
       vec
       (recur (assoc-in-vec vec y x value) (rest col))))))

(defn empty-board []
  (vec (repeat 8 (vec (repeat 8 false)))))

;; State
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
(def css-square-hint {:border "2px solid red"})
(def css-figure {:display "block"
                 :width (str (- (:cell css-state) (:figure-gap css-state)) "px")
                 :height (str (- (:cell css-state) (:figure-gap css-state)) "px")})

(defn dispatch-img [fig team]
  (let [team-prefix (if (= team :dark) "d" "l")
        type-prefix (subs (str fig) 1 2)]
    (str "/figures/Chess_" type-prefix team-prefix "t60.png")))

;; Fig movement
(defn square-empty? [board y x]
  "prueft ob square leer ist, wenn nicht gibt es die Farbe der Figur zurueck"
  (let [square (nth-in-vec board y x)]
    (if (= square nil)
      true
      (:team square))))

;; (square-empty? @last-board 6 1)


(defn square-moveable? [figure board y x]
  "prueft ob die Figur auf ein Square bewegt werden kann, gibt true zurueck wenn ja, :enemy wenn dort ein Feind war und false wenn das feld nicht existiert"
  (if (in-range board y x)
    (let [moveable (square-empty? board y x)]
      (if (= moveable true)
        true
        (and (not (= (:team figure) moveable))
             :enemy)))
    false))

;; (in-range @last-board 0 -1)
;; (square-moveable? (nth-in-vec @last-board 6 0) @last-board 7 1)

(defn probe-while
  [figure board current-y current-x y-direction x-direction]
  "prueft ob die Figur sich in eine Richtung bewegen werden kann"
  (loop [pos-board (vec (repeat 8 (vec (repeat 8 false))))
         cur-y (+ current-y y-direction)
         cur-x (+ current-x x-direction)
         moveable (square-moveable? figure board cur-y cur-x)]
    (if (or (not moveable))
      pos-board
      (if (= moveable :enemy)
        ;; gib das Board hier nach zurueck
        (assoc-in-vec pos-board cur-y cur-x true)
        ;; mach weiter
        (let [new-board (assoc-in-vec pos-board cur-y cur-x true)] ; setzt board an der richtigen stelle auf true
          (println "recur" cur-y cur-x)
          (recur new-board (+ cur-y y-direction) (+ cur-x x-direction) (square-moveable? figure board (+ cur-y y-direction) (+ cur-x x-direction))))))))

(defn combine-vec
  [fn vec1 vec2]
  (mapv fn vec1 vec2))

(defn combine-vec-2d
  [f vec1 vec2]
  (mapv (partial combine-vec f) vec1 vec2))

(defn combine-probe-while [figure board current-y current-x y-dir x-dir]
  "prueft via probe-while mit allen Kombinationen aus dem array y-dir und x-dir"
  (reduce
   (partial combine-vec-2d (fn [x y] (or x y)))
   (vec (repeat 8 (vec (repeat 8 false))))
   (for [y y-dir
         x x-dir]
     (probe-while figure board current-y current-x y x))))

(defprotocol Figure
  "Protokoll für die move-set funktion einer figur"
  (move-set [this board current-y current-x]))

(defrecord Pawn [team moved]
  Figure
  (move-set [this board current-y current-x]
    ;; TODO pawn kann manchmal Figure falsch schlagen, diagonale mit nth-in-vec pruefen
    (let [possible (empty-board)
          direction (if (= (:team this) :light) 1 -1)
          one (square-moveable? this board (+ current-y direction) current-x)
          two (if (not (:moved this)) (square-moveable? this board (+ current-y (* 2 direction)) current-x))
          diagonal-minus (not (= (:team this) (or
                                               (:team (nth-in-vec board current-y (- current-x 1)))
                                               (:team this))))
          diagonal-plus (not (= (:team this) (or
                                              (:team (nth-in-vec board current-y (+ current-x 1)))
                                              (:team this))))]
      (assoc-in-vec possible (filter
                              (fn [x] (nth x 2))
                              (list [(+ current-y direction) current-x one] [(+ current-y (* 2 direction)) current-x two]
                                    [current-y (- current-x 1) diagonal-minus] [current-y (+ current-x 1) diagonal-plus]))))))



;(filter (fn [x] (nth x 2)) (list [1 2 true] [2 3 false]))
;(map (fn [x] (println x)) '([1 2 true] [2 3 false]))

(defrecord King [team]
  Figure
  (move-set [this board current-y current-x]
    (let [points (list [(+ current-y 1) current-x] [(- current-y 1) current-x]
                       [current-y (- current-x 1)] [current-y (+ current-x 1)]
                       [(+ current-y 1) (+ current-x 1)] [(- current-y 1) (- current-x 1)]
                       [(- current-y 1) (+ current-x 1)] [(+ current-y 1) (- current-x 1)])]
      (assoc-in-vec (empty-board)
                    (filter (fn [x] (square-moveable? this board (nth x 0) (nth x 1))) points)))))

(defrecord Queen [team]
  Figure
  (move-set [this board current-y current-x]
    (combine-probe-while this board current-y current-x [1 -1 0] [1 -1 0])))

(defrecord Castle [team]
  Figure
  (move-set [this board current-y current-x]
    (reduce (partial combine-vec-2d (fn [x y] (or x y)))
            (combine-probe-while this board current-y current-x [1 -1] [0])
            (combine-probe-while this board current-y current-x [0] [1 -1]))))

(defrecord Bishop [team]
  Figure
  (move-set [this board current-y current-x]
    (combine-probe-while this board current-y current-x [1 -1] [1 -1])))

(defrecord Noble [team]
  Figure
  (move-set [this board current-y current-x]
    (let [points (list [(+ current-y 2) (- current-x 1)] [(+ current-y 2) (+ current-x 1)]
                       [(- current-y 2) (- current-x 1)] [(- current-y 2) (+ current-x 1)]
                       [(+ current-y 1) (+ current-x 2)] [(- current-y 1) (- current-x 2)])]
      (assoc-in-vec (empty-board)
                    (map (fn [x] (conj x (square-moveable? this board (nth x 0) (nth x 1)))) points)))))

(defn create-figure [fig team]
  (assoc
   (cond
     (= :pawn fig) (Pawn. team false)
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
                    :possible (empty-board)
                    :lightTeam #{}
                    :darkTeam #{}
                    :lightKing nil
                    :darkKing nil
                    :selected nil})) ; ausgewaehlte Figur
;; zeiger auf das board
(def board-state (r/cursor state [:board]))
(def possible-state (r/cursor state [:highlight]))
(def selected-state (r/cursor state [:selected]))
;; muss derefed werden, quasi cursor mit einer Funktion
(def last-board (reagent.ratom/make-reaction #(peek (:board @state))))


;; Methoden zum interagieren mit dem Schachbrett


(defn set-selected! [figure]
  "waehlt eine Figur an, merkt sich diese im Zustand und gibt sie zurueck"
  (reset! selected-state figure))

(defn set-possible! [possible]
  "waehlt eine Figur an, merkt sich diese im Zustand und gibt sie zurueck"
  (reset! possible-state possible))

(defn set-last-board! [board]
  "Fuegt ein neues Board ein"
  (swap! board-state (fn [board-state] (conj board-state board))))

(defn move-figure [board figure to-y to-x]
  "bewegt eine Figur und gibt das neue Board zurueck"
  (let [figure (if (instance? app.main/Pawn (:figure figure)) (assoc-in figure [:figure :moved] :true) figure)]
      (assoc-in-vec (assoc-in-vec board (:y figure) (:x figure) nil) to-y to-x (:figure figure))))

(defn maybe-move-figure [board click-y click-x]
  "Waehlt entweder eine Figur aus oder bewegt sie, veraendert den Zustand"
  (if @selected-state
    ;; Mit Zustand arbeiten
    (if (nth-in-vec @possible-state click-y click-x)
      (do
        (set-last-board! (move-figure board @selected-state click-y click-x))
        (set-selected! nil)
        (set-possible! nil))
      (do
        (set-selected! nil)
        (set-possible! nil)))
    ;; Mit parameter arbeiten
    (let [figure (nth-in-vec board click-y click-x)]
      (if figure
        (do
          (set-selected! {:figure figure
                          :y click-y
                          :x click-x})
          (set-possible! (move-set figure board click-y click-x)))
        (println "Auf dem Square ist keine Figur")))))

(instance? app.main/Castle (nth-in-vec @last-board 0 0))

;; Komponenten


(defn figure [figure]
  [:img (style/use-style css-figure {:src (:img figure)})])

(defn square [y x fig highlight]
  "Eine Komponente zum darstellen eines Quadrats"
  (let [tf (iterate (fn [x] (not x)) true)]
    [:div.Square
     (style/use-style
      (into (into css-square
                  (if (nth tf (+ (* y 8) x y))
                    {:background-color "gray"}
                    {:background-color "#222222"}))
            (when highlight
              css-square-hint))
      {:on-click
       #(maybe-move-figure @last-board y x)
       ;#(println y x )
       })
     (when fig
       [figure fig])]))

(defn map-board [board highlight]
  "Gibt eine Sequenz valider Hiccup Syntax in From <row> 8 x <square /></row>"
  (map-indexed
   (fn [rIndex row]
     (into [:div.Row (style/use-style css-row)]
           (map-indexed
            (fn [cIndex cell]
              [square rIndex cIndex cell (nth-in-vec highlight rIndex cIndex)])
            row)))
   board))

(defn board [board highlight]
  "Valide Hiccup Syntax zum darstellen des 8x8 Brett via divs"
  (into
   [:div.Board (style/use-style css-board)]
   (map-board board highlight)))

(defn game []
  [:div.Game (style/use-style css-game)
   [board @last-board @possible-state]])

(defn init-app []
  (style/class "*" {:box-sizing "border-box"})
  (r/render [game state] (.getElementById js/document "root")))

(defn ^:export main! []
  (println "App loaded!")
  (style/init)
  (init-app))
