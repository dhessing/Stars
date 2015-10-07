(ns stars.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [clojure.set])
  (:import goog.History
           [goog.ui IdGenerator]))

;; -------------------------
;; Util

(defn guid []
  (.getNextUniqueId (.getInstance IdGenerator)))

(defn indexed [coll]
  (map-indexed vector coll))

;; -------------------------
;; Core

(def POINTS
  {21 1
   22 1
   23 1
   24 1
   25 2
   26 2
   27 2
   28 2
   29 3
   30 3
   31 3
   32 3
   33 4
   34 4
   35 4
   36 4})

(def PHASES
  {:roll-phase #{:roll :pick-tile :steal-tile}
   :pick-phase #{:pick}
   :dead-phase #{:next}})

(defn new-player []
  {:id (guid) :name ""})


;; -------------------------
;; State

(def initial-state {:players [(new-player)
                              (new-player)]
                    :phase   :roll-phase
                    :player  0
                    :screen  :setup})

(defonce app-state (atom initial-state))

;; -------------------------
;; Queries

(defn action-disabled? [state action]
  (not (contains? ((:phase state) PHASES) action)))

(defn sum-chosen [chosen]
  (apply + (replace {"*" 5} chosen)))

(defn get-available-tiles [state]
  (sort (remove (set (concat (mapcat :tiles (:players state))
                             (:removed-tiles state)))
                (set (keys POINTS)))))

(defn get-selectable-tiles [state]
  (let [chosen (:chosen state)
        tiles (set (filter (partial >= (sum-chosen chosen))
                           (get-available-tiles state)))]
    (if (some #{"*"} (:chosen state))
      (conj tiles (sum-chosen chosen))
      #{})))

(defn get-selectable-faces [state]
  (clojure.set/difference
   (set (:thrown state))
   (set (:chosen state))))

(defn get-score [tiles]
  (reduce + (map (partial get POINTS) tiles)))

(defn get-player-scores [players]
  (for [[i {:keys [name tiles]}] (indexed players)]
    {:index i
     :name  (if (empty? name) (str "Player " (inc i)) name)
     :tiles tiles
     :score (get-score tiles)}))

;; -------------------------
;; Actions

(defn add-player [state]
  (update state :players conj (new-player)))

(defn update-player-name [state name index]
  (assoc-in state [:players index :name] name))

(defn remove-player [state player]
  (update state :players #(vec (remove #{player} %))))

(defn set-phase [state phase]
  (assoc state :phase phase))

(defn end-game? [state]
  (if (empty? (get-available-tiles state))
    (assoc state :screen :end-game)
    state))

(defn next-turn [state]
  (let [num-players (count (:players state))]
    (-> state
        (update :player #(mod (inc %) num-players))
        (assoc :phase :roll-phase)
        (dissoc :thrown :chosen)
        (end-game?))))

(defn roll [state]
  (let [n (- 8 (count (:chosen state)))
        throw (repeatedly n #(rand-nth [1 2 3 4 5 "*"]))
        new-state (assoc state :thrown throw)]
    (if (empty? (get-selectable-faces new-state))
      (set-phase new-state :dead-phase)
      (set-phase new-state :pick-phase))))

(defn pick [state face]
  (when (some #{face} (get-selectable-faces state))
    (let [n (count (filter #{face} (:thrown state)))
          new-state (-> state
                        (update :chosen #(apply conj % (repeat n face)))
                        (update :thrown #(remove (partial = face) %)))]
      (if (and (= 8 (count (:chosen new-state)))
               (empty? (get-selectable-tiles new-state)))
        (set-phase new-state :dead-phase)
        (set-phase new-state :roll-phase)))))

(defn lose-tile [state]
  (let [current-player (:player state)]
    (-> state
        (update-in [:players current-player :tiles] rest)
        (update :removed-tiles #(conj % (last (get-available-tiles state))))
        (next-turn))))

(defn pick-tile [state tile]
  (let [current-player (:player state)]
    (-> state
        (update-in [:players current-player :tiles] #(conj % tile))
        (next-turn))))

(defn steal-tile [state player]
  (let [tile (first (get-in state [:players player :tiles]))]
    (-> state
        (update-in [:players player :tiles] rest)
        (pick-tile tile))))


;; -------------------------
;; Components

(defn star []
  [:i.fa.fa-star-o])

(defn golden-star []
  [:i.fa.fa-star {:style {:color "goldenrod"}}])

(defn navbar []
  (let [active (session/get :current-page)
        navlink-active? #(if (= active %) "nav-link active" "nav-link")]
    [:nav {:class "navbar navbar-static-top navbar-dark bg-inverse"}
     [:ul {:class "nav navbar-nav"}
      [:li {:class "nav-item"}
       [:a {:class (navlink-active? :home), :href "#"} [golden-star] " Stars"]]
      [:li {:class "nav-item"}
       [:a {:class (navlink-active? :game), :href "#/game"} "Game"]]]]))

(defn roll-button []
  [:button.btn.btn-primary.stars-btn
   {:on-click #(swap! app-state roll)
    :disabled (or (action-disabled? @app-state :roll)
                  (= 8 (count (:chosen @app-state))))}
   "Roll"])

(defn die-view [face]
  (if (= "*" face)
    [star]
    face))

(defn die-button [face active]
  [:button.btn.btn-secondary.die
   {:on-click      #(swap! app-state pick face)
    :on-mouse-over #(reset! active face)
    :on-mouse-out  #(reset! active nil)
    :class         (when (= face @active) "active")
    :disabled      (or (action-disabled? @app-state :pick)
                       (not (contains? (get-selectable-faces @app-state) face)))}
   (die-view face)])

(defn die-list []
  [:div.m-b
   [:div.btn-toolbar
    (let [active (atom nil)
          thrown-indexed (indexed (:thrown @app-state))]
      (for [[i face] thrown-indexed]
        ^{:key i}
        [die-button face active]))
    [roll-button]]
   (when (= :dead-phase (:phase @app-state))
     [:div
      [:p "You died"]
      [:button.btn.btn-primary
       {:on-click #(swap! app-state lose-tile)}
       "Next Player"]])])

(defmulti tile-view (fn [tile _] tile))

(defmethod tile-view :default [tile action]
  [:button.btn.btn-secondary.tile
   {:on-click #(action)
    :disabled (or (action-disabled? @app-state :pick-tile)
                  (not-any? #{tile} (get-selectable-tiles @app-state)))}
   [:div.tile-top tile]
   [:div.tile-bottom
    (for [i (range (get POINTS tile))]
      ^{:key i} [:i.fa.fa-star])]])

(defmethod tile-view nil []
  [:button.btn.btn-secondary.tile.tile-nothing
   {:disabled true}])

(defn tile-button [tile]
  [tile-view tile #(swap! app-state pick-tile tile)])

(defn player-tile [player]
  (if-let [tile (first (get-in @app-state [:players player :tiles]))]
    [tile-view tile #(swap! app-state steal-tile player)]
    [tile-view nil]))


;; -------------------------
;; Screens

(defn setup-screen []
  [:div
   [:h1 "Players"]
   [:form
    (for [[i {:keys [name] :as player}] (indexed (:players @app-state))]
      ^{:key i}
      [:div.form-group.row
       [:label.form-control-label.col-xs-1 "Player " (inc i)]
       [:div.col-xs-8
        [:input.form-control
         {:type        "text"
          :placeholder "Name"
          :value       name
          :on-change   #(swap! app-state update-player-name
                               (-> % .-target .-value) i)}]]
       [:button.btn.btn-danger.col-xs-1
        {:on-click #(swap! app-state remove-player player)}
        [:i.fa.fa-trash-o.fa-form]]])]
   [:div.btn-toolbar
    [:button.btn.btn-secondary
     {:on-click #(swap! app-state add-player)}
     "Add Player"]
    [:button.btn.btn-primary
     {:on-click #(swap! app-state assoc :screen :game)}
     "Done"]]])

(defn game-screen []
  [:div
   [:div.card-deck-wrapper
    [:div.card-deck.m-b-md
     (let [current-player (:player @app-state)]
       (for [[i {:keys [name tiles]}] (indexed (:players @app-state))]
         (let [current? (= i current-player)]
           ^{:key i}
           [:div.card (when current? {:style {:border-color "#333"}})
            [:div.card-header
             (if (empty? name)
               (str "Player " (inc i))
               name)]
            [:div.card-block
             [player-tile i]]
            [:div.card-footer.text-muted
             (let [tiles-count (count tiles)]
               [:span tiles-count " " (if (= 1 tiles-count) "tile" "tiles")])]])))]]
   [:div.card
    [:div.card-block
     [:div.btn-toolbar.m-b-md
      (for [tile (get-available-tiles @app-state)]
        ^{:key tile}
        [tile-button tile])]
     [die-list]
     (let [chosen (:chosen @app-state)]
       [:div
        [:div.btn-toolbar.m-b
         (for [[i face] (indexed chosen)]
           ^{:key i}
           [:button.btn.btn-secondary.die {:disabled true} (die-view face)])]
        (when (not-empty chosen) [:p "Total: " (sum-chosen chosen)])])]]])

(defn end-game-screen []
  [:div
   [:h1 "Scoring"]
   [:div.card-deck-wrapper.m-t.m-b
    [:div.card-deck
     (let [player-scores (get-player-scores (:players @app-state))
           winning-score (:score (apply max-key :score player-scores))]
       (for [{:keys [index name tiles score]} player-scores]
         ^{:key index}
         [:div.card
          (when (= score winning-score) {:class "card-inverse card-success"})
          [:div.card-block
           [:h3.card-title
            (when (= winning-score score)
              [:span
               [:i.fa.fa-star.text-warning.fa-spin]
               " "])
            name]
           [:p.card-text score " points"]
           [:div.btn-toolbar
            (for [tile tiles]
              ^{:key tile}
              [tile-button tile])]]]))]]
   [:button.btn.btn-primary
    {:on-click #(reset! app-state initial-state)} "New Game"]])

; Screen routing
(defmulti current-screen #(:screen @app-state))
(defmethod current-screen :setup [] [setup-screen])
(defmethod current-screen :game [] [game-screen])
(defmethod current-screen :end-game [] [end-game-screen])

;; -------------------------
;; Pages

(defn game-page []
  [:div.container.m-t-md
   [current-screen]])

(defn home-page []
  [:div
   [:div.jumbotron.jumbotron-fluid
    [:div.container
     [:h1.display-3 [golden-star] " Welcome to Stars! "]
     [:p.lead "A browser based dice game, programmed in Clojure."]
     [:p.lead
      [:a.btn.btn-primary.btn-lg {:href "#/game"} "To the game »"]]]]
   [:div.container
    [:h1 "Gameplay"]
    [:ul
     [:li "The goal of the game is to own as many tiles with stars as possible."]
     [:li "There are 16 tiles numbered 21 to 36, each worth between 1 and 4 stars."]
     [:li "There are also 8 dice, numbered 1, 2, 3, 4, 5 and " [star] ". " "A " [star] " is worth 5 points."]
     [:li "You collect tiles by rolling dice, and strategically choosing which faces to keep. The sum of your chosen faces decides which tile you can pick."]
     [:li "You start your turn by rolling all 8 dice. You can now choose one of the thrown faces, you get to keep all the dice with that face."]
     [:li "You can now choose to roll again using the remaining dice, or you can pick a tile if your score is high enough and contains at least one " [star] "."]
     [:li "When rolling again, you cannot pick a face that you already chose this turn."]
     [:li "You can also steal the top tile from another player if your score is exactly the same and you have at least one " [star] "."]
     [:li "If you end your throw with no possible dice to pick or if there aren't any dice left to throw, you " [:em "die"] " and your top tile will go back to the playing field. Also, the highest scoring tile will be completely removed from the game as well."]
     [:li "The game will end when all the tiles in the playing field are gone."]
     [:li "The player with the most stars on his tiles wins."]]]])

; Page routing
(defmulti current-page #(session/get :current-page))
(defmethod current-page :game [] [game-page])
(defmethod current-page :home [] [home-page])

;; -------------------------
;; Routes

(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
                    (session/put! :current-page :home))

(secretary/defroute "/game" []
                    (session/put! :current-page :game))

;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

;; -------------------------
;; Initialize app

(defn root-page [] [:div [navbar] [current-page]])

(defn mount-root []
  (reagent/render [root-page] (.getElementById js/document "app")))

(defn init! []
  (hook-browser-navigation!)
  (mount-root))
