(ns web.game-test
  (:require
   [cljc.java-time.instant :as inst]
   [clojure.test :refer :all]
   [web.app-state :as app-state]
   [web.game]
   [web.lobby :as lobby]
   [web.stats :as stats]
   [web.ws :as ws]))

(defn- player [uid side]
  {:uid uid :side side :user {:username uid}})

(defn- finished-game [gameid state]
  (let [players [(player "alice" "Corp") (player "bob" "Runner")]]
    {:gameid gameid :started true :room "casual" :format "standard" :title "t"
     :pool (lobby/join-pool! gameid) :state (atom state)
     :players players :original-players players :spectators []}))

(defn- click-rematch! [uid gameid]
  @(ws/-msg-handler {:id :game/rematch :uid uid :?data {:gameid gameid} :timestamp (inst/now)
                     :ring-req {:system/db nil :user {:username uid}}}))

(defn- with-lobby
  "Runs `f` with only `lobby` registered and the db/websocket side effects stubbed.
  `f` gets an atom of the log lines sent to the old game."
  [lobby f]
  (let [before @app-state/app-state
        log (atom [])]
    (try
      (swap! app-state/app-state assoc :lobbies {(:gameid lobby) lobby} :block-game-creation false)
      (with-redefs [ws/chsk-send! (fn [_ _])
                    ws/connected-uids (constantly nil)
                    stats/game-finished (fn [& _]) stats/update-deck-stats (fn [& _])
                    stats/update-game-stats (fn [& _]) stats/push-stats-update (fn [& _])
                    web.game/handle-message-and-send-diffs! (fn [_ _ _ msg] (swap! log conj msg))]
        (f log))
      (finally
        (doseq [{:keys [pool gameid]} (app-state/get-lobbies)]
          (lobby/leave-pool! pool gameid))
        (reset! app-state/app-state before)))))

(deftest rematch-needs-both-players-test
  (with-lobby
    (finished-game "old" {:end-time (inst/now)})
    (fn [log]
      (testing "one request keeps the finished game open and tells the opponent once"
        (click-rematch! "alice" "old")
        (click-rematch! "alice" "old")
        (is (= #{"Corp"} (:rematch-requests (app-state/get-lobby "old"))))
        (is (= ["alice wants a rematch."] @log)))

      (testing "the second player's request closes the game and opens a fresh lobby"
        (click-rematch! "bob" "old")
        (let [[new-lobby :as lobbies] (app-state/get-lobbies)]
          (is (= 1 (count lobbies)))
          (is (not= "old" (:gameid new-lobby)))
          (is (not (:started new-lobby)))
          (is (= [["alice" "Corp"] ["bob" "Runner"]] (mapv (juxt :uid :side) (:players new-lobby))))))

      (testing "a late click on the closed game does nothing"
        (let [lobbies (:lobbies @app-state/app-state)]
          (click-rematch! "bob" "old")
          (is (= lobbies (:lobbies @app-state/app-state))))))))

(deftest rematch-only-after-game-end-test
  (with-lobby
    (finished-game "running" {})
    (fn [log]
      (click-rematch! "alice" "running")
      (is (nil? (:rematch-requests (app-state/get-lobby "running"))))
      (is (empty? @log)))))
