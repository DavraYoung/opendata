(ns opendata.telegram.core
  (:require [clojure.core.async :refer [<!!]]
            [opendata.config :refer :all]
            [morse.handlers :as handler]
            [morse.polling :as polling-server]
            [mount.core :refer :all]
            [morse.api :as t])
  (:gen-class))

(def token (env :telegram-token))

(handler/defhandler
  handler
  (handler/command-fn
    "start"
    (fn [{{id :id :as chat} :chat :as fuck}]
      (println "Bot joined new chat: " chat)
      (t/send-text token id "Welcome to telegram-bot-template!")))

  (handler/command-fn
    "help"
    (fn [{{id :id :as chat} :chat}]
      (println "Help was requested in " chat)
      (t/send-text token id "Help is on the way")))

  (handler/message-fn
    (fn [{{id :id} :chat :as message}]
      (println "Intercepted message: " message)
      (t/send-text token id "I don't do a whole lot ... yet."))))


(defstate telegram-server
          :start (polling-server/start (env :telegram-token) handler)
          :stop (polling-server/stop telegram-server))
