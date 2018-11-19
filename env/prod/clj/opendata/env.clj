(ns opendata.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[opendata started successfully]=-"))
   :stop
   (fn []
     (log/info "\n-=[opendata has shut down successfully]=-"))
   :middleware identity})
