(ns ring-toy.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.stacktrace :as trace]
            [ring.middleware.params :as params]
            [ring.middleware.cookies :as cookies]
            [ring.middleware.session :as session]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn seen-before [request]
  (try (Integer/parseInt (((request :cookies) "yo") :value))
       (catch Exception e :never-before)))

(defn handler [request]
  ;; handle our request when its good (200).
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str "<h1>Hello World</h1>")
   :session (let [rs (request :session)]
              (if empty? rs)
              "I am a session. Fear me."
              (str rs "!"))})

(defn html-escape [string]
  (str/escape string {\< "&lt;", \> "&gt;"}))

(defn html-preformatted-escape [string]
  (str "<pre>" (html-escape string) "</pre>"))

(defn format-request [name request kill-keys kill-headers]
  (let [r1 (reduce dissoc request kill-keys)
        r (reduce (fn [h n] (update-in h [:headers] dissoc n)) r1 kill-headers)]
    (with-out-str
      (println "---------------------------------")
      (println name)
      (println "---------------------------------")
      (pp/pprint r)
      (println "---------------------------------"))))

;; Remove some of the less interesting keys in teh map.
(def kill-keys [:body :character-encoding :remote-addr :server-name :server-port :ssl-client-cert :scheme :content-type :content-length])
(def kill-headers ["user-agent" "accept" "accept-encoding" "accept-language" "accept-charset" "cache-control" "connection"])

(defn wrap-spy [handler spyname include-body]
  ;; Print the request and response.
  (fn [request]
    (let [incoming (format-request (str spyname ":\n Incoming Request:") request kill-keys kill-headers)]
      (println incoming)
      (let [response (handler request)]
        (let [r (if include-body response (assoc response :body "#<?>"))
              outgoing (format-request (str spyname ":\n Outgoing Response Map:") r kill-keys kill-headers)]
          (println outgoing)
          (update-in response [:body] (fn [x] (str (html-preformatted-escape incoming) x (html-preformatted-escape outgoing)))))))))

(def app
  (-> #'handler
      (trace/wrap-stacktrace)
      (wrap-spy "What the Handler sees." true)
      (session/wrap-session)
      (cookies/wrap-cookies)
      (params/wrap-params)
      (wrap-spy "What the Webserver sees." false)
      (trace/wrap-stacktrace)))

(defonce server (jetty/run-jetty #'app {:port 8080 :join? false}))
