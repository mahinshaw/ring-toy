(ns ring-toy.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.util.request :as req]
            [ring.util.response :as resp]
            [ring.middleware.stacktrace :as trace]
            [ring.middleware.params :as params]
            [ring.middleware.cookies :as cookies]
            [ring.middleware.session :as session]
            [ring.middleware.flash :as flash]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

;; function to discover if the cookie named "yo" exists.
(defn seen-before [request]
  (try (Integer/parseInt (((request :cookies) "yo") :value))
       (catch Exception e :never-before)))

(defn link
  ([href] (str "<a href=\"" href "\">" href "</a>"))
  ([href title] (str "<a href=\"" href "\">" title "</a>")))

(defn html-escape [string]
  (str/escape string {\< "&lt;", \> "&gt;"}))

(defn html-preformatted-escape [string]
  (str "<pre>" (html-escape string) "</pre>"))

(defn format-request [name request]
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

(defn wrap-spy [handler spyname]
  ;; Print the request and response.
  (fn [request]
    (let [incoming (format-request (str spyname ":\n Incoming Request:") request)]
      (println incoming)
      (let [response (handler request)]
        (let [outgoing (format-request (str spyname ":\n Outgoing Response Map:") response)]
          (println outgoing)
          (update-in response [:body] (fn [x] (str (html-preformatted-escape incoming) x (html-preformatted-escape outgoing)))))))))

;; fancy hander!
;; (defn handler [request]
;;   ;; Handlin' requests!
;;   (case (req/request-url request)
;;     "/favicon.ico" {:status 404
;;                     :body (str (link "/" "Go Home!"))
;;                     :session (update-in (request :session) [:favicon] (fnil inc 0))}
;;     "/" {:body (str "<h1>home " (request :flash) "</h1>"
;;                     "<p> favicon requests: " (get-in request [:session :favicon] 0)
;;                     "<p> bother requests: " (get-in request [:session :bother] 0)
;;                     "<p>" (link "/bother")
;;                     "<p>" (link "/"))}
;;     "/bother" {:status 302, :headers {"location" "/"}, :body ""
;;                :flash "(bothered)"
;;                :session (update-in (request :session) [:bother] (fnil inc 0))}))
(defn status-response [code body]
  {:status code
   :headers {"Content-Type" "text/html"}
   :body body})

(def ok-response (partial status-response 200))

(def goodness (atom 0))
(def evilness (atom 0))

(defn good [request]
  (let [good (get-in request [:session :good] 0)]
    (swap! goodness inc)
    (assoc (resp/redirect "/")
      :flash :good
      :session (assoc (request :session) :good (inc good)))))

(defn evil [request]
  (let [evil (get-in request [:session :evil] 0)]
    (swap! evilness inc)
    (assoc (resp/redirect "/")
      :flash :evil
      :session (assoc (request :session) :evil (inc evil)))))

(defn home [request]
  (let [f (request :flash)
        good (get-in request [:session :good] 0)
        evil (get-in request [:session :evil] 0)]
    (response (str "<h1>The Moral Maze</h1>"
                   "Good " good " : Evil " evil "<p>"
                   (if f
                     (str "You last chose " (if (= f :evil) "Evil" "Good") ".<p> What do you choose now: ")
                     "What do you choose: ")
                   (str (link "/good" "Good") " or " (link "/evil" "Evil") "?")
                   "<p> Global Good: " @goodness " Evil: " @evilness))))

(defn handler [request]
  (case (request :uri)
    "/" (home request)
    "/good" (good request)
    "/evil" (evil request)
    "/favicon.ico" {:flash (request :flash)}
    (status-response 404 (str "<h1> 404 Where are you? " (request :uri) "</h1>"))))

(def app
  (-> #'handler
      (trace/wrap-stacktrace)
      (wrap-spy "What the Handler sees.")
      (flash/wrap-flash)
      (wrap-spy "What the Flash Middleware sees.")
      (session/wrap-session)
      (cookies/wrap-cookies)
      (params/wrap-params)
      (wrap-spy "What the Webserver sees.")
      (trace/wrap-stacktrace)))

(defonce server (jetty/run-jetty #'app {:port 8080 :join? false}))
