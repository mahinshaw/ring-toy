;; www.learnclojure.com/2013/01 ring tutorials.
;; this class represents the final product (the last of his tutorial)
;; It was helpful in understanding all the things that go on in a web app.

(ns ring-toy.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.util.request :as req]
            [ring.util.response :as resp]
            [ring.middleware.stacktrace :as trace]
            [ring.middleware.params :as params]
            [ring.middleware.cookies :as cookies]
            [ring.middleware.session :as session]
            [ring.middleware.session.cookie :as scookie]
            [ring.middleware.session.memory :as mem]
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

(defn header1 [header]
  (str "<h1>" header "</h1>"))

(defn header-and-link [header href title]
  (str (header1 header) " " (link href title)))

(defn html-escape [string]
  (str/escape string {\< "&lt;", \" "&quot;", \& "&amp;", \> "&gt;"}))

(defn html-pre-escape [string]
  (str "<pre>" (html-escape string) "</pre>"))

(defn hppp[s] (html-pre-escape (with-out-str (binding [pp/*print-right-margin* 120] (pp/pprint s)))))
(defn hpp[s] (html-pre-escape (str s)))
(defn hp[s] (html-escape (str s)))

;; Remove some of the less interesting keys in teh map.
(def kill-keys [:body :character-encoding :remote-addr :server-name :server-port :ssl-client-cert :scheme :content-type :content-length])
(def kill-headers ["user-agent" "accept" "accept-encoding" "accept-language" "accept-charset" "cache-control" "connection"])

(defn format-request [name request]
  (let [r1 (reduce dissoc request kill-keys)
        r (reduce (fn [h n] (update-in h [:headers] dissoc n)) r1 kill-headers)]
    (with-out-str
      (println "---------------------------------")
      (println name)
      (println "---------------------------------")
      (pp/pprint r)
      (println "---------------------------------"))))

(defn wrap-spy [handler spyname]
  ;; Print the request and response.
  (fn [request]
    (let [incoming (format-request (str spyname ":\n Incoming Request:") request)]
      (println incoming)
      (let [response (handler request)]
        (let [outgoing (format-request (str spyname ":\n Outgoing Response Map:") response)]
          (println outgoing)
          (if (= (type (response :body)) java.lang.String)
            (update-in response [:body] (fn [x] (str (html-pre-escape incoming) x (html-pre-escape outgoing))))
            response))))))

(defn status-response [code body]
  {:status code
   :headers {"Content-Type" "text/html"}
   :body body})

(def ok-response (partial status-response 200))

(defn good [request]
  (let [good (get-in request [:session :good] 0)]
    (assoc (ok-response (header-and-link "Good" "/" "Choose Again"))
      :session (assoc (request :session) :good (inc good)))))

(defn evil [request]
  (let [evil (get-in request [:session :evil] 0)]
    (assoc (ok-response (header-and-link "Evil" "/" "Choose Again"))
      :session (assoc (request :session) :evil (inc evil)))))

(defn home [request]
  (let [good (get-in request [:session :good] 0)
        evil (get-in request [:session :evil] 0)
        name (get-in request [:session :name] "The Anonymous User")]
    (ok-response (str (header1 "The Moral Maze")
                      "<p>Welcomes: <b>" name "</b>"
                      (str " (" (link "/namechange" "Change") ")")
                      (str "<p> (" (link "/change-identity" (str "Not " name "? Log in as someone else.")))
                      "<p>Good " good " : Evil " evil
                      "<p> What do you choose: "
                      (str (link "/good" "Good") " or " (link "/evil" "Evil") "?")
                      (str "<p><hr/>" (link "/database" "Database") " or " (link "/highscores" "Highscores"))))))

(defn namechange [request]
  (ok-response (str "<form name=\"form\" method=\"post\" action=\"/change-my-name\">"
                    "<input name=\"newname\" value=\"" ((request :session) :name "type name here") "\">")))

(defn change-my-name [request]
  (let [newname ((request :params) "newname")]
    (assoc (ok-response (str "ok " newname "<p>" (link "/" "back")))
      :session (assoc (request :session) :name newname))))

(defn change-my-identity [request]
  (let [newid ((request :params) "newidentity")]
    (if-let [newsessioncookie (ffirst (filter (fn [[k v]] (= (v :name) newid)) @db))]
      (assoc (ok-response (str "if you say so...<i>" newid "</i><p>" (link "/" "Home")))
        :cookies {"ring-session" {:value newsessioncookie}})
      (ok-response "<span style=\"color:red\"><b><i>I Think Not!</i></b></span>"))))

(defn change-identity [request]
  (ok-response (str "<form name=\"form\" method=\"post\" action=\"/change-my-identity\">"
                 "If you ain't " ((request :session) :name "dat geezer") " den who <i>are</i> you? :"
                 "<input name=\"newidentity\" value=\"" ((request :session) :name "type name here") "\">")))

(defn get-anon []
  (str "anonuser" (. java.util.UUID randomUUID)))

;; (defn handler [request]
;;   (case (request :uri)
;;     "/" (home request)
;;     "/good" (good request)
;;     "/evil" (evil request)
;;     (status-response 404 (str "<h1> 404 Where are you? " (request :uri) "</h1>"))))

;; (defn old-handler [request]
;;   (if-let [userid ((request :session) :_userid)]
;;     (do
;;       (println "request-from: " userid)
;;       (subhandler (assoc request :userid userid)))
;;     (let [userid (get-anon)]
;;       (println "assigning new id: " userid)
;;       (let [oldsession (request :session)]
;;         (let [response (subhandler (assoc request :userid userid))]
;;           (if-let [newsession (response :session)]
;;             (assoc response :session (assoc newsession :_userid userid))
;;             (assoc response :session (assoc oldsession :_userid userid))))))))

(defmacro routefn [& addresses]
  `(fn [~'request]
     (case (~'request :uri)
       ~@(mapcat (fn [x] [(str "/" x) (list x 'request)]) addresses)
       "/" (home ~'request)
       (status-response 404 (str "<h1> 404 Where are you? " (:uri ~'request) "</h1>")))))

(defonce db (atom {}))

(defn database [request]
  (ok-response
   (str (header1 "Database")
        "<pre>" "(swap! db (fn [x] (merge x " (hppp @db) ")))" "</pre>")))

(defn highscores [request]
  (let [score (fn [[k v]]
                (let [e (v :evil 0)
                      g (v :good 0)
                      n (v :name "anon")
                      r (if (zero? (+ e g)) 1/2 (/ e (+ e g)))]
                  [r n k g e]))
        hst (sort (map score @db))]
    (ok-response (str
                  (header1 "High Score Table")
                  "<table>"
                  (str "<tr>" "<td>" "User ID" "<th/>" "<th>" "Chose Good" "<th/>" "<th>" "Chose Evil" "<th/>" "</tr>")
                  (apply str (for [i hst] (str "<tr>" "<td>" (hp (i 1)) "<td/>" "<td>" (hp (i 2)) "<td/>" "<td>" (hp (i 3)) "<td/>" "</tr>")))
                  "</table>"))))

(def handler (routefn good evil database highscores namechange change-my-name change-my-identity change-identity))

(def app
  (-> #'handler
      (trace/wrap-stacktrace)
      (wrap-spy "Handler.")
      (params/wrap-params)
      ;; (flash/wrap-flash)
      ;; (wrap-spy "What the Flash Middleware sees.")
      (session/wrap-session {:store (mem/memory-store db)})
      ;; (cookies/wrap-cookies)
      ;; (wrap-spy "What the Webserver sees.")
      (trace/wrap-stacktrace)))

(defonce server (jetty/run-jetty #'app {:port 8080 :join? false}))

(defn sprocess [session uri]
  (let [ns (:session (handler {:uri uri :session session}))]
    (if (nil? ns) session ns)))
