(ns ring-toy.core-test
  (:require [clojure.test :refer :all]
            [ring-toy.core :refer :all]))

(deftest sitetest
  (testing "page status"
    (is (= (map (fn [x] ((handler {:uri x}) :status)) ["/" "/good" "/evil"]) '(200 200 200)))
    (is (= (map (fn [x] ((handler {:uri x}) :status)) ["/home" "/favicon.ico"]) '(404 404))))
  (testing "html"
    (is (re-find #"Good\W*0\W:\WEvil\W0" ((handler {:uri "/"}) :body)))
    (is (re-find #"Good\W*10\W:\WEvil\W0" ((handler {:uri "/" :session {:good 10}}) :body)))
    (is (re-find #"Good\W*10\W:\WEvil\W20" ((handler {:uri "/" :session {:good 10 :evil 20}}) :body))))
  (testing "session"
    (is (= 21 (((handler {:uri "/evil" :session {:good 10 :evil 20}}) :session) :evil)))
    (is (= 10 (((handler {:uri "/evil" :session {:good 10 :evil 20}}) :session) :good)))
    (is (= (reduce sprocess {:userid "fred" :good 2}
                   ["/evil" "/good" "/" "/home" "/evil" "/favicon.ico" "/evil" "/evil"])
           {:good 3, :evil 4, :userid "fred"}))))
