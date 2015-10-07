(ns stars.handler
  (:require [compojure.core :refer [GET defroutes]]
            [compojure.route :refer [not-found resources]]
            [ring.middleware.defaults :refer [site-defaults wrap-defaults]]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [include-js include-css]]
            [prone.middleware :refer [wrap-exceptions]]
            [ring.middleware.reload :refer [wrap-reload]]
            [environ.core :refer [env]]))

(def home-page
  (html
   [:html
    [:head
     [:meta {:charset "utf-8"}]
     [:meta {:name    "viewport"
             :content "width=device-width, initial-scale=1"}]
     [:meta {:http-equiv "x-ua-compatible"
             :content    "ie=edge"}]
     (apply include-css
            (if (env :dev)
              ["vendor/bootstrap/dist/css/bootstrap.css"
               "vendor/fontawesome/css/font-awesome.css"
               "css/site.css"]
              ["vendor/bootstrap/dist/css/bootstrap.min.css"
               "vendor/fontawesome/css/font-awesome.min.css"
               "css/site.min.css"]))]
    [:body
     [:div#app]
     (include-js "js/app.js")
     ]]))

(defroutes routes
           (GET "/" [] home-page)
           (resources "/")
           (not-found "Not Found"))

(def app
  (let [handler (wrap-defaults #'routes site-defaults)]
    (if (env :dev) (-> handler wrap-exceptions wrap-reload) handler)))
