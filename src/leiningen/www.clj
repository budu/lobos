; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns leiningen.www
  "Creates this project website."
  (:use (hiccup core page-helpers)
        cljss.core))

(def reset-style
  (css [(each :body :h1 :h2 :h3 :h4 :h5) :margin 0 :padding 0]))

(def css-vars
  {:font-title "'Palatino Linotype','Book Antiqua',Palatino,FreeSerif,serif"
   :font-main  "Verdana, Arial"
   :color-main "#404a3e"})

(def page-style
  (css css-vars
   ;; tags
   [:body :font-family :$font-main]
   [:pre :display :inline]
   [(each :h2 :p) :margin "0.7em 0"]
   ;; sections
   [:#wrapper :background-color :$color-main
              :border "solid 5px $color-main"
              :margin "0 10%"]
   [(each :#header :#content :#footer) :padding :1em]
   [(each :#header :#footer) :color :white]
   ;; links
   [:a :color :#69a :text-decoration :none]
   [:a:hover :color :#8aa :text-decoration :underline]
   [:a:visited :color :#479]
   ;; header
   [:#header
    [:h1 :font-family :$font-title
         :font-style :italic
         :font-size :4em
         :margin-left :155px]
    [:img :float :left
          :margin-bottom :1em]
    [:ul :display :inline
         :float :right
         :list-style :none
         :padding 0
     [:li :display :inline
          :margin "0 0.5em"]]]
   [($ :#header :ul :li)
    [(each :a :a.visited) :border-radius :0.5em
                          :background-color :#243
                          :color :#fa0
                          :padding "0.5em 1.5em"]
    [:a:hover :background-color :#687
              :text-decoration :none]]
   ;; content
   [:#content :background-color :#eee
              :background-image "url('img/tracks.png')"
              :background-repeat :repeat-y
              :border-radius :1em
              :clear :both
              :color :$color-main
              :padding "0.5em 1.5em"
              :padding-left :200px
    [:div.section :padding-left :0.5em
     [:h2 :margin-left :-0.5em]]
    ;; downloads
    [:div#downloads :text-align :center
     [:div.download :border "solid 1px $color-main"
                    :border-radius :0.5em
                    :display :inline-block
                    :margin :0.5em
                    :padding "0.5em 1em"
      [:a :margin "0 0 0 1em"]]]]
   ;; footer
   [($ :#footer :p) :font-size :0.7em
                    :padding-bottom :3em
                    :text-align :center]
   ;; SyntaxHighlighter
   [:div.syntaxhighlighter :border-radius :0.3em
                           :padding "0.5em 0.3em"]))

(defn page-layout [title body]
  (html
   (doctype :html5)
   [:html
    [:head [:title title]
     [:link {:rel "shortcut icon" :href "favicon.ico" :type "image/x-icon"}]
     [:style {:type "text/css"} reset-style]
     [:style {:type "text/css"} page-style]
     ;; jQuery
     (include-js "https://ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.min.js")
     (include-js "js/main.js")
     ;; SyntaxHighlighter
     (include-js "sh/js/shCore.js" "sh/js/shBrushXml.js" "sh/js/shBrushClojure.js")
     (include-css "sh/css/shCore.css" "sh/css/shThemeDjango.css")]
    [:body
     [:div#wrapper
      [:div#header
       [:img#logo {:src "img/logo.png"}]
       [:h1 "Lobos"]
       (unordered-list
        [(link-to "index.html" "Home")
         (link-to "downloads.html" "Downloads")
         (link-to "documentation.html" "Documention")
         (link-to "contribute.html" "Contribute")])]
      [:div#content body]
      [:div#footer
       [:p "&copy; 2011 Nicolas Buduroi. All rights reserved."]]]]]))

(defmacro defpage [pname [title & [filename]] & body]
  `(def ~pname {:filename ~(or filename (name pname))
                :content (page-layout ~title
                           (html [:div {:id ~(name pname)} ~@body]))}))

(defn section [title & content]
  (-> content
      (conj [:h2 title] :div.section)
      vec))

(defpage home ["Home" "index"]
  (section "Description"

    [:p [:b "Lobos"] " is a library to help you create and modify
    database schemas using Clojure code."]

    [:p "It is currently being actively developed and its API isn't
    stable yet. In its present form, it only provide an imperative DSL
    to generated database specific data definition statements. That is
    the foundation upon which the migration and declarative schema
    features will be built."])

  (section "Installation"
  
    [:p "You can use " [:b "Lobos"] " in your projects using either
    Leiningen or Cake by adding the following dependency to you project
    file:"]

    [:pre.brush:.clojure "[lobos \"0.6.0-SNAPSHOT\"]"]

    [:p [:b "Lobos"] " is also available through Maven:"]

    [:pre.brush:.xml (h
"<dependency>
  <groupId>lobos</groupId>
  <artifactId>lobos</artifactId>
  <version>0.6.0-SNAPSHOT</version>
</dependency>")]

    [:p "You can always add the current or previous version manually,
    consult the " (link-to "downloads.html" "downloads page") " to find
    the version you want."]))

(defn download-link [tag type text]
  (link-to (str "https://github.com/budu/lobos/" type "ball/" tag)
           text))

(defn download-links [tag & [text]]
  (html
   [:div.download (or text (str "Lobos version " tag ":") " ")
    (download-link tag "zip" "download .zip")
    (download-link tag "tar" "download .tar.gz")] [:br]))

(defpage downloads ["Downloads"]
  (let [tags (->> ".git/refs/tags"
                  java.io.File.
                  .listFiles
                  (map #(.getName %))
                  (reverse))]
    (html
     (section "Latest Stable Version"
       (download-links (first tags)))
     (section "Latest Snapshot"
       (download-links "master" "Current Lobos snapshot:"))
     (section "Previous Versions"
       (for [tag (next tags)]
         (download-links tag))))))

(defpage documentation ["Documentation"]
  (section "Documentation"
    [:p "For now, only the commented code generated with Marginalia is
    available:"]
    [:ul
     [:li (link-to "doc/uberdoc.frontend.html" "Frontend documentation")]
     [:li (link-to "doc/uberdoc.backend.html"  "Backend documentation for the compiler")]
     [:li (link-to "doc/uberdoc.analyzer.html" "Documentation for the analyzer")]]))


(defpage contribute ["Contribute"]
  (section "Contributing to Lobos"
    [:p "As this project hasn't yet reached the magical 1.0 version,
    this is the best time for anyone to contribute their idea and/or
    code. If you want to participate the first step would be to visit
    the online repository on "
    (link-to "https://github.com/budu/lobos" "Github") "."]
    [:p "You can also " (link-to "mailto:nbuduroi@gmail.com" "contact me")
    " directly for any questions about this project."])
  (section "Issue Tracker"
    [:p "All issues, suggestions and requests should be added to the
    project's " (link-to "https://github.com/budu/lobos/issues" "issue
    tracker") "."])
  (section "Wiki"
    [:p "Finally a " (link-to "https://github.com/budu/lobos/wiki" "wiki")
    " is also available. Its main purpose is to discuss upcoming
    features in more depth before being implemented."]))

(defn www
  "Creates this project website."
  [project & args]
  (doseq [page [home downloads documentation contribute]]
    (spit (str "www/" (:filename page) ".html") (:content page))))
