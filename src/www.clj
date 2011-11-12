; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(require '[clojure.string :as str])

(defmacro ! [& ss]
  `(-> (html ~@ss)
       (str/replace #"`(.*?)`" #(html [:code (second %)]))
       (str/replace #"\*\*(.*?)\*\*" #(html [:b (second %)]))
       (str/replace #"\*(.*?)\*" #(html [:i (second %)]))))

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
   [:li :margin-bottom :0.2em]
   [:p :margin "0.3em 0 0.5em"]
   [:code :background-color :#F8F8FF
          :border "1px solid #DEDEDE"
          :color :#444444
          :font-size :1.1em
          :padding "0 0.2em"]
   [(each :h1 :h2 :h3) :font-family :$font-title]
   ;; sections
   [:#wrapper :background-color :$color-main
              :border "solid 5px $color-main"
              :margin "0 15%"]
   [(each :#header :#content :#footer) :padding :1em]
   [(each :#header :#footer) :color :white]
   ;; links
   [:a :color :#6a9 :text-decoration :none]
   [:a:visited :color :#497]
   [:a:hover :color :#798 :text-decoration :underline]
   ;; header
   [:#header
    [:h1 :font-style :italic
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
   [:#content :background-color :#f6f6f6
              :background-image "url('img/tracks.png')"
              :background-repeat :repeat-y
              :border-radius :1em
              :clear :both
              :color :$color-main
              :padding "1em 1.5em 2em 200px"
    [:div.section :padding-bottom :1em
                  :padding-left :0.5em
     [:h2 :border-bottom "1px solid #aaa"
          :margin-bottom :0.5em
          :margin-left :-0.5em
          :padding-bottom :0.2em
          :font-size :2em]]
    [:div.subsection :margin-bottom :1em
                     :padding-left :0.5em
     [:h3 :font-size :1.4em
          :margin "0.5em 0"
          :margin-left :-0.5em]]
    ;; downloads
    [:div#downloads :text-align :center
     [:h2 :text-align :left]
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
                           :margin-left "1em !important"
                           :padding "0.7em 0.3em"
                           :width "85% !important"]))

(defn page-layout [title body]
  (html
   (doctype :html5)
   [:html
    [:head [:title (str "Lobos - " title)]
     [:link {:rel "shortcut icon" :href "favicon.ico" :type "image/x-icon"}]
     [:style {:type "text/css"} reset-style]
     [:style {:type "text/css"} page-style]
     ;; jQuery
     (include-js "https://ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.min.js")
     (include-js "js/main.js")
     ;; SyntaxHighlighter
     (include-js "sh/js/shCore.js" "sh/js/shBrushXml.js" "sh/js/shBrushClojure.js")
     (include-css "sh/css/shCore.css" "sh/css/shThemeDjango.css")
     ;; Google Analytics Tracker
     [:script {:type "text/javascript"}
      (slurp "www/js/ga-tracker.js")]]
    [:body
     [:div#wrapper
      [:div#header
       [:img#logo {:src "img/logo.png"}]
       [:h1 "Lobos"]
       (unordered-list
        [(link-to "index.html" "Home")
         (link-to "documentation.html" "Documention")
         (link-to "downloads.html" "Downloads")
         (link-to "contribute.html" "Contribute")])]
      [:div#content body]
      [:div#footer
       [:p "&copy; 2011 Nicolas Buduroi. All rights reserved."]]]]]))

(defn section [title & content]
  (-> content
      (conj [:h2 title] :div.section)
      vec))

(defn subsection [title & content]
  (-> content
      (conj [:h3 title] :div.subsection)
      vec))

(defpage home ["Home" "index"]
  (section "Description"

    [:p [:b "Lobos"] " is a library to help you create and modify
    database schemas using Clojure code. It currently has support for
    H2, MySQL, PostgreSQL, SQLite and SQL Server."]

    [:p "It is currently being actively developed and its API isn't
    stable yet. In its present form, it only provide an imperative DSL
    to generated database specific data definition statements. That is
    the foundation upon which the migrations and declarative schema
    features will be built."])

  
  (section "Installation"
  
    [:p "You can use " [:b "Lobos"] " in your projects using either
    Leiningen or Cake by adding the following dependency to you project
    file:"]

    [:pre.brush:.clojure "[lobos \"0.8.0\"]"]

    [:p [:b "Lobos"] " is also available through Maven:"]

    [:pre.brush:.xml (h
"<dependency>
  <groupId>lobos</groupId>
  <artifactId>lobos</artifactId>
  <version>0.8.0</version>
</dependency>")]

    [:p "You can always add the current or previous version manually,
    consult the " (link-to "downloads.html" "downloads page") " to find
    the version you want."])

  (section "Example"
             
    [:p [:b "Lobos"] " is easy to use, you can try it by creating a new
    Leiningen project, adding the " [:b "Lobos"] " dependency as above
    and another dependency for the database system you want to
    use. Here's an example using the H2 database:"]
    
    [:pre.brush:.clojure "[com.h2database/h2 \"1.3.154\"]"]

    [:p "Now fire up a REPL and enter the following code:"]
    
    [:pre.brush:.clojure (h
"(use 'lobos.connectivity
     'lobos.core
     'lobos.schema)

(def h2
  {:classname   \"org.h2.Driver\"
   :subprotocol \"h2\"
   :subname     \"./lobos\"})

(create h2
  (table :users
    (integer :id :primary-key)
    (varchar :name 100)))")]

    [:p "For a more complete understanding of how to use "
    [:b "Lobos"] " refer to
    the " (link-to "documentation.html" "documentation page") "."]))

(defn download-link [tag type text]
  (link-to (str "https://github.com/budu/lobos/" type "ball/" tag)
           text))

(defn download-links [tag & [text]]
  (html
   [:div.download (or text (str "Lobos version " tag ":") " ")
    (download-link tag "zip" "download .zip")
    (download-link tag "tar" "download .tar.gz")] [:br]))

(defpage documentation ["Documentation"]
  (section "Actions"
           
    [:p (!"There's three different kind of actions supported by
    **Lobos**: `create`, `alter` and `drop`. All actions take an
    optional first argument which can either be a connection key, a
    connection spec or a schema. Actions are directly executed against
    the given or default connection.")]

    (subsection "create"
                
      [:p (!"The `create` action can be use to insert schemas, tables or
      indexes.")]

          [:pre.brush:.clojure (h
"(create
  (table :users
    (integer :id :primary-key)
    (varchar :name 100)))")])
    
    (subsection "alter"
                
      [:p (!"The `alter` action only purpose is the modify a table
      definition.")]

      [:pre.brush:.clojure (h
"(alter (table :users (check :name (> (length :name) 1))))")])
    
    (subsection "drop"

      [:p (!"The `drop` action can be use to remove schemas, tables or
      indexes.")]

      [:pre.code.brush:.clojure "(drop (table :users))"]))

  (section "Elements"
           
    [:p "There's a complete set of schema elements which are an abstract
    representation of SQL objects and their constituent. They've been
    designed to be easily composed so that you can write you're own
    helpers to create schemas of arbitrary complexity."]

    [:p (!"The first one is the `Schema` element, which can be used to
    create or drop schemas. It can also be used as the first argument of
    an action, in which case the schema elements given will produce
    qualified identifiers once compiled.")]

    [:pre.code.brush:.clojure "[schema-name options? & elements]"]

    [:p (!"It can take an optional map of options which for now only
    support the `:db-spec` key. You can specify a number of schema
    elements that will be created if that schema is passed to the create
    action.")]

    (subsection "Schema Elements"
      [:h4 "Table"]

      [:p (!"The `Table` element is a macro that compose its given
      elements. It can take columns, constraints or indexes in any
      order.")]

      [:pre.code.brush:.clojure "([name & elements])
Macro
  Constructs an abstract table definition containing the given
  elements. Takes an arbitrary number of table elements."]

      [:p "All table elements are functions taking a table element as
      first argument. You can write your own helpers to construct
      custom table macro like in the following example."]

      [:pre.code.brush:.clojure "(defn surrogate-key [table]
  (integer table :id :auto-inc :primary-key))

(defn datetime-tracked [table]
  (-> table
      (timestamp :updated_on)
      (timestamp :created_on (default (now)))))

(defmacro tbl [name & elements]
  `(-> (table ~name
         (surrogate-key)
         (datetime-tracked))
       ~@elements))"]

      [:br]
      [:h4 "Index"]

      [:p "Indexes may be created outside a table definition. They need
      at least a table name and a sequence of column names."]

      [:pre.code.brush:.clojure "(index :users [:name])"]

      [:p (!"You can also provide an index name and some options. For now
      only the `:unique` option is recognized.")]

      [:pre.code.brush:.clojure "(index :users :users_unique_name [:name] :unique)"]

      [:p (!"Take note that at this point, you must provide an index name
      when you're calling `index` with options.")])
    
    (subsection "Table Elements"

      [:p (!"Table elements are meant to be used inside the `table`
      macro. There are three type of table elements: `Column`,
      `Constraint` and `Index`.")]

      [:h4 "Column"]

      [:p (!"Columns are contructed using the `column` function or the
      typed column function that we'll talked about later. This function
      take the table it will be attached to, a column name, an optional
      data-type definition and a set of options.")]

      [:pre.code.brush:.clojure "(column *table* :name (data-type :varchar 100) :unique :not-null)"]

      [:p "Here's a list of available options:"]

    	(unordered-list (map #(! %) [
        "`:unique` which construct an unique constraint on that column"
        "`:primary-key` which make the current column the primary key"
        "`[:refer tname & options]` which add a foreign key constraint to
     the specified table. The options are the same as the `foreign-key`
     function with the expection that you can specify only one parent
     column."
        "`:not-null` prevents this column from being null"
        "`:auto-inc` (for integers types) which makes it auto-populated with
     incremented integers"
        "`[:encoding enc]` (for character types) determines which encoding to
     use if supported by the database. Also see the natianal character types."
        "`[:collate type]` (for character types) determines how equality is
     handled"
        "`:time-zone` (for time types) determines if the type includes a time-zone"]))

      [:h4 "Constraint"]

      [:p (!"There's three types of constraint: `UniqueConstraint`,
      `ForeignKeyConstraint` and `CheckConstraint`. Each can be
      construction using specific helpers with the exception of
      `UniqueConstraint` which have two.")]

      [:pre.code.brush:.clojure "(unique *table* [:name])"]

      [:p "and"]

      [:pre.code.brush:.clojure "(primary-key *table* [:id])"]

      [:p (!"Foreign key constraint can be defined using the
      `foreign-key` functions.")]

      [:pre.code.brush:.clojure "[table name? columns parent-table parent-columns? match? & triggered-actions]"]

      [:p (!"If `parent-columns` aren't specified, the `columns` will be used.")]

      [:p (!"The `match` optional argument can be one of `:full`,
      `:partial` or `:simple`, but note that this isn't supported by
      most databases.")]

      [:p (!"You can specify `triggered-actions` with pairs of keyword,
      the first of the pairs must be one of `:on-delete` or
      `:on-update`, while the second one can be one of `:cascade`,
      `:set-null`, `:restrict`, `:set-default` or `:no-action`. The
      actions keywords are directly translated to SQL keywords, so you
      can specify custom ones if the database you're using provide more.")]

      [:p "You can optionally specify the constraint name just after the
      table for unique and foreign-key contraints."]

      [:h4 "Index"]

      [:p "Indexes can also be defined inside a table definition in the
      same way as a constraint. See the `Index` schema element
      documentation above."]))
  
  (section "Data Types"

    [:p "Columns can be defined using the data-type name as a
    function. This enable more compact table definition."]

    (subsection "Simple Typed Column"

      [:p "Simple typed columns don't take any arguments."]

    	(unordered-list (map #(! %) [ "`smallint`" "`integer`" "`bigint`" "`real`" "`double-precision`" ]))

      [:p (!"The `double-precision` typed column is aliased as `double`.")])

    (subsection "Numeric Column"

      [:p "Numeric columns take an optional precision and scale arguments."]

      [:pre.code.brush:.clojure "[table column-name & [precision scale & options]]"]

    	(unordered-list (map #(! %) [ "`numeric`" "`decimal`" ])))

    (subsection "Optional Precision Column"

      [:p "Optional precision columns take an optional precision argument."]

      [:pre.code.brush:.clojure "[table column-name & [precision & options]]"]

      (unordered-list (map #(! %) [ "`float`" ])))

    (subsection "Optional Length Column"

      [:p "Optional length columns take an optional length argument."]

      [:pre.code.brush:.clojure "[table column-name & [length & options]]"]

      (unordered-list (map #(! %) [ "`char`" "`nchar`" "`clob`" "`nclob`" "`binary`" "`blob`" ]))

      [:p (!"The `clob` and `nclob` typed columns are aliased as `text` and `ntext`.")])

    (subsection "Length Bounded Column"

      [:p "Length bounded columns must have a length argument."]

      [:pre.code.brush:.clojure "[table column-name length & options]"]

      (unordered-list (map #(! %) [ "`varchar`" "`nvarchar`" "`varbinary`" ]))))

  (section "Commented Code"
           
    [:p "For a better understanding of how this library work you can
    consult the commented source code:"]
    
    [:ul
     [:li (link-to "doc/uberdoc.frontend.html" "Frontend documentation")]
     [:li (link-to "doc/uberdoc.backend.html"  "Backend documentation for the compiler")]
     [:li (link-to "doc/uberdoc.analyzer.html" "Documentation for the analyzer")]
     [:li (link-to "doc/uberdoc.migration.html" "Migration documentation")]]))

(defpage downloads ["Downloads"]
  (let [tags (->> ".git/refs/tags"
                  java.io.File.
                  .listFiles
                  (map #(.getName %))
                  (sort)
                  (reverse))]
    (html
     (section "Latest Stable Version"
       (download-links (first tags)))
     (section "Latest Snapshot"
       (download-links "master" "Current Lobos snapshot:"))
     (section "Previous Versions"
       (for [tag (next tags)]
         (download-links tag))))))

(defpage contribute ["Contribute"]
  (section "Contributing to Lobos"
    [:p "As this project hasn't yet reached the magical 1.0 version,
    this is the best time for anyone to contribute their idea and/or
    code. If you want to participate, the first step would be to visit
    the online repository on "
    (link-to "https://github.com/budu/lobos" "github") "."]
    
    [:p "You can also visit the "
     (link-to "http://groups.google.com/group/lobos-library" "Google Group")
     " to ask questions about the code, start discussion about
     features, bug fixing or any subject related to the project."])
  
  (section "Issue Tracker"
    [:p "All issues, suggestions and requests should be added to the
    project's " (link-to "https://github.com/budu/lobos/issues" "issue
    tracker") "."])
  (section "Wiki"
    [:p "Finally a " (link-to "https://github.com/budu/lobos/wiki" "wiki")
    " is also available. Its main purpose is to discuss upcoming
    features in more depth before being implemented."]))

(def pages [home downloads documentation contribute])
