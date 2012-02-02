# Lobos

**Lobos** is a SQL database schema manipulation and migration library
written in [Clojure]. It currently support supports H2, MySQL,
PostgreSQL, SQLite and SQL Server.

If you have any questions, please join the [mailing list].

## Features

 * A comprehensive data definition language DSL.
 * Migrations for schema changes.
 * An analyzer to query the schema.

## Installation

Lobos is available through [Clojars].

For the latest release, in Cake/Leiningen, use:

#### `project.clj`
```clojure
:dependencies [[lobos "1.0.0-SNAPSHOT"]]
```

or in Maven:

#### `pom.xml`
```xml
<dependency>
  <groupId>lobos</groupId>
  <artifactId>lobos</artifactId>
  <version>1.0.0-SNAPSHOT</version>
</dependency>
```

## Usage

Here's a small tutorial on how to use Lobos.

### Basic Example

Start by creating a new project with your preferred tool. Assuming a
[Leiningen] compatible project file, add **Lobos** and a database driver
to the `dependencies` section:

```clojure
:dependencies [...
               [lobos "1.0.0-SNAPSHOT"]
               [postgresql "9.1-901.jdbc4"]]
```

Once you have your dependencies downloaded, open up a REPL and load
these namespaces:

```clojure
(use '(lobos connectivity core schema))
```

You'll get warnings about some already defined function, just ignore
this for now.

Then you'll need to create a connection, the following snippet define
one and makes it the default global connection:

```clojure
(def db
     {:classname "org.postgresql.Driver"
      :subprotocol "postgresql"
      :user "test"
      :password "test123"
      :subname "//localhost:5432/test"})

(open-global db)
```

You can now send DDL statements (called *actions*) directly like this:

```clojure
(create (table :users (integer :id :unique)))
```

You can omit the connection altogether. In that case, actions will use
the connection bound by `with-connection` or the default one.

```clojure
(drop (table :users))
```

Now that you've tested the basics of **Lobos** at the REPL, let's try
out a more real-world example.

### Real-world Example

By *real-world*, I'm not talking about a production ready database
schema, it's about how to integrate **Lobos** into your real
projects. We'll continue using the previously created test project, but
this time we'll use files. In your `src/` directory, create a directory
named `lobos` and a file called `config.clj` inside it.

#### `src/lobos/config.clj`
```clojure
(ns lobos.config
  (:use lobos.connectivity))

(def db
  {:classname "org.postgresql.Driver"
   :subprotocol "postgresql"
   :user "test"
   :password "test123"
   :subname "//localhost:5432/test"})

(open-global db)
```

Next, we'll see how to create helpers that will show the composable
nature of the **Lobos** DSL. This part is entirely optional.

Add a new file called `helpers.clj`

#### `src/lobos/helpers.clj`
```clojure
(ns lobos.helpers
  (:refer-clojure :exclude [bigint boolean char double float time])
  (:use (lobos schema)))
```

The above namespace declaration exclude some `clojure.core` definitions
that clashes with data-type functions, this prevents warnings from being
shown.

Every schema element definition is a simple function that return an
intermediate representation that abstract real database schema element.
This allow you to make your own functions (and macros) that can help
define a database schema more concisely.

Now, let's define a bunch of useful helpers:

#### `src/lobos/helpers.clj`
```clojure
(defn surrogate-key [table]
  (integer table :id :auto-inc :primary-key))

(defn timestamps [table]
  (-> table
      (timestamp :updated_on)
      (timestamp :created_on (default (now)))))

(defn refer-to [table ptable]
  (let [cname (-> (->> ptable name butlast (apply str))
                  (str "_id")
                  keyword)]
    (integer table cname [:refer ptable :id :on-delete :set-null])))
```

The first one add a standardized surrogate key called `id` to the
specified table. The second takes a table definition and add two
generally useful columns to it. Finally, the third one create a
foreign-key column to another table given its name. We can use these
helpers in the same way as already existing column definition functions.

To wrap it up, we'll add a macro that we'll use instead of the included
`table` macro. It will help us create tables which implicitly include a
surrogate key and the timestamp columns.

#### `src/lobos/helpers.clj`
```clojure
(defmacro tbl [name & elements]
  `(-> (table ~name
              (surrogate-key)
              (timestamps))
       ~@elements))
```

We have everything set up in place to create our first migrations, so
let's do that.

### Migrations

By default all migrations are kept in a single file in the
`lobos.migrations` namespace. It'll get automatically loaded by
migration commands, so there's no need to load it yourself. This is a normal
Clojure source file so if you prefer having only one migration per file,
just do that and require these files in the migrations namespace.

#### `src/lobos/migrations.clj`
```clojure
(ns lobos.migrations
  (:refer-clojure :exclude [alter drop
                            bigint boolean char double float time])
  (:use (lobos [migration :only [defmigration]] core schema
               config helpers)))
```

Migrations are define using the `defmigration` macro which is composed
of two bodies, one making whatever changes you want to do, the other
reverting those changes.

#### `src/lobos/migrations.clj`
```clojure
(defmigration add-users-table
  (up [] (create
          (tbl :users
            (varchar :name 100 :unique)
            (check :name (> (length :name) 1)))))
  (down [] (drop (table :users))))

(defmigration add-posts-table
  (up [] (create
          (tbl :posts
            (varchar :title 200 :unique)
            (text :content)
            (refer-to :users))))
  (down [] (drop (table :posts))))

(defmigration add-comments-table
  (up [] (create
          (tbl :comments
            (text :content)
            (refer-to :users)
            (refer-to :posts))))
  (down [] (drop (table :comments))))
```

Each migrations must have a unique name that will be used by **Lobos**
to figure out which ones have been applied.

To apply all pending migrations, use the `migrate` function in a REPL:

```clojure
(migrate)
;=> add-users-table
;=> add-posts-table
;=> add-comments-table
```

This function can take a bunch of migration names in which case it will
only apply those.

If, for some reason, you need to rollback some migrations, use the aptly
named `rollback` function:

```clojure
(rollback)
;=> add-comments-table
```

By default it will rollback only the most recently applied migration. It
can also take migration names, an integer or the `:all` keyword and
perform the appropriate rollback.

### Analyzer

Lobos includes a database analyzer which use the database meta-data or
information schema to construct an abstract schema definition from an
actual database schema.

Here's an interactive session that show some possible uses:

```clojure
(use 'lobos.analyzer)
;=> nil
(-> (analyze-schema) :tables keys)
;=> (:comments :lobos_migrations :posts :users)
(-> (analyze-schema) :tables :users :columns :name :data-type :name)
;=> :varchar
(-> (analyze-schema) :tables :posts :constraints :posts_unique_title :columns)
;=> [:title]
```

## License

Distributed under the [Eclipse Public License], the same as Clojure.


[Clojure]:                http://clojure.org/
[mailing list]:           http://groups.google.com/group/lobos-library
[Clojars]:                http://clojars.org/lobos
[Leiningen]:              https://github.com/technomancy/leiningen
[Eclipse Public License]: http://budu.github.com/lobos/epl-v10.html

