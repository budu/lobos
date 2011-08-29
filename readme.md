# Lobos

**Lobos** is a library to create and manipulate abstract SQL database
schemas and applying them to any supported RDBMS that support a flexible
migrations mechanism. It is based on the original ClojureQL but exclude
the query language part which is better handled by the [new ClojureQL]
project.

This is currently an early release, use it at your own risk. You can
have a look at the [roadmap] for more information about future releases
and consult the [history] to see what have been done.

**Lobos** supports H2, MySQL, PostgreSQL, SQLite and SQL Server. You'll
need to add the relevant JDBC driver manually. For SQLite you'll need a
custom [SQLite JDBC driver].

You may also be interested in joining the [Lobos Google Group].

## Usage

Here's a quick overview of how it works in its current state. For more
in depth documentation you can refer to the [commented code] for the
front-end.

### Basics

First you'll need to use at least the following namespaces:
    
    (use 'lobos.connectivity
         'lobos.core
         'lobos.schema)

Then you'll need a connection. The following example define a connection
and makes it the default global connection:
    
    (def db
         {:classname "org.postgresql.Driver"
          :subprotocol "postgresql"
          :user "test"
          :password "test123"
          :subname "//localhost:5432/test"})

    (open-global db)

You can send DDL statements (called *actions*) directly to a connected
database like this:

    user> (create db (table :users (integer :id :unique)))
    nil

You can omit the connection altogether. In that case, actions will use
the connection bound by `with-connection` or the default one.

    user> (drop (table :users))
    nil

### More Complex Example

**Lobos** supports a comprehensive set of features for creating tables.
Here's a more complex example using custom helpers to define a complete
schema:

    (ns sample-schema
      (:refer-clojure :exclude [alter compile drop
                                bigint boolean char double float time])
      (:use (lobos core schema)))
    
    (defn surrogate-key [table]
      (integer table :id :auto-inc :primary-key))
    
    (defn datetime-tracked [table]
      (-> table
          (timestamp :updated_on)
          (timestamp :created_on (default (now)))))
    
    (defn refer-to [table ptable]
      (let [cname (-> (->> ptable name butlast (apply str))
                      (str "_id")
                      keyword)]
        (integer table cname [:refer ptable :id :on-delete :set-null])))
    
    (defmacro tbl [name & elements]
      `(-> (table ~name
             (surrogate-key)
             (datetime-tracked))
           ~@elements))
    
    (def sample-schema
      (schema :lobos
       (tbl :users
        (varchar :name 100 :unique)
        (check :name (> (length :name) 1)))
    
       (tbl :posts
        (varchar :title 200 :unique)
        (text :content)
        (refer-to :users))
    
       (tbl :comments
        (text :content)
        (refer-to :users)
        (refer-to :posts))))

Then you can use the `create` action to create that schema:

    user> (use 'sample-schema)
    nil
    user> (create sample-schema)
    nil

### Altering Tables

There's also the `alter` action which let you manipulate tables:

    user> (alter :add (table :users (text :about-me)))
    nil
    user> (alter :add (table :users
                        (text :location)
                        (text :occupation)))
    nil
    user> (alter :add (table :comments (check :comment-limit (< (length :content) 144))))
    nil
    user> (alter :modify (table :users (column :location (default "Somewhere"))))
    nil
    user> (alter :drop (table :users (column :occupation)))
    nil
    user> (alter :rename (table :users (column :location :to :origin)))
    nil

### Dropping Schema Elements

The `drop` action has the optional `behavior` parameter that works even
on database without built-in support for it:

    user> (drop sqlserver-spec (table :users) :cascade)
    nil

### Debugging

You can always set the debug level to see the compiled statement:

    user> (set-debug-level :sql)
    :sql
    user> (create (table :users (integer :id :unique)))
    CREATE TABLE "lobos"."users" ("id" INTEGER, CONSTRAINT "unique_id" UNIQUE ("id"))
    nil

As you can see **Lobos** use delimited identifiers by default and schema
qualified identifiers when an action use a schema.

### Configuration

The preferred location to keep your database specs and connection for
**Lobos** is in the `src/lobos/config.clj` file. The namespace contained
in it will be used by default when generating the migrations file. This
is just a convention, you can use any other means you wish using normal
Clojure code.

### Migrations

#### Generating migrations

**Lobos** records each actions and include a set of commands to create
migrations, run and rollbacks them. When you execute an action, the
call used will be recorded inside a stash file found in
`.lobos_stash.clj`.

    user> (create (table :users (integer :id)))
    nil
    user> (print-stash)

    (create (table :users (integer :id)))

Recorded actions can be dumped into one or more migrations using the
`generate-migration` command:

    user> (generate-migration 'create-users)
    nil

This command will create a migration definition into the migration file
(located in `src/lobos/migrations.clj` by default) for all action found
in the stash file.

You can always use `generate-migration` to create a new (empty)
migration if there's no recorded actions and edit them directly from the
migrations file.

The `generate-migration` command will automatically generate the undo
method for some actions.

#### Migrate & Rollback

Then you can use the `migrate` and `rollback` commands:

    user> (rollback)
    create-users
    user> (migrate)
    create-users

These commands will run the `up` or `down` part of migrations in the
correct order. Both can take any number of migration names and will only
run these. By default the `migrate` command run all pending migrations
while the `rollback` command only affect the last one.

The `rollback` command can also take a number or `:all` as argument, in
which case it will rollback only the number of migrations that you want.
    
There's also three print commands to look where you are into the
migration process: `print-stash`, `print-pending` and `print-done`.

### Analyzer

Lobos includes a database analyzer which use the database meta-data or
information schema to construct an abstract schema definition from an
actual database schema. This feature is only experimental for the
moment and is used internally integration testing.

    user> (use 'lobos.analyzer)
    nil
    user> (analyze-schema :test)
    #:lobos.schema.Schema{...}
    user> (-> :test analyze-schema :elements :users :columns :name)
    #:lobos.schema.Column{...}
    user> (-> :test analyze-schema :elements :posts :constraints :posts_fkey_user_id)
    #:lobos.schema.ForeignKeyConstraint{...}

This feature may eventually be split into its own project and is quite
limited in its current form. Currently it doesn't support check or
foreign keys constraints and has *very* limited support for parsing SQL
expressions.

## Installation

Lobos is available through Clojars.

For the latest release, in Cake/Leiningen:

    [lobos "0.8.0"]

in Maven:

    <dependency>
      <groupId>lobos</groupId>
      <artifactId>lobos</artifactId>
      <version>0.8.0</version>
    </dependency>

## License

Copyright (C) 2011 Nicolas Buduroi. All rights reserved

Distributed under the Eclipse Public License, the same as Clojure. See
the file epl-v10.html in the project root directory.

[new ClojureQL]: https://github.com/LauJensen/clojureql
[roadmap]: https://github.com/budu/lobos/blob/master/roadmap.md
[history]: https://github.com/budu/lobos/blob/master/history.md
[SQLite JDBC driver]: https://github.com/budu/sqlitejdbc
[commented code]: http://budu.github.com/lobos/doc/uberdoc.frontend.html
[Lobos Google Group]: http://groups.google.com/group/lobos-library
