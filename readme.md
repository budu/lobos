# Lobos

Lobos is a library to create and manipulate abstract SQL database
schemas and applying them to any supported RDBMS. It is based on the
[original ClojureQL] but exclude the query language part which is better
handled by the [new ClojureQL] project. It aims to add higher-level
features like declarative schema manipulation and built-in migration
support.

This is currently an early release, use at your own risk. You can have a
look at the [roadmap] for more information about future releases and
consult the [history] to see what have been done.

Lobos supports H2, MySQL, PostgreSQL, SQLite and SQL Server. You'll need
to add the relevant JDBC driver manually.

## Usage

Here's a quick overview of how it works in its current state.

### Basics

First you'll need to use at least the following namespaces:
    
    (use 'lobos.connectivity
         'lobos.core
         'lobos.schema)

You'll also need to include the namespace corresponding to the database
you want to communicate with:

    (use 'lobos.backends.postgresql)

Then you'll need a connection. The following example define a test
connection and makes it the default global connection:
    
    (def db
         {:classname "org.postgresql.Driver"
          :subprotocol "postgresql"
          :user "test"
          :password "test123"
          :subname "//localhost:5432/test"})

    (open-global db)

You can send DDL statements (called actions) directly to a connected
database like this:

    user> (create db (table :users (integer :id :unique)))
    nil

You can omit the connection altogether and the actions will use the
default one.

    user> (drop (table :users (integer :id :unique)))
    nil

### More Complex Example

Lobos now supports a quite comprehensive set of features for creating
tables, here's a more complex example:

    user> (create (table :users
                    (integer :id :auto-inc :primary-key)
                    (varchar :name 100 :unique)
                    (integer :age)
                    (timestamp :created_on [:default :current_timestamp])
                    (check :old-enough (> :age 18))
                    (check :not-too-old (< :age 118))))
    
    user> (create (table :posts
                    (integer :id :auto-inc :primary-key)
                    (varchar :title 200 :unique)
                    (clob :content)
                    (timestamp :created-on [:default :current_timestamp])
                    (integer :user_id)
                    (foreign-key [:user_id] :users [:id])))

The `drop` action has the optional `behavior` parameter that works even
on database without built-in support for it:

    user> (drop sqlserver-spec (table :users) :cascade)
    nil
                    
### Schemas    

You can use a schema which you'll first need to define:

    (defschema sample-schema :public)

Then you can make that schema the default one:

    (set-default-schema sample-schema)

Now you can call actions on the database to which the schema is attached
and the actions will return the schema definition instead of nil:

    user> (create (table :users (integer :id :unique)))
    #:lobos.schema.Schema{...}
    user> (drop (table :users))
    #:lobos.schema.Schema{...}

### Debugging

You can always set the debug level to see the compiled statement:

    user> (set-debug-level :sql)
    :sql
    user> (create (table :users (integer :id :unique)))
    CREATE TABLE "lobos"."users" ("id" INTEGER, CONSTRAINT "unique_id" UNIQUE ("id"))

As you can see Lobos use delimited identifiers by default and schema
qualified identifiers when an action use a schema.

### Analyzer

Lobos includes a database analyzer which use the database meta-data to
construct an abstract schema definition from an actual database
schema. This feature is only experimental for the moment and is used
internally to update the global schema map and for integration testing.
You can try it out like that:

    user> (use 'lobos.analyzer)
    nil
    user> (analyze-schema :test)
    #:lobos.schema.Schema{...}
    user> (-> :test analyze-schema :elements :users :columns :name)
    #:lobos.schema.Column{...}
    user> (-> :test analyze-schema :elements :posts :constraints :posts_fkey_user_id)
    #:lobos.schema.ForeignKeyConstraint{...}

This feature may eventually be split into its own project and is quite
limited in its current form. Currently it doesn't support check
constraints and need a custom JDBC driver for [SQLite].

## Installation

Lobos is available through Clojars.

For the latest release, in Cake/Leiningen:

    [lobos "0.5.0"]

in Maven:

    <dependency>
      <groupId>lobos</groupId>
      <artifactId>lobos</artifactId>
      <version>0.5.0</version>
    </dependency>

## License

Copyright (C) 2010 Nicolas Buduroi. All rights reserved

Distributed under the Eclipse Public License, the same as Clojure. See
the file epl-v10.html in the project root directory.

[original ClojureQL]: http://gitorious.org/clojureql
[new ClojureQL]: https://github.com/LauJensen/clojureql
[roadmap]: https://github.com/budu/lobos/blob/master/roadmap.md
[history]: https://github.com/budu/lobos/blob/master/history.md
[SQLite]: https://github.com/budu/sqlitejdbc
