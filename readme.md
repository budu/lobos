# Lobos

Lobos is a library to create and manipulate abstract SQL database
schemas and applying them to any supported RDBMS. It is based on the
[original ClojureQL] but exclude the query language part which is better
handled by the [new ClojureQL] project. It aims to add higher-level
features like declarative schema manipulation and built-in migration
support.

This is currently a very early release, use at your own risk. You can
have a look at the [roadmap] for more information about future releases
and consult the [history] to see what have been done.

## Usage

There's a quick overview of how it works in its current state below, but
take note that not much has been done yet. Lobos currently supports H2,
MySQL, PostgreSQL, SQLite and SQL Server.

First you'll need to use at least the following namespaces:
    
    (use 'lobos.connectivity
         'lobos.core
         'lobos.schema)

You'll also need to use the namespace corresponding to the database
you're using:

    (use 'lobos.backends.postgresql)

Then you'll need a connection, the following example define a test
connection and makes it the default global connection:
    
    (def db
         {:classname "org.postgresql.Driver"
          :subprotocol "postgresql"
          :user "test"
          :password "test123"
          :subname "//localhost:5432/test"})

    (open-global db)

You can send DDL statement directly to a connected database like this:

    user> (create db (table :users (integer :id :unique)))
    nil
    user> (drop db (table :users (integer :id :unique)))
    nil

Or use a schema which you'll first need to define:

    (defschema sample-schema :public)

And you also make a schema a default one:

    (set-default-schema sample-schema)

Now you can send DDL statement to the database to which the schema is
attached:

    user> (create (table :users (integer :id :unique)))
    #:lobos.schema.Schema{...}
    user> (drop (table :users))
    #:lobos.schema.Schema{...}

You can always set the debug level to see the compiled statement:

    user> (set-debug-level :sql)
    :sql
    user> (create (table :users (integer :id :unique)))
    CREATE TABLE "lobos"."users" ("id" INTEGER, CONSTRAINT "unique_id" UNIQUE ("id"))

As you can see Lobos use delimited and schema qualified identifiers.

## Installation

For now the only way to install Lobos is to clone the repository and
compile it yourself.

## License

Copyright (C) 2010 Nicolas Buduroi. All rights reserved

Distributed under the Eclipse Public License, the same as Clojure. See
the file epl-v10.html in the project root directory.

[original ClojureQL]: http://gitorious.org/clojureql
[new ClojureQL]: https://github.com/LauJensen/clojureql
[roadmap]: https://github.com/budu/lobos/blob/master/roadmap.md
[history]: https://github.com/budu/lobos/blob/master/history.md
