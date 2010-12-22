# Lobos

Lobos is a library to create and manipulate abstract SQL database
schemas and applying them to any supported RDBMS. It is based on the
[original ClojureQL] but exclude the query language part which is better
handled by the [new ClojureQL] project.

This is currently a very early release, use at your own risk. Have a
look at the [roadmap] for more information about future releases.

## Usage

There's a quick overview of how it works in its current state below, but
take note that not much has been done yet. There's only one back-end
based on the SQL Standard for now, it's the default one used by the
compiler when there's no connection specified. Look at the code to learn
more.

First you'll need to use at least the following namespaces:
    
    (use 'lobos.connectivity
         'lobos.core
         'lobos.schema)

Then you'll need a connection, the following example define a test
connection and use it as the default global connection:
    
    (def db
         {:classname "org.postgresql.Driver"
          :subprotocol "postgresql"
          :user "test"
          :password "test123"
          :subname "//localhost:5432/test"})

    (open-global db)

We now must define which schema we'd like to work on:

    (defschema sample-schema :public)

And we can make it the default schema like that:

    (set-default-schema sample-schema)

Finally you can test the available actions (only create and drop tables
for now) to manipulate a schema:

    user> (create nil (table :users (integer :id :unique)))
    #:lobos.schema.Schema{...}
    user> (drop nil (table :users))
    #:lobos.schema.Schema{...}

You can always use the debug action to see the compiled statement:

    user> (debug build-create-statement (table :users (integer :id :unique)))
    CREATE TABLE users (id INTEGER, UNIQUE (id))

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
