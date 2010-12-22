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

Here's how to define a schema:

    (use 'lobos.core
         'lobos.schema)

    (defschema sample-schema :public nil
      (table :users
             (integer :id :unique)
             (varchar :name 100 [:default "joe"])))

Then you can build a create statement for that schema. To have a peek at
the resulting SQL output, you can use the debug function:
    
    user> (debug create (sample-schema))
    CREATE SCHEMA public 
    
    CREATE TABLE users (id INTEGER, name VARCHAR(100) DEFAULT 'joe', UNIQUE (id))
    nil

To send that compiled statement to a database you first need to define a
connection. Here's how to define a global one:

    (use 'lobos.connectivity)
    
    (def db
         {:classname "org.postgresql.Driver"
          :subprotocol "postgresql"
          :user "test"
          :password "test123"
          :subname "//localhost:5432/test"})
    
    (open-global db)

Using `open-global` without connection name defines a default
connection. Now you can execute the create statement like this:

    user> (execute (create (sample-schema) nil))
    nil

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
