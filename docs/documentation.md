
* toc
{:toc}

**Lobos** is divided into some well defined concepts that we'll explore
in more details here.

First and foremost, there's the *abstract schema* which is a
representation of an actual schema, it is meant to be database
agnostic. This construct is in fact just a set of nested maps. An
abstract schema is composed of *[elements]* (tables, columns, etc.) of
which *[typed columns]* need special attention. Then there's the
*[actions]* that apply changes to a schema. Finally we'll describe the
*[migration commands]* that help you organize the modifications you want
to apply.

After seeing those concepts, we'll delve into some tangential
considerations like *[connectivity]* and the *[analyzer]*.

## Elements

All elements are defined in the `lobos.schema` namespace.

### Schema Element

The schema element act as the root element of an abstract schema. It can
be built using the `schema` function that takes a `schema-name`, an
optional map of `options` and some `elements`. For now, only *[tables]* and
*[indexes]* are supported.

```clojure
(schema :foo)
;=> #:lobos.schema.Schema{:sname :foo, :tables {}, :indexes {}, :options {}}
```

The only option supported is `:db-spec` which is used internally to pass
around the connection information.

### Table Element

As you'd expect, the table element is the central piece of this
library. There's a macro, called `table`, and a function called `table*`
that let you create one.

The `table*` function is similar to `schema`, but the given sub-elements
must be disjoined.

```clojure
(table* :foo)
;=> #:lobos.schema.Table{:name :foo, :columns {}, :constraints {}, :indexes {}}
(table* :foo (column* :bar ...))
;=> #:lobos.schema.Table{:name :foo, :columns {:bar ...}, ...}
```

The macro is quite simple, it takes a `name` and some `elements` which
can be either columns, constraints or indexes in any order.

```clojure
(table :foo)
;=> #:lobos.schema.Table{:name :foo, :columns {}, :constraints {}, :indexes {}}
(table :foo (column :bar ...))
;=> #:lobos.schema.Table{:name :foo, :columns {:bar ...}, ...}
```

Let's take a look at it's source code as this could be useful if you
want to create your own table macro.

```clojure
(defmacro table
  [name & elements]
  `(-> (table* ~name) ~@(reverse elements)))
```

As you see, it just call the `table*` function while threading the table
sub-elements, which are functions taking a table as their first
argument. The elements are being reversed as the columns are conj into a
`PersistentArrayMap` which put new columns at the beginning.

When creating your own table macro, you must take care of assembling the
elements in the order that you want and remember to reverse them. For
example if you need a macro to create tables in which the first column
is always `bar` and the last one should be `baz` you must write
something like this:

```clojure
(defmacro table
  [name & elements]
  `(-> (table* ~name)
       (column :baz)
       ~@(reverse elements))
       (column :bar))
```

### Index Element

Indexes can be created like table. They must evidently refer to a
specific *[column]* inside an existing table. The `index` function is
rather simple it must take at least a `table` name and a sequence of
column names:

```clojure
(index :users [:name])
;=> #lobos.schema.Index{:iname :users_index_name, :tname :users, :columns [:name], :options nil}
```

In this case the index name will be automatically created by joining the
table name, the word 'index' (or 'unique') and the column names with
underscores. To choose the `name` of the index or to provide some
`options` (only the `:unique` option is recognized for now) you can call
it like that:

```clojure
(index :users nil [:name])
;=> #lobos.schema.Index{:iname :users_index_name, :tname :users, :columns [:name], :options nil}
```

As you see, providing nil as name will let the function create the index
name like in the first example.

### Constraint Element

Before explaining how to define columns, we'll go over how to create
constraints. There's three kind of constraint: unique, foreign key and
check.

Unique constraints comes in two flavor. For normal unique constraints,
use the `unique` function, which takes a `table`, an optional name and a
sequence of `column-names`.

```clojure
(unique (table :users) [:name])
;=> #lobos.schema.Table{:name :users, :columns {}, :constraints {:users_unique_name #lobos.schema.UniqueConstraint{:cname :users_unique_name, :ctype :unique, :columns [:name]}}, :indexes {}}
```

Then there's the `primary-key` function which identical apart from the
fact that only one can be added per tables.

```clojure
(primary-key (table :users) [:id])
;=> #lobos.schema.Table{:name :users, :columns {}, :constraints {:users_primary_key_id #lobos.schema.UniqueConstraint{:cname :users_primary_key_id, :ctype :primary-key, :columns [:id]}}, :indexes {}}
```

The associated index name can be specified just before the column names
for both of them. When not included, the name will be generated by the
same rules as the `index` function described previously.

### Column Element

Tables would be useless without columns! Columns definition strive to be
database agnostic, so you'll have to learn a bit about them and how they
map to your favorite target RDBMS. This section is more focused on the
internals of **Lobos** and you can safely skip it as the
*[typed columns]* are the preferred way of defining columns.

There's two main functions to define columns: `column*` and
`column`. The former parsing the column specific options and the later
extracting the possible constraints that could be embedded into
them. Another difference is that `column*` will simple create a column
element while `column` is meant to be used in combination with a table
element.

The `column*` function takes a `column-name`, a `data-type` and a
sequence of `options` and produce an abstract column element.

```clojure
(column* :foo nil nil)
;=> #lobos.schema.Column{:cname :foo, :data-type nil, :default nil, :auto-inc false, :not-null false, :others []}
```

It ain't much useful in itself, so before going through the `column`
function, let's see how data types are being represented. There's a
`data-type` function taking a `name`, a sequence of `args` and a map of
`options`.

```clojure
(data-type :numeric [8 3])
;=> #lobos.schema.DataType{:name :numeric, :args [8 3], :options {}}
```

The options include `encoding`, `collate` and `time-zone`. Only the
later is currently supported by some of the backends.

The `column` function provides a generic interface to create any columns
of any data type. It takes a `table` to which it'll be attached a
`column-name`, an optional `data-type` and a sequence of `options`. It
returns the given table with the newly added column.

```clojure
(column (table :users) :name (data-type :varchar [100]) :not-null)
;=> #lobos.schema.Table{:name :users, :columns {:name #lobos.schema.Column{:cname :name, :data-type #lobos.schema.DataType{:name :varchar, :args [100], :options {}}, :default nil, :auto-inc false, :not-null true, :others []}}, :constraints {}, :indexes {}}
```

Here's a list of available options:

 * `:unique` which construct an unique constraint on that column
 * `:primary-key` which make the current column the primary key
 * `[:refer tname & options]` which add a foreign key constraint to
   the specified table. The options are the same as the `foreign-key`
   function with the exception that you can specify only one parent
   column.
 * `:not-null` prevents this column from being null
 * `:auto-inc` (for integers types) which makes it auto-populated with
   incremented integers
 * `[:encoding enc]` (for character types) determines which encoding to
   use if supported by the database. Also see the natianal character types.
 * `[:collate type]` (for character types) determines how equality is
   handled
 * `:time-zone` (for time types) determines if the type includes a time-zone

## Typed Columns

TODO

## Actions

TODO

## Migration Commands

TODO

## Connectivity

TODO

## Analyzer

TODO


[actions]:            #actions
[elements]:           #elements
[tables]:             #table-element
[typed columns]:      #typed-columns
[migration commands]: #migration-commands
[connectivity]:       #connectivity
[analyzer]:           #analyzer
