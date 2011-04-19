# Lobos History

## 0.7

 * Add support for creating indexes
 * Improved compiler support for value expression
 * Improved the analyzer to be able to use database specific information
   schema
 * Removed the global schema map

## 0.6

 * Support for altering tables
 * Improved the schema creation process
 * Added the refer column option
 * Added text and ntext aliases
 * Major refactoring of integration tests
 * Added system testing with only one test for now 

## 0.5

 * Added remaining scalar data-types
 * Added support for foreign key and check constraints
 * Rewrote part of the analyzer as a multi-method
 * Added drop cascade support for SQL Server

## 0.4

 * Added a comprehensive set of data types
 * Improve integration testing
 * Minor compiler improvements
 * Implementation of some more backends (MySQL, SQLite and SQL Server)
 * Bug fixing

## 0.3

 * Unit tests for compiler, connectivity and schema namespaces
 * Basic integration testing for all backends
 * Implementation of the H2 backend
 * Lots of issues fixed, API improved

## 0.2

 * Implementation of the PostgreSQL backend
 * Support for creating and dropping schemas
 * Support for schema-scoped table operations
 * Support for auto-inc column option

## 0.1

 * Basic implementation of the compiler, ast, schema, analyzer,
   connectivity and core namespaces
 * Only default SQL Standard backend
 * Support for creating and dropping tables
 * Support for integer, timestamp and varchar data-types
 * Support for primary key and unique constraints
 * Support for not null column option and basic default clause support
