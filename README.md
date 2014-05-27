# Polyxena

Prolyxena is an Erlang Cassandra CQL driver.

# Project purpose and maturity

Polyxena was created __for learning purposes__, there is no intention to compete with
existing libraries, even though there are plans to make it rock and keep the spirit
of [Clojurewerkz](http://clojurewerkz.org) and make it feature-complete, up-to-date, 
with a sane API and docs.

# Differences from existing libs

Main difference is a bit clearer protocol consumption API, although that may be me 
knowing Erlang not well enough and not fully understanding all the crazy things people
are doing in their libs.

Next difference is that Polyxena is using [poolboy](https://github.com/devinus/poolboy)
for connection pooling. I haven't investigated every single existing library, but
I haven't seen one that uses poolboy.

# Example usage

```erlang
application:set_env(polyxena, pools,
                    [{pool1, [{size, 10}, {max_overflow, 0}],
                      [{hostname, "192.168.60.15"},
                       {port, 9042},
                       {keyspace, "polyxena"}
                      ]}]),

application:start(polyxena),
polyxena_sup:start_link(),

polyxena:execute_cql(pool1, "USE \"polyxena\";"),
polyxena:execute_cql(pool1, "CREATE TABLE \"users\" (age int, name varchar, PRIMARY KEY (name));"),
polyxena:execute_cql(pool1, "INSERT INTO \"users\" (name, age) VALUES ('Alex', 19);"),
polyxena:execute_cql(pool1, "INSERT INTO \"users\" (name, age) VALUES ('Alex2', 20);"),
polyxena:execute_cql(pool1, "INSERT INTO \"users\" (name, age) VALUES ('Alex3', 25);"),
polyxena:execute_cql(pool1, "SELECT * from users").
```

## Copyright

Copyright (c) Alex Petrov

Licensed under the Apache Public License 2.0.
