-module(polyxena_tryout).

-export([tryout/0
        ]).

tryout() ->
    application:set_env(polyxena, pools,
                        [{pool1, [{size, 1}, {max_overflow, 0}],
                          [{hostname, "192.168.60.15"},
                           {port, 9042},
                           {keyspace, "cassaforte_keyspace"}
                          ]}]),

    application:start(polyxena),
    polyxena_sup:start_link(),

    %% polyxena:execute_cql(pool1, "DROP KEYSPACE \"cassaforte_keyspace\";"),
    %% polyxena:execute_cql(pool1, "CREATE KEYSPACE \"cassaforte_keyspace\" WITH replication = {'class' : 'SimpleStrategy', 'replication_factor' : 1};"),
    polyxena:execute_cql(pool1, "USE \"cassaforte_keyspace\";"),
    %% polyxena:execute_cql(pool1, "CREATE TABLE \"test_uuid\" (pk int, f uuid, PRIMARY KEY (pk));"),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_uuid\" (pk, f) VALUES (1, f412e400-c445-1131-bdc6-03f9e757eb34);"),
    polyxena:execute_cql(pool1, "INSERT INTO \"test_uuid\" (pk, f) VALUES (2, f412e400-c445-1131-bdc6-15f9e757eb34);"),
    polyxena:execute_cql(pool1, "SELECT * FROM test_uuid;").


    %% polyxena:execute_cql(pool1, "DROP KEYSPACE \"cassaforte_keyspace\";").

    %% execute_cql(pool1, "CREATE KEYSPACE \"cassaforte_keyspace\" WITH replication = {'class' : 'SimpleStrategy', 'replication_factor' : 1};"),

%% polyxena_tryout:tryout().


    %% polyxena:execute_cql(pool1, "CREATE TABLE \"users\" (age int, name varchar, PRIMARY KEY (name));"),
    %% polyxena:execute_cql(pool1, "INSERT INTO \"users\" (name, age) VALUES ('Alex', 19);"),
    %% polyxena:execute_cql(pool1, "INSERT INTO \"users\" (name, age) VALUES ('Alex2', 20);"),
    %% polyxena:execute_cql(pool1, "INSERT INTO \"users\" (name, age) VALUES ('Alex3', 25);"),
    %% polyxena:execute_cql(pool1, "SELECT * from users").
