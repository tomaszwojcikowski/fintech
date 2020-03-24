fintech
=====

A Cowboy OTP application

Build
-----

    $ rebar3 compile
    $ rebar3 rel

Test
-----

    $ make mysql
    $ rebar3 ct

Endpoints
------

 * http://localhost:8080/list - List past transactions

    Output:
    ```json
        [{ 
            "id": "transaction-id", 
            "from": "account-id-from", 
            "to": "account-id-to", 
            "amount": 123,
			"created_at": "utc-date"
        },
        ...
        ]
    ```
 
 * http://localhost:8080/pending - List pending transactions

    Output:
    ```json
        [{ 
            "id": "transaction-id", 
            "from": "account-id-from", 
            "to": "account-id-to", 
            "amount": 123
        },
        ...
        ]
    ```
  * http://localhost:8080/new - Create transaction

    Output:
    ```json
        [{ 
            "from": "account-id-from", 
            "to": "account-id-to", 
            "amount": 123
        },
        ...
        ]
    ```

Options
------

    ```erlang
    [{mysql, [
            {host, "localhost"}, {port, 33061}, 
            {user, "fintech"}, {password, "fintech"}, 
            {database, "fintech"},
            {auth_plugin, "mysql_native_password"},
            {transactions_timeout, 10000},
            {query_timeout, 3000}
        ]},
    {accounts_file, "accounts.json"}]
    ```
