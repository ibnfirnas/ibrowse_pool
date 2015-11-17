ibrowse_pool
============

An alternative front-end to `ibrowse`, which allows running multiple instances.

Motivation
----------

ibrowse automatically creates a connection pool for every host/port pair, but
offers no way to create multiple pools for the same host/port pair (say if you
want to use different pipelining settings for different parts of the API).

Usage example
-------------

```erlang
$ erl -pa deps/*/ebin -pa ebin
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.1  (abort with ^G)
1>
1> application:ensure_all_started(ibrowse_pool).
{ok,[crypto,asn1,public_key,ssl,ibrowse,ibrowse_pool]}
1>
2>
2> ibrowse_pool:start_supervised(ibrowse_pool_spec:of_pairs([{name, pool_a}])).
{ok,<0.57.0>}
3>
3> ibrowse_pool:send_req(pool_a, "https://httpbin.org/ip", [], get, "", 5000).
{ok,"200",
    [{"Server","nginx"},
     {"Date","Tue, 03 Nov 2015 19:43:24 GMT"},
     {"Content-Type","application/json"},
     {"Content-Length","32"},
     {"Connection","keep-alive"},
     {"Access-Control-Allow-Origin","*"},
     {"Access-Control-Allow-Credentials","true"}],
    "{\n  \"origin\": \"206.160.140.2\"\n}\n"}
5>
```
