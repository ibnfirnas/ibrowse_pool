ibrowse_pool
============

An alternative front-end to `ibrowse`, which allows running multiple instances.

Motivation
----------

ibrowse automatically creates a connection pool for every host/port pair, but
offers no way to create multiple pools for the same host/port pair (say if you
want to use different pipelining settings for different parts of the API).
