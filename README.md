# BRDF - Bitcoin chain data represented as RDF

[agraph-examples]: https://github.com/franzinc/agraph-examples/tree/master/data/bitcoin
[agraph-installation]: https://franz.com/agraph/support/documentation/current/server-installation.html

This example demonstrates the use of BP library to import Bitcoin
chain data into an RDF triple store, specifically Franz Inc.'s
AllegroGraph, which is written in Common Lisp and provides a
high-performance direct Common Lisp interface.

Note that direct Common Lisp AllegroGraph interface only works on
AllegroCL, so the same limitation applies to the CL source code in
this example (`brdf.lisp`).

This is an extension of the example [here][agraph-examples] that was
originally written in Python.



## Setup

First we need to install AllegroGraph. The instructions on how to do
this can be found [here][agraph-installation]. Once AllegroGraph is
installed, we can start the server by running

    <path/to/AG>/bin/agraph-control --config <path/to/AG>/lib/agraph.cfg start

and stop it when necessary by running

    <path/to/AG>/bin/agraph-control --config <path/to/AG>/lib/agraph.cfg stop

Next, we need to get the direct Common Lisp client for
AllegroGraph. It is distributed as a `.fasl` file and can be
downloaded from the same place as the AllegroGraph server.



## Supported graph types

This tool will eventually support several types of Bitcoin data
graphs, but currently only one is available:
  - `chain` - a complete representation of Bitcoin time chain.

Graph is the the first argument to the function `brdf:start-load`,
described below.



## Running the loader

In order to start the loader, run

``` lisp
(brdf:start-load :chain
                 "http://user:password@127.0.0.1:8332"
                 "http://user:password@127.0.0.1:10035/repositories/brdf"
                 :workers 4 :cleanp t)
```

You should see a loader log that looks similar to this:

``` lisp
[2021-09-21T20:44:40] Blocks: 0/701589, txs: 0/672153493, progress: 0.00000
[2021-09-21T20:44:50] Blocks: 1494/701589, txs: 1520/672153493, progress: 0.00000
[2021-09-21T20:45:00] Blocks: 3068/701589, txs: 3118/672153493, progress: 0.00000
...
```

In order to stop the loader, run

``` lisp
(brdf:stop-load)
```

Once the function call above returns, all the workers have been
stopped and no data will be added to the repository.

In order to continue the load, run the same form but make sure the
value of `cleanp` argument is `nil`:

``` lisp
(brdf:start-load :chain
                 "http://user:password@127.0.0.1:8332"
                 "http://user:password@127.0.0.1:10035/repositories/brdf"
                 :workers 4)
```

By default, the status of the load will be checked every 10
seconds. As the load continues, it might make sense to change the
status check period to a larger value (e.g. 1 hour) by evaluating

``` lisp
(setf brdf:*status-check-period* 3600)
```

since it is expected to take anywhere from several days to several
weeks on an average home machine.
