WHATCHAMACALLIT?
================

Whatchamacallit is an abstraction layer for the Erlang WX user interface library, though I like to think of it as a sanity layer.

The goal of this project is to become for WX what JQuery has become for DOM manipulations. To do so, it:

1) Hides the C-style interface to WX with a more 'Erlangy' API
2) Manages the state of WX controls, so you don't have to
3) Simplifies many common GUI tasks (creating windows, and controls)
4) Strives to build GUI controls in a declarative fashion
5) Adds support for two-way data-binding on GUI controls
6) Has a pluggable repository so you can store state where it's most convenient (memory, database, riak, something else...)
