This library deals with computing a function’s inverse. This is, of course, not
possible in general, so the applicability of this library comes with some caveats:

  * The function’s domain must be enumerable, and preferably rather small. We
    provide a few suggestions and utilities for how to enumerate the domain.

  * The function’s codomain must belong to the `Eq` class. An `Ord` or `Hashable`
    instance is also nice, to accommodate a data structure for efficient lookups.

  * The functions for inverting injections, surjections, and bijections require
    some care to use correctly, because the library does not verify these
    properties.

The main purpose of this library is to provide documentation and convenience.
It does not contain a great quantity of code, so a user hesitant to incur a
dependency on the package might well choose only to read and borrow its techniques.
