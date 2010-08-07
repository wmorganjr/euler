# euler

These are my solutions to the [Project Euler](http://projecteuler.net)
problems. This project has several goals:

1. Help me master [VimClojure](http://kotka.de/projects/clojure/vimclojure.html)
2. Explore the use of the ->> (thread-last) macro for enhancing readability.
The ->> macro forces me to keep my functions short, and reenforces the
representation of a function as a transformation from inputs to outputs, which
is an appropriate metaphor in many cases.
3. Explore clojure.test, especially the with-test macro, which puts tests and
code together, helping me to develop a more test-driven style.

## Usage

    roo:euler wmorgan$ lein repl
    user=> (use 'euler.problems)
    nil
    user=> (problem-1)
    "Elapsed time: 16.784 msecs"
    233168
    user=> (problem-12)
    "Elapsed time: 1415.429 msecs"
    76576500

## License

Copyright (C) 2010 Will Morgan

Distributed under the Eclipse Public License, the same as Clojure.
