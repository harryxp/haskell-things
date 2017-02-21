Mortgage Calculator in Haskell.

This demonstrates how a command line interface and a web interface can share
the same Haskell library.

See [ghcjs-playground](https://github.com/harryxp/ghcjs-playground) for general
instructions for setting up a `ghcjs` project.

Standalone (command line):

    cd standalone
    stack setup

    stack build
    stack exec MortgageCalc
    stack exec MortgageCalc -- -v 400000 4.5 30
    stack exec MortgageCalc -- 400000 4.5 30

Web:

    cd web
    stack setup

    stack build
    open index.html     # generated JS files are referenced by index.html

TODO:

* There's a small difference between results calculated by the two versions.  Find out why.
* Move common library code to a proper place.

