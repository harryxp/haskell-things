Useful links:

    https://docs.haskellstack.org/en/stable/README/#quick-start-guide
    https://docs.haskellstack.org/en/stable/ghcjs/

Standalone:

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
