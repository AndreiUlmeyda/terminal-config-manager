#!/bin/bash

set -e

stack exec ormolu -- --mode inplace $(fd '\.hs$' src test)
stack exec hlint -- src/ test/
stack build --ghc-options="-Wall -Werror" --test
test/e2e/test-tui.exp