#!/bin/bash
cabal install --only-dependencies
cabal build
./dist/build/nkmd-as/nkmd-as < cat.S
