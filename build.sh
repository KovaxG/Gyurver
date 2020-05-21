#!/bin/bash

# Elm stuff
pushd Frontend
pwd | echo
elm make src/Main.elm --output ../Content/main.html
