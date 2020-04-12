#!/bin/bash

# Elm stuff
pushd Frontend/Cokkolo
pwd | echo
elm make src/Main.elm --output ../../Content/addegg.html
elm make src/CokkList.elm --output ../../Content/egglist.html
