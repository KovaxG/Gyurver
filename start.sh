#!/bin/bash

# Elm stuff
pushd Frontend/Cokkolo
pwd | echo
elm make src/CokkList.elm --output ../../Content/egglist.html --optimize
elm make src/Eredmenyek.elm --output ../../Content/eredmenyek.html --optimize