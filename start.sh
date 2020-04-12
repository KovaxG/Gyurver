#!/bin/bash

# Making settings.txt
ip route get 1 | awk '{print $(NF-2);exit}' >> settings.txt

# Elm stuff
pushd Frontend/Cokkolo
pwd | echo
elm make src/Main.elm --output ../../Content/addegg.html
