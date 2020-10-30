#!/bin/bash

# Remove previous release
rm -r Release

# Folder structure
mkdir Release
pushd Release
mkdir Content
mkdir Data
popd

# Move favicon
cp Content/favicon.ico Release/Content/favicon.ico
cp -r Content/pdfs Release/Content/pdfs


# Elm stuff
pushd Frontend
elm make src/Main.elm --output ../Release/Content/index.html
popd

# Haskell stuff
ghc Main.hs
mv ./Main Release/Gyurver

