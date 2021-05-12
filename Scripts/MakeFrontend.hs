module Scripts.MakeFrontend where

import qualified System.Process as Proc
import qualified System.Directory as Dir

main :: IO ()
main = do
  putStrLn "Making Frontend..."
  Dir.setCurrentDirectory "Frontend"
  Proc.callCommand "elm make src/Main.elm --output=../Content/index.html --optimize"

  putStrLn "Minifying Code..."
  Dir.setCurrentDirectory "../Content"
  contents <- readFile "index.html"
  let (makeFile, js) = splitFile contents
  writeFile "elm.js" js
  Proc.callCommand "uglifyjs elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output elm.min.js"
  minifiedJs <- readFile "elm.min.js"
  writeFile "index.html" $ makeFile minifiedJs

  putStrLn "Removing files..."
  Dir.removeFile "elm.js"
  Dir.removeFile "elm.min.js"

  putStrLn "Done."

splitFile :: String -> (String -> String, String)
splitFile contents =
  let (htmlFileStart, jsWithHTMLEnding) = span (/="<script>") $ lines contents
      htmlUntilOpeningScriptTag = htmlFileStart ++ [head jsWithHTMLEnding]
      (htmlFileEnd, jsWithScriptTags) = span (/= "</script>") $ reverse jsWithHTMLEnding
      htmlClosingScriptTagToEnd = head jsWithScriptTags : reverse htmlFileEnd
  in ( \js -> unlines $ htmlUntilOpeningScriptTag ++ js : htmlClosingScriptTagToEnd
     , unlines $ init $ tail $ reverse jsWithScriptTags
     )
