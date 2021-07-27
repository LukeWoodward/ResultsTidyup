@echo off

echo Results Tidyup release build

if not exist release mkdir release

rmdir /s /q elm-stuff

REM Compile ResultsTidyup in production mode.
elm make --optimize src/ResultsTidyup.elm  --output results-tidyup.js
uglifyjs results-tidyup.js --compress "pure_funcs='F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9',pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=release\results-tidyup.min.js

copy /y index-release.html release\index.html
copy /y styles.css release\
copy /y bootstrap.min.css release\
copy /y jquery-3.6.0.min.js release\
copy /y about.html release\about.html
