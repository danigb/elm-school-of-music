mkdir -p tmp && elm-make tests/Checks.elm --output tmp/runchecks.js && node tmp/runchecks.js
