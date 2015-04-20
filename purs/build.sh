#!/usr/bin/env sh

COMMAND=$1

set -eu

PATH=.cabal-sandbox/bin:node_modules/.bin:$PATH

command -v pulp >/dev/null 2>&1 || { npm install pulp; }

pulp dep update --quiet

if [ "$COMMAND" = "watch" ]
then
  GOPTS="--watch"
  OPTS=""
else
  GOPTS=""
  OPTS="--optimise"
fi

pulp $GOPTS browserify \
  --main Naggy \
  --build-path dist \
  --to ../js/naggy.js \
  $OPTS
